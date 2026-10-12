package cleaner

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.UUID
import scala.util.control.NonFatal

class QuarantineService(config: AppConfig, state: StateService, history: HistoryService) {
  private val matcher = new RuleMatcher(config)
  // объект хранится в карантине под уникальным номером записи
  private def stored(e: QuarantineEntry): Path = config.quarantine.resolve(e.id)
  // добавляем указанное количество дней к начальному времени
  private def deadline(start: Long, days: Int): Long = Math.addExact(start, Math.multiplyExact(days.toLong, 86400000L))
  // берём записи с завершённым помещением в карантин
  def active: List[QuarantineEntry] = state.entries.filter(_.status == "active")
  def expired: List[QuarantineEntry] = active.filter(_.expiresAt <= System.currentTimeMillis())
  def volume: Long = state.entries.map(_.size).sum

  def find(id: String): QuarantineEntry = {
    state.entries.find(_.id == id).getOrElse(throw new IllegalArgumentException("Запись не найдена"))
  }

  // показываем записи карантина и предупреждения о незавершённых операциях
  def show(): Unit = {
    if (state.entries.isEmpty) println("Подтверждённых записей нет.")
    for (e <- state.entries) {
      println(e.id + " | " + e.name + " | " + e.size + " байт | до " +
        Instant.ofEpochMilli(e.expiresAt) + " | " + e.status + " | " + e.original)
      if (!FileService.exists(stored(e))) println("  Внимание: данные записи отсутствуют.")
      if (e.status != "active") println("  Незавершённая операция. Данные оставлены для ручной проверки.")
    }
    for (p <- FileService.children(config.quarantine)) {
      if (!state.entries.exists(_.id == p.getFileName.toString)) {
        println("НЕПОДТВЕРЖДЁННЫЙ объект: " + p + " — оставлен без изменений")
      }
    }
  }
  // помещаем один объект в карантин и создаём запись для восстановления

  private def put(path: Path, root: Path, info: FileInfo, days: Int,
                  rule: String, category: String): QuarantineEntry = {
    require(days >= 0, "Срок должен быть >= 0")
    require(Files.isWritable(path.getParent), "Нет права перемещения")
    require(Files.getFileStore(config.quarantine).getUsableSpace >= info.size, "Недостаточно места в карантине")
    val now = System.currentTimeMillis()
    val entry = QuarantineEntry(UUID.randomUUID().toString, path, root, path.getFileName.toString,
      info.isDirectory, info.size, now, days, deadline(now, days), rule, category,
      info.fingerprint, "pending", "")
    state.update(entry)
    FileService.move(path, stored(entry))
    val movedInfo = FileService.info(stored(entry))
    require(!FileService.exists(path) && movedInfo == info, "Перемещение не прошло проверку")
    val completed = entry.copy(status = "active")
    state.update(completed)
    require(PropertyFiles.list(PropertyFiles.read(config.stateFile), "entries").contains(entry.id),
      "Запись не найдена в состоянии")
    completed
  }

  // повторно проверяем выбранные объекты и помещаем каждый в карантин
  def quarantine(candidates: List[Candidate], daysOverride: Int, foundCount: Int = -1): Unit = {
    var moved = 0
    var bytes = 0L
    var errors = 0
    // один номер связывает все события этой очистки
    val operationId = UUID.randomUUID().toString
    val totalFound = if (foundCount >= 0) foundCount else candidates.size
    val details = "roots=" + candidates.map(_.root.id).distinct.mkString(",") +
      "; profiles=" + candidates.map(_.profile).distinct.mkString(",") +
      "; found=" + totalFound +
      "; selectedCount=" + candidates.size +
      "; selected=" + candidates.map(c => c.id.toString + ":" + history.path(c)).mkString(",")
    history.record("QUARANTINE_START", details, operationId = operationId)
    for (c <- candidates) {
      try {
        history.record("RECHECK", history.path(c), operationId = operationId)
        val current = FileService.info(c.path)
        // сведения о файле могли измениться пока пользователь выбирал объекты
        require(current == c.info, "Объект изменился после поиска")
        require(matcher.matches(c.rule, c.path, c.root, current), "Правило больше не подходит")
        matcher.checkSafe(c.path, c.root, c.rule)
        // срок пользователя важнее срока сохранённого у кандидата
        val days = if (daysOverride >= 0) daysOverride else c.retentionDays
        // put перемещает один объект и возвращает запись карантина
        val e = put(c.path, c.root.path, current, days, c.rule.id, c.rule.category)
        moved += 1
        bytes += e.size
        println("Перемещён: " + c.relative + "; номер: " + e.id)
        history.record("QUARANTINE_OK", history.path(c) + "; id=" + e.id +
          "; bytes=" + e.size + "; days=" + e.retentionDays, operationId = operationId)
      } catch {
        case NonFatal(e) =>
          errors += 1
          println("Пропущен " + c.path + ": " + e.getMessage)
          history.record("QUARANTINE_ERROR", history.path(c) + "; " + e.getClass.getSimpleName,
            "ERROR", operationId)
      }
    }
    // сохраняем итог всей операции
    val status = if (errors == 0) "OK" else if (moved == 0) "ERROR" else "PARTIAL"
    history.record("QUARANTINE_END", details + "; moved=" + moved + "; sourceBytes=" + bytes +
      "; quarantineBytes=" + volume + "; errors=" + errors, status, operationId)
  }

  // проверяем состояние записи и сохранённые метаданные объекта
  private def verify(e: QuarantineEntry): Unit = {
    require(e.status == "active", "Операция по записи не завершена")
    val info = FileService.info(stored(e))
    require(info.size == e.size && info.isDirectory == e.isDirectory && info.fingerprint == e.fingerprint,
      "Объект карантина изменён или повреждён")
  }

  def restore(id: String, target: Path, replaceConfirmed: Boolean): Unit = {
    val e = find(id)
    verify(e)
    FileService.checkNoLinks(target)
    // проверяем путь назначения и доступ к родительской папке
    require(!matcher.protectedPath(target), "Путь восстановления защищён")
    require(Files.isDirectory(target.getParent) && Files.isWritable(target.getParent),
      "Родительская папка отсутствует или недоступна для записи")
    require(Files.getFileStore(target.getParent).getUsableSpace >= e.size, "Недостаточно свободного места")
    // истечение срока не запрещает восстановление пока данные не удалены
    if (e.expiresAt <= System.currentTimeMillis()) println("Срок истёк, но данные ещё можно восстановить.")
    if (FileService.exists(target)) {
      require(replaceConfirmed, "Путь занят: нужно отдельное подтверждение замены")
      for (p <- FileService.tree(target)) require(!matcher.protectedPath(p), "Замена затрагивает защищённый объект")
      // Заменяемый объект тоже сохраняем в карантине, а не удаляем.
      // прежний объект сохраняем в карантине перед заменой
      val backup = put(target, target.getParent, FileService.info(target),
        config.defaultRetentionDays, "manual-replacement", "replacement-backup")
      history.record("REPLACEMENT_BACKUP", "id=" + backup.id)
      println("Прежний объект сохранён в карантине: " + backup.id)
    }
    // сохраняем начало восстановления на случай прерывания
    state.update(e.copy(status = "restoring", restoreTarget = target.toString))
    FileService.move(stored(e), target)
    // проверяем что объект появился на месте восстановления
    // и больше не находится в карантине
    val restored = FileService.info(target)
    require(!FileService.exists(stored(e)) && restored.fingerprint == e.fingerprint && restored.size == e.size,
      "Восстановление не прошло проверку")
    // запись больше не нужна в активном состоянии карантина
    history.record("RESTORE", history.restoredDetails(e, target))
    state.remove(e.id)
    println("Восстановлено: " + target)
  }

  def extend(id: String, days: Int): Unit = {
    require(days > 0, "Добавьте хотя бы один день")
    val e = find(id)
    require(e.status == "active", "Запись неактивна")
    val date = deadline(math.max(System.currentTimeMillis(), e.expiresAt), days)
    state.update(e.copy(expiresAt = date, retentionDays = Math.addExact(e.retentionDays, days)))
    history.record("EXTEND", "id=" + id + "; days=" + days + "; until=" + Instant.ofEpochMilli(date))
  }

  // окончательно удаляем выбранные просроченные объекты
  // меню вызывает этот метод только после подтверждения
  def purge(entries: List[QuarantineEntry]): Unit = {
    var deleted = 0
    var bytes = 0L
    var errors = 0
    for (e <- entries) {
      try {
        require(e.expiresAt <= System.currentTimeMillis(), "Срок хранения ещё не истёк")
        verify(e)
        state.update(e.copy(status = "deleting"))
        FileService.deleteTree(stored(e))
        require(!FileService.exists(stored(e)), "Удаление не завершено")
        history.record("PURGE", "id=" + e.id + "; bytes=" + e.size)
        state.remove(e.id)
        deleted += 1
        bytes += e.size
      } catch {
        case NonFatal(error) =>
          errors += 1
          println("Не удалён " + e.id + ": " + error.getMessage)
          history.record("PURGE_ERROR", "id=" + e.id + "; " + error.getClass.getSimpleName, "ERROR")
      }
    }
    history.record("PURGE_END", "deleted=" + deleted + "; bytes=" + bytes +
      "; errors=" + errors + "; quarantineBytes=" + volume, if (errors == 0) "OK" else "PARTIAL")
    println("Удалено объектов: " + deleted + "; освобождено байт: " + bytes + "; ошибок: " + errors)
  }
}
