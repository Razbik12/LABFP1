package cleaner

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.time.Instant
import java.util.UUID
import scala.collection.mutable.ListBuffer

// записывает события программы в историю и журнал
class HistoryService(config: AppConfig) {

  // события текущего запуска, ReportService использует их для отчёта
  val session = ListBuffer[String]()

  // убираем табуляцию и переносы строк из текста
  // иначе одно событие может случайно занять несколько строк или колонок
  private def clean(text: String): String =
    text.replace('\t', ' ').replace('\r', ' ').replace('\n', ' ')

  // записываем событие, например начало поиска или перенос в карантин
  // если статус не передали, используем OK
  def record(
              action: String,
              details: String,
              status: String = "OK",
              operationId: String = ""
            ): Unit = {

    // одна запись содержит пять полей:
    // время, уникальный номер, действие, статус и подробности
    // поля разделяем табуляцией
    val line = List(
      Instant.now().toString,
      if (operationId.isEmpty) UUID.randomUUID().toString else operationId,
      clean(action),
      clean(status),
      clean(details)
    ).mkString("\t")

    // сохраняем событие в памяти для отчёта текущего запуска
    session += line

    // одну и ту же запись добавляем в историю и в журнал
    for (file <- List(config.historyFile, config.logFile)) {
      try {

        // создаём родительскую папку, если её ещё нет
        Files.createDirectories(file.getParent)

        // проверяем что путь не проходит через ссылку
        FileService.checkNoLinks(file)

        // CREATE создаёт файл если его нет
        // APPEND дописывает в конец, прежние записи сохраняются
        Files.write(
          file,
          (line + System.lineSeparator()).getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )

      } catch {
        // если запись не удалась, показываем тип ошибки
        case e: java.io.IOException =>
          println(
            "Не удалось записать историю/журнал: " +
              e.getClass.getSimpleName
          )
      }
    }
  }

  // подготавливаем путь кандидата для записи в журнал
  def path(candidate: Candidate): String = {

    // полный путь пишем только если это включено в настройках
    if (config.logFullPaths) {
      candidate.path.toString
    } else {

      // по умолчанию получаем что-то вроде demo:sample/notes.tmp
      candidate.root.id + ":" + candidate.relative
    }
  }

  // сведения о восстановленной записи, содержимое файлов сюда не попадает
  def restoredDetails(entry: QuarantineEntry, target: java.nio.file.Path): String = {
    val rootId = config.roots.find(_.path == entry.root).map(_.id).getOrElse("original-root")

    // полные пути используем только если это разрешено настройкой
    def display(path: java.nio.file.Path): String = {
      if (config.logFullPaths) path.toString
      else rootId + ":" + entry.root.relativize(path).toString
    }

    List(
      "id=" + entry.id,
      "original=" + display(entry.original),
      "root=" + (if (config.logFullPaths) entry.root.toString else rootId),
      "name=" + entry.name,
      "type=" + (if (entry.isDirectory) "directory" else "file"),
      "bytes=" + entry.size,
      "movedAt=" + Instant.ofEpochMilli(entry.movedAt),
      "days=" + entry.retentionDays,
      "expiresAt=" + Instant.ofEpochMilli(entry.expiresAt),
      "rule=" + entry.rule,
      "category=" + entry.category,
      "fingerprint=" + entry.fingerprint,
      "status=restored",
      "target=" + display(target),
      "restoredAt=" + Instant.now()
    ).mkString("; ")
  }

  // вызывается из меню когда пользователь выбирает просмотр истории
  def show(): Unit = {
    if (!Files.exists(config.historyFile)) {
      println("История пока пуста.")
      return
    }

    // открываем файл истории для чтения в UTF-8
    val reader = Files.newBufferedReader(
      config.historyFile,
      StandardCharsets.UTF_8
    )

    try {
      var line = reader.readLine()

      // null означает что дошли до конца файла
      while (line != null) {

        // разделяем запись на пять полей
        // -1 сохраняет пустые поля, даже если они находятся в конце строки
        val fields = line.split("\t", -1)

        // проверяем количество полей, неправильную строку пропуксаем
        if (fields.length != 5) {
          println("Повреждённая строка истории пропущена.")
        } else {

          // для вывода заменяем табуляцию на понятный разделитель
          println(fields.mkString(" | "))
        }

        // читаем следующую запись
        line = reader.readLine()
      }

    } finally {

      // закрываем файл даже если при чтении случилась ошибка
      reader.close()
    }
  }
}
