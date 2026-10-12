package cleaner

import java.nio.file.{Files, Paths}
import java.util.Properties
import scala.util.control.NonFatal
import PropertyFiles._
//сохраняет записи карантина между запусками программы.
// загружает записи карантина из файла и сохраняет их изменения
// load превращает текстовые поля в QuarantineEntry
// save превращает записи обратно в текст и записывает на диск
// update добавляет или заменяет одну запись, remove убирает запись по номеру
// поэтому карантин сохраняется между запусками программы
class StateService(config: AppConfig, history: HistoryService) {
  private var savedEntries: List[QuarantineEntry] = Nil
  private var isDamaged: Boolean = false
  def entries: List[QuarantineEntry] = savedEntries
  def damaged: Boolean = isDamaged

  load()

  private def load(): Unit = {
    if (!Files.exists(config.stateFile)) return
    try {
      val p = read(config.stateFile)
      require(text(p, "version") == "1", "Неизвестная версия состояния")
      val ids = list(p, "entries")
      require(ids.distinct.size == ids.size, "Повтор записи карантина")
      require(integer(p, "count", -1) == ids.size, "Неверное число записей")
      val loaded = ids.map { id =>
        require(id.matches("[0-9a-f-]{36}"), "Неверный номер записи")
        val k = id + "."
        // без этих полей нельзя надёжно восстановить запись
        val requiredFields = List(
          "original", "root", "name", "directory", "size", "moved",
          "days", "expires", "rule", "category", "fingerprint", "status"
        )
        for (field <- requiredFields) {
          require(text(p, k + field).nonEmpty, "В записи отсутствует поле: " + field)
        }
        require(p.containsKey(k + "restoreTarget"), "Нет поля restoreTarget")
        val original = Paths.get(text(p, k + "original"))
        val root = Paths.get(text(p, k + "root"))
        require(original.isAbsolute && root.isAbsolute && original.normalize() == original,
          "Неверный исходный путь")
        require(root.normalize() == root && original.startsWith(root) && original != root,
          "Объект должен находиться внутри своей точки поиска")
        val name = text(p, k + "name")
        require(name != "." && name != ".." && !name.contains("/") && !name.contains("\\") &&
          original.getFileName != null && name == original.getFileName.toString,
          "Неверное имя объекта")
        val status = text(p, k + "status")
        require(Set("pending", "active", "restoring", "deleting").contains(status), "Неверное состояние")
        val size = number(p, k + "size", -1)
        val days = integer(p, k + "days", -1)
        val moved = number(p, k + "moved", -1)
        val expires = number(p, k + "expires", -1)
        val fingerprint = text(p, k + "fingerprint")
        require(size >= 0 && days >= 0 && moved >= 0 && expires >= moved &&
          fingerprint.matches("[0-9a-f]{64}"), "Повреждены метаданные")
        val restoreTarget = text(p, k + "restoreTarget")
        if (status == "restoring") {
          require(restoreTarget.nonEmpty && Paths.get(restoreTarget).isAbsolute,
            "Нет правильного пути восстановления")
        }
        QuarantineEntry(id, original, root, name,
          bool(p, k + "directory", false), size, moved, days, expires,
          text(p, k + "rule"), text(p, k + "category"), fingerprint, status, text(p, k + "restoreTarget"))
      }
      savedEntries = loaded
    } catch {
      case NonFatal(e) =>
        isDamaged = true
        history.record("STATE_DAMAGED", e.getClass.getSimpleName, "ERROR")
        println("Состояние повреждено. Изменения карантина запрещены. Файлы оставлены на месте.")
    }
  }

  def save(updated: List[QuarantineEntry]): Unit = {
    require(!damaged, "Сначала разберите повреждение состояния; программа ничего не удаляет")
    val p = new Properties()
    p.setProperty("version", "1")
    p.setProperty("count", updated.size.toString)
    p.setProperty("entries", updated.map(_.id).mkString(";"))
    for (e <- updated) {
      val k = e.id + "."
      val fields = Map("original" -> e.original.toString, "root" -> e.root.toString,
        "name" -> e.name, "directory" -> e.isDirectory.toString, "size" -> e.size.toString,
        "moved" -> e.movedAt.toString, "days" -> e.retentionDays.toString,
        "expires" -> e.expiresAt.toString, "rule" -> e.rule, "category" -> e.category,
        "fingerprint" -> e.fingerprint, "status" -> e.status, "restoreTarget" -> e.restoreTarget)
      for ((key, value) <- fields) p.setProperty(k + key, value)
    }
    FileService.checkNoLinks(config.stateFile)
    PropertyFiles.save(config.stateFile, p)
    savedEntries = updated
  }

  def update(entry: QuarantineEntry): Unit = save(entries.filterNot(_.id == entry.id) :+ entry)
  def remove(id: String): Unit = save(entries.filterNot(_.id == id))
}
