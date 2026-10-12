package cleaner

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Properties
// PropertyFiles читает и сохраняет настройки в формате ключ=значение
// помогает получать из текста строки, числа, списки и логические значения
// при сохранении сначала пишет временный файл, потом заменяет основной
// общие методы для текстовых настроек и состояния карантина
object PropertyFiles {

  // читаем файл и получаем пары ключ=значение
  def read(path: Path): Properties = {
    val reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)

    try {
      val properties = new Properties()

      // разбираем строки файла и сохраняем значения в properties
      properties.load(reader)

      // возвращаем прочитанные настройки
      properties

    } finally {
      // закрываем файл даже если чтение закончилось ошибкой
      reader.close()
    }
  }

  // сохраняем пары ключ=значение в файл
  def save(path: Path, properties: Properties): Unit = {
    Files.createDirectories(path.getParent)

    // сначало записываем в отдельный временный файл рядом с основным
    val temporary = Files.createTempFile(
      path.getParent,
      "save-",
      ".tmp"
    )

    try {
      val writer = Files.newBufferedWriter(
        temporary,
        StandardCharsets.UTF_8
      )

      try {
        // записываем настройки, TemporaryCleaner добавляется как комментарий
        properties.store(writer, "TemporaryCleaner")
      } finally {
        writer.close()
      }

      // заменяем основной файл только после завершения записи
      // атомарная замена не оставляет основной файл записанным наполовину
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )

    } finally {
      // убираем временный файл если он ещё остался
      Files.deleteIfExists(temporary)
    }
  }

  // читаем список значений разделённых точкой с запятой
  def list(p: Properties, key: String): List[String] = {
    p.getProperty(key, "")
      .split(";")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
  }
  // читаем строку, если ключа нет используем default
  def text(
            p: Properties,
            key: String,
            default: String = ""
          ): String = {
    p.getProperty(key, default).trim
  }
  // читаем строку и превращаем её в число Long
  def number(p: Properties, key: String, default: Long): Long = {
    text(p, key, default.toString).toLong
  }

  // читаем строку и превращаем её в число Int
  def integer(p: Properties, key: String, default: Int): Int = {
    text(p, key, default.toString).toInt
  }
  // разрешаем только true или false
  def bool(
            p: Properties,
            key: String,
            default: Boolean
          ): Boolean = {
    text(p, key, default.toString) match {
      case "true" => true
      case "false" => false
      case _ =>
        throw new IllegalArgumentException(
          "Нужно true или false: " + key
        )
    }
  }
}