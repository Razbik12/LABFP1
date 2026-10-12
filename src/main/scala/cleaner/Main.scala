package cleaner

import java.nio.channels.FileChannel
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]): Unit = {
    try {
    // Если передан арг- берем его, если нет - то испл config. Absolutlypath-полный путь к файлу
      val file = Paths.get(if (args.nonEmpty) args(0) else "config.properties").toAbsolutePath.normalize()
      val service = new ConfigService(file) //создать читателя настроек
      val config = service.load() // прочитать настрйоки
      Files.createDirectories(config.stateFile.getParent) //создание служ папки
      val lockPath = config.stateFile.resolveSibling("application.lock") // защита от 2 одновремен запусков
      FileService.checkNoLinks(lockPath)
      val channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      try {
        val lock = channel.tryLock()
        require(lock != null, "Программа уже запущена с этим состоянием")
        try new ConsoleMenu(service, config).run() // создание меню и показ
        finally lock.release()
      } finally channel.close()
    } catch {
      case NonFatal(e) => println("Ошибка запуска: " + e.getMessage)
    }
  }
}
