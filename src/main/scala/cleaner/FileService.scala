package cleaner

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable.ListBuffer

// общие действия с файлами, используются при поиске и работе с карантином
object FileService {

  // при проверках не переходим по символическим ссылкам
  private val noLinks = LinkOption.NOFOLLOW_LINKS

  // проверяем существует ли объект по этому пути
  def exists(path: Path): Boolean = Files.exists(path, noLinks)

  // проверяем сам объект и все папки выше него, чтобы не пройти через ссылку
  def checkNoLinks(path: Path): Unit = {
    var part = path.toAbsolutePath.normalize()

    // постепенно поднимаемся от объекта к корню диска
    while (part != null) {
      if (exists(part)) {

        // читаем сведения об объекте, содержимое файла здесь не читается
        val a = Files.readAttributes(
          part,
          classOf[BasicFileAttributes],
          noLinks
        )

        require(
          !a.isSymbolicLink && !a.isOther,
          "Ссылка или специальный объект запрещены"
        )

        // сравниваем реальный путь с переходом по ссылкам и без перехода
        require(
          part.toRealPath() == part.toRealPath(noLinks),
          "Переход через ссылку запрещён"
        )
      }

      // получаем родительскую папку для следующей проверки
      part = part.getParent
    }
  }

  // вызывается при проверке настроек, пробуем создать проверку по маске
  // если маска написана неправильно, будет ошибка
  def validateMask(mask: String): Unit = {
    FileSystems.getDefault.getPathMatcher("glob:" + mask)
  }

  // проверяем подходит ли имя или путь под маску
  // например notes.tmp под *.tmp или папка fixtures под её исключение
  def matches(mask: String, value: String): Boolean = {

    // приводим разделители пути к одному виду
    val normalized = value.replace('\\', '/')

    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + mask)

    if (matcher.matches(Paths.get(normalized))) {
      true

    } else if (mask.startsWith("**/")) {

      // убираем **/ и проверяем ещё раз
      // это нужно для fixtures прямо внутри точки поиска
      matches(mask.drop(3), normalized)

    } else if (mask.endsWith("/**")) {

      // проверяем также саму папку, а не только то что внутри неё
      matches(mask.dropRight(3), normalized)

    } else {
      false
    }
  }

  // возвращаем только непосредственные файлы и папки внутри path
  // глубже этот метод сам не заходит, это делает Scanner
  def children(path: Path): List[Path] = {
    val stream = Files.newDirectoryStream(path)
    val result = ListBuffer[Path]()

    try {
      val iterator = stream.iterator()

      // добавляем каждый найденный путь в список
      while (iterator.hasNext) {
        result += iterator.next()
      }

    } finally {
      // закрываем доступ к каталогу даже если случилась ошибка
      stream.close()
    }

    // возвращаем обычный список, отсортированный по строке пути
    result.toList.sortBy(_.toString)
  }

  // собираем сам объект и всё его содержимое, включая вложенные папки
  // используется для подсчёта размера, проверки и удаления
  def tree(path: Path): List[Path] = {
    checkNoLinks(path)

    val result = ListBuffer[Path]()

    Files.walkFileTree(path, new SimpleFileVisitor[Path] {

      // этот метод вызывается перед обходом очередной папки
      override def preVisitDirectory(
                                      p: Path,
                                      a: BasicFileAttributes
                                    ): FileVisitResult = {

        // если поток попросили прервать, прекращаем обход
        if (Thread.currentThread().isInterrupted) {
          throw new InterruptedException("Операция прервана")
        }

        checkNoLinks(p)
        result += p

        // продолжаем обход содержимого папки
        FileVisitResult.CONTINUE
      }

      // этот метод вызывается когда обход дошёл до файла
      override def visitFile(
                              p: Path,
                              a: BasicFileAttributes
                            ): FileVisitResult = {
        checkNoLinks(p)

        // разрешаем обычные файлы, специальные объекты не берём
        require(a.isRegularFile, "Специальный файл запрещён")

        result += p
        FileVisitResult.CONTINUE
      }
    })

    result.toList
  }

  // получаем FileInfo для кандидата, например нашего notes.tmp
  // перед перемещением этот метод вызывается ещё раз для сравнения
  def info(path: Path): FileInfo = {

    // SHA-256 делает строку-отпечаток из собранных сведений об объектах
    val digest = MessageDigest.getInstance("SHA-256")
    var size = 0L

    // сортируем объекты, чтобы порядок обхода не менял отпечаток
    val objects = tree(path).sortBy(p => path.relativize(p).toString)

    for (p <- objects) {
      val a = Files.readAttributes(
        p,
        classOf[BasicFileAttributes],
        noLinks
      )

      // складываем размеры файлов, у папок размер отдельно не прибавляем
      // addExact выдаст ошибку, если сумма не помещается в Long
      if (a.isRegularFile) {
        size = Math.addExact(size, a.size())
      }

      // в строку попадают относительный путь, тип, размер и дата изменения
      // содержимое пользовательских файлов для отпечатка не читаем
      val line =
        path.relativize(p).toString + "|" +
          a.isDirectory + "|" +
          (if (a.isDirectory) 0L else a.size()) + "|" +
          a.lastModifiedTime().toMillis + "\n"

      // добавляем сведения очередного объекта в расчёт отпечатка
      digest.update(line.getBytes(StandardCharsets.UTF_8))
    }

    // отдельно берём тип и дату самого объекта, который проверяем
    val a = Files.readAttributes(
      path,
      classOf[BasicFileAttributes],
      noLinks
    )

    // байты отпечатка переводим в строку из шестнадцатеричных цифр
    FileInfo(
      size,
      a.lastModifiedTime().toMillis,
      a.isDirectory,
      digest.digest().map(b => "%02x".format(b & 255)).mkString
    )
  }

  // перемещаем объект, например из demo в карантин или обратно
  def move(source: Path, target: Path): Unit = {
    checkNoLinks(source)
    checkNoLinks(target)

    // существующий объект по пути назначения не перезаписываем
    require(!exists(target), "Путь назначения уже занят")

    // в этой версии перенос разрешён только внутри одной файловой системы
    require(
      Files.getFileStore(source) == Files.getFileStore(target.getParent),
      "Нужен карантин на том же диске: перенос между дисками не поддерживается"
    )

    Files.move(source, target)
  }

  // окончательно удаляем объект и всё что внутри него
  // подтверждение пользоватлея запрашивается в меню до вызова удаления
  def deleteTree(path: Path): Unit = {

    // сначало собираем и проверяем всё дерево
    // если эта проверка завершится ошибкой, до удаления не дойдём
    val objects = tree(path)

    // идём в обратном порядке: сначала содержимое, потом родительские папки
    for (p <- objects.reverse) {
      checkNoLinks(p)
      Files.delete(p)
    }
  }
}