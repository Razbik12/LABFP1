package cleaner

import java.nio.file.{Files, Path, Paths}
import java.util.Properties

// эти методы читают из настроек строки, числа, списки и т.д.
import PropertyFiles._

// file хранит путь к config.properties, с ним дальше и работаем
class ConfigService(val file: Path) {

  // из строки получаем путь, относительные пути считаем от папки настроек
  private def path(value: String): Path = {
    require(value.nonEmpty, "Путь не может быть пустым")

    // если путь начинается с ~, подставляем домашнюю папку пользоватлея
    val expanded = if (value == "~" || value.startsWith("~/")) {
      System.getProperty("user.home") + value.drop(1)
    } else value

    val result = Paths.get(expanded)

    // полный путь уже готов, а например demo надо добавить к папке config.properties
    if (result.isAbsolute) result.normalize()
    else file.getParent.resolve(result).normalize()
  }

  // вызывается в Main: сначала читаем файл, потом собираем настройки программы
  def load(): AppConfig = parse(read(file))

  // p содержит пары вроде maximumScanDepth=30, из них создаём наши объекты
  private def parse(p: Properties): AppConfig = {

    // читаем срок, если он не указан, -1 означает что отдельного срока нет
    def retention(key: String): Int = {
      if (!p.containsKey(key)) return -1

      val days = integer(p, key, 0)
      require(days >= 0, "Отрицательный срок: " + key)
      days
    }

    // переходы по ссылкам эта версия не поддерживает
    require(
      !bool(p, "followSymbolicLinks", false),
      "Эта версия не переходит по символическим ссылкам. Укажите false."
    )

    val depth = integer(p, "maximumScanDepth", 30)

    // берём список точек, например demo, и для каждой собираем SearchRoot
    val roots = list(p, "roots").map { id =>

      // для demo получаем root.demo. и дальше дописываем name, path и другие поля
      val k = "root." + id + "."

      SearchRoot(
        id,
        text(p, k + "name", id),
        path(text(p, k + "path")),
        text(p, k + "profile", text(p, "defaultProfile", "safe")),
        integer(p, k + "depth", depth),
        bool(p, k + "enabled", true),
        list(p, k + "exclusions")
      )
    }

    // тут собираются правила
    val rules = list(p, "rules").map { id =>
      val k = "rule." + id + "."

      Rule(
        id,
        text(p, k + "name", id),
        text(p, k + "description"),
        text(p, k + "category"),
        text(p, k + "type", "file"),
        text(p, k + "exactName"),
        list(p, k + "patterns"),
        text(p, k + "regex"),
        text(p, k + "extension"),
        text(p, k + "relativePattern"),
        list(p, k + "parentContains"),
        list(p, k + "contains"),
        number(p, k + "minimumAgeHours", 0),
        number(p, k + "minimumSize", 0),
        number(p, k + "maximumSize", Long.MaxValue),
        retention(k + "retentionDays"),
        integer(p, k + "priority", 0),
        bool(p, k + "enabled", true)
      )
    }

    // профиль full и остальные получают свои правила, категории и ограниченя
    val profiles = list(p, "profiles").map { id =>
      val k = "profile." + id + "."

      Profile(
        id,
        list(p, k + "rules"),
        list(p, k + "categories"),
        list(p, k + "excludedRules"),
        retention(k + "retentionDays"),
        number(p, k + "maximumSize", Long.MaxValue)
      )
    }

    // читаем исключения, например fixtures и *.keep.tmp из учебного примера
    val exclusions = list(p, "exclusions").map { id =>
      val k = "exclusion." + id + "."
      val kind = text(p, k + "kind")
      var value = text(p, k + "value")

      // у исключений по пути тоже нужно получить полный путь
      if (kind == "path" || kind == "tree") {
        value = path(value).toString
      }

      Exclusion(id, kind, value, text(p, k + "scope"))
    }

    // все прочитанные данные складываем в один AppConfig
    val c = AppConfig(
      file,
      path(text(p, "quarantineDirectory")),
      path(text(p, "stateFile")),
      path(text(p, "logFile")),
      path(text(p, "historyFile")),
      path(text(p, "reportFile")),
      text(p, "defaultProfile", "safe"),
      integer(p, "defaultRetentionDays", 14),
      depth,
      bool(p, "askAboutExpiredOnStartup", true),
      bool(p, "logFullPaths", false),
      roots,
      rules,
      profiles,
      exclusions,
      list(p, "globalExclusions")
    )

    // перед возвратом проверяем настройки, при ошибке дальше не идём
    validate(c)
    c
  }

  // проверяем что настройки подходят для работы программы
  def validate(c: AppConfig): Unit = {

    // distinct убирает повторы, если размер уменьшился значит были одинаковые id
    def unique(ids: List[String], what: String): Unit = {
      require(
        ids.distinct.size == ids.size,
        "Повторяются идентификаторы: " + what
      )
      require(
        ids.forall(_.matches("[A-Za-z0-9_-]+")),
        "Неверный идентификатор: " + what
      )
    }

    // у точек, правил, профилей и исключений проверяем их идентификаторы
    unique(c.roots.map(_.id), "точки")
    unique(c.rules.map(_.id), "правила")
    unique(c.profiles.map(_.id), "профили")
    unique(c.exclusions.map(_.id), "исключения")

    // отрицательная глубина и отрицательный срок нам не подходят
    require(c.maximumDepth >= 0, "Глубина должна быть >= 0")
    require(c.defaultRetentionDays >= 0, "Срок должен быть >= 0")

    // этот вызов выдаст ошибку, если профиля по умолчанию нет
    c.profile(c.defaultProfile)

    // карантин должен существовать и быть доступным для записи
    require(
      Files.isDirectory(c.quarantine),
      "Каталог карантина отсутствует"
    )
    FileService.checkNoLinks(c.quarantine)
    require(
      Files.isWritable(c.quarantine),
      "Карантин недоступен для записи"
    )

    // настройки, история и остальные служебные файлы должны иметь разные пути
    val serviceFiles = List(
      c.file,
      c.stateFile,
      c.logFile,
      c.historyFile,
      c.reportFile
    )

    require(
      serviceFiles.distinct.size == serviceFiles.size,
      "Служебные файлы должны различаться"
    )

    for (p <- serviceFiles) {
      FileService.checkNoLinks(p)
      require(
        !p.startsWith(c.quarantine),
        "Служебный файл не должен лежать в карантине"
      )
    }

    // нельзя сделать весь диск или домашнюю папку карантином
    require(
      c.quarantine != c.quarantine.getRoot &&
        c.quarantine != Paths.get(
          System.getProperty("user.home")
        ).toAbsolutePath.normalize(),
      "Нельзя использовать корень диска или домашний каталог как карантин"
    )

    // собираем существующие названия и id для следующих проверок
    val categories = c.rules.map(_.category)
    val ruleIds = c.rules.map(_.id)
    val exclusionIds = c.exclusions.map(_.id)

    require(
      c.globalExclusions.forall(exclusionIds.contains),
      "Неизвестное общее исключение"
    )

    // проверяем каждую точку: доступ, профиль, исключения и расположение карантина
    for (r <- c.roots) {
      require(r.depth >= 0, "Отрицательная глубина: " + r.id)

      require(
        Files.isDirectory(r.path) && Files.isReadable(r.path),
        "Точка недоступна: " + r.id
      )

      FileService.checkNoLinks(r.path)

      // пробуем открыть папку, сразу закрываем, здесь её файлы ещё не ищем
      val stream = Files.newDirectoryStream(r.path)
      stream.close()

      c.profile(r.profile)

      require(
        !c.technicalPaths.exists(p => r.path.startsWith(p)),
        "Точка в служебном каталоге"
      )
      require(
        !r.enabled || !c.quarantine.startsWith(r.path),
        "Карантин внутри активной точки"
      )
      require(
        r.exclusions.forall(exclusionIds.contains),
        "Неизвестное исключение точки"
      )
    }

    // профиль не должен ссылаться на несуществующие правила или категории
    for (p <- c.profiles) {
      require(
        (p.rules ++ p.excludedRules).forall(ruleIds.contains),
        "Неизвестное правило профиля"
      )
      require(
        p.categories.forall(categories.contains),
        "Неизвестная категория профиля"
      )
      require(
        p.retentionDays >= -1 && p.maximumSize >= 0,
        "Неверные ограничения профиля"
      )
    }

    // у правил проверяем тип объекта, размеры, сроки и правильность масок
    for (r <- c.rules) {
      require(r.category.nonEmpty, "Пустая категория: " + r.id)

      require(
        Set("file", "directory", "any").contains(r.targetType),
        "Неверный тип: " + r.id
      )

      require(
        r.retentionDays >= -1 &&
          r.minimumAgeHours >= 0 &&
          r.minimumSize >= 0 &&
          r.maximumSize >= r.minimumSize,
        "Неверные ограничения правила: " + r.id
      )

      for (
        mask <- r.patterns ++
          r.parentContains ++
          r.contains ++
          List(r.relativePattern).filter(_.nonEmpty)
      ) {
        FileService.validateMask(mask)
      }

      // пробуем собрать регулярное выражение, если оно кривое будет ошибка
      if (r.regex.nonEmpty) {
        java.util.regex.Pattern.compile(r.regex)
      }
    }

    // исключение тоже проверяем, чтобы не было непонятного типа или пустого значения
    for (e <- c.exclusions) {
      require(
        Set(
          "path",
          "tree",
          "name",
          "mask",
          "extension",
          "category",
          "rule",
          "youngerHours"
        ).contains(e.kind),
        "Неверный вид исключения"
      )

      require(e.value.nonEmpty, "Пустое исключение")

      if (e.kind == "mask") FileService.validateMask(e.value)
      if (e.scope.nonEmpty) FileService.validateMask(e.scope)

      if (e.kind == "youngerHours") {
        require(e.value.toLong >= 0, "Отрицательный возраст")
      }

      if (e.kind == "category") {
        require(
          categories.contains(e.value),
          "Неизвестная категория исключения"
        )
      }

      if (e.kind == "rule") {
        require(
          ruleIds.contains(e.value),
          "Неизвестное правило исключения"
        )
      }
    }
  }

  // вызывается из меню когда добавляем, меняем или удаляем точку поиска
  def saveRoots(roots: List[SearchRoot]): AppConfig = {
    val p = read(file)

    // получаем строку с id через ;, в таком виде список хранится в файле
    p.setProperty("roots", roots.map(_.id).mkString(";"))

    // перезаписываем данные каждой точки в настройках
    for (r <- roots) {
      val k = "root." + r.id + "."

      p.setProperty(k + "name", r.name)

      // путь записываем через /, чтобы обратные слеши не мешали чтению настроек
      p.setProperty(k + "path", r.path.toString.replace('\\', '/'))

      p.setProperty(k + "profile", r.profile)
      p.setProperty(k + "depth", r.depth.toString)
      p.setProperty(k + "enabled", r.enabled.toString)
      p.setProperty(k + "exclusions", r.exclusions.mkString(";"))
    }

    // сначала проверяем новые настойки, только потом сохраняем в файл
    val config = parse(p)
    save(file, p)
    config
  }

  // этот метод вызывается из меню, когда пользователь вводит путь к папке
  def userPath(value: String): Path = path(value)
}