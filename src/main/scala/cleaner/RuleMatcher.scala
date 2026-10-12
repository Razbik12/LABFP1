package cleaner

import java.nio.file.{Files, Path, Paths}

class RuleMatcher(config: AppConfig) {
  // possible быстро отсеивает правила которые не подходят по типу, имени или пути
  // после него matches проверяет оставшиеся условия, включая возраст и размер
  def possible(rule: Rule, path: Path, root: SearchRoot): Boolean = {
    val name = path.getFileName.toString
    val directory = Files.isDirectory(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)
    (rule.targetType == "any" || (rule.targetType == "directory") == directory) &&
      (rule.exactName.isEmpty || rule.exactName == name) &&
      (rule.patterns.isEmpty || rule.patterns.exists(FileService.matches(_, name))) &&
      (rule.regex.isEmpty || name.matches(rule.regex)) &&
      (rule.extension.isEmpty || name.endsWith(rule.extension)) &&
      (rule.relativePattern.isEmpty || FileService.matches(rule.relativePattern, root.path.relativize(path).toString))
  }

  // проверяем есть ли в папке хотя бы один объект под заданные маски
  // например для target проверяем наличие build.sbt или pom.xml рядом
  private def containsAny(path: Path, masks: List[String]): Boolean = {
    if (masks.isEmpty) return true
    if (!Files.isDirectory(path)) return false
    FileService.children(path).exists { p =>
      Files.isRegularFile(p, java.nio.file.LinkOption.NOFOLLOW_LINKS) &&
        masks.exists(m => FileService.matches(m, p.getFileName.toString))
    }
  }

  // проверяем конкретный объект по всем условиям правила
  // например notes.tmp должен подходить под маску и быть старше 24 часов
  // если хоть одно заданное условие не выполнено, возвращаем false
  // если все проверки пройдены, возвращаем true
  def matches(rule: Rule, path: Path, root: SearchRoot, info: FileInfo): Boolean = {
    val name = path.getFileName.toString
    val relative = root.path.relativize(path).toString
    val ageHours = (System.currentTimeMillis() - info.modified) / 3600000L
    if (rule.targetType == "file" && info.isDirectory) return false
    if (rule.targetType == "directory" && !info.isDirectory) return false
    if (rule.exactName.nonEmpty && rule.exactName != name) return false
    if (rule.patterns.nonEmpty && !rule.patterns.exists(FileService.matches(_, name))) return false
    if (rule.regex.nonEmpty && !name.matches(rule.regex)) return false
    if (rule.extension.nonEmpty && !name.endsWith(rule.extension)) return false
    if (rule.relativePattern.nonEmpty && !FileService.matches(rule.relativePattern, relative)) return false
    if (ageHours < rule.minimumAgeHours) return false
    if (info.size < rule.minimumSize || info.size > rule.maximumSize) return false
    if (!containsAny(path.getParent, rule.parentContains)) return false
    if (rule.contains.nonEmpty && (!info.isDirectory || !containsAny(path, rule.contains))) return false
    true
  }

  // собираем объяснение из условий правила
  // это объяснение потом показывается в подробностях найденного объекта
  def reason(rule: Rule): String = {
    val parts = scala.collection.mutable.ListBuffer[String]()
    parts += rule.description
    if (rule.exactName.nonEmpty) parts += ("имя = " + rule.exactName)
    if (rule.patterns.nonEmpty) parts += ("имя подходит под маску " + rule.patterns.mkString(", "))
    if (rule.regex.nonEmpty) parts += ("имя подходит под выражение " + rule.regex)
    if (rule.extension.nonEmpty) parts += ("расширение " + rule.extension)
    if (rule.relativePattern.nonEmpty) parts += ("путь подходит под " + rule.relativePattern)
    if (rule.parentContains.nonEmpty) parts += ("рядом есть " + rule.parentContains.mkString(" или "))
    if (rule.contains.nonEmpty) parts += ("внутри есть " + rule.contains.mkString(" или "))
    parts += ("возраст не меньше " + rule.minimumAgeHours + " ч.")
    parts += ("размер от " + rule.minimumSize + " до " + rule.maximumSize + " байт")
    parts.filter(_.nonEmpty).mkString("; ")
  }

  // проверяем защищён ли путь от помещения в карантин
  // защищаем корень диска, домашнюю папку, сами точки поиска и служебные пути
  def protectedPath(path: Path): Boolean = {
    val home = Paths.get(System.getProperty("user.home")).toAbsolutePath.normalize()
    path == path.getRoot || path == home || config.roots.exists(_.path == path) ||
      config.technicalPaths.exists(p => path.startsWith(p) || p.startsWith(path))
  }

  // проверяем нужно ли пропустить объект по исключениям
  // rule передаётся когда проверяем исключение по правилу или категории
  def excluded(path: Path, root: SearchRoot, rule: Option[Rule] = None): Boolean = {
    val ids = config.globalExclusions ++ root.exclusions
    config.exclusions.filter(e => ids.contains(e.id)).exists { e =>
      def matchesPath(current: Path): Boolean = {
        val relative = root.path.relativize(current).toString
        val name = if (current.getFileName == null) "" else current.getFileName.toString
        val inScope = e.scope.isEmpty || FileService.matches(e.scope, relative)
        inScope && (e.kind match {
          case "path" => current == Paths.get(e.value)
          case "tree" => current.startsWith(Paths.get(e.value))
          case "name" => name == e.value
          case "mask" => FileService.matches(e.value, relative) || FileService.matches(e.value, name)
          case "extension" => name.endsWith(e.value)
          case "category" => rule.exists(_.category == e.value)
          case "rule" => rule.exists(_.id == e.value)
          case "youngerHours" =>
            (System.currentTimeMillis() - Files.getLastModifiedTime(current).toMillis) / 3600000L < e.value.toLong
          case _ => false
        })
      }

      // сначала проверяем сам файл или папку
      var found = matchesPath(path)

      // если исключена папка, её содержимое тоже не трогаем
      // проверяем и родителей точки поиска, чтобы нельзя было обойти .git
      if (Set("path", "tree", "name", "mask").contains(e.kind)) {
        var parent = path.getParent
        while (!found && parent != null) {
          found = matchesPath(parent)
          parent = parent.getParent
        }
      }
      found
    }
  }
    // проверяем безопасность объекта и всего его содержимого

  // при нарушении останавливаем обработку этого объекта с сообщением
  def checkSafe(path: Path, root: SearchRoot, rule: Rule): Unit = {
    require(path.startsWith(root.path) && path != root.path, "Объект вне точки поиска")
    FileService.checkNoLinks(root.path)
    for (p <- FileService.tree(path)) {
      require(!protectedPath(p), "Внутри есть защищённый объект")
      require(!excluded(p, root, Some(rule)), "Объект или его содержимое исключены")
      require(Files.isReadable(p), "Нет доступа для чтения")
      val info = FileService.info(p)
      for (r <- config.rules if r.enabled) {
        if (matches(r, p, root, info)) {
          require(!excluded(p, root, Some(r)), "Внутри есть объект исключённого правила")
        }
      }
    }
  }
}
