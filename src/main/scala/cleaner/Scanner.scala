package cleaner

import java.nio.file.{Files, Path, LinkOption}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

class Scanner(config: AppConfig, history: HistoryService) {
  private val matcher = new RuleMatcher(config)

  // обходит выбранные папки и собирает кандидатов на очистку
  // объекты проверяет через RuleMatcher, найденное сохраняет как Candidate
  // результат поиска возвращает в ConsoleMenu
  def scan(roots: List[SearchRoot], profileOverride: String): ScanResult = {
    val found = ListBuffer[Candidate]()
    val skipped = ListBuffer[String]()
    val warnings = ListBuffer[String]()
    var checked = 0L
    val profiles = roots.map(r => if (profileOverride.isEmpty) r.profile else profileOverride)
    history.record("SCAN_START", "roots=" + roots.map(_.id).mkString(",") + "; profiles=" + profiles.mkString(","))
    for (a <- roots; b <- roots if a.id < b.id && (a.path.startsWith(b.path) || b.path.startsWith(a.path))) {
      warnings += ("Пересекаются точки " + a.id + " и " + b.id + ". Повторы будут убраны.")
    }
//Проверяем, не прервана ли операция
    def visit(path: Path, root: SearchRoot, profile: Profile, depth: Int): Unit = {
      if (Thread.currentThread().isInterrupted) throw new InterruptedException("Поиск прерван")
      try {
        checked += 1
        //каждые 100 объектов программа показывает ход поиска:
        // сколько проверено, сколько найдено и какой объём набран.
        if (checked % 100 == 0) {
          println("Проверено: " + checked + "; кандидатов: " + found.size +
            "; байт: " + found.map(_.info.size).sum + "; каталог: " + path.getParent)
        }
        //проверяем путь на ссылки
        FileService.checkNoLinks(path)
        //пропускаем служебные объекты
        if (config.technicalPaths.exists(path.startsWith(_))) {
          skipped += (root.id + ":" + root.path.relativize(path) + " — служебный объект")
          return
        }
        //проверяем исключения
        if (matcher.excluded(path, root)) {
          skipped += (root.id + ":" + root.path.relativize(path) + " — исключение")
          return
        }
        // выбираем подходящие правила и основное из них
        // проверяем безопасность определяем срок и создаём Candidate
        // добавляем его в результаты, само перемещение будет позже через меню
        var accepted = false
        val possible = config.rulesFor(profile).filter(r => matcher.possible(r, path, root))
        if (!matcher.protectedPath(path) && possible.nonEmpty) {
          try {
          val info = FileService.info(path)
          val matches = possible.filter(r => matcher.matches(r, path, root, info) &&
            !matcher.excluded(path, root, Some(r)))
          val ordered = matches.sortBy(r => (-r.priority.toLong, -r.specificity, config.rules.indexOf(r)))
          if (ordered.nonEmpty && info.size <= profile.maximumSize) {
            val rule = ordered.head
            try {
              matcher.checkSafe(path, root, rule)
              val retention = if (profile.retentionDays >= 0) profile.retentionDays
                else if (rule.retentionDays >= 0) rule.retentionDays else config.defaultRetentionDays
              val item = Candidate(0, path, root, profile.id, root.path.relativize(path).toString,
                info, rule, ordered.tail.map(_.id), retention, matcher.reason(rule),
                List("Проверьте список: совпадение с правилом не гарантирует ненужность данных."))
              found += item
              history.record("CANDIDATE", history.path(item) + "; rule=" + rule.id)
              accepted = true
            } catch {
              case NonFatal(e) => skipped += (root.id + ":" + root.path.relativize(path) + " — " + e.getMessage)
            }
          }
          } catch {
            case NonFatal(e) =>
              skipped += (root.id + ":" + root.path.relativize(path) + " — " + e.getMessage)
              history.record("SCAN_ERROR", root.id + "; " + e.getClass.getSimpleName, "ERROR")
          }
        }
        // если папка сама не выбрана кандидатом, проверяем её содержимое
        if (!accepted && Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
          if (depth < math.min(root.depth, config.maximumDepth))
            // получаем детей папки и каждый передаём в этот же метод visit
          {
            for (child <- FileService.children(path)) visit(child, root, profile, depth + 1)
          } else skipped += (root.id + ":" + root.path.relativize(path) + " — предел глубины")
        }
      }
      // ошибка одного объекта не должна останавливать весь поиск
      // сохраняем причину пропуска и записываем ошибку в историю
      catch {
        case NonFatal(e) =>
          skipped += (root.id + ":" + root.path.relativize(path) + " — " + e.getMessage)
          history.record("SCAN_ERROR", root.id + "; " + e.getClass.getSimpleName, "ERROR")
      }
    }

    try {
      for (root <- roots) {
        println("Сканирование: " + root.path)
        val profile = config.profile(if (profileOverride.isEmpty) root.profile else profileOverride)
        try {
        FileService.checkNoLinks(root.path)
        if (math.min(root.depth, config.maximumDepth) == 0) skipped += (root.id + " — предел глубины 0")
        for (child <- FileService.children(root.path) if math.min(root.depth, config.maximumDepth) > 0) {
          visit(child, root, profile, 1)
        }
        } catch {
          case NonFatal(e) =>
            skipped += (root.id + " — ошибка чтения точки: " + e.getMessage)
            history.record("SCAN_ERROR", root.id + "; " + e.getClass.getSimpleName, "ERROR")
        }
      }
    } catch {
      case _: InterruptedException => warnings += "Поиск прерван. Показан неполный результат."
      case NonFatal(e) =>
        skipped += ("Ошибка чтения точки: " + e.getMessage)
        history.record("SCAN_ERROR", e.getClass.getSimpleName, "ERROR")
    }
    // список кандидатов без повторов и вложенных объектов выбранных папок
    val unique = ListBuffer[Candidate]()
    for (c <- found.toList.sortBy(_.path.getNameCount)) {
      if (!unique.exists(x => c.path.startsWith(x.path))) unique += c
    }
    // назначаем номера 1, 2, 3 и так далеедальш
    val candidates = unique.toList.zipWithIndex.map { case (c, index) => c.copy(id = index + 1) }
    history.record("SCAN_END", "checked=" + checked + "; found=" + candidates.size +
      "; errorsOrSkipped=" + skipped.size)
    ScanResult(candidates, checked, roots.map(_.id), profiles, skipped.toList, warnings.toList)
  }
}
