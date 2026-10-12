package cleaner

import java.nio.file.Files
import java.time.Instant
import scala.io.StdIn
import scala.util.control.NonFatal
import cleaner.menu.{MenuContainer2, MenuLeaf, MenuLoop}

// связывает пункты меню с поиском, настройками и карантином
class ConsoleMenu(configService: ConfigService, initialConfig: AppConfig) {

  // текущие настройки, они могут поменяться во время работы
  private var config = initialConfig

  // объекты для истории, состояния карантина и работы с его файлами
  private var history = new HistoryService(config)
  private var state = new StateService(config, history)
  private var quarantine = new QuarantineService(config, state, history)

  // показывает список найденного и подробности об объектах
  private val view = new ResultView()

  // пока поиск не запускали, результат пустой
  private var result = ScanResult(Nil, 0, Nil, Nil, Nil, Nil)

  // сюда попадают объекты, которые пользователь выбрал для карантина
  private var selected = List[Candidate]()

  // задаём вопрос и читаем ответ из консоли
  private def ask(question: String): String = {
    print(question + ": ")
    val answer = StdIn.readLine()

    // null означает что ввод закончился, а не просто нажали enter
    if (answer == null) {
      throw new java.io.EOFException("Ввод завершён")
    }

    // убираем пробелы в начале и конце ответа
    answer.trim
  }

  // подтверждение принимается только если написали ДА
  private def confirm(question: String): Boolean =
    ask(question + " Напишите ДА") == "ДА"

  // из ответа вроде 1,2,3 получаем список отдельных строк
  private def ids(text: String): List[String] =
    text.split(",").toList.map(_.trim).filter(_.nonEmpty)

  // применяем новые настройки и заново создаём связанные с ними объекты
  private def changeConfig(next: AppConfig): Unit = {
    config = next

    // сохраняем историю текущего запуска, чтобы она не потерялась
    val previousSession = history.session.toList
    history = new HistoryService(config)
    history.session ++= previousSession

    state = new StateService(config, history)
    quarantine = new QuarantineService(config, state, history)

    // старые результаты могли уже не подходить под новые настойки
    result = ScanResult(Nil, 0, Nil, Nil, Nil, Nil)
    selected = Nil
  }

  // выводим все настроенные точки поиска
  private def roots(): Unit = {
    config.roots.foreach(r =>
      println(
        r.id + " | " + r.name + " | " + r.path +
          " | " + r.profile +
          " | глубина=" + r.depth +
          " | активна=" + r.enabled +
          " | исключения=" + r.exclusions.mkString(";")
      )
    )
  }

  // edit=false добавляет точку, edit=true меняет существующую
  private def editRoot(edit: Boolean): Unit = {
    roots()

    val id = ask("Идентификатор (латиница, цифры, дефис)")

    // при изменении точка должна быть, а при добавлении id должен быть свободным
    if (edit) {
      require(config.roots.exists(_.id == id), "Точка не найдена")
    } else {
      require(!config.roots.exists(_.id == id), "Идентификатор занят")
    }

    // собираем точку из ответов пользоватлея
    val root = SearchRoot(
      id,
      ask("Название"),
      configService.userPath(ask("Путь")),
      ask("Профиль"),
      ask("Глубина: 0 — не заходить внутрь, 2147483647 — без ограничения").toInt,
      ask("Активна? true/false").toBoolean,
      ids(ask("Идентификаторы исключений через запятую или Enter"))
    )

    // убираем прежнюю точку с этим id и добавляем новую версию
    val updated = config.roots.filterNot(_.id == id) :+ root

    // сохраняем точки в файл и применяем полученные настройки
    changeConfig(configService.saveRoots(updated))
    history.record(if (edit) "ROOT_EDIT" else "ROOT_ADD", id)
  }

  // спрашиваем где искать и с каким профилем, потом запускаем поиск
  private def scan(): Unit = {
    roots()

    val answer = ask("Точки через запятую или all для всех активных")

    // all выбирает активные точки, иначе ищем точки по введённым id
    val chosen =
      if (answer == "all") {
        config.roots.filter(_.enabled)
      } else {
        ids(answer).map(id =>
          config.roots.find(_.id == id).getOrElse {
            throw new IllegalArgumentException("Нет точки: " + id)
          }
        ).distinct
      }

    require(chosen.nonEmpty, "Точки не выбраны")

    // пустой ответ оставляет профиль, который указан у каждой точки
    val profile = ask(
      "Профиль или Enter, чтобы использовать профиль каждой точки"
    )

    if (profile.nonEmpty) {
      config.profile(profile)
    }

    // папки могли исчезнуть после запуска, поэтому проверяем настройки ещё раз
    configService.validate(config)

    // выбор из прошлого поиска больше не нужен
    selected = Nil

    result = new Scanner(config, history).scan(chosen, profile)

    // сразу показываем найденное по категориям, большие объекты идут первыми
    view.show(result.candidates, "category", "size", "")
    result.warnings.foreach(println)
    println("Пропусков/ошибок: " + result.skipped.size)

    report()
  }

  // выбераем какие найденные объекты потом переносить в карантин
  private def select(): Unit = {
    ask("Выбор: ids, category, rule, all, clear") match {

      // выбираем конкретные номера из результатов поиска
      case "ids" =>
        val chosen = ids(ask("Номера через запятую")).map(_.toInt)

        require(
          chosen.forall(id => result.candidates.exists(_.id == id)),
          "Неизвестный номер"
        )

        selected = result.candidates.filter(c => chosen.contains(c.id))

      // выбираем все объекты одной категории
      case "category" =>
        val category = ask("Категория")
        selected = result.candidates.filter(_.rule.category == category)

      // выбираем объекты с этим основным правилом
      case "rule" =>
        val rule = ask("Идентификатор правила")
        selected = result.candidates.filter(_.rule.id == rule)

      case "all" =>
        selected = result.candidates

      case "clear" =>
        selected = Nil

      case _ =>
        println("Неизвестный способ выбора")
    }

    println("Выбрано: " + selected.size)
  }

  // показываем план и переносим выбранное только после подтверждения
  private def moveSelected(): Unit = {
    require(selected.nonEmpty, "Сначала выберите объекты")

    view.show(selected, "category", "path", "")

    val answer = ask("Срок в днях или Enter, чтобы оставить сроки правил")

    // -1 здесь означает что пользователь не задал свой срок
    val days = if (answer.isEmpty) -1 else answer.toInt
    require(days >= 0 || answer.isEmpty, "Отрицательный срок")

    // показываем какой срок будет у каждого выбранного объекта
    for (c <- selected) {
      println(
        c.relative + " — " +
          (if (days >= 0) days else c.retentionDays) + " дней"
      )
    }

    if (confirm("Поместить эти объекты в карантин?")) {
      quarantine.quarantine(selected, days, result.candidates.size)
      selected = Nil
      report()
    }
  }

  // возвращаем объект из карантина по исходному или другому пути
  private def restore(): Unit = {
    quarantine.show()

    val id = ask("Полный номер записи")
    val entry = quarantine.find(id)

    // сначала предполагаем восстановление на прежнее место
    var target = entry.original
    var replace = false

    if (FileService.exists(target)) {

      // если место занято, пользователь решает как поступить
      ask(
        "Путь занят. Выберите: name — новое имя, folder — другая папка, replace — заменить, cancel"
      ) match {

        case "name" =>
          val name = ask("Новое имя без пути")

          // здесь нужно только имя, переходы в другие папки не разрешаем
          require(
            name.nonEmpty &&
              !name.contains("/") &&
              !name.contains("\\") &&
              name != "." &&
              name != "..",
            "Нужно только имя"
          )

          target = target.resolveSibling(name)

        case "folder" =>
          // берём другую папку, имя объекта оставляем прежним
          target = configService.userPath(
            ask("Другая папка")
          ).resolve(entry.name)

        case "replace" =>
          // замена требует отдельного подтверждения
          replace = confirm(
            "Заменить существующий объект? Его копия будет сохранена в карантине."
          )

          if (!replace) return

        case _ =>
          return
      }

    } else {

      // если исходный путь свободен, можно оставить его или указать другой
      val alternative = ask(
        "Путь для восстановления или Enter для исходного"
      )

      if (alternative.nonEmpty) {
        target = configService.userPath(alternative)
      }
    }

    quarantine.restore(id, target, replace)
    report()
  }

  // окончательное удаление перечисленных записей, сначала обязательно спрашиваем
  private def purge(items: List[QuarantineEntry]): Unit = {
    if (items.isEmpty) {
      println("Нет подходящих записей.")
      return
    }

    items.foreach(e =>
      println(e.id + " | " + e.name + " | " + e.size + " байт")
    )

    if (confirm(
      "Окончательное удаление необратимо. Удалить перечисленные объекты?"
    )) {
      quarantine.purge(items)
      report()
    }
  }

  // показываем записи с истёкшим сроком и спрашиваем что с ними делать
  private def expired(): Unit = {
    val items = quarantine.expired
    history.record("EXPIRED_CHECK", "count=" + items.size)

    if (items.isEmpty) {
      println("Просроченных записей нет.")
      return
    }

    println(
      "Просрочено: " + items.size +
        "; объём: " + items.map(_.size).sum + " байт"
    )

    // считаем сколько полных дней прошло с самого раннего перемещения
    val oldestDays = math.max(0L,
      (System.currentTimeMillis() - items.map(_.movedAt).min) / 86400000L)
    println("Самый старый объект: " + oldestDays + " дней")

    ask(
      "list — список, all — удалить все, ids — выбрать, extend — продлить, Enter — оставить"
    ) match {

      case "list" =>
        items.foreach(e =>
          println(
            e.id + " | " + e.original +
              " | до " + Instant.ofEpochMilli(e.expiresAt)
          )
        )

      case "all" =>
        purge(items)

      case "ids" =>
        val selectedIds = ids(ask("Полные номера через запятую"))

        // проверяем что все введённые номера есть среди просроченных
        require(
          selectedIds.forall(id => items.exists(_.id == id)),
          "Неизвестная просроченная запись"
        )

        purge(items.filter(e => selectedIds.contains(e.id)))

      case "extend" =>
        quarantine.extend(
          ask("Полный номер"),
          ask("На сколько дней").toInt
        )

      case _ =>
        println("Данные оставлены без изменений.")
    }
  }

  // сохраняем отчёт о поиске и операциях текущего запуска
  private def report(): Unit =
    new ReportService(config, history).save(result, quarantine)

  // перечитываем config.properties после ручного изменения
  private def reload(): Unit = {
    val next = configService.load()

    // пути хранения во время работы менять нельзя, для этого нужен перезапуск
    require(
      next.quarantine == config.quarantine &&
        next.stateFile == config.stateFile,
      "Для изменения путей карантина и состояния перезапустите программу"
    )

    changeConfig(next)
    history.record(
      "RELOAD",
      "Конфигурация загружена; прежние результаты сброшены"
    )
  }

  // удаляем точку из настроек, саму папку этот метод не удаляет
  private def removeRoot(): Unit = {
    roots()

    val id = ask("Идентификатор точки")
    require(config.roots.exists(_.id == id), "Точка не найдена")

    changeConfig(
      configService.saveRoots(config.roots.filterNot(_.id == id))
    )

    history.record("ROOT_REMOVE", id)
  }

  // показываем профили и какие правила или категории в них входят
  private def showProfiles(): Unit = {
    config.profiles.foreach(p =>
      println(
        p.id +
          " | категории=" + p.categories.mkString(",") +
          " | правила=" + p.rules.mkString(",") +
          " | исключены=" + p.excludedRules.mkString(",")
      )
    )
  }

  // пользователь выбирает группировку, сортировку и фильтр результатов
  private def showResults(): Unit = {
    view.show(
      result.candidates,
      ask("Группировка: root, category, rule"),
      ask("Сортировка: size, age, path, category"),
      ask("Фильтр: часть пути / категория / Enter")
    )
  }

  // находим кандидата по номеру и показываем все сведения о нём
  private def showDetails(): Unit = {
    val id = ask("Номер объекта").toInt

    val candidate = result.candidates.find(_.id == id).getOrElse {
      throw new IllegalArgumentException("Объект не найден")
    }

    view.details(candidate)
  }

  // добавляем дни к сроку хранения одной записи карантина
  private def extendRetention(): Unit = {
    quarantine.extend(
      ask("Полный номер записи"),
      ask("На сколько дней").toInt
    )
  }

  // получаем выбранные записи карантина по их длинным номерам
  private def purgeSelected(): Unit = {
    val entries = ids(
      ask("Полные номера через запятую")
    ).distinct.map(quarantine.find)

    purge(entries)
  }

  // как на лабе: контейнер открывает подменю, лист запускает действие
  private def createMenu(): MenuContainer2 = {

    // действия с точками поиска
    val rootMenu = MenuContainer2("Точки поиска", Seq(
      MenuLeaf("Посмотреть точки", () => roots()),
      MenuLeaf("Добавить точку", () => editRoot(false)),
      MenuLeaf("Изменить точку", () => editRoot(true)),
      MenuLeaf("Удалить точку", () => removeRoot())
    ))

    // действия с поиском и найденными объектами
    val searchMenu = MenuContainer2("Поиск и результаты", Seq(
      MenuLeaf("Посмотреть профили", () => showProfiles()),
      MenuLeaf("Начать поиск", () => scan()),
      MenuLeaf("Посмотреть результаты", () => showResults()),
      MenuLeaf("Подробнее об объекте", () => showDetails()),
      MenuLeaf("Выбрать объекты", () => select()),
      MenuLeaf(
        "Поместить выбранные объекты в карантин",
        () => moveSelected()
      )
    ))

    // действия с уже перемещёнными объектами
    val quarantineMenu = MenuContainer2("Карантин", Seq(
      MenuLeaf("Посмотреть карантин", () => quarantine.show()),
      MenuLeaf("Восстановить объект", () => restore()),
      MenuLeaf("Продлить срок хранения", () => extendRetention()),
      MenuLeaf(
        "Окончательно удалить выбранные записи",
        () => purgeSelected()
      ),
      MenuLeaf("Просроченные записи", () => expired())
    ))

    // история, отчёт и перезагрузка настроек
    val serviceMenu = MenuContainer2("История и настройки", Seq(
      MenuLeaf("Посмотреть историю", () => history.show()),
      MenuLeaf("Сохранить отчёт", () => report()),
      MenuLeaf("Перезагрузить настройки", () => reload())
    ))

    // собираем четыре подменю в главное, назад и выход добавляет MenuLoop
    MenuContainer2(
      "Менеджер временных файлов",
      Seq(rootMenu, searchMenu, quarantineMenu, serviceMenu)
    )
  }

  // выводим причину ошибки и записываем её тип в журнал
  private def showError(error: Throwable): Unit = {
    println("Операция не выполнена: " + error.getMessage)
    history.record("ERROR", error.getClass.getSimpleName, "ERROR")
  }

  // этот метод запускается из Main
  def run(): Unit = {
    history.record(
      "START",
      "Программа запущена; конфигурация проверена"
    )

    try {
      try {
        // при старте показываем карантин
        quarantine.show()

        // если настройка включена и есть просроченное, спрашиваем о нём
        if (config.askExpired && quarantine.expired.nonEmpty) {
          expired()
        }

      } catch {
        // закрытый ввод передаём наружу, остальные ошибки показываем пользователю
        case e: java.io.EOFException => throw e
        case NonFatal(error) => showError(error)
      }

      // передаём меню действия для обработки ошибок и неправильного номера
      val menu = new MenuLoop(
        error => showError(error),
        () => history.record("UNKNOWN_COMMAND", "Неверный номер пункта")
      )

      // создаём дерево меню и начинаем читать выбор пользователя
      menu.run(createMenu())

    } catch {
      // если ввод закончился, просто завершаем работу
      case _: java.io.EOFException => ()

    } finally {
      // завершение записываем и при обычном выходе, и при ошибке
      history.record("EXIT", "Программа завершена")
    }
  }
}
