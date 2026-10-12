package cleaner

import java.time.Instant
// показывает результаты поиска, данные получает из Candidate
class ResultView {
  def show(items: List[Candidate], group: String, sort: String, filter: String): Unit = {
    var result = items.filter(c => filter.isEmpty || c.relative.contains(filter) || c.rule.category == filter)
    result = sort match {
      case "size" => result.sortBy(c => -c.info.size)
      case "age" => result.sortBy(_.info.modified)
      case "category" => result.sortBy(_.rule.category)
      case "path" => result.sortBy(_.path.toString)
      case _ => throw new IllegalArgumentException("Сортировка: size, age, category, path")
    }
    val grouped = group match {
      case "root" => result.groupBy(_.root.id)
      case "rule" => result.groupBy(_.rule.id)
      case "category" => result.groupBy(_.rule.category)
      case _ => throw new IllegalArgumentException("Группировка: root, rule, category")
    }
    for ((name, members) <- grouped.toList.sortBy(_._1)) {
      println("\n" + name + ": " + members.size + " объектов, " + members.map(_.info.size).sum + " байт")
      for (c <- members) println(c.id.toString + " | " + c.info.size + " байт | " + c.relative)
    }
    println("Всего: " + result.size + " объектов, " + result.map(_.info.size).sum + " байт")
  }

  def details(c: Candidate): Unit = {
    println("Объект: " + c.id + "\nПуть: " + c.path + "\nТочка: " + c.root.id)
    println("Тип: " + (if (c.info.isDirectory) "каталог" else "файл"))
    println("Размер: " + c.info.size + " байт\nИзменён: " + Instant.ofEpochMilli(c.info.modified))
    println("Категория: " + c.rule.category + "\nПравило: " + c.rule.name)
    println("Другие правила: " + c.otherRules.mkString(", ") + "\nХранить дней: " + c.retentionDays)
    println("Причина: " + c.reason)
    c.warnings.foreach(println)
  }
}
