package cleaner
// профиль определяет какие правила применять при поиске
// например editors ищет временные файлы редакторов,
// full включает все категории
case class Profile(
  id: String,
  rules: List[String],
  categories: List[String],
  excludedRules: List[String],
  retentionDays: Int,
  maximumSize: Long
)
