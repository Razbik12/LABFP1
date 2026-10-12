package cleaner
// общий результат поиска, Scanner создаёт его и возвращает в ConsoleMenu

case class ScanResult(
  candidates: List[Candidate],
  checked: Long,
  roots: List[String],
  profiles: List[String],
  skipped: List[String],
  warnings: List[String]
)
