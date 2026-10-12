package cleaner

import java.nio.file.Path

// запись об одном объекте в карантине
// хранит сведения для восстановления, проверки и окончательного удаления
case class QuarantineEntry(
  id: String,
  original: Path,
  root: Path,
  name: String,
  isDirectory: Boolean,
  size: Long,
  movedAt: Long,
  retentionDays: Int,
  expiresAt: Long,
  rule: String,
  category: String,
  fingerprint: String,
  status: String,
  restoreTarget: String
)
