package cleaner

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class ReportService(config: AppConfig, history: HistoryService) {
  def save(scan: ScanResult, quarantine: QuarantineService): Unit = {
    val text = new StringBuilder("ОТЧЁТ TEMPORARY CLEANER\n")
    text.append("Точки: " + scan.roots.mkString(", ") + "\n")
    text.append("Профили: " + scan.profiles.mkString(", ") + "\n")
    text.append("Проверено: " + scan.checked + "\nНайдено: " + scan.candidates.size + "\n")
    text.append("Предполагаемый объём: " + scan.candidates.map(_.info.size).sum + " байт\n")
    for ((category, items) <- scan.candidates.groupBy(_.rule.category).toList.sortBy(_._1)) {
      text.append(category + ": " + items.size + " объектов, " + items.map(_.info.size).sum + " байт\n")
    }
    text.append("\nНАЙДЕННЫЕ ОБЪЕКТЫ\n")
    for (c <- scan.candidates) {
      text.append(c.id.toString + " | " + c.path + " | " + c.info.size + " байт | " + c.rule.id +
        " | " + c.retentionDays + " дней | " + c.reason + "\n")
      c.warnings.foreach(w => text.append("Предупреждение: " + w + "\n"))
    }
    text.append("\nПРОПУСКИ И ОШИБКИ ДОСТУПА\n" + scan.skipped.mkString("\n") + "\n")
    text.append("\nПРЕДУПРЕЖДЕНИЯ\n" + scan.warnings.mkString("\n") + "\n")
    text.append("\nОПЕРАЦИИ ТЕКУЩЕГО ЗАПУСКА\n" + history.session.mkString("\n") + "\n")
    text.append("\nКАРАНТИН: " + quarantine.volume + " байт по записям состояния\n")
    FileService.checkNoLinks(config.reportFile)
    Files.createDirectories(config.reportFile.getParent)
    Files.write(config.reportFile, text.toString.getBytes(StandardCharsets.UTF_8))
    history.record("REPORT", "Отчёт сохранён")
    println("Отчёт: " + config.reportFile)
  }
}
