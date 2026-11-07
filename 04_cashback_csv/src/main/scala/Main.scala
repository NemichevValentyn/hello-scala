import cashback.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

@main def main(args: String*): Unit =
  val path = args.headOption.getOrElse("testdata/variant_05_bank_cashback_account.csv")
  val file = Path.of(path)
  if !Files.isRegularFile(file) then
    println(s"Немає файлу $path")
    println("Згенеруйте журнал з репозиторію курсу:")
    println("  cd n04_state_event_logs/scripts")
    println("  python generate_event_logs.py --variant 5 --rows 500000")
    println("Очікуване ім'я: variant_05_bank_cashback_account.csv")
  else
    val lines = Files.readAllLines(file).asScala.toList
    val report = Journal.load(lines)
    val account = report.state
    println("=== Варіант 5. Кешбек-рахунок онлайн-банку ===")
    println(s"Файл:                    ${file.getFileName}")
    println(s"Рядків даних:            ${lines.size - 1}")
    println(s"Відхилено парсером:      ${report.parseErrors.size}")
    println(s"Прийнято подій:          ${report.accepted}")
    println(s"Заборонено інваріантом:  ${report.rejected.size}")
    println(s"Доступний кешбек:        ${account.availableCashback.show}")
    account.topCategory match
      case Some((category, sum)) =>
        println(s"Топ-категорія:           ${category.name} (${sum.show})")
      case None =>
        println("Топ-категорія:           немає нарахувань")
    println(s"Виведено за весь час:    ${account.totalWithdrawnAmount.show}")
    println(s"Інваріант балансу:       ${if account.invariantHolds then "дотримано" else "ПОРУШЕНО"}")
