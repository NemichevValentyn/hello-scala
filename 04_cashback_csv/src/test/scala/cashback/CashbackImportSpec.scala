package cashback

import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import scala.io.Source

class CashbackImportSpec extends AnyFunSuite:

  test("короткий сценарій CSV: доступний кешбек, топ-категорія і сума виведень"):
    val report = Journal.load(
      List(
        "event,p1,p2,p3,p4",
        "ACCRUE,CAFE,1000.00,5.00,",
        "ACCRUE,GROCERIES,3000.00,4.00,",
        "ACCRUE,CAFE,600.00,5.00,",
        "PARTNER_BONUS,40.00,,,",
        "WITHDRAW,100.00,,,"
      )
    )

    assert(report.parseErrors.isEmpty)
    assert(report.rejected.isEmpty)
    assert(report.state.availableCashback == Money(140))
    assert(report.state.topCategory.map((category, sum) => (category.name, sum)).contains(("GROCERIES", Money(120))))
    assert(report.state.totalWithdrawnAmount == Money(100))
    assert(report.state.invariantHolds)

  test("невалідний рядок іде в Left і не змінює вже накопичений стан"):
    val before = Journal.load(List("event,p1,p2,p3,p4", "ACCRUE,CAFE,1000.00,5.00,"))
    val after = Journal.load(
      List(
        "event,p1,p2,p3,p4",
        "ACCRUE,CAFE,1000.00,5.00,",
        "NOT_AN_EVENT,1,2,3,4",
        "ACCRUE,FUEL",
        "ACCRUE,bad token,10.00,5.00,"
      )
    )

    assert(after.parseErrors.size == 3)
    assert(after.parseErrors.exists(_.isInstanceOf[ParseError.UnknownEvent]))
    assert(after.parseErrors.exists(_.isInstanceOf[ParseError.MalformedRow]))
    assert(after.parseErrors.exists(_.isInstanceOf[ParseError.InvalidToken]))
    assert(after.state == before.state)
    assert(after.state.availableCashback == Money(50))

  test("виведення менше 100 грн парситься, але інваріант його забороняє"):
    val parsed = Parser.parseLine(3, "WITHDRAW,50.00,,,")
    assert(parsed.isRight)

    val report = Journal.load(
      List(
        "event,p1,p2,p3,p4",
        "ACCRUE,CAFE,2000.00,10.00,",
        "WITHDRAW,50.00,,,"
      )
    )

    assert(report.parseErrors.isEmpty)
    report.rejected match
      case List(RejectedEvent(_, CashbackEvent.Withdraw(amount), BusinessError.BelowMinimum(_, minimum))) =>
        assert(amount == Money(50))
        assert(minimum == Money(100))
      case other =>
        fail(s"Очікувалась одна відмова через поріг, отримано $other")
    assert(report.state.availableCashback == Money(200))
    assert(report.state.totalWithdrawnAmount == Money.zero)
    assert(report.state.invariantHolds)

  test("повернення анулює найстаріше нарахування і зменшує суму категорії"):
    val report = Journal.load(
      List(
        "event,p1,p2,p3,p4",
        "ACCRUE,CAFE,1000.00,10.00,",
        "ACCRUE,GROCERIES,1000.00,5.00,",
        "REFUND_REVERSAL,40.00,,,"
      )
    )

    assert(report.parseErrors.isEmpty)
    assert(report.rejected.isEmpty)
    assert(report.state.availableCashback == Money(110))
    assert(report.state.categoryAccruals.map((category, sum) => category.name -> sum) == Map(
      "CAFE" -> Money(60),
      "GROCERIES" -> Money(50)
    ))
    assert(report.state.topCategory.map(_._1.name).contains("CAFE"))
    assert(report.state.invariantHolds)

  test("хвостові порожні колонки не губляться, а зайва колонка відхиляється"):
    assert(Parser.parseLine(1, "WITHDRAW,150.00,,,").isRight)
    assert(Parser.parseLine(2, "WITHDRAW,150.00,,,EXTRA").isLeft)
    assert(Parser.parseLine(3, "ACCRUE,CAFE,10.00,101,").left.exists(_.isInstanceOf[ParseError.InvalidPercent]))
    assert(Parser.parseLine(4, "PARTNER_BONUS,0.00,,,").left.exists(_.isInstanceOf[ParseError.InvalidAmount]))

  test("фрагмент згенерованого variant_05_bank_cashback_account.csv згортається без винятків"):
    val lines = resourceLines("variant_05_fragment.csv")
    val report = Journal.load(lines)
    val dataRows = lines.size - 1

    assert(lines.head == "event,p1,p2,p3,p4")
    assert(report.parseErrors.nonEmpty)
    assert(report.accepted > 0)
    assert(report.parseErrors.size + report.accepted + report.rejected.size == dataRows)
    assert(report.state.invariantHolds)

  test("повний журнал на 500000 рядків ділить рейки і тримає інваріант"):
    val file = bigLog
    assert(file.isFile, s"Немає ${file.getPath}. Команда генерації є в README.")
    val lines = fileLines(file)
    val report = Journal.load(lines)
    val dataRows = lines.size - 1
    val errorRate = report.parseErrors.size.toDouble / dataRows

    assert(dataRows == 500000)
    assert(report.parseErrors.size + report.accepted + report.rejected.size == dataRows)
    assert(errorRate > 0.02 && errorRate < 0.15)
    assert(report.rejected.nonEmpty)
    assert(report.state.invariantHolds)
    assert(report.state.availableCashback >= Money.zero)

  private def resourceLines(name: String): List[String] =
    val source = Source.fromResource(name)
    try source.getLines().toList
    finally source.close()

  private def fileLines(file: File): List[String] =
    val source = Source.fromFile(file, "UTF-8")
    try source.getLines().toList
    finally source.close()

  private def bigLog: File =
    List(
      new File("testdata/variant_05_bank_cashback_account.csv"),
      new File("04_cashback_csv/testdata/variant_05_bank_cashback_account.csv")
    ).find(_.isFile).getOrElse(new File("testdata/variant_05_bank_cashback_account.csv"))
