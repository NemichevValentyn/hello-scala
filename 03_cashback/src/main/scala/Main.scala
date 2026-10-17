import cashback.*
import cashback.CashbackEvent.*
import cashback.ApplyResult.*

@main def main(): Unit =
  val events = Seq(
    Accrual(AccrualId("a1"), Category("Кафе"), Money(500), BigDecimal(5)),
    Accrual(AccrualId("a2"), Category("Продукти"), Money(2000), BigDecimal(3)),
    Accrual(AccrualId("a3"), Category("Кафе"), Money(800), BigDecimal(5)),
    PartnerBonus("Rozetka", Money(50)),
    Withdrawal(Money(100)),
    Refund(AccrualId("a1"))
  )

  CashbackAccount.fromEvents(events) match
    case Ok(account) =>
      println("=== Кешбек-рахунок онлайн-банку (Варіант 5) ===")
      println(s"Доступний кешбек:        ${account.availableCashback.show}")
      account.topCategory match
        case Some((cat, sum)) =>
          println(s"Топ-категорія:           ${cat.name} (${sum.show})")
        case None =>
          println("Топ-категорія:           немає нарахувань")
      println(s"Виведено за весь час:    ${account.totalWithdrawnAmount.show}")
    case Rejected(reason) =>
      println(s"Помилка обробки подій: $reason")
