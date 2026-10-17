package cashback

import org.scalatest.funsuite.AnyFunSuite
import cashback.CashbackEvent.*
import cashback.ApplyResult.*

class CashbackAccountSpec extends AnyFunSuite:

  private val cafe = Category("Кафе")
  private val products = Category("Продукти")

  test("сценарій: нарахування - бонус - виведення - підсумкові розрахунки"):
    val events = Seq(
      Accrual(AccrualId("a1"), cafe, Money(1000), BigDecimal(5)),
      Accrual(AccrualId("a2"), products, Money(3000), BigDecimal(4)),
      Accrual(AccrualId("a3"), cafe, Money(600), BigDecimal(5)),
      PartnerBonus("Silpo", Money(40)),
      Withdrawal(Money(100))
    )

    CashbackAccount.fromEvents(events) match
      case Ok(account) =>
        assert(account.availableCashback == Money(140))
        assert(account.topCategory.contains((products, Money(120))))
        assert(account.totalWithdrawnAmount == Money(100))
      case Rejected(reason) =>
        fail(s"Очікувався успіх, отримано відхилення: $reason")

  test("граничний випадок: виведення менше мінімального порогу (100 грн) заборонено"):
    val start = CashbackAccount.empty()
    val withBalance = start.applyEvent(
      Accrual(AccrualId("a1"), cafe, Money(2000), BigDecimal(5))
    )

    withBalance match
      case Ok(account) =>
        account.applyEvent(Withdrawal(Money(99))) match
          case Rejected(reason) =>
            assert(reason.contains("мінімальний поріг"))
            assert(account.availableCashback == Money(100))
            assert(account.totalWithdrawnAmount == Money.zero)
          case Ok(_) =>
            fail("Виведення менше порогу мало бути відхилене")
      case Rejected(reason) =>
        fail(s"Нарахування мало пройти: $reason")

  test("Refund анулює раніше нарахований кешбек і зменшує суму категорії"):
    val events = Seq(
      Accrual(AccrualId("a1"), cafe, Money(500), BigDecimal(10)),
      Accrual(AccrualId("a2"), cafe, Money(300), BigDecimal(10)),
      Refund(AccrualId("a1"))
    )

    CashbackAccount.fromEvents(events) match
      case Ok(account) =>
        assert(account.availableCashback == Money(30))
        assert(account.topCategory.contains((cafe, Money(30))))
        assert(!account.accruals.contains(AccrualId("a1")))
      case Rejected(reason) =>
        fail(s"Очікувався успіх: $reason")

  test("партнерський супербонус збільшує доступний баланс без категорії"):
    CashbackAccount.empty().applyEvent(PartnerBonus("Rozetka", Money(75))) match
      case Ok(account) =>
        assert(account.availableCashback == Money(75))
        assert(account.categoryAccruals.isEmpty)
      case Rejected(reason) =>
        fail(s"Бонус мав пройти: $reason")

  test("виведення більше доступного балансу заборонено"):
    val result = CashbackAccount
      .empty()
      .applyEvent(Accrual(AccrualId("a1"), products, Money(1000), BigDecimal(5)))
      match
        case Ok(acc) => acc.applyEvent(Withdrawal(Money(100)))
        case r       => r

    result match
      case Rejected(reason) =>
        assert(reason.contains("Недостатньо"))
      case Ok(_) =>
        fail("Овердрафт мав бути заборонений")
