package cashback

import CashbackEvent.*
import ApplyResult.*

/**
 * Стан кешбек-рахунку онлайн-банку.
 *
 * @param available          доступний до виведення баланс
 * @param categoryAccruals   суми нарахувань по категоріях (без бонусів/виведень)
 * @param totalWithdrawn     загальна сума успішних виведень за весь час
 * @param accruals           активні (ще не анульовані) нарахування для Refund
 * @param minWithdrawal      мінімальний поріг виведення (за замовчуванням 100 грн)
 */
final case class CashbackAccount(
    available: Money = Money.zero,
    categoryAccruals: Map[Category, Money] = Map.empty,
    totalWithdrawn: Money = Money.zero,
    accruals: Map[AccrualId, AccrualRecord] = Map.empty,
    minWithdrawal: Money = Money(100)
):

  def availableCashback: Money = available

  def topCategory: Option[(Category, Money)] =
    categoryAccruals.maxByOption((_, amount) => amount.toBigDecimal)

  def totalWithdrawnAmount: Money = totalWithdrawn

  def applyEvent(event: CashbackEvent): ApplyResult = event match
    case Accrual(id, category, purchaseAmount, percent) =>
      applyAccrual(id, category, purchaseAmount, percent)

    case Withdrawal(amount) =>
      applyWithdrawal(amount)

    case PartnerBonus(_, amount) =>
      applyPartnerBonus(amount)

    case Refund(accrualId) =>
      applyRefund(accrualId)

  def applyEvents(events: Seq[CashbackEvent]): ApplyResult =
    events.foldLeft[ApplyResult](Ok(this)):
      case (Ok(acc), event)      => acc.applyEvent(event)
      case (rejected: Rejected, _) => rejected

  private def applyAccrual(
      id: AccrualId,
      category: Category,
      purchaseAmount: Money,
      percent: BigDecimal
  ): ApplyResult =
    if accruals.contains(id) then
      Rejected(s"Нарахування з id '${id.value}' вже існує")
    else if !purchaseAmount.isPositive then
      Rejected("Сума покупки має бути додатною")
    else if percent <= 0 then
      Rejected("Відсоток кешбеку має бути додатним")
    else
      val cashback = Money(purchaseAmount.toBigDecimal * percent / BigDecimal(100))
      val record = AccrualRecord(category, cashback)
      Ok(
        copy(
          available = available + cashback,
          categoryAccruals = addToCategory(category, cashback),
          accruals = accruals + (id -> record)
        )
      )

  private def applyWithdrawal(amount: Money): ApplyResult =
    if !amount.isPositive then
      Rejected("Сума виведення має бути додатною")
    else if amount < minWithdrawal then
      Rejected(
        s"Виведення заборонено: сума ${amount.show} менша за мінімальний поріг ${minWithdrawal.show}"
      )
    else if amount > available then
      Rejected(
        s"Недостатньо кешбеку: доступно ${available.show}, запрошено ${amount.show}"
      )
    else
      Ok(
        copy(
          available = available - amount,
          totalWithdrawn = totalWithdrawn + amount
        )
      )

  private def applyPartnerBonus(amount: Money): ApplyResult =
    if !amount.isPositive then
      Rejected("Сума партнерського бонусу має бути додатною")
    else
      Ok(copy(available = available + amount))

  private def applyRefund(accrualId: AccrualId): ApplyResult =
    accruals.get(accrualId) match
      case None =>
        Rejected(s"Нарахування '${accrualId.value}' не знайдено або вже анульовано")
      case Some(record) =>
        val afterRefund = available - record.cashback
        if afterRefund < Money.zero then
          Rejected(
            s"Неможливо анулювати кешбек ${record.cashback.show}: недостатньо доступного балансу (${available.show})"
          )
        else
          Ok(
            copy(
              available = afterRefund,
              categoryAccruals = subtractFromCategory(record.category, record.cashback),
              accruals = accruals - accrualId
            )
          )

  private def addToCategory(category: Category, amount: Money): Map[Category, Money] =
    val current = categoryAccruals.getOrElse(category, Money.zero)
    categoryAccruals + (category -> (current + amount))

  private def subtractFromCategory(category: Category, amount: Money): Map[Category, Money] =
    val current = categoryAccruals.getOrElse(category, Money.zero)
    val updated = current - amount
    if updated.toBigDecimal <= 0 then categoryAccruals - category
    else categoryAccruals + (category -> updated)

final case class AccrualRecord(category: Category, cashback: Money)

object CashbackAccount:
  def empty(minWithdrawal: Money = Money(100)): CashbackAccount =
    CashbackAccount(minWithdrawal = minWithdrawal)

  def fromEvents(
      events: Seq[CashbackEvent],
      minWithdrawal: Money = Money(100)
  ): ApplyResult =
    empty(minWithdrawal).applyEvents(events)
