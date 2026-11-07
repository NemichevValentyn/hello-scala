package cashback

import scala.annotation.tailrec

final case class AccrualLot(category: Category, remaining: Money)

/**
 * Стан кешбек-рахунку після згортання журналу.
 *
 * Повернення списує найстаріші ще живі нарахування (FIFO) і зменшує суму
 * відповідної категорії. Партнерський бонус збільшує лише доступний баланс.
 * Виведення менше 100 грн не змінює стан.
 */
final case class CashbackAccount(
  available: Money = Money.zero,
  categoryAccruals: Map[Category, Money] = Map.empty,
  totalWithdrawn: Money = Money.zero,
  openAccruals: Vector[AccrualLot] = Vector.empty,
  accrued: Money = Money.zero,
  minWithdrawal: Money = CashbackAccount.MinWithdrawal
):
  def availableCashback: Money = available

  def topCategory: Option[(Category, Money)] =
    categoryAccruals.maxByOption((_, amount) => amount.toBigDecimal)

  def totalWithdrawnAmount: Money = totalWithdrawn

  def accruedRemaining: Money = accrued

  def invariantHolds: Boolean =
    available >= Money.zero &&
      totalWithdrawn >= Money.zero &&
      categoryAccruals.values.forall(_ >= Money.zero) &&
      openAccruals.forall(_.remaining > Money.zero) &&
      categoryAccruals.values.foldLeft(Money.zero)(_ + _) == accruedRemaining

  def applyEvent(event: CashbackEvent): Either[BusinessError, CashbackAccount] =
    event match
      case CashbackEvent.Accrue(category, purchase, percent) =>
        Right(applyAccrue(category, purchase, percent))
      case CashbackEvent.Withdraw(amount) =>
        applyWithdraw(amount)
      case CashbackEvent.PartnerBonus(amount) =>
        Right(copy(available = available + amount))
      case CashbackEvent.RefundReversal(amount) =>
        applyRefund(amount)

  def replay(events: Seq[(Int, CashbackEvent)]): (List[RejectedEvent], CashbackAccount) =
    val (rejectedRev, state) =
      events.foldLeft((List.empty[RejectedEvent], this)):
        case ((rejected, account), (row, event)) =>
          account.applyEvent(event) match
            case Left(error) => (RejectedEvent(row, event, error) :: rejected, account)
            case Right(next) => (rejected, next)
    (rejectedRev.reverse, state)

  private def applyAccrue(category: Category, purchase: Money, percent: Percent): CashbackAccount =
    val cashback = Money(purchase.toBigDecimal * percent.toBigDecimal / BigDecimal(100))
    if !cashback.isPositive then this
    else
      copy(
        available = available + cashback,
        categoryAccruals = add(categoryAccruals, category, cashback),
        openAccruals = openAccruals :+ AccrualLot(category, cashback),
        accrued = accrued + cashback
      )

  private def applyWithdraw(amount: Money): Either[BusinessError, CashbackAccount] =
    if amount < minWithdrawal then
      Left(BusinessError.BelowMinimum(amount, minWithdrawal))
    else if amount > available then
      Left(BusinessError.InsufficientCashback(amount, available))
    else
      Right(copy(available = available - amount, totalWithdrawn = totalWithdrawn + amount))

  private def applyRefund(amount: Money): Either[BusinessError, CashbackAccount] =
    if amount > accrued then
      Left(BusinessError.RefundExceedsAccrued(amount, accrued))
    else if amount > available then
      Left(BusinessError.InsufficientCashback(amount, available))
    else
      consume(openAccruals, amount.toBigDecimal, Map.empty) match
        case None =>
          Left(BusinessError.RefundExceedsAccrued(amount, accrued))
        case Some((lots, deltas)) =>
          Right(
            copy(
              available = available - amount,
              categoryAccruals = subtract(categoryAccruals, deltas),
              openAccruals = lots,
              accrued = accrued - amount
            )
          )

  @tailrec
  private def consume(
    rest: Vector[AccrualLot],
    left: BigDecimal,
    deltas: Map[Category, Money]
  ): Option[(Vector[AccrualLot], Map[Category, Money])] =
    if left.signum == 0 then Some((rest, deltas))
    else if rest.isEmpty then None
    else
      val lot = rest.head
      val tail = rest.tail
      val lotAmount = lot.remaining.toBigDecimal
      if lotAmount <= left then
        consume(tail, left - lotAmount, add(deltas, lot.category, lot.remaining))
      else
        val taken = Money(left)
        val updated = lot.copy(remaining = lot.remaining - taken)
        Some((updated +: tail, add(deltas, lot.category, taken)))

  private def add(values: Map[Category, Money], category: Category, amount: Money): Map[Category, Money] =
    values.updated(category, values.getOrElse(category, Money.zero) + amount)

  private def subtract(values: Map[Category, Money], deltas: Map[Category, Money]): Map[Category, Money] =
    deltas.foldLeft(values):
      case (acc, (category, amount)) =>
        val updated = acc.getOrElse(category, Money.zero) - amount
        if updated <= Money.zero then acc - category
        else acc.updated(category, updated)

object CashbackAccount:
  val MinWithdrawal: Money = Money(BigDecimal("100.00"))

  def empty: CashbackAccount = CashbackAccount()
