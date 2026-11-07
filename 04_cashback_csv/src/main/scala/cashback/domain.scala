package cashback

import scala.math.BigDecimal.RoundingMode

opaque type Money = BigDecimal

object Money:
  private val Max = BigDecimal("9999999.99")
  private val Decimal = """-?\d+(\.\d+)?""".r

  def apply(value: BigDecimal): Money = value.setScale(2, RoundingMode.HALF_UP)
  def apply(value: Int): Money = apply(BigDecimal(value))
  def zero: Money = apply(0)

  def parse(raw: String): Either[String, Money] =
    parseDecimal(raw, "Сума").flatMap { value =>
      if value.scale > 2 then Left("Більше двох знаків після коми")
      else if value <= 0 then Left("Сума має бути більшою за 0")
      else if value > Max then Left("Сума перевищує допустимий максимум")
      else Right(Money(value))
    }

  private[cashback] def parseDecimal(raw: String, label: String): Either[String, BigDecimal] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Left(s"$label відсутня")
    else if !Decimal.matches(trimmed) then Left(s"Некоректний запис $label")
    else
      try Right(BigDecimal(trimmed))
      catch case _: NumberFormatException => Left(s"Некоректний запис $label")

  extension (m: Money)
    def toBigDecimal: BigDecimal = m
    def +(other: Money): Money = Money(m + other)
    def -(other: Money): Money = Money(m - other)
    def >=(other: Money): Boolean = m >= other
    def >(other: Money): Boolean = m > other
    def <(other: Money): Boolean = m < other
    def <=(other: Money): Boolean = m <= other
    def isPositive: Boolean = m > 0
    def show: String = s"$m грн"

opaque type Percent = BigDecimal

object Percent:
  def parse(raw: String): Either[String, Percent] =
    Money.parseDecimal(raw, "Відсоток").flatMap { value =>
      if value.scale > 2 then Left("Більше двох знаків після коми")
      else if value < 0 || value > 100 then Left("Відсоток має бути в межах 0..100")
      else Right(value)
    }

  extension (p: Percent) def toBigDecimal: BigDecimal = p

opaque type Category = String

object Category:
  val Allowed: Set[String] = Set(
    "CAFE",
    "GROCERIES",
    "PHARMACY",
    "FUEL",
    "TAXI",
    "ONLINE",
    "CINEMA"
  )

  def fromString(raw: String): Either[String, Category] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Left("Порожня категорія")
    else if !trimmed.matches("""[A-Za-z0-9._-]{1,64}""") then Left("Некоректний токен категорії")
    else if !Allowed.contains(trimmed) then Left(s"Невідома категорія '$trimmed'")
    else Right(trimmed)

  extension (c: Category) def name: String = c

enum CashbackEvent:
  case Accrue(category: Category, purchase: Money, percent: Percent)
  case Withdraw(amount: Money)
  case PartnerBonus(amount: Money)
  case RefundReversal(amount: Money)

enum ParseError:
  case MalformedRow(row: Int, raw: String, reason: String)
  case UnknownEvent(row: Int, code: String)
  case InvalidAmount(row: Int, raw: String, reason: String)
  case InvalidPercent(row: Int, raw: String, reason: String)
  case InvalidToken(row: Int, raw: String, reason: String)

  def describe: String = this match
    case MalformedRow(row, _, reason) => s"рядок $row: $reason"
    case UnknownEvent(row, code) => s"рядок $row: невідома подія '$code'"
    case InvalidAmount(row, raw, reason) => s"рядок $row: сума '$raw' — $reason"
    case InvalidPercent(row, raw, reason) => s"рядок $row: відсоток '$raw' — $reason"
    case InvalidToken(row, raw, reason) => s"рядок $row: категорія '$raw' — $reason"

enum BusinessError:
  case BelowMinimum(amount: Money, minimum: Money)
  case InsufficientCashback(requested: Money, available: Money)
  case RefundExceedsAccrued(requested: Money, accrued: Money)

  def describe: String = this match
    case BelowMinimum(amount, minimum) =>
      s"Виведення заборонено: сума ${amount.show} менша за мінімальний поріг ${minimum.show}"
    case InsufficientCashback(requested, available) =>
      s"Недостатньо кешбеку: доступно ${available.show}, запрошено ${requested.show}"
    case RefundExceedsAccrued(requested, accrued) =>
      s"Неможливо анулювати ${requested.show}: залишок нарахованого кешбеку ${accrued.show}"

final case class RejectedEvent(row: Int, event: CashbackEvent, error: BusinessError)

final case class ImportReport(
  parseErrors: List[ParseError],
  rejected: List[RejectedEvent],
  state: CashbackAccount,
  accepted: Int
)
