package cashback

import scala.math.BigDecimal.RoundingMode

opaque type Money = BigDecimal

object Money:
  def apply(value: BigDecimal): Money = value.setScale(2, RoundingMode.HALF_UP)
  def apply(value: Double): Money = apply(BigDecimal(value))
  def apply(value: Int): Money = apply(BigDecimal(value))
  def zero: Money = apply(0)

  extension (m: Money)
    def toBigDecimal: BigDecimal = m
    def +(other: Money): Money = Money(m + other)
    def -(other: Money): Money = Money(m - other)
    def *(factor: BigDecimal): Money = Money(m * factor)
    def >=(other: Money): Boolean = m >= other
    def >(other: Money): Boolean = m > other
    def <(other: Money): Boolean = m < other
    def <=(other: Money): Boolean = m <= other
    def isPositive: Boolean = m > 0
    def isNonNegative: Boolean = m >= 0
    def abs: Money = Money(m.abs)
    def show: String = s"$m грн"

opaque type Category = String

object Category:
  def apply(name: String): Category = name.trim
  extension (c: Category) def name: String = c

opaque type AccrualId = String

object AccrualId:
  def apply(id: String): AccrualId = id
  extension (id: AccrualId) def value: String = id

enum CashbackEvent:
  case Accrual(
      id: AccrualId,
      category: Category,
      purchaseAmount: Money,
      cashbackPercent: BigDecimal
  )
  case Withdrawal(amount: Money)
  case PartnerBonus(partner: String, amount: Money)
  case Refund(accrualId: AccrualId)

enum ApplyResult:
  case Ok(account: CashbackAccount)
  case Rejected(reason: String)
