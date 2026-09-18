import scala.collection.parallel.CollectionConverters._

object Workshop:

  // =========================================================================
  // ЗАВДАННЯ 2: РЕФАКТОРИНГ В ЧИСТЕ ФП (EXPRESSION-ORIENTED)
  // =========================================================================
  
  // 1. Готові рівні ризику бізнес-домену:
  sealed trait RiskLevel
  case object HighRisk extends RiskLevel
  case object MediumRisk extends RiskLevel
  case object LowRisk extends RiskLevel

  // 2. Ваше завдання: реалізувати чисту функцію класифікації як ВИРАЗ (без var).
  // У Scala звичайний `if-else` повертає значення (як тернарний оператор у C++/Java).
  // Умова:
  // - Якщо amount > 80.0 -> HighRisk
  // - Якщо amount > 50.0 -> MediumRisk
  // - Інакше -> LowRisk
  def categorize(amount: Double): RiskLevel =
    if amount > 80.0 then HighRisk
    else if amount > 50.0 then MediumRisk
    else LowRisk

  // 3. Реалізуйте чисту функцію для отримання коефіцієнта (як вираз):
  // HighRisk -> 1.5, MediumRisk -> 1.2, LowRisk -> 1.0
  def getMultiplier(level: RiskLevel): Double =
    level match
      case HighRisk   => 1.5
      case MediumRisk => 1.2
      case LowRisk    => 1.0
   


  @main def runWorkshop(): Unit = ()
  

  
    val data: Vector[Double] = (1 to 100).toVector.map(_.toDouble)

    println("Завдання 1:")
    var totalRisk = 0.0 

    data.par.foreach { transactionId =>
      val risk = math.sin(transactionId) * math.cos(transactionId) + math.tan(transactionId % 1.0)
      totalRisk += risk
    }

    println(s"Сумарний ризик (некоректний через race condition): $totalRisk")

    println("Завдання 2:")
    
    val finalRiskSum = data
      .filter(_ > 50.0)
      .map { amount =>
        val level = categorize(amount)
        val multiplier = getMultiplier(level)
        amount * multiplier
      }
      .sum

    println(s"Сумарний фінальний ризик: $finalRiskSum")

  def main(args: Array[String]): Unit = runWorkshop()