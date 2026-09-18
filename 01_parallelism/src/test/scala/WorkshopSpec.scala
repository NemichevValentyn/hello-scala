import org.scalatest.funsuite.AnyFunSuite

// Тестовий клас, який перевіряє логіку з Workshop.scala
class WorkshopSpec extends AnyFunSuite:

  // Запускається командою: sbt test
  
  test("Функція categorize повинна правильно визначати рівні ризику"):
    // Очікуємо, що транзакція на 90.0 поверне HighRisk
    assert(Workshop.categorize(90.0) == Workshop.HighRisk)
    
    // Перевірте самостійно для інших граничних значень (наприклад, 60.0 та 10.0)
    // assert(...)

  test("Функція getMultiplier повинна повертати правильні коефіцієнти"):
    assert(Workshop.getMultiplier(Workshop.HighRisk) == 1.5)
    assert(Workshop.getMultiplier(Workshop.MediumRisk) == 1.2)
    assert(Workshop.getMultiplier(Workshop.LowRisk) == 1.0)

