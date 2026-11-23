package runner.bots

import runner.*

/**
 * Мажоритарний арбітр тріади MISD.
 * Один і той самий незмінний Observation отримують три різні чисті функції.
 * Спільного змінного поля між ними немає: кожен decide нічого не записує.
 *
 * У стартовому шаблоні (практика p06) були дві пастки. Обидві закриті тут.
 *
 * Пастка A — голоси рахувались до фільтра безпеки.
 * Два боти могли зійтися на самогубній дії (монета під стелею, біг у Low),
 * і кворум 2:1 перемагав рефлекс. Тепер ReflexBot.crashes відсіює дію
 * ДО підрахунку. У кворум потрапляють лише дії, які живі на upcoming.head.
 * Сам підрахунок — три порівняння, без Map: groupBy на кожному тіку
 * створює сміття, а пауза GC у benchmark перетворюється на Timeout.
 *
 * Пастка B — нічия 1:1:1 віддавалась votes.head.
 * У конструкторі першим стоїть GreedyBot, тому head завжди був його голосом.
 * Тепер за відсутності кворуму пріоритет такий:
 *   StrategicBot (бачить t+2 і далі), потім ReflexBot, і лише якщо обидва
 *   відсіяні вето — єдина безпечна дія, що лишилась (нею може бути і жадібна).
 */
class MajorityBot(val bots: List[CyberBot]) extends CyberBot:

  def this(seed: Long = 2026L) = this(List(
    new GreedyBot(seed + 101L),
    new ReflexBot(seed + 202L),
    new StrategicBot(seed + 303L)
  ))


  private val greedyBot = bots(0)
  private val reflexBot = bots(1)
  private val strategicBot = bots(2)

  override def name: String = "MajorityEnsembleBot"

  override def decide(observation: Observation): Action =
    val budget = observation.timeBudgetNanos
    if budget < 80_000L then
      Physics.safest(observation.hero.lane, observation.upcoming)
    else if budget < 400_000L then
      Planner.choose(observation, 2, Objective.Strategic)
    else
      vote(observation)

  private def vote(observation: Observation): Action =
    val greedy = greedyBot.decide(observation)
    val reflex = reflexBot.decide(observation)
    val strategic = strategicBot.decide(observation)

    val greedyOk = !ReflexBot.crashes(observation, greedy)
    val reflexOk = !ReflexBot.crashes(observation, reflex)
    val strategicOk = !ReflexBot.crashes(observation, strategic)

    
    if greedyOk && reflexOk && greedy == reflex then greedy
    else if greedyOk && strategicOk && greedy == strategic then greedy
    else if reflexOk && strategicOk && reflex == strategic then reflex
    else if strategicOk then strategic
    else if reflexOk then reflex
    else if greedyOk then greedy
    else Physics.safest(observation.hero.lane, observation.upcoming)
