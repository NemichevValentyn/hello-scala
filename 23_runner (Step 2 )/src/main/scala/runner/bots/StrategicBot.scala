package runner.bots

import runner.*


class StrategicBot(seed: Long = 303L) extends CyberBot:
  override def name: String = "StrategicBot"

  override def decide(observation: Observation): Action =
    val depth = Physics.adaptiveDepth(observation.timeBudgetNanos)
    Planner.choose(observation, depth, Objective.Strategic)
