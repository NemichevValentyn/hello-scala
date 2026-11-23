package runner.bots

import runner.*


class GreedyBot(seed: Long = 101L) extends CyberBot:
  override def name: String = "GreedyBot"

  override def decide(observation: Observation): Action =
    val depth = Physics.adaptiveDepth(observation.timeBudgetNanos)
    Planner.choose(observation, depth, Objective.Greedy)
