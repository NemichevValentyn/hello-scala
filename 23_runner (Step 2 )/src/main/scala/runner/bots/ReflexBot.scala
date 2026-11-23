package runner.bots

import runner.*


class ReflexBot(seed: Long = 202L) extends CyberBot:
  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    Physics.safest(observation.hero.lane, observation.upcoming)

object ReflexBot:
  /**
   * Safety Veto для MajorityBot.
   */
  def crashes(observation: Observation, action: Action): Boolean =
    observation.upcoming match
      case Nil => false
      case slice :: _ =>
        val lane = Physics.destination(observation.hero.lane, action)
        val stance = Physics.stanceOf(action)
        !Physics.survives(slice, lane, stance)
