package runner.bots

import runner.*


class RandomBot(seed: Long = 42L) extends CyberBot:
  override def name: String = "RandomWalkerBot"

  override def decide(observation: Observation): Action =
    val lane = observation.hero.lane
    val safe = Physics.preference.filter { action =>
      observation.upcoming.headOption.forall { slice =>
        val (nextLane, stance) = Physics.resolve(lane, action)
        Physics.survives(slice, nextLane, stance)
      }
    }
    val pool = if safe.nonEmpty then safe else Physics.preference
    val index = Math.floorMod(observation.hero.distance + seed.toInt, pool.length)
    pool(index)
