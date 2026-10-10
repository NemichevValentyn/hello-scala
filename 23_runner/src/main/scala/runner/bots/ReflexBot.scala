package runner.bots

import runner.*

class ReflexBot(seed: Long = 202L) extends CyberBot:
  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    val slices = observation.upcoming.toVector
    if slices.isEmpty then Action.KeepRunning
    else
      val future = survivalFrom(slices, 1)
      val ranked = legalActions(observation.hero.lane).map { action =>
        action -> actionValue(observation.hero.lane, action, slices(0), future)
      }
      val best = ranked.map(_._2).max
      prefer(ranked.filter(_._2 == best).map(_._1))

  private def survivalFrom(slices: Vector[TunnelSlice], from: Int): Map[Lane, Int] =
    val afterEnd = Lane.values.map(_ -> 0).toMap
    (slices.length - 1).to(from, -1).foldLeft(afterEnd) { (future, idx) =>
      Lane.values.map { lane =>
        val score = legalActions(lane).map { action =>
          actionValue(lane, action, slices(idx), future)
        }.max
        lane -> score
      }.toMap
    }

  private def actionValue(
      lane: Lane,
      action: Action,
      slice: TunnelSlice,
      future: Map[Lane, Int]
  ): Int =
    resolve(lane, action) match
      case None => -1
      case Some((nextLane, stance)) =>
        if survives(nextLane, stance, slice) then 1 + future.getOrElse(nextLane, 0)
        else 0

  private def legalActions(lane: Lane): List[Action] =
    val shift = lane match
      case Lane.Left   => List(Action.MoveRight)
      case Lane.Right  => List(Action.MoveLeft)
      case Lane.Center => List(Action.MoveLeft, Action.MoveRight)
    List(Action.KeepRunning, Action.Duck, Action.Jump) ++ shift

  private def resolve(lane: Lane, action: Action): Option[(Lane, Stance)] =
    action match
      case Action.MoveLeft =>
        lane.left.map((_, Stance.Running))
      case Action.MoveRight =>
        lane.right.map((_, Stance.Running))
      case Action.Jump =>
        Some((lane, Stance.Jumping))
      case Action.Duck =>
        Some((lane, Stance.Ducking))
      case Action.KeepRunning =>
        Some((lane, Stance.Running))

  private def survives(lane: Lane, stance: Stance, slice: TunnelSlice): Boolean =
    val low = slice.hasObstacleAt(lane, Height.Low)
    val mid = slice.hasObstacleAt(lane, Height.Mid)
    val high = slice.hasObstacleAt(lane, Height.High)
    stance match
      case Stance.Running => !low && !mid
      case Stance.Jumping => !high
      case Stance.Ducking => !low

  private def prefer(actions: List[Action]): Action =
    List(Action.KeepRunning, Action.Duck, Action.Jump, Action.MoveLeft, Action.MoveRight)
      .find(actions.contains)
      .getOrElse(Action.KeepRunning)
