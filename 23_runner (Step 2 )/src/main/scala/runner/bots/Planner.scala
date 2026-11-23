package runner.bots

import runner.*


enum Objective:
  /** Нічия життя+сума: забрати більший номінал уже на найближчому зрізі. */
  case Greedy
  /** Нічия життя+сума: закінчити горизонт у центрі (звідти два напрямки втечі). */
  case Strategic

object Planner:

  private val SurvShift = 16 //Це кількість бітів, на яку треба посунути значення survived вліво при пакуванні 6 + 10 (10 = 2^10 = 1024, стеля монет на горизонті) (6 = 2^6 = 64, стеля readiness на горизонті) (6 це кладе номінал монети, яку дія забере прямо зараз на найближчому зрізі від 0 до 60 а 10 це максимальна кількість монет яку можливо зібрати за 9 зрізів 540)
  private val CoinShift = 6 // кількість бітів readiness, coins їх пропускають
  private val LaneBits = 20 // survived + coins + readiness = 4 + 10 + 6 = 20
  private val LaneMask = (1L << LaneBits) - 1L // створює бітову маску з рівно 20 одиниць справа (1L << 20 ставить одиницю на 20-ту позицію - 1L перетворює її на 20 одиниць підряд.)

   /**
     * case class Score(survived: Int, coins: Int, readiness: Int):
  def isBetterThan(other: Score): Boolean =
    if survived != other.survived then survived > other.survived
    else if coins != other.coins then coins > other.coins
    else readiness > other.readiness
    Можна булоб замінити код вище проте, але це створює об'єкт Score на кожному ребрі.
     */


  def choose(observation: Observation, depth: Int, objective: Objective): Action =
    val lane = observation.hero.lane
    observation.upcoming match
      case Nil =>
        Action.KeepRunning
      case nearest :: later =>
        val future = futureScores(later, depth - 1, objective)
        pick(lane, nearest, future, objective)

  private def futureScores(slices: List[TunnelSlice], remaining: Int, objective: Objective): Long =
    if remaining <= 0 || slices.isEmpty then leaf(objective)
    else
      val after = futureScores(slices.tail, remaining - 1, objective)
      val slice = slices.head
      pack3(
        bestOnLane(Lane.Left, slice, after),
        bestOnLane(Lane.Center, slice, after),
        bestOnLane(Lane.Right, slice, after)
      )


  private def leaf(objective: Objective): Long =
    val centerReady =
      objective match
        case Objective.Strategic => 1
        case Objective.Greedy    => 0
    pack3(packLane(0, 0, 0), packLane(0, 0, centerReady), packLane(0, 0, 0))

  private def bestOnLane(lane: Lane, slice: TunnelSlice, after: Long): Long =
    higher(
      higher(
        higher(outcome(lane, Action.KeepRunning, slice, after), outcome(lane, Action.Duck, slice, after)),
        higher(outcome(lane, Action.Jump, slice, after), outcome(lane, Action.MoveLeft, slice, after))
      ),
      outcome(lane, Action.MoveRight, slice, after)
    )

  
  private def pick(lane: Lane, slice: TunnelSlice, after: Long, objective: Objective): Action =
    val keep = shown(lane, Action.KeepRunning, slice, after, objective)
    val duck = shown(lane, Action.Duck, slice, after, objective)
    val jump = shown(lane, Action.Jump, slice, after, objective)
    val left = shown(lane, Action.MoveLeft, slice, after, objective)
    val right = shown(lane, Action.MoveRight, slice, after, objective)
    val best = higher(higher(higher(higher(keep, duck), jump), left), right)
    if keep == best then Action.KeepRunning
    else if duck == best then Action.Duck
    else if jump == best then Action.Jump
    else if left == best then Action.MoveLeft
    else Action.MoveRight

  /**
   * Greedy на корені підміняє readiness номіналом монети цього зрізу.
   * Це нічого не вирішує, поки інша дія живе довше або збирає більшу суму:
   * ці біти молодші за survived і coins.
   */
  private def shown(
      lane: Lane,
      action: Action,
      slice: TunnelSlice,
      after: Long,
      objective: Objective
  ): Long =
    val raw = outcome(lane, action, slice, after)
    if objective == Objective.Greedy && raw != 0L then
      val dest = Physics.destination(lane, action)
      packLane(survivedOf(raw), coinsOf(raw), Physics.coinGain(slice, dest, Physics.stanceOf(action)))
    else raw

  private def outcome(lane: Lane, action: Action, slice: TunnelSlice, after: Long): Long =
    val dest = Physics.destination(lane, action)
    val stance = Physics.stanceOf(action)
    if !Physics.survives(slice, dest, stance) then 0L
    else
      val future = laneScore(after, dest)
      packLane(
        survivedOf(future) + 1,
        coinsOf(future) + Physics.coinGain(slice, dest, stance),
        readyOf(future)
      )

  private def packLane(survived: Int, coins: Int, readiness: Int): Long =
    ((survived & 15).toLong << SurvShift) |
      ((coins & 1023).toLong << CoinShift) |
      (readiness & 63).toLong

  private def pack3(left: Long, center: Long, right: Long): Long =
    (left << 40) | (center << 20) | right

  private def laneScore(packed: Long, lane: Lane): Long =
    val shifted = lane match
      case Lane.Left   => packed >>> 40
      case Lane.Center => packed >>> 20
      case Lane.Right  => packed
    shifted & LaneMask

  private def survivedOf(score: Long): Int = ((score >>> SurvShift) & 15L).toInt
  private def coinsOf(score: Long): Int = ((score >>> CoinShift) & 1023L).toInt
  private def readyOf(score: Long): Int = (score & 63L).toInt

  private def higher(a: Long, b: Long): Long = if a > b then a else b
