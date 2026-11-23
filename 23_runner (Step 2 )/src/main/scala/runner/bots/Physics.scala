package runner.bots

import runner.*


object Physics:


  val preference: List[Action] = List(
    Action.KeepRunning,
    Action.Duck,
    Action.Jump,
    Action.MoveLeft,
    Action.MoveRight
  )

  
  def resolve(lane: Lane, action: Action): (Lane, Stance) =
    (destination(lane, action), stanceOf(action))

  def destination(lane: Lane, action: Action): Lane =
    action match
      case Action.MoveLeft =>
        lane match
          case Lane.Right  => Lane.Center
          case Lane.Center => Lane.Left
          case Lane.Left   => Lane.Left
      case Action.MoveRight =>
        lane match
          case Lane.Left   => Lane.Center
          case Lane.Center => Lane.Right
          case Lane.Right  => Lane.Right
      case _ =>
        lane

  def stanceOf(action: Action): Stance =
    action match
      case Action.Jump => Stance.Jumping
      case Action.Duck => Stance.Ducking
      case _           => Stance.Running

 
  def survives(slice: TunnelSlice, lane: Lane, stance: Stance): Boolean =
    val low = slice.hasObstacleAt(lane, Height.Low)
    val mid = slice.hasObstacleAt(lane, Height.Mid)
    val high = slice.hasObstacleAt(lane, Height.High)
    stance match
      case Stance.Running => !low && !mid
      case Stance.Ducking => !low
      case Stance.Jumping => !high

  
  def coinGain(slice: TunnelSlice, lane: Lane, stance: Stance): Int =
    stance match
      case Stance.Running =>
        coinValue(slice, lane, Height.Low) + coinValue(slice, lane, Height.Mid)
      case Stance.Jumping =>
        coinValue(slice, lane, Height.High)
      case Stance.Ducking =>
        coinValue(slice, lane, Height.Low)

  private def coinValue(slice: TunnelSlice, lane: Lane, height: Height): Int =
    slice.itemAt(lane, height) match
      case CellItem.Coin(value) => value
      case _                    => 0

  
  def adaptiveDepth(budgetNanos: Long): Int =
    if budgetNanos >= 1_000_000L then 9
    else if budgetNanos >= 300_000L then 4
    else if budgetNanos >= 30_000L then 2
    else 1

  
  def safest(lane: Lane, upcoming: List[TunnelSlice]): Action =
    upcoming match
      case Nil =>
        Action.KeepRunning
      case slice :: _ =>
        if safe(lane, Action.KeepRunning, slice) then Action.KeepRunning
        else if safe(lane, Action.Duck, slice) then Action.Duck
        else if safe(lane, Action.Jump, slice) then Action.Jump
        else if safe(lane, Action.MoveLeft, slice) then Action.MoveLeft
        else if safe(lane, Action.MoveRight, slice) then Action.MoveRight
        else Action.KeepRunning

  private def safe(lane: Lane, action: Action, slice: TunnelSlice): Boolean =
    Physics.survives(slice, destination(lane, action), stanceOf(action))
