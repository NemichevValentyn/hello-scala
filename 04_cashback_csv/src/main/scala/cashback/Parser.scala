package cashback

object Parser:
  private val Header = "event,p1,p2,p3,p4"

  def parseBatch(lines: Seq[String]): (List[ParseError], List[(Int, CashbackEvent)]) =
    val (errors, events) = lines.zipWithIndex
      .flatMap { (line, idx) =>
        val row = idx + 1
        if row == 1 && isHeader(line) then None
        else Some(parseLine(row, line).map(event => (row, event)))
      }
      .partitionMap(identity)
    (errors.toList, events.toList)

  def parseLine(row: Int, raw: String): Either[ParseError, CashbackEvent] =
    val line = raw.stripPrefix("\uFEFF").trim
    if line.isEmpty then
      Left(ParseError.MalformedRow(row, raw, "Порожній рядок"))
    else
      val cols = line.split(",", -1)
      if cols.length != 5 then
        Left(ParseError.MalformedRow(row, raw, s"Очікувалось 5 колонок, отримано ${cols.length}"))
      else
        val code = cols(0).trim
        val p1 = cols(1)
        val p2 = cols(2)
        val p3 = cols(3)
        val p4 = cols(4)
        code match
          case "" =>
            Left(ParseError.MalformedRow(row, raw, "Порожній код події"))
          case "ACCRUE" =>
            parseAccrue(row, raw, p1, p2, p3, p4)
          case "WITHDRAW" =>
            parseAmount(row, raw, "WITHDRAW", p1, p2, p3, p4).map(CashbackEvent.Withdraw.apply)
          case "PARTNER_BONUS" =>
            parseAmount(row, raw, "PARTNER_BONUS", p1, p2, p3, p4).map(CashbackEvent.PartnerBonus.apply)
          case "REFUND_REVERSAL" =>
            parseAmount(row, raw, "REFUND_REVERSAL", p1, p2, p3, p4).map(CashbackEvent.RefundReversal.apply)
          case other =>
            Left(ParseError.UnknownEvent(row, other))

  private def isHeader(line: String): Boolean =
    line.stripPrefix("\uFEFF").trim == Header

  private def parseAccrue(
    row: Int,
    raw: String,
    p1: String,
    p2: String,
    p3: String,
    p4: String
  ): Either[ParseError, CashbackEvent] =
    if p4.trim.nonEmpty then
      Left(ParseError.MalformedRow(row, raw, "Зайве значення в p4"))
    else
      for
        category <- Category.fromString(p1).left.map(reason => ParseError.InvalidToken(row, p1, reason))
        purchase <- Money.parse(p2).left.map(reason => ParseError.InvalidAmount(row, p2, reason))
        percent <- Percent.parse(p3).left.map(reason => ParseError.InvalidPercent(row, p3, reason))
      yield CashbackEvent.Accrue(category, purchase, percent)

  private def parseAmount(
    row: Int,
    raw: String,
    code: String,
    p1: String,
    p2: String,
    p3: String,
    p4: String
  ): Either[ParseError, Money] =
    if p2.trim.nonEmpty || p3.trim.nonEmpty || p4.trim.nonEmpty then
      Left(ParseError.MalformedRow(row, raw, s"Подія $code використовує лише p1"))
    else
      Money.parse(p1).left.map(reason => ParseError.InvalidAmount(row, p1, reason))

object Journal:
  def load(lines: Seq[String]): ImportReport =
    val (errors, events) = Parser.parseBatch(lines)
    val (rejected, state) = CashbackAccount.empty.replay(events)
    ImportReport(
      parseErrors = errors,
      rejected = rejected,
      state = state,
      accepted = events.size - rejected.size
    )
