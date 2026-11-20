package services.identity.vocabulary

/** The days a listing screens on, each once, in order — held as epoch days: a corpus holds a listing per film and
 *  venue, and most screen on a handful of days. Or [[ScreeningDays.Unknown]]: a read of them that failed — no day,
 *  but never "screens on none" (a failed read is not data): the broadcast take waits on it. Or the SEASON alone
 *  ([[ScreeningDays.ofSeason]]): a lean read keeps no day of a film billing no stage work, only the first it screens
 *  on, which dates the season it screens in and nothing else — no day of it is read as a screening day. */
final class ScreeningDays private (private val epochDays: Array[Int], private val seasonOnly: Option[Int] = scala.None,
                                   private val firstYearOnly: Option[Int] = scala.None) {
  /** No day, and known to be none. */
  def isEmpty: Boolean = epochDays.isEmpty
  /** The days could not be read. */
  def isUnknown: Boolean = this eq ScreeningDays.Unknown
  def days: Seq[java.time.LocalDate] = if (isUnknown) Nil else epochDays.toSeq.map(day => java.time.LocalDate.ofEpochDay(day.toLong))
  def contains(day: java.time.LocalDate): Boolean = !isUnknown && java.util.Arrays.binarySearch(epochDays, day.toEpochDay.toInt) >= 0
  def first: Option[java.time.LocalDate] = days.headOption
  def last: Option[java.time.LocalDate]  = days.lastOption
  /** The performing season the first day falls in, by the year it opens: a season runs from July to June, so
   *  January 2027 is the 2026/27 season's, as August 2026 is. Of a lean read's season alone, that season. */
  def season: Option[Int] = first.map(ScreeningDays.seasonOf).orElse(seasonOnly)
  /** The year of the first day — of a lean read's season alone too, which keeps it ([[ScreeningDays.ofSeason]]): what a rule
   *  dating a film against its screenings reads. Read off [[first]], it was none in prod's lean read, and the current-release
   *  test of a billed piece never passed there (DE "Über Unterbiberger – Vom Woid in die Welt" ×9, LeanWholeGuard 10-21). */
  def firstYear: Option[Int] = first.map(_.getYear).orElse(firstYearOnly)
  /** `other`'s days beside these — unknown when either is. */
  def ++(other: ScreeningDays): ScreeningDays =
    if (isUnknown || other.isUnknown) ScreeningDays.Unknown
    else if (isEmpty && other.isEmpty) (seasonOnly ++ other.seasonOnly).minOption.fold(ScreeningDays.None)(season =>
      new ScreeningDays(Array.emptyIntArray, Some(season), (firstYearOnly ++ other.firstYearOnly).minOption))
    // days date the season themselves: a season alone beside them adds nothing
    else if (other.isEmpty) this else if (isEmpty) other else new ScreeningDays((epochDays ++ other.epochDays).distinct.sorted)
  override def equals(other: Any): Boolean = other match {
    case that: ScreeningDays => java.util.Arrays.equals(epochDays, that.epochDays) && seasonOnly == that.seasonOnly
    case _ => false
  }
  override def hashCode: Int = java.util.Arrays.hashCode(epochDays) * 31 + seasonOnly.##
  override def toString: String =
    if (isUnknown) "ScreeningDays(unknown)" else seasonOnly.fold(days.mkString("ScreeningDays(", ", ", ")"))(s => s"ScreeningDays(season $s)")
}

object ScreeningDays {
  val None: ScreeningDays = new ScreeningDays(Array.emptyIntArray)
  /** How many years before its screenings' a film's may be and still be a current release ([[BilledPiece]], a feed link's
   *  overrule: [[FeedLinkOverrule]]). */
  val CurrentRelease = 1
  /** The day standing for "not read" where days travel as showtimes (`models.LeanListing.unread`). */
  val UnreadDay: java.time.LocalDate = models.LeanListing.UnreadDay
  /** The day marking, where days travel as showtimes, the other day as the season's alone (`models.LeanListing.seasonOnly`). */
  val SeasonOnlyDay: java.time.LocalDate = models.LeanListing.SeasonOnlyDay
  /** Days that could not be read. */
  val Unknown: ScreeningDays = new ScreeningDays(Array(UnreadDay.toEpochDay.toInt))
  /** The month a performing season opens in. */
  val SeasonOpens = 7
  def of(days: Iterable[java.time.LocalDate]): ScreeningDays =
    if (days.isEmpty) None
    else if (days.exists(_ == UnreadDay)) Unknown
    else if (days.exists(_ == SeasonOnlyDay)) days.filter(_ != SeasonOnlyDay).minOption.fold(None)(ofSeason)
    else new ScreeningDays(days.iterator.map(_.toEpochDay.toInt).toArray.distinct.sorted)
  /** No day, only the season `firstDay` falls in (and its year, [[ScreeningDays.firstYear]]): what a lean read knows of a
   *  film billing no stage work. Two films screening in one season are equal however their first days move — the year is
   *  outside equality and hash — so a day passing moves no listing. */
  def ofSeason(firstDay: java.time.LocalDate): ScreeningDays =
    new ScreeningDays(Array.emptyIntArray, Some(seasonOf(firstDay)), Some(firstDay.getYear))
  private def seasonOf(day: java.time.LocalDate): Int = if (day.getMonthValue >= SeasonOpens) day.getYear else day.getYear - 1
}
