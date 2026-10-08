package services.movies

/**
 * Is one title a DECORATION of another — the same film under a banner, a
 * programme, a format tag the rules did not strip — rather than a different film?
 *
 * The ONE definition, asked from two places that used to disagree: the settle's
 * containment edge (the since-deleted `FilmCanonicalizer.groupByFilm`), which folds a decorated row
 * onto its base, and the scrape-time divert gate (`MovieCache.recordCinemaScrape`),
 * which decides whether a listing is a known film or a newcomer. When only the
 * settle knew the answer, every venue's first scrape of "gb Fallen Angels by Noël
 * Coward." was a newcomer: diverted, resolved to the same film, folded onto it,
 * re-keyed — 92 times in nine days for one film, and the same for every banner a
 * chain puts in front of a title. Asking here at landing time puts the slot on the
 * film's row and there is nothing left to fold.
 *
 * The rule: the base's tokens run along one EDGE of the decorated title (a banner
 * wraps a title, it does not interleave with it), the decorated title is strictly
 * longer, and what it adds does not name another entry in the series
 * ([[SequelMarker]]).
 */
object TitleContainment {

  def tokens(s: String): Seq[String] =
    tools.TextNormalization.lettersAndDigitsRuns(tools.TextNormalization.deburr(s).toLowerCase(java.util.Locale.ROOT))

  /** PREFIX-or-SUFFIX run, not mid-string: even a 1-token base can't be swallowed by
   *  an unrelated title that merely mentions the word in the middle. */
  def isTokenRun(base: Seq[String], whole: Seq[String]): Boolean =
    base.nonEmpty && whole.lengthIs > base.length && (runsFrom(whole, 0, base) || runsFrom(whole, whole.length - base.length, base))

  /** `whole.startsWith(base, at)` — walked in place over the lists [[tokens]] makes (and indexed over an indexed one): the
   *  resolver asks [[isTokenRun]] of every title pair it compares, and `startsWith`/`endsWith` built iterators each time. */
  private def runsFrom(whole: Seq[String], at: Int, base: Seq[String]): Boolean = whole match {
    case w: List[String] if base.isInstanceOf[List[?]] =>
      var rest = w.drop(at); var want = base.asInstanceOf[List[String]]
      while (want.nonEmpty && rest.nonEmpty && rest.head == want.head) { rest = rest.tail; want = want.tail }
      want.isEmpty
    case w: IndexedSeq[?] if base.isInstanceOf[IndexedSeq[?]] =>
      val b = base.asInstanceOf[IndexedSeq[String]]
      var i = 0
      while (i < b.length && at + i < w.length && w(at + i) == b(i)) i += 1
      i == b.length
    case _ => whole.startsWith(base, at)
  }

  /** `whole` is a decorated screening of the film titled `base` (`latestYear`: [[LatestTitleYear]]). */
  def decorates(base: Seq[String], whole: Seq[String], latestYear: Int): Boolean =
    isTokenRun(base, whole) && !SequelMarker(latestYear).namesAnotherEntry(base, whole)
}
