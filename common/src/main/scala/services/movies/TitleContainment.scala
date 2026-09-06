package services.movies

/**
 * Is one title a DECORATION of another — the same film under a banner, a
 * programme, a format tag the rules did not strip — rather than a different film?
 *
 * The ONE definition, asked from two places that used to disagree: the settle's
 * containment edge (`FilmCanonicalizer.groupByFilm`), which folds a decorated row
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
    tools.TextNormalization.deburr(s).toLowerCase(java.util.Locale.ROOT)
      .split("[^\\p{L}\\p{N}]+").iterator.filter(_.nonEmpty).toSeq

  /** PREFIX-or-SUFFIX run, not mid-string: even a 1-token base can't be swallowed by
   *  an unrelated title that merely mentions the word in the middle. */
  def isTokenRun(base: Seq[String], whole: Seq[String]): Boolean =
    base.nonEmpty && whole.lengthIs > base.length && (whole.startsWith(base) || whole.endsWith(base))

  /** `whole` is a decorated screening of the film titled `base`. */
  def decorates(base: Seq[String], whole: Seq[String]): Boolean =
    isTokenRun(base, whole) && !SequelMarker.namesAnotherEntry(base, whole)
}
