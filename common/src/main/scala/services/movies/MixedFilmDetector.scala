package services.movies

import models.{MovieRecord, Source, SourceData}

/**
 * Decides whether one `movies` row is actually holding TWO DIFFERENT FILMS, and
 * which of its cinema slots belong to which.
 *
 * A row is keyed by its title, so two unrelated films released here under the same
 * Polish one land on it together. `FilmCanonicalizer.clusterByFilm` would keep them
 * apart, but it splits ROWS by tmdbId and a row holding both films has only one —
 * so whichever it resolves to, the cinemas showing the other film are mis-served.
 *
 * Two live examples:
 *   - "Joanna d'Arc" — Kino Muranów screens Besson's 1999 film (160 min, original
 *     title "Joan of Arc"); Kino Nowe Horyzonty screens Pálmason's 2025
 *     "Jóhanna af Örk". One row, and it resolves to neither.
 *   - "Obcy" — 36 cinemas screen Ozon's "L'étranger" (120 min), one screens Brandt
 *     Andersen's "I Was A Stranger" (2024, 103 min). Right for 36, wrong for one.
 *
 * The evidence is always what the CINEMAS published — never anything derived from
 * the resolution, which would just be the answer this is checking.
 *
 * Two signals had to be given up along the way, each because the corpus disproved
 * it:
 *
 *   - THE DIRECTOR DOES NOT IDENTIFY A FILM. Cinemas credit different roles:
 *     "Drzewo magii" is directed by Ben Gregor and written by Simon Farnaby, and
 *     cinemas print one or the other, so the names never overlap on what is
 *     plainly one film.
 *   - A DIFFERING TITLE IS NOT ENOUGH ON ITS OWN. A third of the corpus's
 *     cinema-vs-TMDB original-title mismatches are the same film named in two
 *     languages ("Candidates of Death" beside "Kandydaci śmierci", "Otto e mezzo"
 *     beside "8½"), and two cinemas doing that would split a good row.
 *
 * What is left is a differing original title CORROBORATED by a differing runtime
 * or year — both language-proof, and both things two prints of one film agree on.
 */
object MixedFilmDetector {

  /** One film's worth of a row: the identity its cinemas published, and the slots
   *  that published it. */
  case class Group(originalTitle: Set[String], slots: Seq[(Source, SourceData)]) {
    def runtimes: Set[Int] = slots.flatMap(_._2.runtimeMinutes).toSet
    def years:    Set[Int] = slots.flatMap(_._2.releaseYear).toSet
  }

  /** The cinema slots partitioned by the film they describe, largest first and then
   *  by identity, so the answer is a pure function of the row.
   *
   *  Empty when the row describes ONE film — the normal case, including when it
   *  describes it inconsistently. */
  def split(record: MovieRecord, normalizer: TitleNormalizer): Seq[Group] = {
    val groups = identityGroups(record, normalizer)
    if (groups.sizeIs < 2 || !groups.exists(g => conflicting(groups.head, g))) Seq.empty else groups
  }

  /** The slots belonging to a film OTHER than the row's main one — what has to
   *  leave the row for each film to get a record of its own. */
  def strays(record: MovieRecord, normalizer: TitleNormalizer): Seq[(Source, SourceData)] =
    split(record, normalizer) match {
      case Seq()          => Seq.empty
      case main +: others => others.filter(conflicting(main, _)).flatMap(_.slots)
    }

  /** Would attaching this cinema's listing to `record` put a SECOND film on the
   *  row? Asked at the scrape boundary, BEFORE the merge, so the bad row is never
   *  created — a row is keyed by its title alone, so otherwise any cinema listing a
   *  same-titled film is merged straight in and only a later settle can undo it.
   *
   *  Same rule as [[conflicting]], and the corroboration is what makes it safe on
   *  raw listing data: the smaller cinemas often put the POLISH title in
   *  `originalTitle`, which then "differs" from the row's real one — but such a
   *  listing still agrees on runtime and year, so it is waved through. An earlier
   *  version of this gate WITHOUT corroboration re-diverted nine known films on
   *  every tick (`ReScrapeIdempotencySpec`). */
  def wouldAddASecondFilm(
    record:        MovieRecord,
    originalTitle: Option[String],
    runtime:       Option[Int],
    year:          Option[Int],
    normalizer:    TitleNormalizer
  ): Boolean = {
    val incomingTitle = titleWords(originalTitle, normalizer)
    incomingTitle.nonEmpty && identityGroups(record, normalizer).headOption.exists { main =>
      titlesDiffer(main.originalTitle, incomingTitle) &&
        corroborated(main.runtimes, runtime.toSet, main.years, year.toSet)
    }
  }

  /** Do these two ROWS describe DIFFERENT films, on what their cinemas published?
   *
   *  Same rule as [[conflicting]], asked ACROSS two rows rather than within one —
   *  what a canonicalisation edge needs before it adopts one row onto another. An
   *  edge that folds on the shape of the titles alone ("Ktoś całkiem obcy" ends
   *  with the whole of "Obcy") otherwise re-creates the very row
   *  `MixedFilmSplitter` splits, and the two chase each other forever.
   *
   *  A row whose cinemas published nothing comparable cannot contradict anything,
   *  so the answer is `false` and the caller's own evidence stands. */
  /** CALL WITH RECORDS THAT CARRY THEIR CINEMAS. The answer comes from `cinemaSlots`, so a
   *  record holding none cannot contradict anything and this returns `false` — the right
   *  default (no evidence is not evidence of difference, and refusing on it would block every
   *  adoption of an enrichment-only row), but one that makes the answer depend on how the
   *  record was READ. Under the storage split a migrated film's `movies` document carries no
   *  `sourceData` at all, so a caller planning on RAW documents — `MongoStagingFolder` does —
   *  gets `false` where the stitched view gives `true`. Pinned in `MixedFilmDetectorSpec`,
   *  along with why it currently costs nothing. */
  def describeDifferentFilms(a: MovieRecord, b: MovieRecord, normalizer: TitleNormalizer): Boolean =
    (identityGroups(a, normalizer).headOption, identityGroups(b, normalizer).headOption) match {
      case (Some(mainA), Some(mainB)) => conflicting(mainA, mainB)
      case _                          => false
    }

  /** Do these two identities describe DIFFERENT films? */
  def conflicting(a: Group, b: Group): Boolean =
    titlesDiffer(a.originalTitle, b.originalTitle) &&
      corroborated(a.runtimes, b.runtimes, a.years, b.years)

  private def identityGroups(record: MovieRecord, normalizer: TitleNormalizer): Seq[Group] =
    record.cinemaSlots
      .filter { case (_, sd) => sd.originalTitle.exists(_.trim.nonEmpty) }
      .groupBy { case (_, sd) => titleWords(sd.originalTitle, normalizer) }
      .toSeq
      .map { case (title, slots) => Group(title, slots) }
      .sortBy(g => (-g.slots.size, g.originalTitle.toSeq.sorted.mkString(" ")))

  private def titlesDiffer(a: Set[String], b: Set[String]): Boolean =
    a.nonEmpty && b.nonEmpty && a.intersect(b).isEmpty

  /** Does something OTHER than the title agree that these are two films?
   *
   *  Runtime and year survive translation, and two genuinely different films
   *  disagree on them: Besson's "Joanna d'Arc" runs 160 minutes and is 26 years
   *  older than the Icelandic one; Ozon's "L'étranger" runs 120 where Brandt
   *  Andersen's runs 103. One film named in two languages agrees on both. A side
   *  that published neither cannot corroborate, and so cannot trigger a split. */
  private def corroborated(aRuntimes: Set[Int], bRuntimes: Set[Int], aYears: Set[Int], bYears: Set[Int]): Boolean = {
    def bothPublished(a: Set[Int], b: Set[Int]) = a.nonEmpty && b.nonEmpty
    def allDiffer(a: Set[Int], b: Set[Int], tolerance: Int) =
      bothPublished(a, b) && a.forall(x => b.forall(y => math.abs(x - y) > tolerance))

    // RUNTIME is the authority whenever both sides published one, and its verdict
    // is final — including a verdict of "same film", which is why the year is not
    // consulted alongside it.
    //
    // The year cannot be trusted to overrule it, because cinemas routinely print
    // the SCREENING year for a repertory title: Kinoteka lists Coppola's 1974
    // "Rozmowa" as 2026. Paired with a typo in the original title ("The
    // Converastion"), that alone read as a second film and split a perfectly good
    // row — while both slots agreed on 113 minutes, which is what actually settles
    // it. The year still speaks when runtime cannot: "Joanna d'Arc"'s two films are
    // 26 years apart and only one of its cinemas publishes a runtime at all.
    if (bothPublished(aRuntimes, bRuntimes)) allDiffer(aRuntimes, bRuntimes, RuntimeAgreementMinutes)
    else allDiffer(aYears, bYears, 1)
  }

  /** An original title as its distinctive WORDS.
   *
   *  Comparing the folded whole string is too brittle for what cinemas publish:
   *  "Terminator 2: Judgement Day (re-release)" against another cinema's
   *  "Terminator 2: Judgment Day" shares no containment — a spelling variant plus a
   *  decoration — and would read as a second film. By words they meet on
   *  "terminator", and still part company from an unrelated title: "Joan of Arc"
   *  keeps nothing of "Jóhanna af Örk", "L'étranger" nothing of "I Was A Stranger".
   *
   *  Short words are dropped for the same reason the director walk drops them:
   *  articles and particles coincide across unrelated titles in every language. */
  private def titleWords(title: Option[String], normalizer: TitleNormalizer): Set[String] =
    title.toSet[String]
      .flatMap(_.split("[^\\p{L}\\p{N}]+"))
      .map(normalizer.sanitize)
      .filter(_.length >= DistinctiveTitleWord)

  /** Shortest word that counts as evidence two titles name the same film. */
  private val DistinctiveTitleWord = 4

  /** Runtimes within this many minutes are the same film — cinemas round, and some
   *  count the credits. Matches the director walk's tolerance. */
  private val RuntimeAgreementMinutes = 2
}
