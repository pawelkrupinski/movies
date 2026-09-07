package services.movies

import models.{MovieRecord, Source, SourceData}
import services.resolution.YearWindow

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
 *
 * And one VETO over all of it: an AGREEING director. That is not the signal given
 * up above — a director difference is manufactured by cinemas printing the writer,
 * where an agreement is only ever published when the two listings really are the
 * same film. It is needed because the corroboration can be wrong: a cinema is free
 * to publish a runtime that is simply incorrect ("Twoje imię", 83 minutes for a
 * 106-minute film), and a wrong runtime beside a translated title reads as a second
 * film on evidence that looks impeccable. See [[sameDirector]].
 */
object MixedFilmDetector {

  /** One film's worth of a row: the identity its cinemas published, and the slots
   *  that published it. `identity` is the ORIGINAL title's words where the row
   *  publishes any, and the plain title's where it publishes none — see
   *  [[identityTitle]]; either way every group on a row is built from the same field. `directors` is pre-computed because comparing names needs
   *  the normalizer, which a slot doesn't carry. */
  case class Group(identity: Set[String], slots: Seq[(Source, SourceData)], directors: Set[String],
                   numbers: Set[String] = Set.empty) {
    def runtimes: Set[Int] = slots.flatMap(_._2.runtimeMinutes).toSet
    def years:    Set[Int] = slots.flatMap(_._2.releaseYear).toSet
  }

  /** The cinema slots partitioned by the film they describe, largest first and then
   *  by identity, so the answer is a pure function of the row.
   *
   *  Empty when the row describes ONE film — the normal case, including when it
   *  describes it inconsistently. */
  def split(record: MovieRecord, normalizer: TitleNormalizer): Seq[Group] = {
    val groups = identityGroups(record, normalizer, allowTitleFallback = true)
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
    director:      Seq[String],
    normalizer:    TitleNormalizer
  ): Boolean = {
    val incomingTitle     = titleWords(originalTitle, normalizer)
    val incomingDirectors = directorKeys(director, normalizer)
    incomingTitle.nonEmpty && publishedIdentity(record, normalizer).exists { main =>
      titlesDiffer(main.identity, incomingTitle) &&
        !sameDirector(main.directors, incomingDirectors) &&
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
    describeDifferentFilms(publishedIdentity(a, normalizer), publishedIdentity(b, normalizer))

  /** [[describeDifferentFilms]] on identities already read by [[publishedIdentity]]. */
  def describeDifferentFilms(a: Option[Group], b: Option[Group]): Boolean =
    (a, b) match {
      case (Some(mainA), Some(mainB)) => conflicting(mainA, mainB)
      case _                          => false
    }

  /** The identity a row's cinemas published for its MAIN film — the side every
   *  cross-row question here compares, or `None` when they published nothing
   *  comparable. Building it walks every slot's title through the normalizer, so a
   *  caller asking about one row against many others (`clusterByFilm` compares a
   *  tmdbId group's main row with every sibling, and every imdbId-sharing pair of
   *  groups row by row) reads it once and asks the [[describeDifferentFilms]]
   *  overload on the result. */
  def publishedIdentity(record: MovieRecord, normalizer: TitleNormalizer): Option[Group] =
    identityGroups(record, normalizer).headOption

  /** Do these two identities describe DIFFERENT films? */
  def conflicting(a: Group, b: Group): Boolean =
    (titlesDiffer(a.identity, b.identity) || sequelApart(a, b)) &&
      !sameDirector(a.directors, b.directors) &&
      corroborated(a.runtimes, b.runtimes, a.years, b.years)

  /** A film beside its own numbered SEQUEL — the one pair `titlesDiffer` structurally
   *  cannot see. `titleWords` drops anything under four characters, so "Kung Fu Panda"
   *  and "Kung Fu Panda 4" reduce to the same two words and read as one film; the
   *  numeral that is the entire difference between a 2008 film and a 2024 one is
   *  filtered out before any comparison happens. `kungfupanda4|2008` sat on that:
   *  sixteen US venues screening the fourth film, one screening the first, on a row
   *  resolved to the first.
   *
   *  Deliberately the NARROWEST rule that sees it: the two sides must agree on every
   *  word and disagree on the numbers. Anything less — comparing numbers whenever the
   *  words merely overlap — reaches titles like "Ojczyzna" beside "Ojczyzna | pokaz
   *  przedpremierowy", which are one film with a programme banner, and strips venues
   *  off a good row. The numbers are also kept OUT of `titleWords`, so they can never
   *  dilute the disjoint-words test that carries every other split. */
  private def sequelApart(a: Group, b: Group): Boolean =
    a.identity.nonEmpty && a.identity == b.identity && a.numbers != b.numbers

  /** Short numeric tokens — the sequel markers `titleWords` drops. Four digits and up
   *  are already words there (a year like "2049"), so this is about the bare "2"/"4"
   *  that distinguishes a sequel and nothing else. */
  private def sequelNumbers(title: Option[String], normalizer: TitleNormalizer): Set[String] =
    title.toSet[String]
      .flatMap(_.split("[^\\p{L}\\p{N}]+"))
      .map(normalizer.sanitize)
      .filter(w => w.nonEmpty && w.length < DistinctiveTitleWord && w.forall(_.isDigit))

  /** The identity field, chosen ONCE per row so every group is compared like for like.
   *
   *  `originalTitle` is the right field and stays the default: it survives translation,
   *  which is the whole reason a differing title is only evidence alongside a runtime or
   *  year. But a row whose cinemas publish NO original title at all yields no groups and
   *  can therefore never split, however plainly mixed it is — and that is not a corner
   *  case, it is every US and UK row, because the Flicks listings carry `title` only.
   *  `kungfupanda4|2008` sat that way: sixteen venues screening "Kung Fu Panda 4" at 94
   *  minutes and one screening "Kung Fu Panda" at 82, on one row resolved to the 2008
   *  film, with the detector structurally unable to see it.
   *
   *  Falling back for the WHOLE row rather than per slot is what keeps it honest: mixing
   *  one venue's original title with another's localised one would compare a translation
   *  against an original and call the difference evidence. A row with even one original
   *  title behaves exactly as before.
   *
   *  And ONLY [[split]] may ask for it. The fallback widens which rows yield groups at
   *  all, and `identityGroups` also answers two questions that are not "should this row
   *  be split": [[wouldAddASecondFilm]] gates a scrape-time divert against an incoming
   *  ORIGINAL title, and [[describeDifferentFilms]] gates whether the canonicaliser may
   *  adopt one row onto another. Letting the fallback reach those cost the fixture corpus
   *  real venues — "Ojczyzna" lost five cinemas' preview screenings and "Rozmowa" lost its
   *  ratings — because a row that used to yield no groups, and so refused nothing, began
   *  refusing adoptions on the strength of a programme banner. Splitting is the only
   *  decision this evidence is good enough for. */
  private def identityTitle(slots: Seq[(Source, SourceData)], allowTitleFallback: Boolean): SourceData => Option[String] =
    if (!allowTitleFallback) _.originalTitle
    else if (slots.exists { case (_, sd) => sd.originalTitle.exists(_.trim.nonEmpty) }) _.originalTitle
    else _.title

  private def identityGroups(record: MovieRecord, normalizer: TitleNormalizer,
                             allowTitleFallback: Boolean = false): Seq[Group] = {
    val slots   = record.cinemaSlots
    val titleOf = identityTitle(slots, allowTitleFallback)
    slots
      .filter { case (_, sd) => titleOf(sd).exists(_.trim.nonEmpty) }
      .groupBy { case (_, sd) => (titleWords(titleOf(sd), normalizer), sequelNumbers(titleOf(sd), normalizer)) }
      .toSeq
      .map { case ((title, numbers), slots) =>
        Group(title, slots, directorKeys(slots.flatMap(_._2.director), normalizer), numbers) }
      .sortBy(g => (-g.slots.size, g.identity.toSeq.sorted.mkString(" "), g.numbers.toSeq.sorted.mkString(" ")))
  }

  private def titlesDiffer(a: Set[String], b: Set[String]): Boolean =
    a.nonEmpty && b.nonEmpty && a.intersect(b).isEmpty

  /** Do both sides credit the same person? Then it is ONE film, whatever the titles
   *  and the runtimes say.
   *
   *  Read this against the header's "the director does not identify a film": that
   *  rule is about a director DIFFERENCE, which cinemas manufacture by printing the
   *  writer instead of the director. AGREEMENT is the other direction and holds
   *  where the difference doesn't — two unrelated films sharing a Polish title do
   *  not also share a director, while one film named in two languages does.
   *
   *  What forced it: "Twoje imię", 2026-08-29. Forty cinemas publish "Kimi no na
   *  wa" at 110 minutes; Kino Nowe Horyzonty publishes "Your Name (re-release)" at
   *  83 — its own page says `czas: 83'` for a 106-minute film. Different words, and
   *  the runtime "corroborates" them, so the row split on every settle — and the
   *  stray then resolved to the SAME tmdbId and folded straight back, splitting
   *  again on the next pass, forever (`CountryConvergenceBehaviour`'s churn axis
   *  caught it). Both sides credit Makoto Shinkai, which is what settles it. */
  private def sameDirector(a: Set[String], b: Set[String]): Boolean =
    a.nonEmpty && b.nonEmpty && a.intersect(b).nonEmpty

  /** Published director names as comparable keys.
   *
   *  Order-insensitive — a Japanese name is printed "Makoto Shinkai" by one cinema
   *  and "Shinkai Makoto" by the next — and whole-name, not per-word: two directors
   *  routinely share a given name ("Michael Bay", "Michael Mann"), so matching on
   *  words the way `titleWords` does would read them as one person. Comma-separated
   *  because some listings pack a whole crew into one string, exactly as the
   *  director walk's `split(",")` does. */
  private def directorKeys(names: Iterable[String], normalizer: TitleNormalizer): Set[String] =
    names.iterator.flatMap(_.split(",")).map(nameKey(_, normalizer)).filter(_.nonEmpty).toSet

  private def nameKey(name: String, normalizer: TitleNormalizer): String =
    name.split("[^\\p{L}\\p{N}]+").map(normalizer.sanitize).filter(_.nonEmpty).sorted.mkString(" ")

  /** Does something OTHER than the title agree that these are two films?
   *
   *  Runtime and year survive translation, and two genuinely different films
   *  disagree on them: Besson's "Joanna d'Arc" runs 160 minutes and is 26 years
   *  older than the Icelandic one; Ozon's "L'étranger" runs 120 where Brandt
   *  Andersen's runs 103. One film named in two languages agrees on both. A side
   *  that published neither cannot corroborate, and so cannot trigger a split. */
  private def corroborated(aRuntimes: Set[Int], bRuntimes: Set[Int], aYears: Set[Int], bYears: Set[Int]): Boolean =
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
    if (aRuntimes.nonEmpty && bRuntimes.nonEmpty)
      aRuntimes.forall(x => bRuntimes.forall(y => math.abs(x - y) > RuntimeAgreementMinutes))
    else YearWindow.contradicts(aYears, bYears, YearWindow.PublishedAdjacency)

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
