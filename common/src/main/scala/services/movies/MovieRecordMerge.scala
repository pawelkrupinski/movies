package services.movies

import models.{MovieRecord, Showtime, Source, SourceData}

/**
 * Pure merge primitive — combines a `victim` row's per-source data onto a
 * `canonical` row. Used by:
 *   - `CaffeineMovieCache.put`'s tmdbId gate (runtime, prevents fresh
 *     duplicates from being persisted).
 *   - The same-tmdbId backfill script (one-shot, collapses legacy
 *     duplicates).
 *
 * Single source of truth so the runtime gate and the backfill agree on what
 * "merge two rows of the same film" means.
 *
 * Rule of thumb:
 *   - Enrichment-side single-source fields (tmdbId, imdbId, ratings,
 *     MC/RT/Filmweb URLs) prefer the canonical, falling back to the victim
 *     when the canonical lacks them. Both rows necessarily have the same
 *     tmdbId-derived data once they're identified as the same film, so
 *     preferring the canonical avoids churn — but when only the victim was
 *     enriched (a freshly-resolved cross-language duplicate became the
 *     canonical base), the fallback keeps its ratings instead of dropping
 *     them.
 *   - `data` is unioned per-source: when only one row has a slot for source
 *     S, that slot survives unchanged; when BOTH rows have a slot for the
 *     same source (the regression case — a cinema reports the film twice
 *     in the same tick under variant titles that resolve to the same
 *     tmdbId, e.g. "Diabeł ubiera się u Prady 2" + "Diabeł ubiera się u
 *     Prady 2 ukraiński dubbing" from CinemaCity Poznań Plaza), the two
 *     slots' **showtimes are merged** (deduplicated, time-sorted) and their
 *     metadata fields reconciled by [[MovieRecordMerge.mergeSlot]] — from the
 *     two SLOTS, not from which row is canonical, so the result cannot depend
 *     on which of them resolved first.
 *
 *     The previous right-biased `++` lost data: the second-resolved variant
 *     (often the dub, with one-off late screenings) overwrote the first
 *     variant's full schedule, so the user's main page listed Prada with
 *     only the dub's handful of showings for that cinema. The row was
 *     "still there" but its slot had silently regressed.
 */
object MovieRecordMerge {

  def union(canonical: MovieRecord, victim: MovieRecord): MovieRecord =
    canonical.copy(
      // Enrichment-side single-source fields prefer the canonical, but fall back
      // to the victim when the canonical lacks them. The two rows are the SAME
      // film (same tmdbId), so their ratings describe one film and only ever
      // converge — taking the victim's when the canonical's is empty can't be
      // wrong, and it stops the merge from DROPPING ratings. This matters once a
      // cluster can hold two RESOLVED rows (a cross-language duplicate folded by
      // shared tmdbId, FilmCanonicalizer.groupByFilm): the union base is the
      // lowest-`canonicalRank` row, which may be a freshly-resolved translation
      // that has a tmdbId but no ratings yet. A canonical-only copy then nulled
      // the rated sibling's scores until the next rating refresh re-fetched them —
      // the "ratings keep disappearing and coming back" flap. With the fallback,
      // `canonical` is order-independent for enrichment, as its callers assume.
      imdbId            = canonical.imdbId.orElse(victim.imdbId),
      imdbRating        = canonical.imdbRating.orElse(victim.imdbRating),
      metascore         = canonical.metascore.orElse(victim.metascore),
      filmwebUrl        = canonical.filmwebUrl.orElse(victim.filmwebUrl),
      filmwebRating     = canonical.filmwebRating.orElse(victim.filmwebRating),
      rottenTomatoes    = canonical.rottenTomatoes.orElse(victim.rottenTomatoes),
      tmdbId            = canonical.tmdbId.orElse(victim.tmdbId),
      metacriticUrl     = canonical.metacriticUrl.orElse(victim.metacriticUrl),
      rottenTomatoesUrl = canonical.rottenTomatoesUrl.orElse(victim.rottenTomatoesUrl),
      searchTitle       = canonical.searchTitle.orElse(victim.searchTitle),
      data              = mergeData(canonical.data, victim.data),
      retainedSynopses  = mergeRetainedSynopses(canonical.retainedSynopses, victim.retainedSynopses)
    )

  /** Combine two retained-synopsis maps, keeping the LONGEST synopsis seen per
   *  source (the canonical's on an exact-length tie). Shared by `union` and by
   *  `MovieCache`'s slot-prune capture so both agree on "best-seen per source"
   *  — the rule that keeps the displayed (longest-wins) synopsis sticky once a
   *  cinema stops listing a film. Commutative on content, so it's
   *  scrape/merge-order independent. */
  def mergeRetainedSynopses(
    a: Map[Source, String],
    b: Map[Source, String]
  ): Map[Source, String] =
    (a.keySet ++ b.keySet).iterator.map { s =>
      s -> (a.get(s).iterator ++ b.get(s).iterator).maxBy(_.length)
    }.toMap

  /** Fold a set of rows of the SAME film into one. The enriched row (the first
   *  carrying a `tmdbId`) is the canonical, so its single-source enrichment
   *  fields survive; every other row's per-source `data` is unioned onto it.
   *  Used wherever several stored rows resolve to one merge key at once, e.g. the
   *  cache's `rehydrate` (a late merge-key rule makes two documents collide).
   *  `records` must be non-empty. */
  def unionAll(records: Seq[MovieRecord]): MovieRecord = {
    require(records.nonEmpty, "MovieRecordMerge.unionAll: no records")
    val canonical = records.find(_.tmdbId.isDefined).getOrElse(records.head)
    records.filterNot(_ eq canonical).foldLeft(canonical)(union)
  }

  private def mergeData(
    canonical: Map[Source, SourceData],
    victim:    Map[Source, SourceData]
  ): Map[Source, SourceData] =
    (canonical.keySet ++ victim.keySet).iterator.map { src =>
      val mergedSlot = (canonical.get(src), victim.get(src)) match {
        case (Some(a), Some(b)) => mergeSlot(a, b)
        case (Some(a), None)    => a
        case (None,    Some(b)) => b
        case (None,    None)    => throw new MatchError(src)   // unreachable: src ∈ keys union
      }
      src -> mergedSlot
    }.toMap

  /** Reconcile two slots filed under the SAME source, as a COMMUTATIVE function of
   *  the pair: `mergeSlot(a, b) == mergeSlot(b, a)`.
   *
   *  One source really can hold two slots at fold time. Cinema City lists a film
   *  both as itself and as a separate "… - wersja rozszerzona" edition, each with
   *  its own chain film id and its own detail payload; both editions resolve to the
   *  same TMDB film, so the rows fold — and both carry a slot under the one
   *  `CinemaCityChain` source. The two payloads disagree (the base edition's
   *  `categoriesAttributes` says "horror", the extended one's says "thriller").
   *
   *  This used to keep the CANONICAL row's slot wholesale and take only the other's
   *  showtimes, so whichever row happened to be tmdbId-bearing when the fold fired
   *  decided every other field. That is arrival order, not data: the Poland
   *  convergence leg replayed one archived corpus three times and got
   *  `genres = Thriller` on one pass and `Horror` on the next for
   *  "Backrooms. Bez wyjścia". Nothing about the row set had changed — only which
   *  edition resolved first.
   *
   *  So decide from the SLOTS themselves. Each field takes the side that published
   *  one; where both published and they differ, [[richer]] picks the same side every
   *  time. Fields neither side has stay empty, and a field only one side has now
   *  SURVIVES the fold instead of being dropped with the losing slot — the merge
   *  gained data as well as determinism. */
  private[services] def mergeSlot(a: SourceData, b: SourceData): SourceData = {
    // The side whose slot is richer overall — the tie-break for a field both
    // published with different values, and the source of the cache-only digest
    // fields below, which are not independently mergeable.
    val (primary, other) = if (richer(a, b)) (a, b) else (b, a)
    def text(pick: SourceData => Option[String]): Option[String] =
      pick(primary).filter(_.nonEmpty).orElse(pick(other).filter(_.nonEmpty))
    def number(pick: SourceData => Option[Int]): Option[Int] =
      pick(primary).orElse(pick(other))
    def list(pick: SourceData => Seq[String]): Seq[String] =
      if (pick(primary).nonEmpty) pick(primary) else pick(other)
    SourceData(
      title           = text(_.title),
      rawTitle        = text(_.rawTitle),
      originalTitle   = text(_.originalTitle),
      englishTitle    = text(_.englishTitle),
      // Longest wins, the same rule `mergeRetainedSynopses` already applies to the
      // stickied copies of these blurbs — a truncated listing teaser must not beat
      // the full detail-page text just because its slot is richer elsewhere.
      synopsis        = (primary.synopsis.iterator ++ other.synopsis.iterator)
                          .filter(_.nonEmpty).maxByOption(_.length),
      cast            = list(_.cast),
      director        = list(_.director),
      runtimeMinutes  = number(_.runtimeMinutes),
      releaseYear     = number(_.releaseYear),
      countries       = list(_.countries),
      genres          = list(_.genres),
      posterUrl       = text(_.posterUrl),
      filmUrl         = text(_.filmUrl),
      trailerUrl      = text(_.trailerUrl),
      showtimes       = dedupShowtimes(a.showtimes ++ b.showtimes),
      language        = text(_.language),
      // Cache-only, and both describe the showtime list they were stamped from — which
      // is neither of these two once the lists are unioned. `ShowtimesDigest.stripForCache`
      // re-stamps them on the way into the cache; carrying the richer side's forward
      // keeps a merged-but-not-yet-restripped slot self-consistent in the meantime.
      showtimesDigest = primary.showtimesDigest.orElse(other.showtimesDigest),
      showtimesCount  = primary.showtimesCount.orElse(other.showtimesCount),
      ageRating       = text(_.ageRating)
    )
  }

  /** Is `a` the side a disagreement should be settled on? A pure function of the two
   *  slots, so it answers the same whichever way round it is asked (`richer(a, b)`
   *  and `richer(b, a)` can't both be true unless the slots are identical, in which
   *  case the choice doesn't matter).
   *
   *  More populated fields first — the slot that describes the film more fully is the
   *  better witness — then a total order on the content so two equally-populated slots
   *  still resolve the same way every time. */
  private def richer(a: SourceData, b: SourceData): Boolean =
    Ordering[(Int, String)].lteq(
      (-populatedFields(a), contentOrder(a)),
      (-populatedFields(b), contentOrder(b)))

  // The fields a disagreement between two slots can land on, named ONCE — both the
  // richness count and the total order below walk exactly these, and a field that
  // appeared in one but not the other would quietly weaken whichever it was missing from.
  private def texts(slot: SourceData): Seq[Option[String]] =
    Seq(slot.title, slot.rawTitle, slot.originalTitle, slot.englishTitle, slot.synopsis,
        slot.posterUrl, slot.filmUrl, slot.trailerUrl, slot.language, slot.ageRating)
  private def numbers(slot: SourceData): Seq[Option[Int]] = Seq(slot.runtimeMinutes, slot.releaseYear)
  private def lists(slot: SourceData): Seq[Seq[String]]   = Seq(slot.cast, slot.director, slot.countries, slot.genres)

  private def populatedFields(slot: SourceData): Int =
    texts(slot).count(_.exists(_.nonEmpty)) + numbers(slot).count(_.isDefined) + lists(slot).count(_.nonEmpty)

  /** A total order over a slot's identifying content — every field a disagreement
   *  could land on, so two slots compare equal here only when they would merge to the
   *  same thing anyway. Showtimes themselves are excluded (they are unioned, never
   *  chosen), but their cache-only digest/count are not: those are the one pair
   *  [[mergeSlot]] takes wholesale, so leaving them out would let two otherwise
   *  identical slots still resolve by argument order.
   *
   *  Joined on separators no value can contain, so one field's content cannot run into
   *  the next and make two different slots compare equal. */
  private def contentOrder(slot: SourceData): String =
    (texts(slot).map(_.getOrElse("")) ++
     (numbers(slot) ++ Seq(slot.showtimesDigest, slot.showtimesCount)).map(_.fold("")(_.toString)) ++
     lists(slot).map(_.mkString("\u001e"))
    ).mkString("\u001f")

  /** Collapse `showtimes` to one entry per *physical* screening, time-sorted.
   *
   *  A screening's identity is `(dateTime, room, format)` — NOT the whole
   *  `Showtime`. `bookingUrl` is a per-source ticket link, not part of what
   *  makes two sessions the same; a plain `.distinct` keyed on the full case
   *  class kept the same screening twice whenever two sources (e.g. Kino Nowe
   *  Horyzonty surfacing one film under two `op.s?id=` event pages) reported
   *  it with different booking links. That produced a phantom duplicate whose
   *  surviving order flipped with scrape/merge order — the "screenings error".
   *
   *  Among entries sharing an identity, the representative is chosen by a pure
   *  function of the data (the one carrying a `bookingUrl`, then the lowest URL
   *  string) so the kept link — and therefore the rendered slot — is identical
   *  whatever order the sources arrived in. */
  def dedupShowtimes(showtimes: Seq[Showtime]): Seq[Showtime] = {
    // Screening identity is (dateTime, room, format) PLUS a per-screening
    // discriminator from the booking URL: hosts that open a slot in several halls
    // at once (Helios premieres, `room=None`) carry a distinct `/screen/<uuid>`,
    // so those parallel screenings must NOT collapse. Everything else contributes
    // `""` and collapses on the slot as before. See BookingUrlScreening.
    def identity(s: Showtime): (java.time.LocalDateTime, Option[String], List[String], String) =
      (s.dateTime, s.room, s.format, BookingUrlScreening.discriminator(s.bookingUrl))
    def rank(s: Showtime): (Boolean, String) =
      (s.bookingUrl.isEmpty, s.bookingUrl.getOrElse(""))
    // Total order (not just dateTime): `groupBy(...).values` iteration order is
    // non-deterministic, so same-time showings could otherwise land in a
    // scrape/merge-order-dependent sequence — a re-fold would then re-write the
    // same content in a different order (the churn `sortShowtimes` exists to kill).
    sortShowtimes(showtimes.groupBy(identity).values.map(_.minBy(rank)).toSeq)
  }

  /**
   * Which of a venue's same-slot listings supplies the scalar film fields —
   * poster, detail link, runtime, year.
   *
   * A cinema can list one film twice in a single scrape: Kino Kultura publishes
   * "Ghost in the Shell" as two screening series, each with its own poster image
   * and its own showings. Both collapse to the slot key `sanitize(title)`, and one
   * of them has to speak for the slot.
   *
   * The rank must be a TOTAL order over the competing data, and previously it was
   * not: `(filmUrl, title)` is identical for both of Kino Kultura's entries — same
   * title, neither carrying a detail link — so `minBy` fell through to whichever
   * the scraper happened to emit first. Shuffle the venue's films and a different
   * poster won, which is exactly the order-dependence
   * `CountryConvergenceBehaviour` fails on against the real archive. Nothing was
   * "last write wins"; the ordering simply ran out of discriminators.
   *
   * Both links are therefore ranked PRESENCE FIRST, the same shape
   * [[dedupShowtimes]] uses for `bookingUrl`: an entry carrying a real value must
   * never lose to one carrying nothing, and only then does the value itself break
   * the tie — a pure function of the data that cannot depend on arrival order.
   *
   * Presence-first matters for `filmUrl` as much as for the poster, and it did not
   * used to hold. The key was `filmUrl.getOrElse("")`, and `""` sorts before every
   * real URL, so a listing with NO detail link beat one that had it. That costs
   * twice over: the cinema renders as plain text instead of a deep link, and
   * `DetailReaper` never enriches the slot at all, because a slot with no `filmUrl`
   * has nothing to fetch — so the film silently loses its synopsis and cast.
   */
  def slotRepresentative(group: Seq[models.CinemaMovie]): models.CinemaMovie =
    group.minBy(slotRepresentativeRank)

  /**
   * Richest first, then TOTAL.
   *
   * The preference keys are the fields whose absence is visible: a detail link, a
   * poster, a synopsis (longest wins, as [[mergeRetainedSynopses]] already does
   * across sources). Those say which duplicate is the better representative.
   *
   * The last key says something different and is the load-bearing one. Enumerating
   * fields cannot make an ordering total — this rank was extended twice, once for
   * the poster and once for the detail link, and each time the next un-enumerated
   * field (synopsis, found by the convergence suite against KinoGram's duplicated
   * "Spider-Man") quietly took its place as the thing that decided by arrival
   * order instead. `toString` is a pure function of the WHOLE listing, so two
   * candidates can tie only if they are genuinely identical — at which point the
   * choice cannot matter. That closes the class of bug rather than its latest
   * instance.
   */
  private def slotRepresentativeRank(cm: models.CinemaMovie): (Boolean, String, String, Boolean, String, Int, String) =
    (cm.filmUrl.isEmpty, cm.filmUrl.getOrElse(""), cm.movie.title,
     cm.posterUrl.isEmpty, cm.posterUrl.getOrElse(""),
     -cm.synopsis.map(_.length).getOrElse(0),
     cm.toString)

  /** Canonical TOTAL order for a cinema slot's showtimes. Sorting at the
   *  ingestion boundary means a re-scrape that returns the same showings in a
   *  different order stores a byte-identical slot, so `MovieCache`'s write-through
   *  equality guard (`updated == before`) skips the write — and with it the
   *  change-stream event and reprojection that a reorder-only "change" would
   *  otherwise trigger. Total across every field (unlike [[dedupShowtimes]]'s
   *  `dateTime`-only sort): `dateTime` first (its ISO string sorts
   *  chronologically), then room/format/bookingUrl, so equal multisets of
   *  showings always collapse to the same sequence. */
  def sortShowtimes(showtimes: Seq[Showtime]): Seq[Showtime] =
    showtimes.sortBy(s => (s.dateTime.toString, s.room.getOrElse(""), s.format.mkString(","), s.bookingUrl.getOrElse("")))
}
