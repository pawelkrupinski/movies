package models

/** The synopsis a record shows — globally, for one city, or from one kind of source — chosen from
 *  every source's blurb by one paragraphed-then-longest rule ([[bestSynopsis]]). Built once per
 *  [[MovieRecord]] instance (its `synopses`), which is what scopes the per-candidate memo. The
 *  public picks are documented where callers read them, on [[MovieRecord]]. */
private[models] final class MovieSynopses(record: MovieRecord) {
  import record.{data, retainedSynopses, slotCinemas}

  /** [[MovieRecord.synopsis]]. */
  def synopsis: Option[String] = bestSynopsis(synopsisCandidatesFor(_ => true))

  /** [[MovieRecord.synopsisForCity]]. */
  def forCity(city: City): Option[String] =
    bestSynopsis(synopsisCandidatesFor(synopsisAppliesToCity(city)), expectedLanguage = Some(city.country.language.getLanguage))

  /** [[MovieRecord.synopsisNonCinema]]. */
  def nonCinema: Option[String] =
    bestSynopsis(synopsisCandidatesFor(Source.cinemaOf(_).isEmpty))

  /** [[MovieRecord.synopsisCinema]]. */
  def cinema: Option[String] =
    bestSynopsis(synopsisCandidatesFor(Source.cinemaOf(_).isDefined))

  /** [[MovieRecord.synopsesForCities]]. */
  def forCities(cities: Seq[City]): Seq[(City, Option[String])] = {
    val pool   = synopsisPool
    val picked = scala.collection.mutable.HashMap.empty[(Seq[(Source, String)], String), Option[String]]
    cities.map { city =>
      val language   = city.country.language.getLanguage
      val candidates = pool.filter { case (source, _) => synopsisAppliesToCity(city)(source) }
      city -> picked.getOrElseUpdate((candidates, language), bestSynopsis(candidates, expectedLanguage = Some(language)))
    }
  }

  /** Does `source`'s synopsis count for `city`? A venue does iff it's one of
   *  the city's cinemas; a chain-detail source (Cinema City, Cineworld) iff one of its
   *  member venues in this city is screening the film; every non-cinema source
   *  (TMDB / IMDb / Filmweb) always does — its blurb is city-independent.
   *
   *  The cinema behind a slot comes from [[Source.cinemaOf]], never a `case _:
   *  Cinema` type test: prod keys every slot it writes as a per-title
   *  `CinemaShowing(cinema, titleKey)`, which is a `Source` but NOT a `Cinema`.
   *  A type test therefore admitted every cinema's blurb into every city (and,
   *  in [[nonCinema]], into the city-independent fallback the read model
   *  stores) — Kino Kosmos's Katowice event blurb for "Frances Ha" won the
   *  Poznań page over Kino Pałacowe's own, and `synopsisByCity` projected empty
   *  for every film. The bare-`Cinema` slot shape this scoping was written
   *  against survives only on legacy rows (see `Source.dropSupersededCinemaSlots`),
   *  which is why every spec covering it stayed green. */
  private def synopsisAppliesToCity(city: City)(source: Source): Boolean =
    Source.cinemaOf(source).forall { cinema =>
      city.cinemaSet.contains(cinema) ||
        CinemaNetworks.detailVenues.get(cinema).exists { venues =>
          slotCinemas.exists(v => venues.contains(v) && city.cinemaSet.contains(v))
        }
    }

  /** Best synopsis over `candidates` — the paragraphed-then-longest selection
   *  shared by [[synopsis]], [[forCity]] and [[nonCinema]].
   *  "Best" = a paragraphed blurb (one carrying a `\n`/`\n\n` break, see
   *  `ScraperParse.blockText`) beats an unbroken single block, and only among
   *  candidates that tie on that does the longest win — a wall of text reads
   *  worse than a properly broken-up one even when marginally longer.
   *
   *  A CMS-duplicated blurb (some cinemas paste the synopsis N× into one field —
   *  see `SynopsisMarkdown.collapseRepeats`) is collapsed to one copy FIRST, on
   *  the raw value before `stripUrls` (which trims, breaking the exact periodicity
   *  collapse needs), so it can't win the length race on its inflated size. URL
   *  tokens are then stripped (a cinema detail page that inlines a trailer link
   *  folds the bare URL into the blurb) so a link-padded blurb can't win on
   *  length either, and any source left empty (nothing but a link) is dropped.
   *  The length compared is the VISIBLE length — markdown emphasis markers are
   *  stripped for the comparison so a blurb can't win just by carrying more
   *  `<b>`/`<i>` tags. The winner is run through `SynopsisMarkdown.sanitize` so
   *  every consumer (web HTML, mobile `/api/details`, OG card, og:description)
   *  sees well-formed markdown regardless of which source produced it.
   *
   *  `expectedLanguage` (an ISO 639-1 tag, from [[forCity]]'s city) is
   *  the FIRST sort key when given, and ONLY for a candidate from a chain-detail
   *  slot (`CinemaNetworks.detailVenues`: [[CinemaCityChain]], [[CineworldChain]],
   *  [[RegalChain]]) — see [[forCity]]
   *  for why the check is scoped that narrowly. Such a candidate that
   *  `tools.TextLanguage.detect`s as a different language loses to any
   *  candidate that doesn't, before the paragraph/length comparison ever
   *  runs. A candidate too short/ambiguous to classify (`detect` → `None`) is
   *  never penalized — the check only fires on a confident wrong-language
   *  read, never a false suspicion. Callers with no city in scope
   *  ([[synopsis]], [[nonCinema]], [[cinema]]) pass `None`
   *  and get the untouched paragraph-then-longest order. */
  private def bestSynopsis(candidates: Seq[(Source, String)], expectedLanguage: Option[String] = None): Option[String] =
    candidates
      .flatMap { case (source, raw) => processedSynopsisCandidate(raw).map(source -> _) }
      .sortBy { case (source, (_, plainText)) =>
        val wrongLanguage = expectedLanguage.exists { expected =>
          Source.cinemaOf(source).exists(CinemaNetworks.detailVenues.contains) &&
            tools.TextLanguage.detect(plainText).exists(_ != expected)
        }
        (if (wrongLanguage) 1 else 0, if (plainText.contains('\n')) 0 else 1, -plainText.length)
      }
      .headOption.map { case (_, (candidate, _)) => tools.SynopsisMarkdown.sanitize(candidate) }

  // The per-candidate processing (collapseRepeats → stripUrls → strip) is a pure,
  // deterministic function of the raw candidate string — but `bestSynopsis` runs it once
  // per (city × display-title variant) during a read-model projection, so a blurb on a
  // film screening in many cities gets re-collapsed/re-stripped dozens of times. Memoise
  // it by raw candidate so each distinct string is processed once per record instance.
  // Pure caching: the winner and the value it wins with are unchanged — the read-model
  // output is byte-identical. Concurrent because a record is shared across reader threads.
  private val synopsisProcessCache =
    new java.util.concurrent.ConcurrentHashMap[String, Option[(String, String)]]()

  /** One raw synopsis candidate processed into `(processedMarkdown, plainText)` for the
   *  [[bestSynopsis]] paragraph-then-length pick — `None` when nothing survives
   *  URL-stripping (a source that was nothing but a link). `plainText` is the emphasis-
   *  stripped form the length/`\n` comparison uses; the winning `processedMarkdown` is
   *  what gets `sanitize`d. Memoised on the raw string — the projection asks for the same
   *  candidate across every city the film screens in. */
  private def processedSynopsisCandidate(raw: String): Option[(String, String)] =
    synopsisProcessCache.computeIfAbsent(raw, (rawValue: String) => {
      val processed = tools.TextNormalization.stripUrls(tools.SynopsisMarkdown.collapseRepeats(rawValue))
      if (processed.isEmpty) None
      else Some(processed -> tools.SynopsisMarkdown.strip(processed))
    })

  /** Synopsis candidates whose source passes `keep`, in source-priority order —
   *  each source's live slot synopsis followed by its retained (post-prune) one.
   *  Candidates come from BOTH the live `data` slots AND `retainedSynopses`
   *  (synopses kept after a cinema dropped the film), so the pool only grows over
   *  the row's life and shrinks only when the whole row is deleted — the result
   *  is sticky/monotonic, never downgrading when a cinema stops listing the film.
   *  Built in priority order (not raw `data`/`Set` iteration, whose order is
   *  JVM/platform-dependent) so that when two sources tie on length the STABLE
   *  `sortBy` in [[bestSynopsis]] keeps the higher-priority source —
   *  deterministic across machines (raw order drifted the whole-corpus snapshot
   *  between a dev box and CI). */
  private def synopsisCandidatesFor(keep: Source => Boolean): Seq[(Source, String)] =
    synopsisPool.filter { case (source, _) => keep(source) }

  /** Every source's synopsis, in priority order (a slot's own before its retained one) — what
   *  each candidate list filters. Filtering the few sources that carry one, rather than every key
   *  of a row with thousands of venue slots, is the same list in the same order: which sources
   *  pass depends on the source alone. */
  private def synopsisPool: Seq[(Source, String)] =
    (data.keySet ++ retainedSynopses.keySet).toSeq
      .sortBy(s => Source.priorityOf(s))
      .flatMap(s => (data.get(s).flatMap(_.synopsis).iterator ++ retainedSynopses.get(s).iterator).map(s -> _))
}
