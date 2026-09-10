package services.movies

import clients.TmdbClient
import models.MovieRecord
import play.api.Logging
import services.enrichment.{LetterboxdIdResolver, WikidataClient}
import services.resolution.{Candidate, Contradiction, ResolutionCache, ResolutionKeys, SearchTitles, TitleMatch, TmdbBasis, Verdict}

/**
 * The SEARCH half of TMDB resolution: which candidate film, if any, a row's
 * cinema evidence points at. Pure of storage — it reads the row it is handed and
 * TMDB, and returns a candidate id with the search hit and the basis it was
 * concluded on. Writing that conclusion onto the row, settling siblings and
 * publishing the outcome is `MovieService`'s job, which is the other reason to
 * change and the reason this is its own class.
 *
 * Three sources, tried in order and each gated by [[Verdict]]: a title search
 * (only when no director is reported, and only when unambiguous), a walk of a
 * reported director's filmography, and the exact reverse lookups from an
 * external id (IMDb, Letterboxd, Filmweb→Wikidata). The id memo (`tmdbIdCache`)
 * caches only a HIT, keyed on the exact hint combination.
 */
class TmdbCandidateSearch(
  tmdb:                 TmdbClient,
  normalizer:           TitleNormalizer,
  tmdbIdCache:          ResolutionCache,
  letterboxdIdResolver: Option[LetterboxdIdResolver],
  wikidata:             Option[WikidataClient]
) extends Logging {

  // ── TMDB resolution ────────────────────────────────────────────────────────

  // Resolution order:
  //   1. Sister-row alias match — when another cache row sharing any title
  //      alias (cleanTitle or `originalTitle` hint) has already been
  //      TMDB-resolved, inherit that resolution. Avoids year-less search
  //      collisions (Bez końca 1985 vs 2026, Belle 2013 vs Hosoda anime,
  //      etc.). This is where the `originalTitle` hint pulls its weight.
  //   2. Polish-localised TMDB title search, verified by director when the
  //      cinema reported one (rejects same-title-different-film hits).
  //   3. Director-page walk — when the cinema reports a director and the
  //      title path either missed or returned a candidate with a different
  //      director, search TMDB for the director by name and pick their
  //      filmography entry whose year matches the cinema's. Solves the
  //      Niedźwiedzica class of mis-resolution: Polish title "Niedźwiedzica"
  //      maps to Grizzly Falls 1999 on TMDB, but the cinema's reported
  //      director "Asgeir Helgestad" leads us to his 2026 film instead.
  //
  // The IMDb id is OPTIONAL: TMDB doesn't always have a cross-reference yet
  // (very recent releases — e.g. "Za duży na bajki 3" tmdbid 1484486 has no
  // imdb_id at TMDB at the time of writing). When we have only a TMDB hit, we
  // still store the row (Filmweb / MC / RT all key off the title, not the
  // IMDb id); `ImdbIdMissing` fires from the async TMDB stage so
  // `ImdbRatings` can recover the id via IMDb's suggestion endpoint.
  //
  // A `tmdb.search(originalTitle, year)` fallback was previously inserted
  // between (2) and (3). Audit on 356 films across 9 cinemas found 0 films
  // with `originalTitle` set but no `director` — i.e. every film the
  // fallback could uniquely help was also reachable via director-walk.
  // Dropped to keep the chain minimal.
  def resolve(
    title:         String,
    year:          Option[Int],
    row:           MovieRecord,
    originalTitle: Option[String] = None,
    director:      Option[String] = None
  ): Option[(Int, Option[TmdbClient.SearchResult], Option[TmdbBasis])] = {
    // Resolve from the row's OWN reported titles. A decorated festival/preview
    // row whose own title doesn't match TMDB ("Opętanie | ŻUŁAWSKI. KINO
    // EKSTAZY", "Ojczyzna (pokaz przedpremierowy)") must still resolve on its
    // own — depending on a relative resolving first was an enrichment-order race
    // that made whole-corpus snapshots flaky. Build candidates from every
    // cinema-reported title + every slot's original (English) title, plus
    // de-decorated forms (each side of a "X | Y" pipe, trailing "(…)" dropped),
    // and try each (verdict-gated, so extra terms can't mis-resolve
    // onto a same-title different film). `apiQuery` additionally strips the
    // accessibility-programme decoration ("Kino bez barier: Freak Show (AD)" →
    // "Freak Show") before hitting TMDB.
    // `row` carries the cinema slots to mine for candidates. On the movies path
    // it's the live cache row; on the cache-free staging path it's the union of
    // the film's per-cinema staging rows (`resolveStagingRecord`'s `existing`).
    // Both therefore build the SAME candidate set — without it the staging row,
    // absent from the cache, collapsed to its single title and missed films the
    // direct path resolved via a cinema-reported / original title.
    val evidence      = row.evidence
    val cinemaTitles  = evidence.titles
    val slotOriginals = row.data.values.flatMap(_.originalTitle).toSet
    // Sorted so the candidate order — hence which query resolves first when
    // several map to the same film — is independent of the (Set) iteration
    // order, which varied run-to-run.
    val extraTitles = (cinemaTitles ++ slotOriginals).toSeq.sorted
    // Try BOTH the raw-stripped and the re-cased-stripped form of every
    // candidate: a cinema that reports a title ALL-CAPS resolves via the re-cased
    // query ("MONTEREY POP" → "Monterey pop"), while one that reports it as-is
    // resolves via the raw query — and TMDB is case-insensitive, so trying both
    // costs nothing but covers every spelling. Order is deterministic
    // (`searchTitleCandidates` is pre-sorted), so resolution is order-independent.
    def queryForms(titles: Iterable[String]): Seq[String] = SearchTitles
      .candidates(title, originalTitle, titles)
      .flatMap(t => Seq(normalizer.apiQuery(t), normalizer.searchQuery(t)))
      .filter(_.nonEmpty).distinct
    val candidates = queryForms(extraTitles)
    // The same set restricted to what the CINEMAS published — `title` and
    // `originalTitle` are already cinema-side (`tmdbHints` reads
    // `cinemaOriginalTitle`), so only `slotOriginals` is dropped. The walk below
    // ranks a credit matching one of these ABOVE a credit matching a derived
    // title, so a row's own previous resolution can never outbid the cinemas.
    val cinemaCandidates = queryForms(cinemaTitles.toSeq.sorted)
    // How many CINEMA SLOTS published each title form — see `FilmEvidence.titleVotes`.
    // `cinemaTitles` is a set, so it cannot tell the film 38 venues are showing from
    // the one a single venue lists under a different name; the walk below weights a
    // credit by the venues naming it.
    val cinemaTitleWeight: Map[String, Int] = evidence.titleVotes(normalizer)
    // Director hints drawn from EVERY cinema slot on the merged row, not just the
    // one cinema event that happened to trigger this stage. Every cinema fires its
    // own `MovieDetailsComplete`, so the triggering event's director varied with
    // arrival order (Helios/Multikino report a director, CinemaCity/Charlie
    // Monroe don't) — and a director-bearing trigger that failed verification
    // recorded a miss before a director-less trigger
    // could resolve the same row, so whether the film enriched hinged on which
    // event won the per-key `pending` race. Sourcing the hints from the row's
    // own slots (sorted) makes the resolution a deterministic function of the
    // row's state — every event computes the same outcome, so the race is moot.
    // CINEMA slots only, though. Mining `row.data.values` swept in the derived
    // `Tmdb`/`Imdb`/`Filmweb` slots too — the previous resolution's own output —
    // so a mis-resolved row corroborated itself: it grew a second "reported"
    // director (the wrong film's), and because the hints are `.sorted` and the
    // walk below takes the FIRST that hits, which film the row re-resolved to
    // came down to alphabetical order. Kino Malta's "Dreams" reports Michel
    // Franco; the row's stale match to Dag Johan Haugerud's "Drømmer" sorted
    // ahead of it and re-won every cycle, so Michel Franco was never walked at
    // all (`MovieServiceTmdbHintsSpec`). `cinemaOriginalTitle`, the other half of
    // this hint pair, is cinema-only for exactly the same reason.
    val rowDirectors = evidence.withDirectors(director.toSeq.flatMap(_.split(","))).directors

    // Cache the id resolution per hint-combination: two cinema rows (or two
    // scrape cycles) with the same title + year + director set + original-title
    // hint resolve to the same film, so the search loop + director-walk runs
    // once and the answer is reused for 24h. The key is built from exactly those
    // hints (sorted, so it's order-independent — see `ResolutionKeys`). Only a
    // HIT is cached; a no-match re-resolves next cycle.
    // A row whose scrape carried no year reads a DELIMITED year written into its
    // title ("Klasyka w NCKF: Generał (1926) 4K", "Following (1998)") as the lookup
    // year — a deterministic hint (pure function of the title). Here it's used only
    // for the SEARCH, so it can't race the canonical rank; the same `EmbeddedYear`
    // is separately PERSISTED onto the row at the scrape boundary
    // (`MovieCache.recordCinemaScrape`), where `canonicalRank` owns the re-key. The
    // `.orElse` means a persisted year already wins here. It unblocks the year-less
    // singleton guard below: with a year the director-less branch takes the safe
    // year-scoped exact-title path instead of refusing every multi-hit title as
    // `searchUnique` does when the year is absent.
    // A year a CINEMA reported, when the row itself has none. A staging row's `year`
    // column is fixed when the newcomer is diverted — before any detail page has been
    // fetched — and the detail then merges `releaseYear` onto the SLOT with nothing
    // back-filling the column. Reading only the column sent films to TMDB year-less
    // whose detail page stated the year plainly: production's Kino Iluzjon slot for
    // "Pokój 666" carries releaseYear 1982 and director Wim Wenders, both parsed from
    // the very page the replay had recorded and merged, and the leg still logged
    // `'Pokój 666' (?) → no match`. A year-less, director-less search is one this
    // method rightly refuses to guess at, so the row never stood a chance.
    //
    // Sorted and distinct so the choice is a pure function of the row rather than of
    // slot iteration order, and taken only when the row has no year of its own — the
    // persisted year still wins. `ImdbIdResolver.resolve` already reads the
    // cinema-reported years exactly this way.
    val reportedYears = row.data.values.flatMap(_.releaseYear).toSeq.distinct.sorted
    // A key year STAMPED FROM A GUESS is not evidence — it is the guess, handed
    // back as if it were a fact. `MovieCache.settleResolved` re-keys a yearless row
    // onto the year of whatever film resolved it, so a TitleOnly conclusion writes
    // its own answer into the key; every later attempt then searches that year and
    // finds the same film, however loudly the cinemas disagree. Prod's
    // `homosapiens|1960` was keyed 1960 because a title-only search picked a
    // 9-minute 1960 short, while twelve venues published 2025. So a row whose
    // conclusion was TitleOnly falls through to what the CINEMAS published — the one
    // year here not derived from the resolution being re-examined. Any stronger
    // basis keeps its year: those were narrowed by evidence in the first place.
    val keyYearIsEvidence = !row.tmdbBasis.flatMap(TmdbBasis.parse).contains(TmdbBasis.TitleOnly)
    val keyYear = Option.when(keyYearIsEvidence)(year).flatten
    // The same trap once more, for the rows that predate a recorded basis — 6194 of
    // prod's 6201 resolved rows carry none, so the guard above passes them all. A key
    // year NO SOURCE EVER REPORTED has only one place it can have come from:
    // `settleResolved` stamping the resolution's own answer back into the key. So it
    // yields to a year the VENUE wrote into its own title, which is the venue saying
    // WHICH film it means. Two UK venues published "Scarface (1932)" and 93 minutes at
    // a row keyed 1983, and the 1983 in the key kept winning.
    //
    // A key year a source DID report is the cinema's own evidence and keeps its
    // precedence, so a decorative "(1926)" on a row scraped as 2015 still loses.
    // `cinemaYears` and not `reportedYears`: the latter includes the TMDB slot's own
    // year, so a mis-resolved row would corroborate its key year with the very
    // resolution being re-examined — "scarface|1983" carries a 1983 TMDB slot.
    val corroboratedKeyYear = keyYear.filter(evidence.years.contains)
    // Only the EMBEDDED year is promoted above the key year. `reportedYears` stays
    // BELOW it, as it always was: it is sorted ascending and spans every slot, so
    // one venue misreporting 1999 on a 2024 film would otherwise hand the search the
    // older year.
    // `cinemaYears` ABOVE `reportedYears`, and both below an evidence-bearing key
    // year. For a TitleOnly row `keyYear` is None by design, so `reportedYears` was
    // the sole fallback — and it spans every slot INCLUDING Tmdb, sorted ascending,
    // so the row handed the search back the very year its own guess had written.
    // `homosapiens|1960` carries a 1960 Tmdb slot against twelve venues publishing
    // 2025: min([1960, 2025]) is 1960, and the guess re-confirmed itself through the
    // fallback this guard exists to route around. What the CINEMAS published is the
    // one year here not derived from the resolution being re-examined.
    val effectiveYear = corroboratedKeyYear
      .orElse(EmbeddedYear.ofAll(Seq(title) ++ candidates ++ cinemaTitles))
      .orElse(keyYear)
      .orElse(evidence.years.headOption)
      .orElse(reportedYears.headOption)
    val hintKey = ResolutionKeys.tmdb(title, effectiveYear, rowDirectors, originalTitle, normalizer)
    // `freshHit` captures the SearchResult on a cache MISS (the loader runs on
    // this thread), so the caller keeps the hit's title/year as a fallback when
    // the full-details fetch fails. On a cache HIT the loader doesn't run and it
    // stays None — only the id is cached.
    // Without a DIRECTOR there is no reliable way to tell same-title films apart, so
    // any title search returning several would resolve via the popularity/year-
    // proximity tie-break — a GUESS, and an order-dependent one once a merged year or
    // a more-popular sibling drifts in ("Guru" alone maps to THREE TMDB films: a
    // Persian "لؤ گورو", Yann Gozlan's "Gourou", and his unrelated "Dalloway"). Refuse
    // rather than guess: when no director is reported, resolve ONLY when the search
    // (year-scoped if a year is present) is unambiguous — exactly one result. A
    // director-bearing row keeps the richer director-walk path below, which can
    // disambiguate. This is the generalised "Zaproszenie" guard (a bare title with two
    // same-title TMDB entries) — see StagingOrderDeterminismSpec.
    // None until the loader below actually runs. A resolution-cache HIT skips it, and
    // the basis of that cached id is not knowable here — claiming `TitleOnly` for it
    // would overwrite a stored `DirectorWalk`/`YearScoped` with the weakest value,
    // which `resolvedOnWeakerEvidenceThanAvailable` then reads as "re-resolve me". The
    // re-resolve hits the same cache, records `TitleOnly` again, and the row churns
    // once every sweep for ever. An unknown basis must stay unknown.
    var searchBasis: Option[TmdbBasis] = None
    var freshHit: Option[TmdbClient.SearchResult] = None
    // The credited person whose filmography a walk hit came from — on the film's
    // crew by construction, so the verdict below can see the agreement even where
    // TMDB spells the name in a way `SamePerson` cannot bridge.
    var walkedBy: Option[String] = None
    val resolvedId = tmdbIdCache.getOrResolve(hintKey) {
      val hit =
        if (rowDirectors.isEmpty)
          // First the strict singleton rule; then, when a YEAR is present, accept a
          // year-scoped search whose TOP hit is an exact-title match even if it
          // returned several films (e.g. "Sundown" alongside "Sundown Town", "DJ at
          // Sundown"). The year + verbatim top is confidence the singleton rule
          // lacks — still no popularity guess (a non-exact top doesn't resolve), and
          // yearless rows are untouched (searchYearExactTop is a no-op without a year).
          candidates.iterator.flatMap(q => tmdb.searchUnique(q, effectiveYear)).nextOption()
            .orElse(candidates.iterator.flatMap(q => tmdb.searchYearExactTop(q, effectiveYear)).nextOption())
            .map(hit => { searchBasis = Some(if (effectiveYear.isDefined) TmdbBasis.YearScoped else TmdbBasis.TitleOnly); hit })
        else {
          // Resolve from this row's own titles only — no sister-row shortcut. Copying
          // a tmdbId from an already-resolved relative was order-dependent (it could
          // only borrow once the relative had resolved), which is what made
          // whole-corpus snapshots flaky. Own-title search + director-walk are
          // order-independent, so the row resolves to the same film every run.
          // Director-walk each reported director in turn (sorted) so a row whose
          // first-sorted director name happens to miss still recovers via the others.
          // When the row reports a director, the WALK is the resolution — the only
          // one. It walks the director's filmography and picks the title-matching
          // credit by lowest id, deterministic across a TMDB adjacent-year
          // DUPLICATE of one film (Yann Gozlan's "Gourou").
          //
          // There is deliberately NO title-search fallback here. Verifying a search
          // hit by director only asks "do this candidate's credits name the reported
          // director" — which cannot separate two films by the SAME director. Gozlan's
          // "Gourou" and "Dalloway" both pass that check, so whichever the title
          // search happened to return won, and the answer moved with the row's
          // (merge-order-dependent) key year. Walking the filmography and matching the
          // title is what actually picks between them; when the walk can't find the
          // film, no match is the honest answer, not a rubber-stamped guess
          // (`DirectorWalkResolvesSpec`). Director-LESS rows are unaffected — they
          // still take the strict unambiguous-search branch above, which never picks
          // between candidates either.
          // The row's own cinema-published runtime and cast travel with the walk so
          // a year-pinned credit can be corroborated by something other than the
          // title. CINEMA-only, like every other hint here — reading the merged
          // fields would hand the check the previous resolution's own numbers.
          rowDirectors.iterator
            .flatMap(d => directorWalk(Some(d), effectiveYear, candidates, evidence.runtimes, evidence.cast, cinemaCandidates, cinemaTitleWeight).map(d -> _))
            .nextOption()
            .map { case (d, hit) => searchBasis = Some(TmdbBasis.DirectorWalk); walkedBy = Some(d); hit }
        }
      freshHit = hit
      hit.map(_.id.toString)
    }.map(_.toInt)
      // Whatever path resolved it (director-walk, year-scoped search, popularity),
      // pin a same-director adjacent-year TMDB duplicate to its lowest id so the
      // outcome can't drift with scrape/merge order. No-op when no director or no dup.
      .map(rid => if (rowDirectors.nonEmpty) collapseDirectorDuplicate(rid, rowDirectors) else rid)
    resolvedId
      // VETO: a title search can land on a film of an entirely different kind —
      // prod matched "Vivaldi i ja" to an 18-minute STABAT MATER concert short
      // while 46 venues advertised the 110-minute feature, and "Homo sapiens?" to
      // a 9-minute animated short. The row then carried that film's year, poster
      // and ratings, which is worse than carrying none. `Verdict` is the one
      // judgement the sweep and the re-verify use too: the cinemas' own minutes deny
      // a category error, and an agreeing credit — a walk hit carries the walked
      // person on its crew by construction — settles a short film in a longer slot
      // as the sweep does, instead of the two disagreeing. Only a RUNTIME rejection
      // vetoes here: a name rejection needs TMDB's crew ids to confirm, and the
      // resolver never spent one before. Only the SEARCH paths are vetoed; the
      // id-based fallbacks below are exact reverse lookups, not guesses.
      .filter { id =>
        val details   = tmdb.fullDetails(id)
        val candidate = Candidate(id,
          titles  = details.toSet.flatMap(d => Set(d.title, d.originalTitle).flatten),
          year    = details.flatMap(_.releaseYear),
          runtime = details.flatMap(_.runtimeMinutes),
          crew    = details.toSeq.flatMap(_.crew) ++ walkedBy.toSeq,
          cast    = details.toSeq.flatMap(_.cast))
        val credible = Verdict.of(evidence, candidate) != Verdict.Reject(Contradiction.Runtime)
        if (!credible)
          logger.info(s"TMDB: '$title' (${year.getOrElse("?")}) → rejecting $id: its " +
            s"runtime is not credible against the cinemas' ${evidence.runtimes.mkString("/")} min")
        credible
      }
      .map(id => (id, freshHit, searchBasis))
      // FALLBACK — exact reverse lookup by a known imdbId, only when the title /
      // director search above found nothing AND the row has no tmdbId yet. Such a
      // row can carry an imdbId from a NON-TMDB source (`OmdbBackfill` recovers one
      // by title+year search for exactly the films TMDB's fuzzy search misses), so
      // TMDB's `/find` returns the exact tmdbId that search couldn't. Gated on an
      // absent tmdbId so a row that's already TMDB-resolved never resurrects a
      // drifted resolution from its TMDB-DERIVED imdbId — a re-enrich whose search
      // now misses must leave that row untouched, not re-confirm the stale id.
      // After TMDB's own `/find` misses, cross to the other id-keyed source —
      // Letterboxd's page scrape — which can hold the imdbId→tmdbId mapping TMDB
      // itself lacks for an obscure title. Same `tmdbId.isEmpty` gate: never
      // resurrect a drifted resolution from a TMDB-derived imdbId. `None`
      // SearchResult — the tmdbId alone drives the by-id details fetch
      // downstream (as on a cache hit).
      .orElse {
        if (row.tmdbId.isEmpty) {
          def viaLetterboxd: Option[Int] =
            for {
              id       <- row.imdbId
              resolver <- letterboxdIdResolver
              tmdbId   <- resolver.resolveTmdbId(id)
            } yield tmdbId
          // Filmweb→Wikidata backstop — for a row with a Filmweb URL but no
          // imdbId to cross-walk. Filmweb enrichment is now un-gated for
          // tmdbId-less rows (see `RatingSources`), so a scraper-supplied /
          // Filmweb-discovered URL yields an entity id Wikidata maps to a TMDB id
          // (P5032 → P4947) — the route for the arthouse/repertoire long tail
          // TMDB's own fuzzy search misses. The chain crosses two external
          // cross-references either of which can be mis-linked (the stored URL can
          // point at the wrong edition), so accept the tmdbId ONLY when the
          // resolved TMDB film's OWN year equals the row's — hard equality, the
          // same-title-different-film guard. `/serial/` URLs (TV, never the
          // screened film) and rows without a year to check both abstain.
          def viaFilmwebWikidata: Option[Int] =
            for {
              client   <- wikidata
              url      <- row.filmwebUrl
              if !url.contains("/serial/")
              filmwebId <- WikidataClient.filmwebEntityId(url)
              ids      <- client.findIdsByFilmwebId(filmwebId)
              tmdbId   <- ids.tmdbId
              rowYear  <- effectiveYear
              if tmdb.fullDetails(tmdbId).flatMap(_.releaseYear).contains(rowYear)
            } yield tmdbId
          row.imdbId.flatMap(tmdb.findByImdbId).map(hit => (hit.id, Some(hit), Some(TmdbBasis.ExternalId)))
            .orElse(viaLetterboxd.map((_, Option.empty[TmdbClient.SearchResult], Some(TmdbBasis.ExternalId))))
            .orElse(viaFilmwebWikidata.map((_, Option.empty[TmdbClient.SearchResult], Some(TmdbBasis.ExternalId))))
        } else None
      }
  }

  /** Walk a cinema-reported director's TMDB filmography and pick the entry the
   *  cinema is actually showing. Needed when the title search lands on the wrong
   *  film (different decade, different language, popularity tie-break gone wrong).
   *
   *  Two ways to pick, in order:
   *    1. TITLE match — a credit whose (clean) title equals one of the cinema's
   *       search candidates. Cinemas routinely report a PRODUCTION year that
   *       drifts ±1 from TMDB's first-release year ("Mi Amor": cinema 2025, TMDB
   *       dates Nicloux's film 2026-05-06), so an exact-year walk would miss it.
   *       The title is the unambiguous signal — a director's two same-year films
   *       almost never share a title — so prefer it, and accept a ±1-year window
   *       to absorb the production/release drift without matching an unrelated
   *       decade's remake.
   *    2. EXACT year — for the case where the cinema's title doesn't match TMDB's
   *       spelling but the year pins the film. Requires a year, AND requires that
   *       year to be UNIQUE in the filmography: a director with two same-year
   *       credits (Andrew Stanton's "In the Blink of an Eye" + "Toy Story 5", both
   *       2026) can't be disambiguated by year, so the walk abstains rather than
   *       guess the first — better no ratings than another film's ratings. */
  private def directorWalk(
    director:         Option[String],
    year:             Option[Int],
    candidates:       Seq[String] = Nil,
    cinemaRuntimes:   Seq[Int] = Nil,
    cinemaCast:       Seq[String] = Nil,
    cinemaCandidates: Seq[String] = Nil,
    cinemaTitleWeight: Map[String, Int] = Map.empty
  ): Option[TmdbClient.SearchResult] = {
    director.flatMap { directory =>
      // Try each person the name could mean, in turn — TMDB's top hit is wrong
      // often enough (a credit-less duplicate stub, or an alias another director
      // lists) that trusting it costs the whole resolution now the walk is the
      // only resolver. A person whose filmography doesn't contain the film simply
      // yields nothing here, which is exactly the signal to try the next one.
      tmdb.findPersonCandidates(directory.split(",").head.trim).iterator.zipWithIndex.flatMap { case (personId, candidateIndex) =>
        // Directing credits first — the common case, unchanged. A cinema that
        // printed the WRITER instead ("Drzewo magii" is directed by Ben Gregor and
        // written by Simon Farnaby, and cinemas print either) would otherwise walk
        // an empty filmography and resolve to nothing, so fall back to what this
        // person WROTE. Same response, same round-trip — TMDB returns the whole
        // crew — and every guard below is unchanged, so widening where the film may
        // be FOUND does not widen what counts as a match.
        val directed = tmdb.personDirectorCredits(personId)
        val credits  = if (directed.nonEmpty) directed else tmdb.personWriterCredits(personId)
        def sanitizedPairs(titles: Seq[String]): Seq[(String, String)] =
          titles.iterator.map(t => t -> normalizer.sanitize(t)).filter(_._2.nonEmpty).toSeq
        val wantedPairs       = sanitizedPairs(candidates)
        val wantedCinemaPairs = sanitizedPairs(cinemaCandidates)
        val wanted            = wantedPairs.map(_._2).toSet
        val wantedCinema      = wantedCinemaPairs.map(_._2).toSet
        def titleOf(f: TmdbClient.SearchResult): Set[String] =
          (Seq(f.title) ++ f.originalTitle.toSeq).map(normalizer.sanitize).filter(_.nonEmpty).toSet
        // Fuzzy title match, scoped to THIS director's filmography (a small, trusted
        // set): a cinema's spelling of a foreign title drifts from TMDB's ("Guru" vs
        // Yann Gozlan's "Gourou"), so an exact match misses and the year-only `byYear`
        // below then pins whichever of the director's films sits at the row's
        // (cinema-disagreed, merge-order-dependent) year — "Dalloway" 2025 vs "Gourou"
        // 2026, the SAME-director cross-film flip. A tight edit-distance match
        // (`TitleMatch.close`: ≤2 and ≤1/3 of the longer title) ties "guru"→"gourou"
        // but never "guru"→"dalloway" — and, without the `SequelMarker` guard below,
        // also tied "...mockingjaypart1" to "...mockingjaypart2" (one character
        // apart): a UK cinema's "Mockingjay - Prt 2" typo pulled BOTH films into
        // `eligible`, no tier matched either spelling exactly, and tier 4's
        // lowest-id tie-break handed the row the OLDER film every time
        // (`DirectorWalkResolvesSpec`). Operates on the RAW (pre-sanitize) title
        // pairs because the sequel check needs word boundaries sanitize discards.
        def titleClose(f: TmdbClient.SearchResult, want: Seq[(String, String)] = wantedPairs): Boolean = {
          val fPairs = (Seq(f.title) ++ f.originalTitle.toSeq)
            .map(t => t -> normalizer.sanitize(t)).filter(_._2.nonEmpty)
          fPairs.exists { case (fRaw, fSan) =>
            want.exists { case (wRaw, wSan) =>
              TitleMatch.close(wSan, fSan) &&
                !SequelMarker.differentInstalments(TitleContainment.tokens(wRaw), TitleContainment.tokens(fRaw))
            }
          }
        }
        // Title match first (±1-year-tolerant); fall back to an exact-year match,
        // but ONLY when that year is unambiguous in the filmography. A director
        // with two same-year credits (Andrew Stanton: "In the Blink of an Eye"
        // and "Toy Story 5", both 2026) can't be told apart by year alone — the
        // old `.find` returned whichever came first, binding a Ukrainian-dubbed
        // "Toy Story 5" listing to "In the Blink of an Eye"'s ratings. Refuse.
        // Pick the LOWEST tmdbId among title-matching credits, not the first in
        // filmography order: a director's film duplicated in TMDB under adjacent
        // years + ids (Yann Gozlan's "Gourou" exists as BOTH 1259983/2026 and
        // 1315702/2025 — the SAME film, two entries) both match here, so `.find`
        // returned whichever the credits happened to list first, flipping the
        // resolved id with scrape/merge order. A genuinely-different same-title
        // remake is still kept apart by the ±1-year window; only a true adjacent-year
        // duplicate ties, and lowest-id breaks that tie deterministically
        // (StagingOrderDeterminismSpec).
        // An EXACT title outranks a merely close one. `titleClose` is fuzzy on
        // purpose, and a SEQUEL sits one character from the film it follows
        // ("Diabeł ubiera się u Prady 2" vs "…u Prady", "Piep*zyć Mickiewicza 3"
        // vs "…Mickiewicza", "…Mockingjay Part 1" vs "…Part 2") — `titleClose`'s
        // own `SequelMarker.differentInstalments` guard now refuses those before
        // they can tie, so only a genuine TMDB duplicate of ONE film reaches the
        // lowest-id tie-break below (which is what it exists to collapse).
        val eligible = if (wanted.isEmpty) Seq.empty else credits.filter { f =>
          titleClose(f) && year.forall(y => f.releaseYear.forall(fy => math.abs(fy - y) <= 1))
        }
        // A CINEMA-reported title outranks a title this row's own earlier resolution
        // wrote. The candidate set deliberately includes the derived Tmdb/Imdb/Filmweb
        // slots' `originalTitle` — that is how a film TMDB doesn't index under its
        // Polish title resolves once Filmweb supplies the original — but as one flat
        // set they competed on equal terms and the lowest-id tie-break decided.
        // "Mistyczka" (Jan Sobierajski, 2026) had drifted onto his OTHER 2026 film
        // "Maryja. Matka papieża"; once TMDB listed the real film both credits matched
        // a candidate exactly and 1646379 < 1731866, so the wrong film re-won every
        // cycle (`MovieServiceTmdbHintsSpec`). Four tiers, cinema-exact first, each
        // still tie-broken by lowest id; derived titles resolve only when no cinema
        // title reaches a credit, which is the case they were added for.
        // Within a tier, the credit the MOST venues name wins; lowest id still breaks
        // a genuine tie (the TMDB adjacent-year duplicate this was written for, where
        // both entries carry the same title and so the same weight).
        def venuesNaming(f: TmdbClient.SearchResult): Int =
          titleOf(f).iterator.map(cinemaTitleWeight.getOrElse(_, 0)).maxOption.getOrElse(0)
        def bestOf(tier: Seq[TmdbClient.SearchResult]): Option[TmdbClient.SearchResult] =
          tier.sortBy(f => (-venuesNaming(f), f.id)).headOption
        val byTitle = Seq(
          eligible.filter(f => titleOf(f).exists(wantedCinema.contains)),
          eligible.filter(f => titleClose(f, wantedCinemaPairs)),
          eligible.filter(f => titleOf(f).exists(wanted.contains)),
          eligible
        ).find(_.nonEmpty).flatMap(bestOf)
        // The year-pinned branch below exists for films whose Polish title has no
        // TMDB entry at all: pl-PL credits fall back to the ORIGINAL title, so
        // "Giulietta i duchy" faces "Giulietta degli spiriti" and `titleClose`
        // cannot bridge that. But pinning on the year ALONE asserts nothing about
        // the film, and a single cinema publishing a wrong director or year then
        // resolves confidently to a stranger — "Głos Hind Rajab" became Łukasz
        // Kowalski's "Lombard", "Zawieście czerwone latarnie" a different Zhang
        // Yimou film.
        //
        // So the year picks the candidate and the TITLE still has to corroborate
        // it, just loosely enough to survive translation: the two must share a
        // distinctive word. Titles of the same film keep a proper noun across
        // languages ("Giulietta", "Munch", "Mavka"); unrelated films share nothing.
        // `TitleMatch.sharesDistinctiveToken` owns which words count and how they
        // are folded — Filmweb's director+year override answers the same question.
        def corroboratedByTitle(f: TmdbClient.SearchResult): Boolean =
          TitleMatch.sharesDistinctiveToken(
            candidates, Seq(f.title) ++ f.originalTitle.toSeq, normalizer.sanitize)

        // `corroboratedByTitle` shares a proper noun across languages, but two
        // instalments of the same series share that noun with EACH OTHER too
        // ("Mockingjay Part 1" / "Part 2" both corroborate on "mockingjay") — so
        // when a row's (merge-order-dependent) `year` happens to land on the
        // WRONG instalment's release year, this tier bound it there anyway,
        // mirroring the `titleClose` collision `SequelMarker` already guards
        // (`DirectorWalkResolvesSpec`). Same veto, applied to the raw candidate
        // titles against the year-pinned credit's own titles.
        def isDifferentInstalment(f: TmdbClient.SearchResult): Boolean = {
          val fRaws = Seq(f.title) ++ f.originalTitle.toSeq
          candidates.exists(wRaw => fRaws.exists(fRaw =>
            SequelMarker.differentInstalments(TitleContainment.tokens(wRaw), TitleContainment.tokens(fRaw))))
        }

        // When the title is FULLY translated it keeps nothing to share — "Trener
        // Tenisa" against "Il Maestro", "Kochanie" against "Gioia mia" — and the
        // year-pinned branch is the only way those resolve. Runtime and cast are
        // evidence the title cannot give and translation cannot touch, and both
        // arrive in the ONE `fullDetails` call, fetched only when the cheap title
        // check has already failed.
        //
        // Measured over the corpus they separate the cases cleanly: the genuine
        // translations agree on runtime to within a minute or two (125/125, 98/97,
        // 93/95) while the bogus matches are nowhere near (89 against Lombard's 78,
        // 142 against Codename Cougar's 76). Cast is the stronger signal where a
        // cinema publishes one — an actor's name is a proper noun in every
        // language — though the cinemas behind these particular rows publish none.
        // ANY reported runtime agreeing is enough, rather than a single chosen one:
        // cinemas disagree by a minute or two, and picking "the" runtime would make
        // the answer depend on which cinema had arrived (`StagingOrderDeterminismSpec`).
        def corroboratedByFacts(f: TmdbClient.SearchResult): Boolean =
          (cinemaRuntimes.nonEmpty || cinemaCast.nonEmpty) && {
            val full = tmdb.fullDetails(f.id)
            val runtimeAgrees = full.flatMap(_.runtimeMinutes).exists(actual =>
              cinemaRuntimes.exists(reported => math.abs(actual - reported) <= TmdbCandidateSearch.RuntimeAgreementMinutes))
            val castAgrees = full.exists { d =>
              val theirs = d.cast.map(normalizer.sanitize).filter(_.nonEmpty).toSet
              cinemaCast.map(normalizer.sanitize).exists(n => n.nonEmpty && theirs.contains(n))
            }
            runtimeAgrees || castAgrees
          }
        val byYear = year.flatMap(y => credits.filter(_.releaseYear.contains(y)) match {
          case Seq(only) if !isDifferentInstalment(only) && (corroboratedByTitle(only) || corroboratedByFacts(only)) =>
            // Collapse a TMDB adjacent-year DUPLICATE of one film: if the year-pinned
            // credit shares its title with a credit ±1 year off (the same film entered
            // twice — "Gourou" as both 2025/1315702 and 2026/1259983), they're ONE
            // film; pick the lowest id so the merge-order-dependent KEY year can't pin
            // whichever duplicate sits at it (StagingOrderDeterminismSpec).
            Some(credits.filter(f => titleOf(f) == titleOf(only) &&
              f.releaseYear.exists(fy => math.abs(fy - y) <= 1)).minBy(_.id))
          case _         => None         // 0 or >1 at the exact year, or no title corroboration → don't guess
        })
        byTitle.orElse(byYear).map { film =>
          logger.info(s"Director-walk: '$directory' (person $personId) year=${year.getOrElse("?")} → tmdbId=${film.id} '${film.originalTitle.getOrElse(film.title)}'")
          film
        }
      }.nextOption()
    }
  }

  /** Collapse a TMDB adjacent-year DUPLICATE of ONE film to its lowest id. TMDB
   *  occasionally lists a single film under two ids a year apart (Yann Gozlan's
   *  "Gourou" as 1315702/2025 AND 1259983/2026 — same title, same director). When
   *  the row reports the director, those entries are provably one film (shared
   *  director + a ±1-year shared title), so the resolved id is pinned to the lowest
   *  — independent of which duplicate the row's (merge-order-dependent) key year or
   *  a popularity tie-break happened to land on. A genuinely-different same-title
   *  remake by the same director is kept apart by needing the SAME title AND ±1
   *  year (a remake is years apart); this only fuses true duplicates. Reuses the
   *  director credits `directorWalk` already fetched (cached), so no extra calls.
   *  Order-independent — see `StagingOrderDeterminismSpec`. */
  private def collapseDirectorDuplicate(id: Int, directors: Seq[String]): Int = {
    val credits = directors.iterator
      .flatMap(d => tmdb.findPerson(d.split(",").head.trim).iterator.flatMap(tmdb.personDirectorCredits))
      .toSeq.distinctBy(_.id)
    def titles(f: TmdbClient.SearchResult): Set[String] =
      (Seq(f.title) ++ f.originalTitle.toSeq).map(normalizer.sanitize).filter(_.nonEmpty).toSet
    credits.find(_.id == id).fold(id) { resolved =>
      // `minOption.getOrElse(id)`, not `.min`: the adjacency filter is EMPTY
      // whenever the resolved credit carries no TMDB release year (its own
      // `resolved.releaseYear.exists(…)` is false, so nothing — not even itself —
      // matches), and a bare `.min` on that empty Seq threw
      // `UnsupportedOperationException: empty.min`, which the resolve mistook for a
      // transient failure and retried forever. No year ⇒ no provable duplicate ⇒
      // keep the resolved id.
      credits.filter(f => titles(f) == titles(resolved) &&
        f.releaseYear.exists(ry => resolved.releaseYear.exists(ay => math.abs(ry - ay) <= 1)))
        .map(_.id).minOption.getOrElse(id)
    }
  }

}

object TmdbCandidateSearch {
  /** How far a cinema's published runtime may sit from TMDB's and still count as
   *  the same film in the walk's year-pinned branch. Two minutes is what the corpus
   *  shows genuine pairs differing by (rounding, and whether the credits roll is
   *  counted). */
  val RuntimeAgreementMinutes: Int = 2
}
