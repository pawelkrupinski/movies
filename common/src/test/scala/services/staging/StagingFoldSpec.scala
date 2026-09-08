package services.staging

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Cinema, CinemaCityWroclavia, Helios, HeliosMagnolia, KinoMuza, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, CaffeineMovieCache, EnrichmentRetrigger, FilmId, MovieRepository, RetriggerKind, StoredMovieRecord, StoredRowsRepository}

class StagingFoldSpec extends AnyFlatSpec with Matchers {

  /** A per-cinema staging row resolved to `tmdbId`, with the cinema-reported year
   *  on its own slot and `tmdbYear` on the Tmdb slot (they can differ). */
  private def staging(cinema: Source, title: String, cinemaYear: Int, tmdbId: Int, tmdbYear: Int): StagingRecord =
    StagingRecord(cinema, title, Some(cinemaYear), MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), releaseYear = Some(cinemaYear)),
        Tmdb   -> SourceData(title = Some(title), releaseYear = Some(tmdbYear)))), titleNormalizer)

  private def repoOf(rows: StoredMovieRecord*): MovieRepository = new StoredRowsRepository(rows.toSeq, titleNormalizer)

  /** THE invariant: the staging fold runs the SAME `groupByFilm`/`clusterByFilm`/
   *  `canonical` collapse the periodic settle (`canonicalizeBySanitize`) runs — just
   *  scoped to the fold's neighbourhood. So the folded `movies` state is ALREADY the
   *  settled steady state: load it exactly as a boot would (`fromStorage` round-trip)
   *  and a full settle must change NOTHING — no re-key, no enrichment re-kick. If
   *  this fails, the fold and the settle have drifted apart. */
  private def settleIsANoOpAfterFold(plan: StagingFold.Plan): Unit = {
    val retriggered = scala.collection.mutable.ListBuffer.empty[Set[RetriggerKind]]
    val rows = plan.moviesUpserts.map { case (id, k, rec) =>
      StoredMovieRecord.fromStorage(id.value, Some(StoredMovieRecord.keyFor(k)), rec, titleNormalizer)
    }
    val cache = new CaffeineMovieCache(repoOf(rows*), retrigger = new EnrichmentRetrigger {
      def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = { retriggered += kinds; () }
    }, normalizer = titleNormalizer)
    val before = cache.snapshot().map(r => (r.title, r.year)).toSet
    cache.canonicalizeBySanitize()
    withClue(s"a settle RE-KEYED the folded state — fold ≠ settle:\n  before=$before\n  after=${cache.snapshot().map(r => (r.title, r.year)).toSet}\n")(
      cache.snapshot().map(r => (r.title, r.year)).toSet shouldBe before)
    withClue(s"a settle re-kicked enrichment after the fold: ${retriggered.toList}\n")(
      retriggered shouldBe empty)
  }

  "planGroup" should "merge same-film staging rows from several cinemas into one movies row" in {
    val rows = Seq(staging(Helios, "Kumotry", 2026, 1454157, 2026), staging(Multikino, "Kumotry", 2026, 1454157, 2026))
    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)
    record.tmdbId shouldBe Some(1454157)
    record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.stagingDeletes should have size 2
    plan.moviesDeletes shouldBe empty
  }

  it should "merge a cross-title same-tmdbId sibling at fold time (the Mandalorian PL/EN duplicate)" in {
    // The English "The Mandalorian and Grogu" newcomer folds while the Polish
    // "Gwiezdne wojny: Mandalorian i Grogu" is ALREADY a movies row, same tmdbId.
    // The folder pulls that sibling in (reconcileTmdbIds), and groupByFilm's
    // bare-title tmdbId edge must collapse the two languages onto ONE row HERE —
    // not leave a duplicate for the periodic settle. The two keys sanitise
    // differently, so only the tmdbId edge can merge them.
    val englishNewcomer = staging(Multikino, "The Mandalorian and Grogu", 2026, 700, 2026)
    val polishSibling = StoredMovieRecord("Gwiezdne wojny: Mandalorian i Grogu", Some(2026),
      MovieRecord(tmdbId = Some(700), data = Map[Source, SourceData](
        (Tmdb: Source)   -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"),
                                       englishTitle = Some("The Mandalorian and Grogu"), releaseYear = Some(2026)),
        (Helios: Source) -> SourceData(title = Some("Gwiezdne wojny: Mandalorian i Grogu"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(englishNewcomer), moviesRows = Seq(polishSibling), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    withClue(s"cross-title rows did not collapse: ${plan.moviesUpserts.map(_._1)}\n")(
      plan.moviesUpserts should have size 1)
    plan.moviesUpserts.head._3.tmdbId shouldBe Some(700)
    plan.moviesUpserts.head._3.data.keySet shouldBe Set(Helios, Multikino, Tmdb) // both languages' cinemas
  }

  it should "collapse ±1-year variants into ONE row re-keyed to the TMDB year (the absorbed settle)" in {
    // Cinema City reports 'Zawodowcy' at the production year 2025, everyone else at
    // the release year 2026 — both resolved to the same tmdbId (tmdbYear 2026). The
    // OLD per-year fold left these as two `movies` rows for a later settle pass;
    // the group-scoped fold must now collapse them HERE into one row keyed to the
    // TMDB year 2026, carrying both cinemas.
    val cc2025  = staging(Multikino, "Zawodowcy", 2025, 1122573, 2026)
    val rest26  = staging(Helios,    "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(cc2025, rest26), Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)                            // re-keyed to the TMDB year
    record.data.keySet shouldBe Set(Multikino, Helios, Tmdb) // no cinema dropped
  }

  it should "re-key an existing movies row to the TMDB year and retire its old key" in {
    // A previously-folded `zawodowcy|2025` movies row (resolved, tmdbYear 2026)
    // plus a fresh 2026 staging row: the settle re-keys onto 2026. The film keeps its
    // id — the plan RETITLES the existing document rather than writing a new one and
    // deleting the old (the group-scoped movies lookup sees it, so nothing is
    // silently overwritten — the bug the old per-year fold guarded against).
    val existing2025 = StoredMovieRecord("Zawodowcy", Some(2025), MovieRecord(
      tmdbId = Some(1122573),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2025)),
        Tmdb      -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2026)))))
    val fresh2026 = staging(Helios, "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(fresh2026), Seq(existing2025), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._2.year shouldBe Some(2026)
    plan.moviesUpserts.head._3.data.keySet shouldBe Set(Multikino, Helios, Tmdb)
    plan.moviesUpserts.head._1 shouldBe existing2025.id
    plan.moviesDeletes shouldBe empty
    plan.retirements shouldBe empty
  }

  // WHY a year change must never be treated as "this film is leaving".
  //
  // A re-key is a RENAME, and the film keeps showing. Until film ids, the plan expressed
  // it as an upsert at the new key plus a DELETE of the old one, and reading that delete
  // as a removal — cascading a cleanup off it, deleting the retired id's `screenings` /
  // `movie_slots` rows — destroyed showtimes that were still the film's only copy. That
  // shipped on 2026-07-27 and took prod PL from 39,413 upcoming showtimes to 18,161 and UK
  // from 22,250 to 7,226 inside twenty minutes (@8033e39c6, reverted @926027438). Now the
  // shape cannot be misread: the row keeps its id, only its key moves, and there is no
  // delete to cascade off.
  it should "re-key a film as a RETITLE — same id, new key, nothing retired" in {
    val existing2025 = StoredMovieRecord("Zawodowcy", Some(2025), MovieRecord(
      tmdbId = Some(1122573),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2025)))))
    val fresh2026 = staging(Helios, "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(fresh2026), Seq(existing2025), titleNormalizer)

    plan.moviesDeletes shouldBe empty
    plan.retirements   shouldBe empty
    plan.moviesUpserts should have size 1
    val (id, winnerKey, winner) = plan.moviesUpserts.head
    id                        shouldBe existing2025.id
    winnerKey.year            shouldBe Some(2026)
    winner.data.keySet        should contain (Multikino)
    // And it is NOT a promotion: no brand-new film appeared.
    plan.newPromotions        shouldBe empty
  }

  it should "retire a second document of the same film INTO the survivor, side rows attributed" in {
    // Two existing rows that turn out to be one film (a legacy duplicate at another year):
    // one id survives, the other is deleted, and the retirement says where its cinemas go.
    val a = StoredMovieRecord("Zawodowcy", Some(2025), MovieRecord(tmdbId = Some(1122573),
      data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2025)))))
    val b = StoredMovieRecord("Zawodowcy", Some(2026), MovieRecord(tmdbId = Some(1122573),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2026)),
                                     Tmdb   -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2026)))))
    val fresh = staging(CinemaCityWroclavia, "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(fresh), Seq(a, b), titleNormalizer)

    plan.moviesUpserts.map(_._1) shouldBe Seq(b.id)
    plan.moviesDeletes           shouldBe Seq(a.id)
    plan.retirements             shouldBe Seq(a.id -> b.id)
    plan.moviesUpserts.head._3.data.keySet shouldBe Set(Multikino, Helios, CinemaCityWroclavia, Tmdb)
  }

  it should "fold a yearless+idless staging stray onto a resolved movies sibling (Dzień objawienia)" in {
    // The stranded-duplicate shape `canonicalizeBySanitize` exists to fix, now
    // healed by the fold: a yearless, unresolved staging row beside an existing
    // resolved yeared movies row collapses onto the resolved row.
    val stray = StagingRecord(Multikino, "Dzień objawienia", None,
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dzień objawienia")))), titleNormalizer)
    val resolvedSibling = StoredMovieRecord("Dzień objawienia", Some(2026), MovieRecord(
      tmdbId = Some(1275779), imdbId = Some("tt15047880"),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stray), Seq(resolvedSibling), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)                     // year-bearing resolved key wins
    record.tmdbId shouldBe Some(1275779)             // enrichment preserved
    record.data.keySet shouldBe Set(Helios, Multikino)
    plan.moviesDeletes shouldBe empty                // resolved row kept its key
  }

  it should "fold MANY yearless+idless per-cinema rows of one event into a single all-cinema row (no clobber)" in {
    // A festival/event film (e.g. 'Maraton Horrorów') that ~50 cinemas report
    // YEARLESS and UNRESOLVED. Each per-cinema row shares the same (sanitize, None)
    // variant key. The fold MUST union them into ONE movies row carrying every
    // cinema — the regression this guards: feeding the per-cinema rows straight
    // through `clusterByFilm` turned each into its OWN singleton cluster (rule 4),
    // collapsing them all onto the same (sanitize, None) key and dropping every
    // cinema but one. `planGroup` unions per-key FIRST, restoring the invariant.
    val cinemas = Cinema.all.take(8)
    val rows = cinemas.map(c => StagingRecord(c, "Maraton Horrorów", None, MovieRecord(
      data = Map[Source, SourceData](c -> SourceData(title = Some("Maraton Horrorów")))), titleNormalizer))

    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, key, record) = plan.moviesUpserts.head
    key.year shouldBe None
    record.data.keySet shouldBe cinemas.toSet               // every cinema survives
    plan.stagingDeletes should have size cinemas.size
  }

  it should "fold a repertory revival's rebroadcast year onto the same film, not split it out" in {
    // THE QUEEN BUDAPEST REGRESSION (2026-09-08). A Cinema City venue advertised its
    // 2026 anniversary screening of the 2012 concert film as "Queen Budapest (2026)"
    // — the CINEMA dating the EVENT, not the film. A now-reverted rule
    // (`separateByAssertedYear`) read that bracketed number as a release-year
    // assertion, disagreed with the other 20 venues' plain "Queen Budapest" (which
    // resolved to tmdbId 142773, TMDB year 2012) by 14 years, and split it into its
    // own staging group — which never resolved (`noMatch`) and lost all 21 venues'
    // showtimes. Real strings from the incident's CI log, run at the fast unit layer
    // this regression should have been caught at instead of an hour-long corpus leg.
    // The 20 plain-titled venues already folded into this movies row, resolved and
    // keyed under TMDB's own year — 2012, YEARLESS on the venues' own slots (they
    // never printed one). The 21st venue's title embeds 2026 and nothing else asserts
    // a year for it, so it is the one row whose asserted year could disagree.
    val existing = StoredMovieRecord("Queen Budapest", Some(2012), MovieRecord(
      tmdbId = Some(142773),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Queen Budapest")),
        Tmdb      -> SourceData(title = Some("Queen Budapest"), releaseYear = Some(2012)))))
    val dated = StagingRecord(Helios, "Queen Budapest (2026)", None, MovieRecord(
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Queen Budapest (2026)")))), titleNormalizer)

    val plan = StagingFold.planGroup(Seq(dated), Seq(existing), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, _, record) = plan.moviesUpserts.head
    record.tmdbId shouldBe Some(142773)
    record.data.keySet shouldBe Set(Multikino, Helios, Tmdb) // no venue lost to a phantom second film
    plan.moviesUpserts.head._1 shouldBe existing.id
    plan.moviesDeletes shouldBe empty
  }

  it should "keep distinct-tmdbId remakes at different years as two movies rows" in {
    // 'Diuna' 1984 (Lynch) vs 2021 (Villeneuve) — distinct tmdbIds, years far
    // apart, so `clusterByFilm` keeps them as two clusters → two `movies` rows.
    // (Two SAME-year distinct-tmdbId rows share one `movies` _id `diuna|YYYY` and
    // can't coexist there at all, so the fold legitimately collapses those — the
    // cache's one-row-per-key invariant, which `planGroup` mirrors.)
    val rows = Seq(staging(Helios, "Diuna", 1984, 841, 1984), staging(Multikino, "Diuna", 2021, 438631, 2021))
    val plan = StagingFold.planGroup(rows, Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)
    plan.moviesUpserts.map(u => (u._2.year, u._3.tmdbId)).toSet shouldBe
      Set((Some(1984), Some(841)), (Some(2021), Some(438631)))
    plan.stagingDeletes should have size 2
  }

  it should "fold staging onto an existing movies sibling without deleting it spuriously" in {
    val stagingRow = staging(Multikino, "Kumotry", 2026, 1454157, 2026)
    val existing = StoredMovieRecord("Kumotry", Some(2026), MovieRecord(
      tmdbId = Some(1454157),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stagingRow), Seq(existing), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._3.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.moviesDeletes shouldBe empty                  // same canonical key → no delete
  }

  it should "list a brand-new film (no pre-existing movies row) in newPromotions" in {
    // Nothing in `movies` for this sanitize group → the folded row is a genuine
    // promotion, so the folder can schedule its first-time ratings.
    val rows = Seq(staging(Helios, "Kumotry", 2026, 1454157, 2026), staging(Multikino, "Kumotry", 2026, 1454157, 2026))
    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.newPromotions shouldBe plan.folded
    plan.newPromotions.map(_._1) shouldBe Seq(CacheKey("Kumotry", Some(2026), titleNormalizer))
  }

  it should "NOT list a film that merely merges into an existing movies row in newPromotions" in {
    // An existing `movies` row joins the cluster → not a new film; it already owns
    // its ratings, so it must not be re-enqueued as a promotion.
    val stagingRow = staging(Multikino, "Kumotry", 2026, 1454157, 2026)
    val existing = StoredMovieRecord("Kumotry", Some(2026), MovieRecord(
      tmdbId = Some(1454157),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stagingRow), Seq(existing), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1                // the merged row IS written
    plan.newPromotions shouldBe empty                    // but it is not a fresh promotion
  }

  it should "list only the brand-new remake when one variant merges and another is new" in {
    // 'Diuna' 1984 already lives in `movies`; the 2021 remake arrives via staging.
    // Distinct tmdbIds, far-apart years → two clusters: 1984 merges (not new), 2021
    // is brand new → only 2021 is a promotion.
    val existing1984 = StoredMovieRecord("Diuna", Some(1984), MovieRecord(
      tmdbId = Some(841),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Diuna"), releaseYear = Some(1984)))))
    val staging1984 = staging(Multikino, "Diuna", 1984, 841, 1984)
    val staging2021 = staging(Multikino, "Diuna", 2021, 438631, 2021)

    val plan = StagingFold.planGroup(Seq(staging1984, staging2021), Seq(existing1984), titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.newPromotions.map(u => (u._1.year, u._2.tmdbId)) shouldBe Seq((Some(2021), Some(438631)))
  }

  // --- "stuck in staging" prod scenarios (2026-06-19) -------------------------
  // Four decorated/foreign titles lingered in pending_movies and folded into
  // `movies` UN-ENRICHED (tmdbId / tmdbNoMatch / imdbId all null) despite their
  // staging rows carrying resolution. These pin whether `planGroup` itself
  // discards the staging row's conclusion.

  it should "preserve tmdbAttempt=Some(services.resolution.TmdbAttempt.Legacy) through a fold (decorated title TMDB couldn't match)" in {
    // "Kino bez barier: Ministranci (AD + CC + PJM)" / "Robin Hood: Koniec
    // legendy/Kino Cafe": the decorated title sanitizes to its own anchor, TMDB
    // returns no match → the staging row is concluded with tmdbAttempt=Some(services.resolution.TmdbAttempt.Legacy). The
    // folded `movies` row MUST stay concluded, else the reaper re-stages it forever.
    val concluded = StagingRecord(Helios, "Kino bez barier: Ministranci (AD + CC + PJM)", None,
      MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy),
        data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kino bez barier: Ministranci (AD + CC + PJM)")))), titleNormalizer)

    val plan = StagingFold.planGroup(Seq(concluded), moviesRows = Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._3.tmdbNoMatch shouldBe true
    plan.stagingDeletes should have size 1
  }

  it should "keep a resolved cinema's tmdbId/imdbId when an UNRESOLVED same-key sibling folds with it" in {
    // "Denʹ istyny - UA" at two Helios venues, same (anchor, 2026) key: Blue City
    // resolved (tmdbId+imdbId), Posnania still blank. The union MUST take the
    // resolved row as base so the folded `movies` row carries the id — not blank it.
    val resolved = StagingRecord(Helios, "Denʹ istyny - UA", Some(2026),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"),
        data = Map[Source, SourceData](
          Helios -> SourceData(title = Some("Denʹ istyny - UA"), releaseYear = Some(2026)),
          Tmdb   -> SourceData(title = Some("Denʹ istyny - UA"), releaseYear = Some(2026)))), titleNormalizer)
    val blank = StagingRecord(Multikino, "Denʹ istyny - UA", Some(2026),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Denʹ istyny - UA"), releaseYear = Some(2026)))), titleNormalizer)

    val plan = StagingFold.planGroup(Seq(resolved, blank), moviesRows = Seq.empty, titleNormalizer)
    settleIsANoOpAfterFold(plan)

    plan.moviesUpserts should have size 1
    val (_, _, record) = plan.moviesUpserts.head
    record.tmdbId shouldBe Some(1275779)
    record.imdbId shouldBe Some("tt15047880")
  }

  // THE 'LALKA' REGRESSION, ROUND 1 (poland/convergence CI run 34244749223, commit
  // bbc45b52, 2026-09-08). Real tmdbIds/imdbIds from that run's log: two Polish
  // cinemas each report the bare title "Lalka" (one prints its own 2026 cinema year,
  // the other its own 2025), and TMDB's search — genuinely ambiguous for a common
  // one-word title — resolves them to two DIFFERENT, unrelated films that both
  // happen to carry TMDB year 2026: 'Lalka' (tmdbId 1321666, a 2026 Maciej Kawalski
  // film) and an unrelated 'Lalka' (tmdbId 1309396). `clusterByFilm` correctly keeps
  // them as two clusters — different tmdbId, different imdbId, nothing says they're
  // one film — but each cluster's OWN `canonical()` vote, run in isolation,
  // concludes the IDENTICAL display title ("Lalka", its only cinema vote) at the
  // IDENTICAL TMDB year (2026), so both plan an upsert at key 'lalka|2026'.
  //
  // The fix does NOT merge them — that would attribute one film's cinemas to the
  // other's title/poster/synopsis/tmdbId, corrupting BOTH (verified against
  // `ScrapeLanding.concludedKeyFor`/`chooseConcluded` in `ScrapeLandingSpec`: BOTH
  // films must stay independently reachable and disambiguable for every future
  // listing). Instead it keeps the LOWER-tmdbId cluster's own, unmodified data at
  // the plain key, and promotes the other under a key suffixed with ITS OWN tmdbId —
  // deterministically, regardless of which cinema's row happened to be read first.
  it should "give two DIFFERENT films that conclude the same (title, year) key their own distinct keys, never merging or double-writing" in {
    def staged(cinema: Source, cinemaYear: Int, tmdbId: Int, tmdbYear: Int, imdbId: String): StagingRecord =
      StagingRecord(cinema, "Lalka", Some(cinemaYear), MovieRecord(
        tmdbId = Some(tmdbId), imdbId = Some(imdbId),
        data = Map[Source, SourceData](
          cinema -> SourceData(title = Some("Lalka"), releaseYear = Some(cinemaYear)),
          Tmdb   -> SourceData(title = Some("Lalka"), releaseYear = Some(tmdbYear)))), titleNormalizer)
    // Neither cinema's OWN reported year is 2026 (both print an off-by-a-bit
    // production year, not TMDB's release year) — deliberately, so that whichever
    // side loses never coincidentally shares its RAW (title, year) key with the
    // concluded key the WINNER is later stored under; that would union the two
    // records at the pre-cluster staging/movies boundary step before this fix
    // ever got a say, a separate (and separately fixable) gap this test isn't
    // about.
    val kawalski = staged(CinemaCityWroclavia, cinemaYear = 2024, tmdbId = 1321666, tmdbYear = 2026, imdbId = "tt37082105")
    val other    = staged(Helios,              cinemaYear = 2025, tmdbId = 1309396, tmdbYear = 2026, imdbId = "tt36749000")

    // Run BOTH arrival orders — the fix's whole point is that the outcome does not
    // depend on which staging row a fold happens to read first.
    for (rows <- Seq(Seq(kawalski, other), Seq(other, kawalski))) withClue(s"rows=$rows\n") {
      val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty, titleNormalizer)
      settleIsANoOpAfterFold(plan)

      // BOTH films are promoted, each with its OWN correct, unmodified identity and
      // ONLY its own cinema's screenings — never both cinemas' data on one record.
      plan.moviesUpserts should have size 2
      val byTmdbId = plan.moviesUpserts.map { case (id, k, r) => r.tmdbId.get -> (id, k, r) }.toMap
      byTmdbId.keySet shouldBe Set(1321666, 1309396)

      // The LOWER tmdbId always keeps the plain key — deterministic, content-based,
      // independent of arrival order.
      val (_, kawalskiKey, kawalskiRecord) = byTmdbId(1321666)
      val (_, otherKey, otherRecord)       = byTmdbId(1309396)
      otherKey    shouldBe CacheKey("Lalka", Some(2026), titleNormalizer)
      kawalskiKey should not be otherKey
      kawalskiKey.cleanTitle shouldBe "Lalka"
      kawalskiKey.year       shouldBe Some(2026)
      kawalskiRecord.imdbId shouldBe Some("tt37082105")
      kawalskiRecord.data.keySet shouldBe Set(CinemaCityWroclavia, Tmdb) // NOT Helios too
      otherRecord.imdbId shouldBe Some("tt36749000")
      otherRecord.data.keySet shouldBe Set(Helios, Tmdb) // NOT CinemaCityWroclavia too

      // Every id is distinct — promoting the loser under a suffixed key must NOT
      // reuse the winner's freshly-minted id (both were minted from the SAME
      // contested plain key before the collision was resolved).
      plan.moviesUpserts.map(_._1).distinct should have size 2

      // Nothing was retired or deleted, and nothing was deferred — both sides
      // carried a tmdbId, so both could be disambiguated and promoted outright.
      plan.moviesDeletes shouldBe empty
      plan.retirements   shouldBe empty
      plan.deferred      shouldBe empty

      // The disambiguation is reported, naming both sides and the suffixed key.
      plan.disambiguated should have size 1
      val d = plan.disambiguated.head
      d.key             shouldBe otherKey
      d.disambiguatedKey shouldBe kawalskiKey
      d.keptTmdbId             shouldBe Some(1309396)
      d.disambiguatedTmdbId    shouldBe Some(1321666)
      StagingFold.disambiguatedCollisionWarning(d) should include ("1321666")
      StagingFold.disambiguatedCollisionWarning(d) should include ("1309396")

      // Both staging rows are consumed — neither promotion was held back.
      plan.stagingDeletes should have size 2
    }
  }

  // THE 'LALKA' REGRESSION, ROUND 2 (the actual reported bug: poland/convergence run
  // 34271323339). By the time BOTH films already have their OWN pre-existing `movies`
  // row — each promoted earlier via one of its own uniquely-decorated titles that
  // never collided ('Kino kobiet: Lalka' vs 'Pora dla Seniora: Lalka') — a NEW bare
  // "Lalka" listing of either makes BOTH clusters conclude the shared plain key again.
  // The old tie-break, `(isNewFilm, FilmId.value)`, had nothing left to break the tie
  // WITH once neither side is `isNewFilm` — `FilmId` is minted from the key a row was
  // FIRST created under, i.e. from ARRIVAL ORDER — so which film "won" the shared key
  // flipped four times in one CI boot as more decorated siblings landed. The new
  // tie-break is the tmdbId itself: the same regardless of which existing row's
  // `FilmId` happens to be lexicographically lower.
  it should "keep the SAME winner between two ALREADY-PROMOTED films colliding on a new bare listing, regardless of which existing FilmId sorts lower" in {
    def existingRow(id: String, cinema: Source, decoratedTitle: String, tmdbId: Int, imdbId: String): StoredMovieRecord = {
      val record = MovieRecord(tmdbId = Some(tmdbId), imdbId = Some(imdbId),
        data = Map[Source, SourceData](
          cinema -> SourceData(title = Some(decoratedTitle), releaseYear = Some(2026)),
          Tmdb   -> SourceData(title = Some("Lalka"), releaseYear = Some(2026))))
      StoredMovieRecord.fromStorage(id,
        Some(StoredMovieRecord.keyFor(CacheKey(decoratedTitle, Some(2026), titleNormalizer))), record, titleNormalizer)
    }
    // `cinemaYear` deliberately differs by film (as round 1's `staged` does): every
    // new listing reports the bare title, but `stagingByKey` groups staging rows by
    // their RAW `(title, year)` BEFORE any cluster analysis runs, so listings that
    // also shared a raw year would pre-merge into one blob here and never reach
    // `clusterByFilm` as two distinct tmdbIds at all — a same-cinema-year bare
    // listing of two different films is already merged upstream of this whole
    // mechanism, a separate (and separately fixable) gap this test isn't about.
    def bareListing(cinema: Source, cinemaYear: Int, tmdbId: Int, imdbId: String): StagingRecord =
      StagingRecord(cinema, "Lalka", Some(cinemaYear), MovieRecord(
        tmdbId = Some(tmdbId), imdbId = Some(imdbId),
        data = Map[Source, SourceData](
          cinema -> SourceData(title = Some("Lalka"), releaseYear = Some(cinemaYear)),
          Tmdb   -> SourceData(title = Some("Lalka"), releaseYear = Some(2026)))), titleNormalizer)

    // Run with the Kawalski film's existing id BOTH lower and higher than the other
    // film's — the shape that actually flipped in CI — and assert the SAME film wins
    // the plain key either way.
    for ((kawalskiId, otherId) <- Seq("f00aaaaaaaaaaaa" -> "f00bbbbbbbbbbbb", "f00zzzzzzzzzzzz" -> "f00111111111111"))
      withClue(s"kawalskiId=$kawalskiId otherId=$otherId\n") {
        val kawalskiExisting = existingRow(kawalskiId, CinemaCityWroclavia, "Kino kobiet: Lalka",      1321666, "tt37082105")
        val otherExisting    = existingRow(otherId,    Multikino,           "Pora dla Seniora: Lalka", 1309396, "tt36749000")
        // TWO new bare listings for Kawalski's film (outvoting its one decorated
        // slot) but only ONE for the other (a 1-1 tie, which the dominant-title vote
        // breaks alphabetically onto "lalka" anyway) — so BOTH clusters' `canonical()`
        // converge on the bare title THIS round, reproducing the shared-key collision.
        val newListings = Seq(
          bareListing(Helios,         cinemaYear = 2024, 1321666, "tt37082105"),
          bareListing(KinoMuza,       cinemaYear = 2024, 1321666, "tt37082105"),
          bareListing(HeliosMagnolia, cinemaYear = 2025, 1309396, "tt36749000"))

        val plan = StagingFold.planGroup(newListings, moviesRows = Seq(kawalskiExisting, otherExisting), titleNormalizer)
        settleIsANoOpAfterFold(plan)

        plan.moviesUpserts should have size 2
        val byId = plan.moviesUpserts.map { case (id, k, r) => id -> (k, r) }.toMap
        // Both films keep their OWN id — a retitle, never a new document.
        byId.keySet shouldBe Set(FilmId(kawalskiId), FilmId(otherId))

        val (otherKey, otherRecord)       = byId(FilmId(otherId))
        val (kawalskiKey, kawalskiRecord) = byId(FilmId(kawalskiId))
        // The lower tmdbId (1309396, "the other film") always keeps the plain key.
        otherRecord.tmdbId    shouldBe Some(1309396)
        kawalskiRecord.tmdbId shouldBe Some(1321666)
        otherKey    shouldBe CacheKey("Lalka", Some(2026), titleNormalizer)
        kawalskiKey should not be otherKey

        plan.deferred      shouldBe empty
        plan.disambiguated should have size 1
        plan.disambiguated.head.keptTmdbId          shouldBe Some(1309396)
        plan.disambiguated.head.disambiguatedTmdbId shouldBe Some(1321666)

        // Idempotent: re-planning from exactly this post-fold state (no new staging
        // rows at all) makes the SAME decision again — each cluster's OWN merged
        // votes already favour the bare title on their own now, so the collision
        // re-detects and re-resolves to the SAME suffixed key, not a fresh
        // (arrival-order-dependent) one.
        val kawalskiAfter = StoredMovieRecord.fromStorage(kawalskiId, Some(StoredMovieRecord.keyFor(kawalskiKey)), kawalskiRecord, titleNormalizer)
        val otherAfter    = StoredMovieRecord.fromStorage(otherId,    Some(StoredMovieRecord.keyFor(otherKey)),    otherRecord,    titleNormalizer)
        val secondPlan    = StagingFold.planGroup(Seq.empty, moviesRows = Seq(kawalskiAfter, otherAfter), titleNormalizer)
        secondPlan.moviesUpserts.map(_._1).toSet shouldBe Set(FilmId(kawalskiId), FilmId(otherId))
        val secondById = secondPlan.moviesUpserts.map { case (id, k, _) => id -> k }.toMap
        secondById(FilmId(otherId))    shouldBe otherKey
        secondById(FilmId(kawalskiId)) shouldBe kawalskiKey
        secondPlan.moviesDeletes shouldBe empty
      }
  }
}
