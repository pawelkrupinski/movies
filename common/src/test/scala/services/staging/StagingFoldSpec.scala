package services.staging

import models.{Cinema, Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, StoredMovieRecord}

class StagingFoldSpec extends AnyFlatSpec with Matchers {

  /** A per-cinema staging row resolved to `tmdbId`, with the cinema-reported year
   *  on its own slot and `tmdbYear` on the Tmdb slot (they can differ). */
  private def staging(cinema: Source, title: String, cinemaYear: Int, tmdbId: Int, tmdbYear: Int): StagingRecord =
    StagingRecord(cinema, title, Some(cinemaYear), MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), releaseYear = Some(cinemaYear)),
        Tmdb   -> SourceData(title = Some(title), releaseYear = Some(tmdbYear)))))

  "planGroup" should "merge same-film staging rows from several cinemas into one movies row" in {
    val rows = Seq(staging(Helios, "Kumotry", 2026, 1454157, 2026), staging(Multikino, "Kumotry", 2026, 1454157, 2026))
    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)
    record.tmdbId shouldBe Some(1454157)
    record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.stagingDeletes should have size 2
    plan.moviesDeletes shouldBe empty
  }

  it should "collapse ±1-year variants into ONE row re-keyed to the TMDB year (the absorbed settle)" in {
    // Cinema City reports 'Zawodowcy' at the production year 2025, everyone else at
    // the release year 2026 — both resolved to the same tmdbId (tmdbYear 2026). The
    // OLD per-year fold left these as two `movies` rows for a later settle pass;
    // the group-scoped fold must now collapse them HERE into one row keyed to the
    // TMDB year 2026, carrying both cinemas.
    val cc2025  = staging(Multikino, "Zawodowcy", 2025, 1122573, 2026)
    val rest26  = staging(Helios,    "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(cc2025, rest26), Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)                            // re-keyed to the TMDB year
    record.data.keySet shouldBe Set(Multikino, Helios, Tmdb) // no cinema dropped
  }

  it should "re-key an existing movies row to the TMDB year and retire its old key" in {
    // A previously-folded `zawodowcy|2025` movies row (resolved, tmdbYear 2026)
    // plus a fresh 2026 staging row: the settle re-keys onto 2026 and DELETES the
    // stale 2025 key (the group-scoped movies lookup sees it, so nothing is
    // silently overwritten — the bug the old per-year fold guarded against).
    val existing2025 = StoredMovieRecord("Zawodowcy", Some(2025), MovieRecord(
      tmdbId = Some(1122573),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2025)),
        Tmdb      -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2026)))))
    val fresh2026 = staging(Helios, "Zawodowcy", 2026, 1122573, 2026)

    val plan = StagingFold.planGroup(Seq(fresh2026), Seq(existing2025))

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._1.year shouldBe Some(2026)
    plan.moviesUpserts.head._2.data.keySet shouldBe Set(Multikino, Helios, Tmdb)
    plan.moviesDeletes shouldBe Seq(CacheKey("Zawodowcy", Some(2025)))
  }

  it should "fold a yearless+idless staging stray onto a resolved movies sibling (Dzień objawienia)" in {
    // The stranded-duplicate shape `canonicalizeBySanitize` exists to fix, now
    // healed by the fold: a yearless, unresolved staging row beside an existing
    // resolved yeared movies row collapses onto the resolved row.
    val stray = StagingRecord(Multikino, "Dzień objawienia", None,
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dzień objawienia")))))
    val resolvedSibling = StoredMovieRecord("Dzień objawienia", Some(2026), MovieRecord(
      tmdbId = Some(1275779), imdbId = Some("tt15047880"),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stray), Seq(resolvedSibling))

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
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
      data = Map[Source, SourceData](c -> SourceData(title = Some("Maraton Horrorów"))))))

    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
    key.year shouldBe None
    record.data.keySet shouldBe cinemas.toSet               // every cinema survives
    plan.stagingDeletes should have size cinemas.size
  }

  it should "keep distinct-tmdbId remakes at different years as two movies rows" in {
    // 'Diuna' 1984 (Lynch) vs 2021 (Villeneuve) — distinct tmdbIds, years far
    // apart, so `clusterByFilm` keeps them as two clusters → two `movies` rows.
    // (Two SAME-year distinct-tmdbId rows share one `movies` _id `diuna|YYYY` and
    // can't coexist there at all, so the fold legitimately collapses those — the
    // cache's one-row-per-key invariant, which `planGroup` mirrors.)
    val rows = Seq(staging(Helios, "Diuna", 1984, 841, 1984), staging(Multikino, "Diuna", 2021, 438631, 2021))
    val plan = StagingFold.planGroup(rows, Seq.empty)
    plan.moviesUpserts.map(u => (u._1.year, u._2.tmdbId)).toSet shouldBe
      Set((Some(1984), Some(841)), (Some(2021), Some(438631)))
    plan.stagingDeletes should have size 2
  }

  it should "fold staging onto an existing movies sibling without deleting it spuriously" in {
    val stagingRow = staging(Multikino, "Kumotry", 2026, 1454157, 2026)
    val existing = StoredMovieRecord("Kumotry", Some(2026), MovieRecord(
      tmdbId = Some(1454157),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stagingRow), Seq(existing))

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._2.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.moviesDeletes shouldBe empty                  // same canonical key → no delete
  }

  it should "list a brand-new film (no pre-existing movies row) in newPromotions" in {
    // Nothing in `movies` for this sanitize group → the folded row is a genuine
    // promotion, so the folder can schedule its first-time ratings.
    val rows = Seq(staging(Helios, "Kumotry", 2026, 1454157, 2026), staging(Multikino, "Kumotry", 2026, 1454157, 2026))
    val plan = StagingFold.planGroup(rows, moviesRows = Seq.empty)

    plan.newPromotions shouldBe plan.moviesUpserts
    plan.newPromotions.map(_._1) shouldBe Seq(CacheKey("Kumotry", Some(2026)))
  }

  it should "NOT list a film that merely merges into an existing movies row in newPromotions" in {
    // An existing `movies` row joins the cluster → not a new film; it already owns
    // its ratings, so it must not be re-enqueued as a promotion.
    val stagingRow = staging(Multikino, "Kumotry", 2026, 1454157, 2026)
    val existing = StoredMovieRecord("Kumotry", Some(2026), MovieRecord(
      tmdbId = Some(1454157),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    val plan = StagingFold.planGroup(Seq(stagingRow), Seq(existing))

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

    val plan = StagingFold.planGroup(Seq(staging1984, staging2021), Seq(existing1984))

    plan.newPromotions.map(u => (u._1.year, u._2.tmdbId)) shouldBe Seq((Some(2021), Some(438631)))
  }
}
