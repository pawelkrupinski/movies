package services.staging

import models.{Cinema, Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

class StagingFoldSpec extends AnyFlatSpec with Matchers {

  private def resolved(cinema: Source, title: String, year: Int, tmdbId: Int): StagingRecord =
    StagingRecord(cinema, title, Some(year), MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), releaseYear = Some(year)),
        Tmdb   -> SourceData(title = Some(title), releaseYear = Some(year)))))

  "plan" should "merge same-film staging rows from several cinemas into one movies row" in {
    val rows = Seq(resolved(Helios, "Kumotry", 2026, 1454157), resolved(Multikino, "Kumotry", 2026, 1454157))
    val plan = StagingFold.plan(rows, moviesRows = Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)
    record.tmdbId shouldBe Some(1454157)
    record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.stagingDeletes should have size 2
    plan.moviesDeletes shouldBe empty
  }

  it should "keep the variant's own year (NOT rekey to the TMDB year) — settle does the cross-year merge" in {
    // Cinema City reports 'Zawodowcy' at the production year 2025; TMDB's release
    // year is 2026. The fold must keep 2025 so the row sits beside the 2026
    // variant and the movies-side ±1 settle merges them — rekeying to 2026 here
    // made the 2025 fold overwrite the 2026 row and drop its cinemas.
    val cc2025 = StagingRecord(Multikino, "Zawodowcy", Some(2025), MovieRecord(
      tmdbId = Some(1122573),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2025)),
        Tmdb      -> SourceData(title = Some("Zawodowcy"), releaseYear = Some(2026))))) // tmdbYear = 2026

    val plan = StagingFold.plan(Seq(cc2025), Seq.empty)

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._1.year shouldBe Some(2025) // variant year kept, not the TMDB 2026
  }

  it should "fold MANY yearless+idless per-cinema rows of one event into a single all-cinema row (no clobber)" in {
    // A festival/event film (e.g. 'Maraton Horrorów') that ~50 cinemas report
    // YEARLESS and UNRESOLVED. Each per-cinema row shares the same (sanitize, None)
    // variant key. The fold MUST union them into ONE movies row carrying every
    // cinema — the regression this guards: feeding them through `clusterByFilm`
    // turned each yearless+idless row into its OWN singleton cluster (rule 4), so
    // all N upserts landed on the same (sanitize, None) key and clobbered down to a
    // single cinema, silently dropping the other ~49. Grouping by tmdbId (all None
    // here → one group) is what keeps them together.
    val cinemas = Cinema.all.take(8)
    val rows = cinemas.map(c => StagingRecord(c, "Maraton Horrorów", None, MovieRecord(
      data = Map[Source, SourceData](c -> SourceData(title = Some("Maraton Horrorów"))))))

    val plan = StagingFold.plan(rows, moviesRows = Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, record) = plan.moviesUpserts.head
    key.year shouldBe None
    record.data.keySet shouldBe cinemas.toSet               // every cinema survives
    plan.stagingDeletes should have size cinemas.size
  }

  it should "keep distinct-tmdbId remakes that share title+year as two movies rows" in {
    val rows = Seq(resolved(Helios, "Diuna", 2024, 841), resolved(Multikino, "Diuna", 2024, 438631))
    val plan = StagingFold.plan(rows, Seq.empty)
    plan.moviesUpserts.map(_._2.tmdbId).toSet shouldBe Set(Some(841), Some(438631))
    plan.stagingDeletes should have size 2
  }

  it should "fold staging onto an existing movies sibling without deleting it spuriously" in {
    val staging = resolved(Multikino, "Kumotry", 2026, 1454157)
    val existing = StoredMovieRecord("Kumotry", Some(2026), MovieRecord(
      tmdbId = Some(1454157),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    val plan = StagingFold.plan(Seq(staging), Seq(existing))

    plan.moviesUpserts should have size 1
    plan.moviesUpserts.head._2.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.moviesDeletes shouldBe empty                  // same canonical key → no delete
  }
}
