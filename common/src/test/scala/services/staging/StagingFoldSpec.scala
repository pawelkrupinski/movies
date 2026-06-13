package services.staging

import models.{Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
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
    val (key, rec) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)
    rec.tmdbId shouldBe Some(1454157)
    rec.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.stagingDeletes should have size 2
    plan.moviesDeletes shouldBe empty
  }

  it should "fold an unresolved ±1-year cinema row onto the resolved sibling (the Kumotry case)" in {
    val resolved2026 = resolved(Helios, "Kumotry", 2026, 1454157)
    val unresolved2025 = StagingRecord(Multikino, "Kumotry", Some(2025), MovieRecord(
      data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2025)))))

    val plan = StagingFold.plan(Seq(resolved2026, unresolved2025), Seq.empty)

    plan.moviesUpserts should have size 1
    val (key, rec) = plan.moviesUpserts.head
    key.year shouldBe Some(2026)                       // TMDB year wins
    rec.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
    plan.stagingDeletes should have size 2
  }

  it should "keep distinct-tmdbId remakes that share a title as two movies rows" in {
    val rows = Seq(resolved(Helios, "Diuna", 1984, 841), resolved(Multikino, "Diuna", 2021, 438631))
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
