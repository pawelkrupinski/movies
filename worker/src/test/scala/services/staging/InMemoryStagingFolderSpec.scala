package services.staging

import models.{Helios, Multikino, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.InMemoryMovieRepo

class InMemoryStagingFolderSpec extends AnyFlatSpec with Matchers {

  private def resolved(cinema: Source, year: Int): MovieRecord =
    MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
      cinema -> SourceData(title = Some("Kumotry"), releaseYear = Some(year)),
      Tmdb   -> SourceData(title = Some("Kumotry"), releaseYear = Some(year))))

  "foldFilm" should "move a film's staging rows into movies and delete them" in {
    val staging  = new InMemoryStagingRepo
    val movies   = new InMemoryMovieRepo
    staging.upsert(Helios, "Kumotry", Some(2026), resolved(Helios, 2026))
    staging.upsert(Multikino, "Kumotry", Some(2026), resolved(Multikino, 2026))
    val folder = new InMemoryStagingFolder(staging, movies)

    folder.foldFilm("Kumotry", Some(2026))

    staging.findAll() shouldBe empty
    val rows = movies.findAll()
    rows should have size 1
    rows.head.record.tmdbId shouldBe Some(1454157)
    rows.head.record.data.keySet shouldBe Set(Helios, Multikino, Tmdb)
  }

  it should "be a no-op when no staging rows match (already folded)" in {
    val staging = new InMemoryStagingRepo
    val movies  = new InMemoryMovieRepo
    new InMemoryStagingFolder(staging, movies).foldFilm("Ghost", Some(2026))
    movies.findAll() shouldBe empty
  }
}
