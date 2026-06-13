package services.staging

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{StoredMovieDto, TitleNormalizer}

import java.time.Instant

/** Pins the cinema-scoped staging `_id` (`cinema|sanitize(title)|year`) and its
 *  round-trip back to a `StagingRecord`, plus that the `movies` storage shape
 *  (`StoredMovieDto` + codec registry) carries a 3-part id unchanged. */
class StagingRecordSpec extends AnyFlatSpec with Matchers {

  private def slot(title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = year)))

  "idFor" should "be cinemaDisplayName|sanitize(title)|year" in {
    StagingRecord.idFor(Helios, "Dzień objawienia", Some(2026)) shouldBe
      s"${Helios.displayName}|${TitleNormalizer.sanitize("Dzień objawienia")}|2026"
  }

  it should "leave the year segment empty for a yearless row" in {
    StagingRecord.idFor(Helios, "Kumotry", None) shouldBe s"${Helios.displayName}|kumotry|"
  }

  "fromStorage" should "recover cinema, title and year from the id" in {
    val id  = StagingRecord.idFor(Helios, "Kumotry", Some(2026))
    val row = StagingRecord.fromStorage(id, slot("Kumotry", Some(2026)))
    row shouldBe defined
    row.get.cinema shouldBe Helios
    row.get.year   shouldBe Some(2026)
    row.get.title  shouldBe "Kumotry"
  }

  it should "recover a yearless row" in {
    val id  = StagingRecord.idFor(Helios, "Kumotry", None)
    StagingRecord.fromStorage(id, slot("Kumotry", None)).flatMap(r => r.year) shouldBe None
  }

  it should "drop a row whose cinema segment is unknown (dropped/renamed cinema)" in {
    StagingRecord.fromStorage("No Such Cinema|kumotry|2026", slot("Kumotry", Some(2026))) shouldBe None
  }

  "the movies storage shape" should "round-trip a 3-part staging id through the codec" in {
    val rec = slot("Kumotry", Some(2026)).copy(
      tmdbId = Some(1454157),
      data   = slot("Kumotry", Some(2026)).data +
        (Tmdb -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026))))
    val id  = StagingRecord.idFor(Helios, "Kumotry", Some(2026))
    val dto = StoredMovieDto.fromDomain(id, rec, Instant.now())
    val back = StoredMovieDto.toDomain(dto).record
    back.tmdbId               shouldBe Some(1454157)
    back.data.keySet          shouldBe Set(Helios, Tmdb)
    StagingRecord.fromStorage(id, back).map(_.cinema) shouldBe Some(Helios)
  }
}
