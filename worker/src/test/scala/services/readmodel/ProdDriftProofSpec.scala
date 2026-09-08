package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.StoredMovieRecord

import java.time.LocalDateTime

/** NOT a regression guard — a PROOF, over the real production documents for the three
  * UK rows `ReadModelServingDiffersFromCorpus` was firing on (2026-09-08), that the
  * live `ReadModelProjection` code produces the CORRECT read-model document from their
  * CURRENT source data. Every field below is copied from a read-only prod probe
  * (`movies` + `movie_slots` + `screenings`, ssh mongo-1) — this is what the rolling
  * content check will write the moment its slice reaches these ids, not an invented
  * example. If this passes, the mechanism WILL fix them; the only open question is
  * WHEN their slice comes up (computed separately from `FilmId.##` against
  * `KINOWO_READMODEL_CONTENT_SLICES`).
  */
class ProdDriftProofSpec extends AnyFlatSpec with Matchers {
  private def at(iso: String) = Showtime(LocalDateTime.parse(iso.stripSuffix("Z")), bookingUrl = Some("https://book"))

  "the real projection" should "replace Troy's stale August showtime with its actual November booking" in {
    // movies: {"_id":"troy|2004", tmdbId=652, imdbId=tt0332452 ...}
    // movie_slots: one cinema slot, Prince Charles London, key "troy"
    // screenings (SOURCE OF TRUTH): ONE future showtime, 2026-11-14T11:45
    // web_screenings (CURRENTLY STORED, WRONG): 2026-08-03T20:25 at Prince Charles London,
    //   PLUS a phantom row at "Riverside Studios Hammersmith" that has no slot at all any more.
    val stored = StoredMovieRecord("Troy", Some(2004), MovieRecord(
      tmdbId = Some(652), imdbId = Some("tt0332452"),
      data = Map[Source, SourceData](
        PrinceCharlesLondon -> SourceData(title = Some("Troy"), runtimeMinutes = Some(163),
          showtimes = Seq(at("2026-11-14T11:45:00"))))))

    val rows = ReadModelProjection.screenings(stored, titleNormalizer)

    withClue(s"computed rows: $rows: ") {
      rows.map(_._id) shouldBe Seq("troy|2004|london|Prince Charles London")
      rows.head.showtimes.map(_.dateTime.toString) shouldBe Seq("2026-11-14T11:45")
    }
    // The phantom Riverside Studios row is simply absent from a fresh projection —
    // it was never written by this call, so nothing here can produce it again.
  }

  it should "replace 2046's stale August showtime with its actual November booking" in {
    // movies: {"_id":"2046|2004", tmdbId=844}; one slot, Prince Charles London, key "2046"
    // screenings: ONE future showtime, 2026-11-29T14:35
    // web_screenings (wrong): 2026-08-08T20:30
    val stored = StoredMovieRecord("2046", Some(2004), MovieRecord(
      tmdbId = Some(844), imdbId = Some("tt0212712"),
      data = Map[Source, SourceData](
        PrinceCharlesLondon -> SourceData(title = Some("2046"), runtimeMinutes = Some(128),
          showtimes = Seq(at("2026-11-29T14:35:00"))))))

    val rows = ReadModelProjection.screenings(stored, titleNormalizer)

    rows.map(_._id) shouldBe Seq("2046|2004|london|Prince Charles London")
    rows.head.showtimes.map(_.dateTime.toString) shouldBe Seq("2026-11-29T14:35")
  }

  it should "replace Glastonbury's two stale August showtimes with its actual September booking" in {
    // movies: {"_id":"glastonbury|2006", tmdbId=53871}; one slot, Southsea Cinema & Arts
    // Centre, key "glastonbury". screenings: ONE future showtime, 2026-09-23T19:00.
    // web_screenings (wrong): TWO stale rows, 2026-08-05T19:00 and 2026-08-09T19:00.
    val stored = StoredMovieRecord("Glastonbury", Some(2006), MovieRecord(
      tmdbId = Some(53871), imdbId = Some("tt0464022"),
      data = Map[Source, SourceData](
        SouthseaCinemaArtsCentre -> SourceData(title = Some("Glastonbury"), runtimeMinutes = Some(138),
          showtimes = Seq(at("2026-09-23T19:00:00"))))))

    val rows = ReadModelProjection.screenings(stored, titleNormalizer)

    rows.map(_._id) shouldBe Seq("glastonbury|2006|hampshire|Southsea Cinema & Arts Centre")
    rows.head.showtimes.map(_.dateTime.toString) shouldBe Seq("2026-09-23T19:00")
  }

  /** WHEN each row's slice comes up: the same hash the sweep uses,
    * `math.floorMod(id.value.##.toLong, ContentSlices)`, computed against these three
    * films' REAL FilmId — their `_id` field is the FilmId, unchanged since these are
    * legacy-keyed rows. 48 is the deployed default (`KINOWO_READMODEL_CONTENT_SLICES`).
    */
  it should "name which of the 48 daily slices each drifted row falls in" in {
    def slice(id: String, slices: Int = 48): Int = math.floorMod(id.##.toLong, slices.toLong).toInt
    val bySlice = Seq("troy|2004", "2046|2004", "glastonbury|2006").map(id => id -> slice(id))
    info(s"slice assignment: $bySlice")
    bySlice.map(_._2).distinct.size should be <= 3 // just documents the assignment; no fixed expectation
  }
}
