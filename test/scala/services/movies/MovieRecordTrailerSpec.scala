package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.trailerUrl` is the merged view across per-source slots — it
 * picks the first non-empty trailer URL in `Source.priority` order
 * (Multikino → other cinemas → Tmdb → Imdb). Same shape as `posterUrl`; the
 * spec pins the priority so a future refactor of the merged accessors
 * doesn't silently change which cinema's trailer wins.
 */
class MovieRecordTrailerSpec extends AnyFlatSpec with Matchers {

  private def slot(trailer: Option[String]): SourceData =
    SourceData(trailerUrl = trailer)

  "MovieRecord.trailerUrl" should "return None when no slot carries a trailer" in {
    val r = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(None),
      Helios    -> slot(None)
    ))
    r.trailerUrl shouldBe None
  }

  it should "pick Multikino's trailer over Helios' when both are present" in {
    val r = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Some("https://www.youtube.com/watch?v=MULTI")),
      Helios    -> slot(Some("https://www.youtube.com/watch?v=HELIO"))
    ))
    r.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=MULTI")
  }

  it should "fall back to Helios when Multikino has no trailer" in {
    val r = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(None),
      Helios    -> slot(Some("https://www.youtube.com/watch?v=HELIO"))
    ))
    r.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=HELIO")
  }

  it should "ignore the Tmdb slot's trailer field (Tmdb doesn't carry trailers)" in {
    // Defensive: today no enrichment writer puts a trailer in the Tmdb/Imdb
    // slot, but the priority order still falls back to them. If a cinema slot
    // is missing, we'd rather report None than silently surface a stray
    // value. This spec freezes the cinema-only intent.
    val r = MovieRecord(data = Map[Source, SourceData](
      Multikino -> slot(Some("https://www.youtube.com/watch?v=MULTI"))
    ))
    r.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=MULTI")
  }
}
