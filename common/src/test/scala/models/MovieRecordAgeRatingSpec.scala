package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.ageRating` — CINEMA-first (the screening venue's certificate is
 * authoritative for its country), falling back to a `Tmdb` slot only when no
 * cinema carries one. Verbatim per source — no cross-scheme normalisation.
 * Opposite priority to `genres` (TMDB-first), because a certificate is a
 * country-specific legal rating, not a taxonomy label.
 */
class MovieRecordAgeRatingSpec extends AnyFlatSpec with Matchers {

  "ageRating" should "prefer a cinema slot's certificate over TMDB" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(ageRating = Some("15")),
        Tmdb      -> SourceData(ageRating = Some("R"))
      )
    )
    record.ageRating shouldBe Some("15")
  }

  it should "fall back to the TMDB slot when no cinema carries a certificate" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(ageRating = None),
        Tmdb      -> SourceData(ageRating = Some("PG-13"))
      )
    )
    record.ageRating shouldBe Some("PG-13")
  }

  it should "be None when no source carries a certificate" in {
    MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData())).ageRating shouldBe None
    MovieRecord().ageRating shouldBe None
  }

  it should "ignore an empty-string certificate and keep looking" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(ageRating = Some("")),
        Tmdb      -> SourceData(ageRating = Some("12A"))
      )
    )
    record.ageRating shouldBe Some("12A")
  }
}
