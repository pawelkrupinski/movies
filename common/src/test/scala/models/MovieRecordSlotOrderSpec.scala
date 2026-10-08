package models

import tools.SpecClock.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A record's derived fields read its slots in PRIORITY order, and slots of equal priority — two titles of one film at
 * one venue, or two sources the priority list does not rank — must be read in an order of their own, not the order the
 * slot map happens to iterate in: an immutable map of up to four entries iterates in insertion order, so the same
 * slots assembled differently gave the same film another year, and with it another stored key. The identity
 * projection, re-assembling a film's slots (`ProjectionDraft.complete` appends the rebuilt ones), flipped a film from
 * `belle|2013` to `belle|1979` over an unchanged corpus.
 */
class MovieRecordSlotOrderSpec extends AnyFlatSpec with Matchers {

  private def both(slots: (Source, SourceData)*): (MovieRecord, MovieRecord) =
    (MovieRecord(data = slots.toMap), MovieRecord(data = slots.reverse.toMap))

  "A record's slots of equal priority" should "give one year and one display title, however the slot map was assembled" in {
    val (a, b) = both(
      CinemaShowing(Helios, "belle") -> SourceData(title = Some("Belle"), releaseYear = Some(2013)),
      CinemaShowing(Helios, "obcy")  -> SourceData(title = Some("Obcy"), releaseYear = Some(1979)))
    a.releaseYear shouldBe b.releaseYear
    a.resolvedYear shouldBe b.resolvedYear
    a.displayTitle("Belle", titleNormalizer) shouldBe b.displayTitle("Belle", titleNormalizer)
  }

  it should "be read in one order across venues the priority list does not rank too" in {
    val (one, other) = (new UsCinema("Regal Union Square", "Union Square"), new UsCinema("AMC Lincoln Square", "Lincoln Square"))
    val (a, b) = both(
      CinemaShowing(one, "dune")   -> SourceData(title = Some("Dune"), releaseYear = Some(2021)),
      CinemaShowing(other, "dune") -> SourceData(title = Some("Dune"), releaseYear = Some(1984)))
    a.releaseYear shouldBe b.releaseYear
  }
}
