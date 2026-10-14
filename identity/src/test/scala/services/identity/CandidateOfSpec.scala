package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.identity.IdentityMeasures.Film

/**
 * A recorded film's base candidate is its record. The corpus holds both — `records` and `base` — for every film,
 * and a copy made to restate the popularity the record already carries was a second Film per film: 30k shells on
 * worker-uk's corpus (heap dump 2026-10-07).
 */
class CandidateOfSpec extends AnyFlatSpec with Matchers {
  private val hits = Seq(Hit(7, "Samson et Dalila", None, Some(2026), 12.0), Hit(7, "Samson et Dalila", None, Some(2026), 30.0))

  "a candidate of a record carrying its popularity" should "be the record itself" in {
    val record = Film("Samson et Dalila", year = Some(2026), popularity = Some(4.0))
    (Candidate.of(7, hits, Some(record)).film eq record) shouldBe true
  }

  "a candidate of a record carrying none" should "take its hits' highest popularity" in {
    val record = Film("Samson et Dalila", year = Some(2026))
    Candidate.of(7, hits, Some(record)).film shouldBe record.copy(popularity = Some(30.0))
  }

  "a candidate of no record" should "be built from its most popular hit" in {
    Candidate.of(7, hits, None).film shouldBe Film("Samson et Dalila", None, Nil, Some(2026), popularity = Some(30.0))
  }
}
