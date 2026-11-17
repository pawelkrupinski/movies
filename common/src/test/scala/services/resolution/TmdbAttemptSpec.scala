package services.resolution

import models.TmdbAttempt
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class TmdbAttemptSpec extends AnyFlatSpec with Matchers {

  private val evidence = FilmEvidence(
    slotTitles = Seq("Guru", "Guru"), originalTitles = Seq("Gourou"), directors = Seq("Yann Gozlan"),
    cast = Nil, runtimes = Seq(100), years = Seq(2026))

  "fingerprint" should "be a function of the inputs, not of their order" in {
    FilmEvidence.fingerprint(evidence, Seq("b", "a")) shouldBe FilmEvidence.fingerprint(evidence, Seq("a", "b", "a"))
    FilmEvidence.fingerprint(evidence, Nil) should not be FilmEvidence.fingerprint(evidence, Seq("a"))
    FilmEvidence.fingerprint(evidence, Nil) should not be FilmEvidence.fingerprint(evidence.withDirectors(Seq("Ang Lee")), Nil)
    FilmEvidence.fingerprint(evidence, Nil) should not be FilmEvidence.fingerprint(evidence.withOriginalTitle(Some("Guru")), Nil)
    FilmEvidence.fingerprint(evidence, Nil) shouldBe FilmEvidence.fingerprint(evidence.withOriginalTitle(Some("Gourou")), Nil)
    FilmEvidence.fingerprint(FilmEvidence.empty, Nil) should fullyMatch regex "[0-9a-f]{40}"
  }

  "covers" should "hold for the same inputs inside the retry window and nothing else" in {
    val at   = Instant.parse("2026-09-06T10:00:00Z")
    val miss = FilmEvidence.attempt(evidence, Nil, at)
    miss.covers(FilmEvidence.fingerprint(evidence, Nil), at) shouldBe true
    miss.covers(FilmEvidence.fingerprint(evidence, Nil), at.plus(TmdbAttempt.RetryAfter)) shouldBe true
    miss.covers(FilmEvidence.fingerprint(evidence, Nil), at.plus(TmdbAttempt.RetryAfter).plusSeconds(1)) shouldBe false
    miss.covers(FilmEvidence.fingerprint(evidence, Seq("x")), at) shouldBe false
  }

  "Legacy" should "cover nothing, so a flag written before attempts were recorded is retried at the next look" in {
    TmdbAttempt.Legacy.covers(FilmEvidence.fingerprint(FilmEvidence.empty, Nil), Instant.EPOCH) shouldBe false
  }
}
