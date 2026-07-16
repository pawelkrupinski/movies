package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class ShowtimesDigestSpec extends AnyFlatSpec with Matchers {

  private def st(d: String, url: String = ""): Showtime =
    Showtime(LocalDateTime.parse(d), if (url.isEmpty) None else Some(url))

  private def rec(showtimes: Seq[Showtime], synopsis: Option[String] = None, rating: Option[Double] = Some(7.0)): MovieRecord =
    MovieRecord(
      imdbRating = rating,
      data = Map[Source, SourceData](Multikino -> SourceData(synopsis = synopsis, showtimes = showtimes))
    )

  "digest" should "be equal for equal content and differ on add / remove / field change" in {
    val a = Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00"))
    ShowtimesDigest.digest(a) shouldBe ShowtimesDigest.digest(Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00")))
    ShowtimesDigest.digest(a) should not be ShowtimesDigest.digest(Seq(st("2026-06-11T10:00")))                // removed one
    ShowtimesDigest.digest(a) should not be ShowtimesDigest.digest(a :+ st("2026-06-11T14:00"))                 // added one
    ShowtimesDigest.digest(a) should not be ShowtimesDigest.digest(st("2026-06-11T10:00", "u") +: a.tail)       // bookingUrl change
  }

  "leanEqual" should "be true for identical records (agrees with ==)" in {
    val r = rec(Seq(st("2026-06-11T10:00")))
    (r == r.copy()) shouldBe true
    ShowtimesDigest.leanEqual(r, r.copy()) shouldBe true
  }

  // The load-bearing safety property: a genuine showtime change must NOT be
  // hidden — otherwise the digest guard would skip a real write (stale read model).
  it should "NOT false-skip a real showtime change" in {
    val a = rec(Seq(st("2026-06-11T10:00")))
    val b = rec(Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00")))   // a new screening appeared
    (a == b) shouldBe false
    ShowtimesDigest.leanEqual(a, b) shouldBe false
  }

  it should "detect non-showtime changes (scalar field and per-slot field)" in {
    val base = rec(Seq(st("2026-06-11T10:00")))
    ShowtimesDigest.leanEqual(base, base.copy(imdbRating = Some(9.0))) shouldBe false          // top-level scalar
    ShowtimesDigest.leanEqual(base, rec(Seq(st("2026-06-11T10:00")), synopsis = Some("x"))) shouldBe false  // slot field
  }

  it should "add/remove of a whole cinema slot be detected" in {
    val one = rec(Seq(st("2026-06-11T10:00")))
    val two = one.copy(data = one.data + (Helios -> SourceData(showtimes = Seq(st("2026-06-11T18:00")))))
    ShowtimesDigest.leanEqual(one, two) shouldBe false
  }

  // Invariant the shadow relies on: leanEqual is true WHENEVER == is true, so the
  // only possible shadow disagreement is a false-skip (a digest collision).
  it should "never be false while == is true" in {
    val samples = Seq(
      rec(Seq.empty),
      rec(Seq(st("2026-06-11T10:00"))),
      rec(Seq(st("2026-06-11T10:00", "u"), st("2026-06-11T12:00")), synopsis = Some("s"))
    )
    samples.foreach(r => ShowtimesDigest.leanEqual(r, r.copy()) shouldBe true)
  }
}
