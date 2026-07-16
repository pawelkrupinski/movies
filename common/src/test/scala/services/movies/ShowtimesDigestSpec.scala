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

  // The load-bearing safety property: `==` is showtime-AGNOSTIC (identity/metadata only,
  // so canonicalize/settle/divert don't churn on showtime changes), but `leanEqual` DOES
  // detect a real showtime change via the digest — otherwise the write-guard would skip a
  // real write (stale read model).
  it should "detect a real showtime change via the digest even though == ignores showtimes" in {
    val a = rec(Seq(st("2026-06-11T10:00")))
    val b = rec(Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00")))   // a new screening appeared
    (a == b) shouldBe true                            // == is showtime-agnostic by design
    ShowtimesDigest.leanEqual(a, b) shouldBe false    // ...but leanEqual catches it — no false-skip
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

  // Reflexive: an identical record is always leanEqual to itself (equal metadata AND
  // equal digest). And a stripped copy (showtimes → digest) stays leanEqual to the full
  // one — the property that lets the cache hold stripped records without churn.
  it should "be true for a record and its stripped form" in {
    val samples = Seq(
      rec(Seq.empty),
      rec(Seq(st("2026-06-11T10:00"))),
      rec(Seq(st("2026-06-11T10:00", "u"), st("2026-06-11T12:00")), synopsis = Some("s"))
    )
    samples.foreach { r =>
      ShowtimesDigest.leanEqual(r, r.copy()) shouldBe true
      ShowtimesDigest.leanEqual(r, ShowtimesDigest.stripForCache(r)) shouldBe true
    }
  }

  // ── slotOps digest-aware: the screenings delete-vector defense ────────────
  // These are the load-bearing safety property for the index-only strip: an
  // updater that carries a stripped slot through (enrichment/rating — showtimes
  // untouched, digest preserved) must NOT emit a screening delete.
  private val src: Source = Multikino
  private def stripped(sts: Seq[Showtime]): SourceData =
    SourceData(showtimes = Nil, showtimesDigest = Some(ShowtimesDigest.digest(sts)))

  "slotOps (digest-aware)" should "emit NO op when a stripped slot's digest is preserved (no phantom delete)" in {
    val prior    = stripped(Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00")))
    val enriched = prior.copy(synopsis = Some("x"))   // metadata change, showtimes untouched
    ScreeningsRepository.slotOps(Map(src -> prior), Map(src -> enriched)) shouldBe empty
  }

  it should "write the real showtimes when a fresh scrape differs from the stripped prior" in {
    val prior = stripped(Seq(st("2026-06-11T10:00")))
    val fresh = SourceData(showtimes = Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00")))
    ScreeningsRepository.slotOps(Map(src -> prior), Map(src -> fresh)) shouldBe
      Map(src.displayName -> Some(fresh.showtimes))
  }

  it should "no-op when a fresh scrape matches the stripped prior's digest" in {
    val real  = Seq(st("2026-06-11T10:00"))
    val prior = stripped(real)
    val fresh = SourceData(showtimes = real)
    ScreeningsRepository.slotOps(Map(src -> prior), Map(src -> fresh)) shouldBe empty
  }

  it should "delete only when showtimes genuinely go empty (a fresh slot with no digest)" in {
    val prior = stripped(Seq(st("2026-06-11T10:00")))
    val gone  = SourceData(showtimes = Nil)   // a listing that legitimately dropped the slot
    ScreeningsRepository.slotOps(Map(src -> prior), Map(src -> gone)) shouldBe Map(src.displayName -> None)
  }

  // ── reStitch: the FULL-REPLACE delete-vector defense ──────────────────────
  // A whole-record write (fold/canonicalize/rekey) carrying a stripped slot would
  // `showtimesOf`-drop it and `replaceFilm`-DELETE its screenings. reStitch re-injects
  // the slot's current screenings first, so a stripped write never deletes what it
  // doesn't own.
  "reStitch" should "re-inject a stripped slot's showtimes from screenings (no fold data loss)" in {
    val real = Seq(st("2026-06-11T10:00"), st("2026-06-11T12:00"))
    val scr  = new InMemoryScreeningsRepository()
    scr.upsertSlot("film1", src.displayName, real)
    val restitched = ScreeningsRepository.reStitch(scr, "film1", Map(src -> stripped(real)))
    restitched(src).showtimes shouldBe real
    // ...so a full write keeps them: showtimesOf is non-empty → replaceFilm won't delete.
    ScreeningsRepository.showtimesOf(restitched) shouldBe Map(src.displayName -> real)
  }

  it should "leave slots with real showtimes untouched" in {
    val fresh = SourceData(showtimes = Seq(st("2026-06-11T10:00")))
    ScreeningsRepository.reStitch(new InMemoryScreeningsRepository(), "film1", Map(src -> fresh)) shouldBe Map(src -> fresh)
  }
}
