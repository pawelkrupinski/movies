package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import scala.util.Random

class MovieRecordMergeSpec extends AnyFlatSpec with Matchers {

  private def slot(
    poster:    String,
    showtimes: Seq[Showtime] = Seq.empty
  ): SourceData = SourceData(
    posterUrl = Some(poster),
    showtimes = showtimes
  )

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = None)

  private val canonical = MovieRecord(
    imdbId        = Some("tt1"),
    imdbRating    = Some(8.0),
    metascore     = Some(75),
    tmdbId        = Some(42),
    data = Map[Source, SourceData](
      Multikino -> slot("m.jpg").copy(title = Some("Foo"), releaseYear = Some(2024)),
      Tmdb      -> SourceData(originalTitle = Some("Canonical"))
    )
  )
  private val victim = MovieRecord(
    imdbId        = Some("tt-stale"),
    imdbRating    = Some(1.0),
    metascore     = None,
    tmdbId        = Some(42),
    data = Map[Source, SourceData](
      Helios -> slot("h.jpg").copy(title = Some("Foo")),
      Tmdb   -> SourceData(originalTitle = Some("Stale"))
    )
  )

  "union" should "keep all enrichment fields from canonical" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.imdbId        shouldBe Some("tt1")
    merged.imdbRating    shouldBe Some(8.0)
    merged.metascore     shouldBe Some(75)
    merged.originalTitle shouldBe Some("Canonical")
    merged.tmdbId        shouldBe Some(42)
  }

  // The cross-language flap: the union base (canonical) is the lowest-canonicalRank
  // row, which can be a freshly-resolved translation duplicate — same tmdbId, no
  // ratings yet — while the VICTIM is the already-rated sibling. The base-only copy
  // dropped the victim's scores until a later refresh re-fetched them. Each
  // enrichment field must fall back to the victim when the canonical lacks it.
  it should "fall back to the victim's enrichment fields when the canonical lacks them" in {
    val unratedBase = MovieRecord(
      tmdbId = Some(42),
      data = Map[Source, SourceData](Helios -> slot("h.jpg").copy(title = Some("Disclosure Day")))
    )
    val rated = MovieRecord(
      imdbId = Some("tt15047880"), imdbRating = Some(6.8), metascore = Some(74),
      rottenTomatoes = Some(80), filmwebRating = Some(5.9),
      filmwebUrl = Some("fw"), metacriticUrl = Some("mc"), rottenTomatoesUrl = Some("rt"),
      tmdbId = Some(42),
      data = Map[Source, SourceData](Multikino -> slot("m.jpg").copy(title = Some("Dzień objawienia")))
    )
    val merged = MovieRecordMerge.union(unratedBase, rated)
    merged.imdbId            shouldBe Some("tt15047880")
    merged.imdbRating        shouldBe Some(6.8)
    merged.metascore         shouldBe Some(74)
    merged.rottenTomatoes    shouldBe Some(80)
    merged.filmwebRating     shouldBe Some(5.9)
    merged.filmwebUrl        shouldBe Some("fw")
    merged.metacriticUrl     shouldBe Some("mc")
    merged.rottenTomatoesUrl shouldBe Some("rt")
  }

  it should "preserve each row's per-cinema title and release year on the merged record" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.cinemaData.keySet                    shouldBe Set(Multikino, Helios)
    merged.cinemaData(Multikino).title          shouldBe Some("Foo")
    merged.cinemaData(Multikino).releaseYear    shouldBe Some(2024)
    merged.cinemaData(Helios).title             shouldBe Some("Foo")
    merged.cinemaData(Helios).releaseYear       shouldBe None
  }

  // Disjoint cinemas: each cinema's slot lands intact, no merging needed.
  it should "carry both rows' cinemaShowings entries when the cinemas don't overlap" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.cinemaData.keySet           shouldBe Set(Multikino, Helios)
    merged.cinemaData(Multikino).posterUrl shouldBe Some("m.jpg")
    merged.cinemaData(Helios).posterUrl    shouldBe Some("h.jpg")
  }

  // Same-cinema collision: this is the regression case
  // (`DiabelPradaEndToEndSpec`) — a cinema reports the same film under two
  // variant titles in the same tick (regular + Ukrainian dub at CinemaCity
  // Poznań Plaza), both resolve to the same tmdbId, and the identity-gate
  // fold collides their slots. The canonical's metadata wins (its
  // filmUrl/poster are anchored to the primary variant), but the showtimes
  // are unioned so no per-cinema schedule data is lost. Old right-bias
  // behaviour silently dropped one of the two schedules.
  it should "union showtimes (canonical metadata wins) when the same cinema appears in both rows" in {
    val canonicalSlot = slot("canon.jpg", showtimes = Seq(at("2026-05-16T18:00"), at("2026-05-16T20:00")))
    val victimSlot    = slot("victim.jpg", showtimes = Seq(at("2026-05-16T22:30")))
    val a = canonical.copy(data = canonical.data + ((Multikino: Source) -> canonicalSlot))
    val b = victim.copy(data    = victim.data    + ((Multikino: Source) -> victimSlot))

    val merged = MovieRecordMerge.union(a, b)

    merged.cinemaData.keySet               shouldBe Set(Multikino, Helios)
    merged.cinemaData(Multikino).posterUrl shouldBe Some("canon.jpg")   // canonical wins on metadata
    merged.cinemaData(Multikino).showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.parse("2026-05-16T18:00"),
      LocalDateTime.parse("2026-05-16T20:00"),
      LocalDateTime.parse("2026-05-16T22:30")
    )
  }

  // Dedup: a showtime present in both slots (e.g., a session that survived
  // a cinema's daily refresh) should only appear once in the union.
  it should "deduplicate showtimes when merging two same-cinema slots" in {
    val shared = at("2026-05-16T18:00")
    val a = canonical.copy(data = canonical.data + ((Multikino: Source) -> slot("canon.jpg", showtimes = Seq(shared, at("2026-05-16T20:00")))))
    val b = victim.copy(data    = victim.data    + ((Multikino: Source) -> slot("victim.jpg", showtimes = Seq(shared, at("2026-05-16T22:30")))))

    val merged = MovieRecordMerge.union(a, b)

    merged.cinemaData(Multikino).showtimes.size shouldBe 3
  }

  // ── Order-dependent phantom screening (the "screenings error") ────────────
  //
  // Kino Nowe Horyzonty surfaces the SAME physical screening (Werdykt,
  // 2026-06-11 12:00) under two `op.s?id=` film pages, so the two showtimes
  // carry the SAME dateTime but DIFFERENT bookingUrls (per-event ticket
  // links). The plain `.distinct` keyed on the whole Showtime treated them as
  // two distinct sessions, so the union kept both — a phantom duplicate whose
  // surviving order (and thus which booking link rendered first) flipped with
  // scrape/merge order. A screening is identified by (dateTime, room, format);
  // bookingUrl is a per-source link, not part of its identity.
  private def withBooking(d: String, booking: String): Showtime =
    Showtime(LocalDateTime.parse(d), bookingUrl = Some(booking))

  it should "collapse a screening reported twice with different bookingUrls into one (order-independent)" in {
    val early = at("2026-06-11T10:00")
    // Same dateTime, two different per-event booking links.
    val dupA  = withBooking("2026-06-11T12:00", "https://nh/bilet.s?eventId=194388")
    val dupB  = withBooking("2026-06-11T12:00", "https://nh/bilet.s?eventId=193538")

    def merge(first: Showtime, second: Showtime): Seq[Showtime] = {
      val a = canonical.copy(data = canonical.data + ((Multikino: Source) -> slot("a.jpg", showtimes = Seq(early, first))))
      val b = victim.copy(data    = victim.data    + ((Multikino: Source) -> slot("b.jpg", showtimes = Seq(second))))
      MovieRecordMerge.union(a, b).cinemaData(Multikino).showtimes
    }

    val forward = merge(dupA, dupB)
    val reverse = merge(dupB, dupA)

    // The phantom duplicate is gone: one 10:00 + one 12:00.
    forward.map(_.dateTime) shouldBe Seq(
      LocalDateTime.parse("2026-06-11T10:00"),
      LocalDateTime.parse("2026-06-11T12:00")
    )
    // And the result — including which bookingUrl survives — is identical
    // regardless of merge order.
    forward shouldBe reverse
  }

  it should "settle five screenings with two collisions to one deduped slot, whatever the split + order" in {
    // The forward/reverse case above proves order-independence for one collision
    // across two showtimes. But `dedupShowtimes` picks each screening's surviving
    // bookingUrl with `groupBy(identity).minBy(rank)` and re-sorts by time, fed by
    // `canonicalSlot.showtimes ++ victimSlot.showtimes` — so the real exposure is
    // how FIVE screenings (two of them duplicate-identity pairs carrying different
    // booking links) partition across the two rows and order WITHIN each row. A
    // single forward/reverse pair can't cover that. Shuffle all five, split them
    // every which way, and assert the deduped slot — times AND surviving links —
    // is one fixed value across orders.
    val unique = withBooking("2026-06-11T10:00", "https://nh/bilet.s?id=u")
    // 12:00 reported twice with different per-event links; "eventA" < "eventB" so
    // `rank` (non-empty first, then lowest URL) keeps eventA.
    val noonA  = withBooking("2026-06-11T12:00", "https://nh/bilet.s?id=eventA")
    val noonB  = withBooking("2026-06-11T12:00", "https://nh/bilet.s?id=eventB")
    // 14:00 likewise; "c1" < "c2" keeps c1.
    val twoA   = withBooking("2026-06-11T14:00", "https://nh/bilet.s?id=c1")
    val twoB   = withBooking("2026-06-11T14:00", "https://nh/bilet.s?id=c2")
    val all    = Seq(unique, noonA, noonB, twoA, twoB)

    def deduped(first: Seq[Showtime], second: Seq[Showtime]): Seq[Showtime] = {
      val a = canonical.copy(data = canonical.data + ((Multikino: Source) -> slot("a.jpg", showtimes = first)))
      val b = victim.copy(data    = victim.data    + ((Multikino: Source) -> slot("b.jpg", showtimes = second)))
      MovieRecordMerge.union(a, b).cinemaData(Multikino).showtimes
    }

    val expected = Seq(unique, noonA, twoA)   // one per physical screening, lowest-URL link wins, time-sorted

    // 10 shuffles × every split point of the shuffled five → varies both the
    // partition across rows and the concatenation order into `dedupShowtimes`.
    (0 until 10).foreach { seed =>
      val shuffled = new Random(seed).shuffle(all)
      (0 to shuffled.size).foreach { splitAt =>
        val (first, second) = shuffled.splitAt(splitAt)
        withClue(s"seed=$seed split=$splitAt first=${first.map(_.bookingUrl)} second=${second.map(_.bookingUrl)}\n") {
          deduped(first, second) shouldBe expected
        }
      }
    }
  }

  it should "keep the longest retained synopsis per source, order-independently" in {
    val a = canonical.copy(retainedSynopses = Map[Source, String](
      Multikino -> "krótki", Helios -> "helios dłuższy zachowany opis"))
    val b = victim.copy(retainedSynopses = Map[Source, String](
      Multikino -> "multikino znacznie dłuższy zachowany opis", Tmdb -> "tmdb"))

    val expected = Map[Source, String](
      Multikino -> "multikino znacznie dłuższy zachowany opis",  // b's is longer
      Helios    -> "helios dłuższy zachowany opis",
      Tmdb      -> "tmdb")
    MovieRecordMerge.union(a, b).retainedSynopses shouldBe expected
    MovieRecordMerge.union(b, a).retainedSynopses shouldBe expected
  }
}
