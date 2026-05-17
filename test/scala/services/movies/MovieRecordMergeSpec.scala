package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

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
    cinemaScrapes = Set(CinemaScrape(Multikino, "Foo", Some(2024))),
    data = Map[Source, SourceData](
      Multikino -> slot("m.jpg"),
      Tmdb      -> SourceData(originalTitle = Some("Canonical"))
    )
  )
  private val victim = MovieRecord(
    imdbId        = Some("tt-stale"),
    imdbRating    = Some(1.0),
    metascore     = None,
    tmdbId        = Some(42),
    cinemaScrapes  = Set(CinemaScrape(Helios, "Foo", None)),
    data = Map[Source, SourceData](
      Helios -> slot("h.jpg"),
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

  it should "union cinemaScrapes from both rows" in {
    val merged = MovieRecordMerge.union(canonical, victim)
    merged.cinemaScrapes shouldBe Set(
      CinemaScrape(Multikino, "Foo", Some(2024)),
      CinemaScrape(Helios,    "Foo", None)
    )
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
}
