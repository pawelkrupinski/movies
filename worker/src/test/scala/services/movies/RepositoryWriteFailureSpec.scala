package services.movies

import models._
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/**
 * A repository write that THROWS must be counted, and must not leave the cache holding a
 * row the store never took.
 *
 * 2026-09-24: a codec bug made `MovieRepository.upsert`, `SlotsRepository.replaceFilm` and
 * `upsertSlot` throw for ~6h (34 failures, 29 films). The repositories logged a WARN and
 * returned `Unit`; `MovieCache.persist` had already cached the row, so every later
 * identical scrape diffed as a no-op and the write was never retried. Two new films never
 * reached the site until a restart, and no metric moved.
 */
class RepositoryWriteFailureSpec extends AnyFlatSpec with Matchers with LoneElement {

  private class RecordingWriteMetrics extends RepositoryWriteMetrics {
    @volatile var failures: Vector[(String, String, String)] = Vector.empty
    def recordWriteFailed(collection: String, op: String, exception: String): Unit =
      failures :+= ((collection, op, exception))
  }

  /** Named, not anonymous: an anonymous subclass capturing a local `var` trips a JVM
   *  VerifyError under this Scala version (see UnreadableRowScrapeSpec). */
  private class UnmovableThrowingRepository(metrics: RepositoryWriteMetrics) extends ThrowingUpsertMovieRepository(metrics, titleNormalizer = titleNormalizer) {
    @volatile var canMoveFilm = true
    override def moveFilm(oldId: FilmId, newId: FilmId): Boolean = oldId == newId || canMoveFilm
  }


  // The caches run at a fixed instant and the showtime sits a day after it.
  private val specClock = java.time.Clock.fixed(java.time.Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val showtime  = Showtime(LocalDateTime.now(specClock).plusDays(1).withHour(20), bookingUrl = None)

  private def listing(title: String) = CinemaMovie(
    movie = Movie(title, releaseYear = Some(2026)), cinema = Multikino,
    posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Seq(showtime))

  "a new film whose upsert throws" should "be counted, rolled out of the cache, and written by the next identical scrape" in {
    val metrics    = new RecordingWriteMetrics
    val repository = new ThrowingUpsertMovieRepository(metrics, titleNormalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val key        = cache.keyOf("Nowy Film", Some(2026))

    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    metrics.failures shouldBe Vector((MovieRepository.Collection, "upsert", "CodecConfigurationException"))
    withClue("a row the store never took must not stay resident — the next scrape would diff it as a no-op: ")(
      cache.get(key) shouldBe None)
    repository.findAll() shouldBe empty

    repository.failing = false
    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    withClue("the identical re-scrape must retry the write, not skip it: ")(
      repository.findAll().map(_.title) shouldBe Seq("Nowy Film"))
    cache.get(key) should not be empty
  }

  // Declined is not written either: `movies` refused the row because another document holds
  // its key or tmdbId. Kept resident, the unwritten row made every identical re-scrape diff as a
  // no-op, exactly as a thrown write did — so it is rolled back the same way.
  "a new film whose upsert is declined for a held identity" should "be rolled out of the cache, and written by the next identical scrape" in {
    val repository = new IdentityHeldMovieRepository(titleNormalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val key        = cache.keyOf("Nowy Film", Some(2026))

    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    withClue("a row the store declined must not stay resident: ")(cache.get(key) shouldBe None)
    repository.findAll() shouldBe empty

    repository.declining = false
    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    withClue("the identical re-scrape must retry the write, not skip it: ")(
      repository.findAll().map(_.title) shouldBe Seq("Nowy Film"))
  }

  "a retitle whose write is declined for a held identity" should "leave the row resident under its old key" in {
    val repository = new IdentityHeldMovieRepository(titleNormalizer = titleNormalizer)
    repository.declining = false
    val cache  = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val before = cache.keyOf("Mroz", None)
    cache.put(before, MovieRecord(imdbId = Some("tt0000001")))

    repository.declining = true
    cache.rekey(before, cache.keyOf("Mroz", Some(2026)), identity, RekeyReason.ResolvedYear)

    withClue("Mongo still holds the row under its old key, so the cache must too: ")(
      cache.get(before).flatMap(_.imdbId) shouldBe Some("tt0000001"))
  }

  "an existing film whose new slot's write throws" should "keep the pre-update row, and land the slot on the next identical scrape" in {
    val metrics    = new RecordingWriteMetrics
    val slots      = new ThrowingSlotsRepository(metrics)
    slots.failing  = false
    val repository = new InMemoryMovieRepository(
      screenings = Some(new InMemoryScreeningsRepository), slots = Some(slots), normalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    cache.recordCinemaScrape(Multikino, Seq(listing("Stary Film")))
    val key        = cache.keyOf("Stary Film", Some(2026))
    val id         = repository.findAll().map(_.id).loneElement
    def heliosSlots = slots.findForFilm(id.value).keySet.filter(_.startsWith(Helios.displayName))

    slots.failing = true
    cache.recordCinemaScrape(Helios, Seq(listing("Stary Film")))

    metrics.failures.map(_._1) should contain only SlotsRepository.Collection
    heliosSlots shouldBe empty
    withClue("the cache must not hold the Helios slot the store never took: ")(
      cache.get(key).toSeq.flatMap(_.data.keys).flatMap(Source.cinemaOf) should not contain Helios)

    slots.failing = false
    cache.recordCinemaScrape(Helios, Seq(listing("Stary Film")))

    withClue("the identical re-scrape must retry the slot write, not skip it: ")(
      heliosSlots should not be empty)
  }

  "a tmdbId fold whose survivor write fails" should "keep the victim's document, so nothing only it held is lost" in {
    val metrics    = new RecordingWriteMetrics
    val repository = new ThrowingUpsertMovieRepository(metrics, titleNormalizer = titleNormalizer)
    repository.failing = false
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val survivor   = cache.keyOf("Survivor Film", Some(2026))
    val victim     = cache.keyOf("Victim Film", Some(2026))
    cache.put(survivor, MovieRecord(tmdbId = Some(4242)))
    cache.put(victim, MovieRecord(imdbId = Some("tt0004242")))
    repository.findAll() should have size 2

    repository.failing = true
    cache.put(victim, MovieRecord(imdbId = Some("tt0004242"), tmdbId = Some(4242)))

    metrics.failures.map(_._2) should contain ("upsert")
    withClue("the victim may only be deleted once the survivor carries its fields: ")(
      repository.findAll().map(_.record.imdbId).toSet shouldBe Set(None, Some("tt0004242")))

    repository.failing = false
    cache.put(victim, MovieRecord(imdbId = Some("tt0004242"), tmdbId = Some(4242)))

    val rows = repository.findAll()
    withClue(s"the retried fold must land: ${rows.map(r => r.title -> r.record.imdbId)}: ")(
      rows.map(_.record.imdbId) shouldBe Seq(Some("tt0004242")))
  }

  // The slot MOVE drops this (cinema, title)'s slot from every OTHER row once this tick has
  // decided which film it belongs to — right once the slot has landed on that film, fatal when
  // the write failed: the venue's showtimes are then deleted from the old row and put nowhere.
  "a first-time write that fails" should "not strip the venue's slot off the row that still holds it" in {
    val metrics    = new RecordingWriteMetrics
    val repository = new UnmovableThrowingRepository(metrics)
    repository.failing = false
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    def listingOf(title: String, year: Option[Int]) = listing(title).copy(movie = Movie(title, releaseYear = year))
    // A yearless row holding Multikino's slot, beside nine fillers so the degraded tick below
    // is a shrink the prune stands down for (see UnreadableRowScrapeSpec's twin of this case).
    cache.recordCinemaScrape(Multikino,
      listingOf("Zaproszenie", None) +: (1 to 9).map(i => listingOf(s"Filler $i", None)))
    val holder = cache.keyOf("Zaproszenie", None)
    def multikinoOnHolder = cache.get(holder).toSeq.flatMap(_.data.keys).flatMap(Source.cinemaOf)
    multikinoOnHolder should contain (Multikino)

    // The redirect's `rekey` onto the 2026 key defers on a move that does not land, so the
    // tick stands on a key in neither cache nor index and takes the first-time `put` branch.
    repository.canMoveFilm = false
    repository.failing     = true
    cache.recordCinemaScrape(Multikino, Seq(listingOf("Zaproszenie", Some(2026))))

    metrics.failures.map(_._2) should contain ("upsert")
    withClue(s"rows: ${repository.findAll().map(r => r.title -> r.record.data.keySet)}; the slot never landed " +
      "anywhere new, so the row holding it must keep it: ")(
      multikinoOnHolder should contain (Multikino))
  }

  // The same move after a first-time write `movies` DECLINED: nothing threw, but the slot is
  // stored nowhere new, so stripping it off the row that holds it deletes the venue's showtimes.
  "a first-time write that is declined" should "not strip the venue's slot off the row that still holds it" in {
    val repository = new IdentityHeldMovieRepository(titleNormalizer = titleNormalizer)
    repository.declining = false
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    def listingOf(title: String, year: Option[Int]) = listing(title).copy(movie = Movie(title, releaseYear = year))
    cache.recordCinemaScrape(Multikino,
      listingOf("Zaproszenie", None) +: (1 to 9).map(i => listingOf(s"Filler $i", None)))
    def storedZaproszenieCinemas = repository.findAll().filter(_.title == "Zaproszenie")
      .flatMap(_.record.data.keys).flatMap(Source.cinemaOf)
    storedZaproszenieCinemas should contain (Multikino)

    repository.canMoveFilm = false
    repository.declining   = true
    cache.recordCinemaScrape(Multikino, Seq(listingOf("Zaproszenie", Some(2026))))

    withClue(s"rows: ${repository.findAll().map(r => (r.title, r.year, r.record.data.keySet))}; the slot landed " +
      "nowhere new, so the stored row holding it must keep it: ")(
      storedZaproszenieCinemas should contain (Multikino))
  }
}
