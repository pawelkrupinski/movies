package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/** The write-time identity gate under the shapes the 2026-09-07 review named: a re-key
 *  that meets a same-film sibling, a store that cannot be read, a side-row move that
 *  does not land. Each one used to strand a document, mint a second one for a key, or
 *  drop a row out of the index while its document still held the key. */
class FilmIdentityWritesSpec extends AnyFlatSpec with Matchers {

  private def slot(cinema: Cinema, title: String) =
    Map[Source, SourceData]((cinema: Source) -> SourceData(title = Some(title), releaseYear = Some(2026),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 12, 20, 0), None))))

  private def split() = {
    val screenings = new InMemoryScreeningsRepository
    val slots      = new InMemorySlotsRepository
    (screenings, new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots)))
  }

  "a re-key that meets a same-film sibling" should "retire the moved document and carry its screenings to the survivor" in {
    val (screenings, repository) = split()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val moved   = CacheKey("Alpha", None, titleNormalizer)
    val sibling = CacheKey("Alfa", Some(2026), titleNormalizer)
    cache.put(sibling, MovieRecord(tmdbId = Some(4242), data = slot(KinoMuza, "Alfa")))
    cache.put(moved,   MovieRecord(data = slot(Helios, "Alpha")))
    val movedId   = cache.idOf(moved).getOrElse(fail("moved row has no id"))
    val siblingId = cache.idOf(sibling).getOrElse(fail("sibling has no id"))

    // The year concludes and the resolution arrives with it: the retitle target has no
    // row, but a same-tmdbId sibling under another spelling does.
    cache.rekey(moved, CacheKey("Alpha", Some(2026), titleNormalizer), _.copy(tmdbId = Some(4242)), RekeyReason.ResolvedYear)

    repository.findAll().map(_.id) shouldBe Seq(siblingId)                 // the moved document is gone
    screenings.findForFilm(movedId.value) shouldBe empty                     // nothing stranded under it
    screenings.findForFilm(siblingId.value).keySet should have size 2        // both cinemas' boards on the survivor
    cache.idOf(moved) shouldBe None
    cache.entries.map(_._1) should contain only sibling
  }

  "a write on a key the store cannot be asked about" should "be deferred, not written under a second id" in {
    val repository = new UnreadableByIdMovieRepository()
    repository.failing = false
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val key   = CacheKey("Beta", Some(2026), titleNormalizer)
    repository.upsert("Beta", Some(2026), MovieRecord(imdbRating = Some(6.5), data = slot(KinoMuza, "Beta")))
    val stored = repository.findAll().map(_.id)

    repository.failing = true
    cache.put(key, MovieRecord(data = slot(Helios, "Beta")))      // a cold key, and the store is unreadable

    cache.skippedUnreadable.get() shouldBe 1
    cache.get(key) shouldBe empty
    repository.failing = false
    repository.findAll().map(_.id) shouldBe stored                 // still one document, the original
  }

  "a fold whose side-row move does not land" should "leave both rows as they were" in {
    val refusing = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository)) {
      override def moveFilm(oldId: FilmId, newId: FilmId): Boolean = oldId == newId
    }
    val cache = new CaffeineMovieCache(refusing, normalizer = titleNormalizer)
    val a = CacheKey("Gamma", Some(2026), titleNormalizer)
    val b = CacheKey("Gama",  Some(2026), titleNormalizer)
    cache.put(a, MovieRecord(tmdbId = Some(7), data = slot(KinoMuza, "Gamma")))
    cache.put(b, MovieRecord(data = slot(Helios, "Gama")))
    val before = refusing.findAll().map(r => r.id -> r.record.tmdbId).toSet

    cache.put(b, MovieRecord(tmdbId = Some(7), data = slot(Helios, "Gama")))   // resolves to a's film

    cache.skippedUnreadable.get() shouldBe 1
    refusing.findAll().map(r => r.id -> r.record.tmdbId).toSet shouldBe before
    cache.entries.map(_._1).toSet shouldBe Set(a, b)
  }

  "a different film wanting a key another film holds" should "stay where it is, not become a second document under that key" in {
    val (_, repository) = split()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val held     = CacheKey("Beta", Some(2026), titleNormalizer)
    val yearless = CacheKey("Beta", None, titleNormalizer)
    cache.put(held,     MovieRecord(tmdbId = Some(1), data = slot(KinoMuza, "Beta") + ((Tmdb: Source) -> SourceData(title = Some("Beta"), releaseYear = Some(2026)))))
    cache.put(yearless, MovieRecord(tmdbId = Some(2), data = slot(Helios, "Beta")   + ((Tmdb: Source) -> SourceData(title = Some("Beta"), releaseYear = Some(2026)))))
    val ids = repository.findAll().map(_.id).toSet

    cache.canonicalizeBySanitize()      // wants to re-key the yearless film onto 2026, which the other film holds

    cache.keyCollisions.get() should be >= 1L
    repository.findAll().map(_.id).toSet shouldBe ids
    repository.findAll().groupBy(_.key(titleNormalizer)).values.map(_.size).max shouldBe 1
    cache.entries.map(_._1).toSet shouldBe Set(held, yearless)
  }

  it should "merge under the sibling's current key when the vote's canonical key is another film's" in {
    // The property's second find: a same-film fold whose `canonical` vote lands on a key a
    // third film holds. The two rows are still one film and still merge — under the
    // sibling's current key.
    val (_, repository) = split()
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val other   = CacheKey("Alpha", Some(2025), titleNormalizer)          // film 2 at the key the vote will want
    val sibling = CacheKey("Alpha", Some(2026), titleNormalizer)          // film 1
    val arriving = CacheKey("Gamma", Some(2025), titleNormalizer)         // film 1 again, under another title
    cache.put(other,    MovieRecord(tmdbId = Some(2), data = slot(KinoMuza, "Alpha") + ((Tmdb: Source) -> SourceData(title = Some("Alpha"), releaseYear = Some(2025)))))
    cache.put(sibling,  MovieRecord(tmdbId = Some(1), data = slot(Helios, "Alpha")   + ((Tmdb: Source) -> SourceData(title = Some("Alpha"), releaseYear = Some(2026)))))
    val otherId = cache.idOf(other).get; val siblingId = cache.idOf(sibling).get

    cache.put(arriving, MovieRecord(tmdbId = Some(1), data = slot(Helios, "Gamma") + ((Tmdb: Source) -> SourceData(title = Some("Gamma"), releaseYear = Some(2025)))))

    val rows = repository.findAll()
    rows.map(_.id).toSet shouldBe Set(otherId, siblingId)
    rows.groupBy(_.key(titleNormalizer)).values.map(_.size).max shouldBe 1
    cache.get(other).flatMap(_.tmdbId) shouldBe Some(2)
    cache.idOf(sibling) shouldBe Some(siblingId)
  }
}
