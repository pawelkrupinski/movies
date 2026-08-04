package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{CinemaMovie, Helios, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The write-side half of "a failed read is not an absent row" — and the mechanism that
 * emptied every country's board on 2026-07-27 while the film counts stayed flat.
 *
 * A scrape whose film is not in the cache builds its merge base from `stored(key)`, and
 * `findById` reported both "no such film" and "I could not read it" as `None`. On the
 * second, the scrape rebuilt a LIVE film from scratch, so the record carried only the
 * cinema being scraped; `MovieRepository.upsert` then wrote that as the whole film and
 * `screenings.replaceFilm` pruned every other cinema's showtimes with its `$nin`.
 *
 * The blast radius is what makes it worth a spec of its own: Caffeine is empty after every
 * restart, so EVERY film takes this branch, and one unreadable read source costs the whole
 * corpus its showtimes. That is exactly what the prod logs showed — page after page of
 * `MovieRepository.findById(…) failed` while showtime volume fell to a third.
 */
class UnreadableRowScrapeSpec extends AnyFlatSpec with Matchers {

  private val showtime = Showtime(LocalDateTime.now().plusDays(1).withHour(20), bookingUrl = None)

  /** A film already in `movies` showing at TWO cinemas — the state a scrape must not undo. */
  private def liveFilm = MovieRecord(
    tmdbId = Some(42),
    data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Live Film"), showtimes = Seq(showtime)),
      Helios    -> SourceData(title = Some("Live Film"), showtimes = Seq(showtime))))

  /** Records what the cache writes back, and can be told to fail its per-row read — the
   *  one thing that separates "absent" from "unreadable". */
  private class Repo(rows: Seq[StoredMovieRecord], readable: Boolean) extends MovieRepository {
    val upserts = scala.collection.mutable.ListBuffer.empty[(String, MovieRecord)]
    def enabled = true
    // EMPTY — `findAll` returns `Seq.empty` on an incomplete scan, so the boot hydrate
    // leaves the cache cold even though the corpus is full. That is not a contrivance:
    // it is precisely the prod state, where the same decode failure broke the corpus scan
    // AND the per-row read at once.
    def findAll() = Seq.empty
    override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) =
      if (!readable) (None, false)
      else (rows.find(r => StoredMovieRecord.idOf(r, titleNormalizer) == id), true)
    def delete(t: String, y: Option[Int]) = ()
    def deleteById(id: String) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = { upserts += ((t, e)); () }
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private val stored = StoredMovieRecord("Live Film", Some(2026), liveFilm)

  /** One cinema's scrape of `title` — the Multikino listing that lands on a cold cache. */
  private def cinemaMovie(title: String) = CinemaMovie(
    movie = models.Movie(title, releaseYear = Some(2026)), cinema = Multikino, posterUrl = None, filmUrl = None,
    synopsis = None, cast = Seq.empty, director = Seq.empty, showtimes = Seq(showtime))

  "a scrape landing on a film whose stored row cannot be READ" should
    "not rewrite that film as if only this cinema showed it" in {
    val repo  = new Repo(Seq(stored), readable = false)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film")))

    // Nothing may be written. Any upsert here carries ONLY Multikino, and `upsert` hands
    // that to `screenings.replaceFilm`, whose `$nin` deletes Helios' showtimes.
    withClue(s"wrote ${repo.upserts.map { case (t, r) => s"$t -> ${r.data.keySet}" }}: ")(
      repo.upserts.filter { case (_, r) => !r.data.contains(Helios) } shouldBe empty)
    cache.skippedUnreadable.get() should be > 0L
  }

  // The other half of the contract: an unreadable read must not become a licence to stop
  // scraping. A row that is genuinely ABSENT is a real newcomer and must still be written.
  it should "still record a genuinely new film when the read succeeded and found nothing" in {
    val repo  = new Repo(Seq.empty, readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Brand New")))

    repo.upserts.map(_._1)      should contain ("Brand New")
    cache.skippedUnreadable.get() shouldBe 0L
  }

  // `rekey` reads the same way and writes the result back under a new key, so an
  // unreadable row there is re-`put` with neither ratings nor cinemas — and `upsert`
  // prunes the film's whole board off the back of it. Deferring costs one settle tick.
  "a re-key whose stored row cannot be READ" should "be deferred, not written from nothing" in {
    val repo  = new Repo(Seq(stored), readable = false)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)

    cache.rekey(CacheKey("Live Film", Some(2026), titleNormalizer), CacheKey("Live Film", Some(2027), titleNormalizer), identity)

    withClue(s"wrote ${repo.upserts.map { case (t, r) => s"$t -> ${r.data.keySet}" }}: ")(
      repo.upserts shouldBe empty)
    cache.skippedUnreadable.get() should be > 0L
  }

  it should "still re-key normally when the row reads back" in {
    val repo  = new Repo(Seq(stored), readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)

    cache.rekey(CacheKey("Live Film", Some(2026), titleNormalizer), CacheKey("Live Film", Some(2027), titleNormalizer), identity)

    val written = repo.upserts.map(_._2)
    withClue(s"wrote ${written.map(_.data.keySet)}: ")(
      written.exists(r => r.data.contains(Helios) && r.data.contains(Multikino)) shouldBe true)
  }

  // And a readable stored row must still merge, keeping the cinemas it already had — the
  // behaviour the guard must not cost us.
  it should "merge onto the stored row when the read succeeded, keeping the other cinemas" in {
    val repo  = new Repo(Seq(stored), readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film")))

    val written = repo.upserts.map(_._2)
    withClue(s"wrote ${written.map(_.data.keySet)}: ")(
      written.exists(_.data.contains(Helios)) shouldBe true)
  }
}
