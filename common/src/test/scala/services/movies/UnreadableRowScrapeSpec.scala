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
  // EMPTY `findAll` — it returns `Seq.empty` on an incomplete scan, so the boot hydrate
  // leaves the cache cold even though the corpus is full. That is not a contrivance: it
  // is precisely the prod state, where the same decode failure broke the corpus scan AND
  // the per-row read at once.
  private class Repo(rows: Seq[StoredMovieRecord], var readable: Boolean,
                     var canMoveFilm: Boolean = true) extends StoredRowsRepository(Seq.empty, titleNormalizer) {
    override def moveFilm(oldId: FilmId, newId: FilmId): Boolean = oldId == newId || canMoveFilm
    override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) =
      if (!readable) (None, false) else (rows.find(_.id == id), true)
    override def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) =
      if (!readable) (None, false) else (rows.find(_.key(normalizer) == StoredMovieRecord.idFor(key)), true)
  }

  /** A cache whose row-update reports failure on demand — what a lost race looks like
   *  to the caller. Named rather than anonymous: an anonymous subclass capturing a
   *  local `var` trips a JVM VerifyError under this Scala version. */
  private class LosesWrites(repo: MovieRepository) extends CaffeineMovieCache(repo, normalizer = titleNormalizer) {
    var loseWrites = false
    override private[services] def putIfPresent(
      key: CacheKey, updater: MovieRecord => MovieRecord): Boolean = {
      val landed = super.putIfPresent(key, updater)
      !loseWrites && landed
    }
  }

  private val stored = StoredMovieRecord("Live Film", Some(2026), liveFilm)

  /** One cinema's scrape of `title` — the Multikino listing that lands on a cold cache. */
  private def cinemaMovie(title: String, year: Int = 2026) = CinemaMovie(
    movie = models.Movie(title, releaseYear = Option.when(year != 0)(year)), cinema = Multikino,
    posterUrl = None, filmUrl = None,
    synopsis = None, cast = Seq.empty, director = Seq.empty, showtimes = Seq(showtime))

  "a scrape landing on a film whose stored row cannot be READ" should
    "not rewrite that film as if only this cinema showed it" in {
    val repo  = new Repo(Seq(stored), readable = false)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film")))

    // Nothing may be written. Any upsert here carries ONLY Multikino, and `upsert` hands
    // that to `screenings.replaceFilm`, whose `$nin` deletes Helios' showtimes.
    withClue(s"wrote ${repo.upserts.map { case (_, t, r) => s"$t -> ${r.data.keySet}" }}: ")(
      repo.upserts.filter { case (_, _, r) => !r.data.contains(Helios) } shouldBe empty)
    cache.skippedUnreadable.get() should be > 0L
  }

  // The slot MOVE, next to this skip. Deciding which film a (cinema, title) belongs to
  // drops the slot from every OTHER row holding it — right once the slot has landed,
  // fatal when it has not.
  //
  // Reaching "has not" needs the redirect arm: it returns `canonical`, which can be
  // `primary` — a key it has just proved is a Caffeine MISS — and returns it whether
  // or not the `rekey` onto it landed. `rekey` defers silently on an unreadable row
  // and on a failing `moveFilm`, so a degraded Mongo leaves the loop standing on a key
  // in neither cache nor index, while `keysForCinemaSlot` still names the row holding
  // the slot. Ungated, the drop deletes this venue's showtimes and puts them nowhere.
  it should "not strip the venue's slot off the row that still holds it" in {
    val repo  = new Repo(Seq.empty, readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    // A yearless row holding Multikino's slot — what the redirect will try to promote.
    // Alongside nine filler films, so the degraded tick below is a SHRINK the prune
    // stands down for (`MinSlotsForShrinkGuard` 8, `PruneFloorRatio` 0.5). Otherwise
    // the prune legitimately removes the stale slot and hides what the move did.
    cache.recordCinemaScrape(Multikino,
      cinemaMovie("Live Film", year = 0) +: (1 to 9).map(i => cinemaMovie(s"Filler $i", year = 0)))
    val holder = cache.keyOf("Live Film", None)
    def multikinoSlots = cache.get(holder).toSeq
      .flatMap(_.data.keys.filter(Source.cinemaOf(_).contains(Multikino)))
    withClue("the seed scrape must leave the yearless row holding Multikino's slot: ")(
      multikinoSlots should not be empty)

    // Mongo goes bad, then the venue republishes the same title WITH a year. The
    // redirect picks the year-bearing key as canonical and asks for a re-key onto it.
    // A re-key is a RETITLE now — the film keeps its id, so no side-collection move is
    // needed and an unmovable store cannot block it — and the resident row is what it
    // retitles. Whichever key the film ends up under, the venue's slot must be on it.
    repo.readable    = false
    repo.canMoveFilm = false
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film", year = 2027)))

    val retitled = cache.keyOf("Live Film", Some(2027))
    def slotsOn(key: CacheKey) = cache.get(key).toSeq.flatMap(_.data.keys.filter(Source.cinemaOf(_).contains(Multikino)))
    withClue(s"skipped=${cache.skippedUnreadable.get()}, holder=${cache.get(holder).map(_.data.keySet)}, " +
             s"retitled=${cache.get(retitled).map(_.data.keySet)}: ")(
      (slotsOn(holder) ++ slotsOn(retitled)) should not be empty)
    withClue("a retitle keeps the film's id: ")(
      cache.idOf(retitled).orElse(cache.idOf(holder)) should not be empty)
  }

  // The other half of the contract: an unreadable read must not become a licence to stop
  // scraping. A row that is genuinely ABSENT is a real newcomer and must still be written.
  it should "still record a genuinely new film when the read succeeded and found nothing" in {
    val repo  = new Repo(Seq.empty, readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Brand New")))

    repo.upserts.map(_._2)      should contain ("Brand New")
    cache.skippedUnreadable.get() shouldBe 0L
  }

  // `rekey` reads the same way and writes the result back under a new key, so an
  // unreadable row there is re-`put` with neither ratings nor cinemas — and `upsert`
  // prunes the film's whole board off the back of it. Deferring costs one settle tick.
  "a re-key whose stored row cannot be READ" should "be deferred, not written from nothing" in {
    val repo  = new Repo(Seq(stored), readable = false)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)

    cache.rekey(CacheKey("Live Film", Some(2026), titleNormalizer), CacheKey("Live Film", Some(2027), titleNormalizer), identity, services.movies.RekeyReason.Canonicalize)

    withClue(s"wrote ${repo.upserts.map { case (_, t, r) => s"$t -> ${r.data.keySet}" }}: ")(
      repo.upserts shouldBe empty)
    cache.skippedUnreadable.get() should be > 0L
  }

  it should "still re-key normally when the row reads back" in {
    val repo  = new Repo(Seq(stored), readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)

    cache.rekey(CacheKey("Live Film", Some(2026), titleNormalizer), CacheKey("Live Film", Some(2027), titleNormalizer), identity, services.movies.RekeyReason.Canonicalize)

    val written = repo.upserts.map(_._3)
    withClue(s"wrote ${written.map(_.data.keySet)}: ")(
      written.exists(r => r.data.contains(Helios) && r.data.contains(Multikino)) shouldBe true)
  }

  // And a readable stored row must still merge, keeping the cinemas it already had — the
  // behaviour the guard must not cost us.
  it should "merge onto the stored row when the read succeeded, keeping the other cinemas" in {
    val repo  = new Repo(Seq(stored), readable = true)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer)
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film")))

    val written = repo.upserts.map(_._3)
    withClue(s"wrote ${written.map(_.data.keySet)}: ")(
      written.exists(_.data.contains(Helios)) shouldBe true)
  }

  // THE END-OF-TICK PRUNE, asked the same question the move above was. `resolved`
  // collects only writes that LANDED, so a skipped write leaves the venue's slot out
  // of `touchedSlots` — and the prune reads that absence as "the venue stopped
  // listing this title" and drops the slot, deleting showtimes this very scrape saw.
  // One known slot keeps the tick off the shrink guard (`MinSlotsForShrinkGuard` is
  // 8), so the prune genuinely runs rather than standing down for its own reasons.
  it should "not prune the venue's slot for a title it listed but could not write" in {
    val cache = new LosesWrites(new Repo(Seq.empty, readable = true))
    val key   = cache.keyOf("Live Film", Some(2026))
    cache.put(key, MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Live Film"), showtimes = Seq(showtime)))))
    def venueSlots = cache.get(key).toSeq.flatMap(_.data.keys.filter(Source.cinemaOf(_).contains(Multikino)))
    venueSlots should not be empty

    // The venue lists exactly the title it already holds a slot for, and the write is lost.
    cache.loseWrites = true
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film", year = 2026)))

    withClue(s"row=${cache.get(key).map(_.data.keySet)}: ")(venueSlots should not be empty)
  }

  // The OTHER way the write can fail to land, which the unreadable-row case cannot
  // reach: the row IS in Caffeine when the loop looks, and gone by the time
  // `putIfPresent` computes — a concurrent `rekey` of a DIFFERENT title invalidates
  // keys without holding this title's lock. The gate has to read the write's own
  // answer for that; assuming `true` because the key was present a moment ago is the
  // same data loss with a narrower window.
  it should "not strip the slot when the write itself reports it did not land" in {
    val cache = new LosesWrites(new Repo(Seq.empty, readable = true))
    // TWO rows carrying this venue's slot for the same title, so the scrape lands on
    // one (the canonical) and the move would drop the other.
    val slot  = SourceData(title = Some("Live Film"), showtimes = Seq(showtime))
    val keep  = cache.keyOf("Live Film", Some(2026))
    val other = cache.keyOf("Live Film", Some(2027))
    cache.put(keep,  MovieRecord(data = Map[Source, SourceData](Multikino -> slot)))
    cache.put(other, MovieRecord(data = Map[Source, SourceData](Multikino -> slot)))
    // Filler so the one-film tick below is a SHRINK the end-of-tick prune stands down
    // for (`MinSlotsForShrinkGuard` 8, `PruneFloorRatio` 0.5). Without it the prune
    // removes the slot for its own good reasons and hides what the move did.
    (1 to 9).foreach { i =>
      cache.put(cache.keyOf(s"Filler $i", Some(2026)), MovieRecord(data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some(s"Filler $i"), showtimes = Seq(showtime)))))
    }
    def slotsOn(k: CacheKey) = cache.get(k).toSeq
      .flatMap(_.data.keys.filter(Source.cinemaOf(_).contains(Multikino)))
    withClue("both seeded rows must hold the venue's slot: ") {
      slotsOn(keep)  should not be empty
      slotsOn(other) should not be empty
    }

    // The write into the canonical row reports that it did not land.
    cache.loseWrites = true
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Live Film", year = 2026)))

    // `other` is the row the move would strip. Asserting the UNION passes even when it
    // is stripped, because `keep` still has its own slot — which is how this test first
    // went green against the very bug it is for.
    withClue(s"keep=${cache.get(keep).map(_.data.keySet)} other=${cache.get(other).map(_.data.keySet)}: ")(
      slotsOn(other) should not be empty)
  }
}
