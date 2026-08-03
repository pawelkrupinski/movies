package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * One corpus, every state the `sourceData` split can leave a film in, driven through the
 * cycle prod runs — scrape, settle, re-scrape, settle — asserting the two properties that
 * were violated across 2026-07-26/28: nothing loses its cinemas or showtimes, and a
 * settled corpus stops writing.
 *
 * The individual regressions have their own specs; this one exists because none of them
 * failed while prod bled. Each fix was proved in isolation and the CORPUS was still
 * losing data, because films are not in one state — they are mid-migration, in every
 * combination at once:
 *
 *   - MIGRATED     — embedded map gone (`$unset`), cinemas only in `movie_slots`
 *   - UN-MIGRATED  — cinemas only in the embedded `movies.sourceData`
 *   - MIXED        — both populated and DISAGREEING, which is what 81 of 82 prod films
 *                    looked like and what made "stored rows win" drop 14 films' cinemas
 *   - RE-SPELLED   — stored spelling differs from canonical, so the settle rewrites it
 *   - DUPLICATED   — two keys, one film, so the settle folds and deletes a victim
 *
 * Every one of those goes through a settle that moves rows, and every move is a rename
 * whose side rows must travel with it. Anything that reads a film's cinemas from one store
 * while another holds them, or deletes a row it is about to rewrite, shows up here as a
 * missing showtime rather than as a passing unit test.
 */
class SourceDataConvergenceSpec extends AnyFlatSpec with Matchers {

  private val when = LocalDateTime.of(2026, 8, 1, 20, 0)
  private def showtime = Seq(Showtime(when, None))

  private def slot(title: String, times: Seq[Showtime] = showtime) =
    SourceData(title = Some(title), showtimes = times)

  private def fixture = {
    val screenings = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings))
    (screenings, repository, new CaffeineMovieCache(repository))
  }


  private def totalShowtimes(s: InMemoryScreeningsRepository): Int =
    s.findAll().values.flatMap(_.values).map(_.size).sum

  /** The corpus, in every migration state at once. */
  private def seedCorpus(cache: CaffeineMovieCache): Unit = {
    // MIGRATED + UN-MIGRATED both look the same through the cache API — the repository
    // routes showtimes into `screenings` either way. What differs is the settle they meet.
    cache.put(CacheKey("Migrated", Some(2026), titleNormalizer),
      MovieRecord(tmdbId = Some(101), data = Map[Source, SourceData](Multikino -> slot("Migrated"))))
    cache.put(CacheKey("Unmigrated", Some(2026), titleNormalizer),
      MovieRecord(tmdbId = Some(102), data = Map[Source, SourceData](Helios -> slot("Unmigrated"))))
    // MIXED: two cinemas on one film, the shape that made a shadowing read drop one.
    cache.put(CacheKey("Mixed", Some(2026), titleNormalizer),
      MovieRecord(tmdbId = Some(103), data = Map[Source, SourceData](
        Multikino -> slot("Mixed"), KinoMuranow -> slot("Mixed"))))
    // RE-SPELLED: stored all-caps, canonical prefers title case, so the settle rewrites it.
    cache.put(CacheKey("RESPELLED", Some(2026), titleNormalizer),
      MovieRecord(tmdbId = Some(104), data = Map[Source, SourceData](Helios -> slot("RESPELLED"))))
    // DUPLICATED: two keys sharing a tmdbId — the settle folds one into the other.
    cache.put(CacheKey("Duplicated", None, titleNormalizer),
      MovieRecord(tmdbId = Some(105), data = Map[Source, SourceData](Multikino -> slot("Duplicated"))))
    cache.put(CacheKey("Duplicated", Some(2026), titleNormalizer),
      MovieRecord(tmdbId = Some(105), data = Map[Source, SourceData](KinoMuza -> slot("Duplicated"))))
  }

  "a corpus in every migration state" should "keep every showtime through a settle" in {
    val (screenings, _, cache) = fixture
    seedCorpus(cache)
    val before = totalShowtimes(screenings)
    withClue("premise — the seeded corpus has showtimes: ")(before shouldBe 7)

    cache.canonicalizeBySanitize()

    withClue(s"the settle lost showtimes: $before -> ${totalShowtimes(screenings)}\n" +
             s"  per film: ${screenings.findAll().view.mapValues(_.values.map(_.size).sum).toMap}\n")(
      totalShowtimes(screenings) shouldBe before)
  }

  it should "keep every CINEMA reachable, wherever the settle moved the row" in {
    val (screenings, _, cache) = fixture
    seedCorpus(cache)
    cache.canonicalizeBySanitize()

    // A fold unions cinemas; nothing may be dropped just because its row lost the key race.
    val cinemas = screenings.findAll().values.flatMap(_.keySet).toSet
    withClue(s"cinemas still holding screenings: $cinemas\n") {
      cinemas should contain (Multikino.displayName)
      cinemas should contain (Helios.displayName)
      cinemas should contain (KinoMuranow.displayName)
      cinemas should contain (KinoMuza.displayName)   // the fold VICTIM's cinema
    }
  }

  it should "be quiescent — a second settle writes nothing" in {
    val (_, repository, cache) = fixture
    seedCorpus(cache)
    cache.canonicalizeBySanitize()

    val writesBefore = repository.upserts.size + repository.deletes.size
    cache.canonicalizeBySanitize()
    val writes = repository.upserts.size + repository.deletes.size - writesBefore

    withClue(s"a settled corpus rewrote itself: $writes write(s)\n")(writes shouldBe 0)
  }

  it should "survive the full prod cycle — scrape, settle, re-scrape, settle" in {
    val (screenings, repository, cache) = fixture
    seedCorpus(cache)
    cache.canonicalizeBySanitize()
    val settled = totalShowtimes(screenings)

    // The next prod tick: each cinema reports its WHOLE listing once, then the settle runs.
    //
    // Grouping by cinema is not cosmetic. `recordCinemaScrape` treats its argument as that
    // venue's complete board and PRUNES the venue's slots from any film the call omits —
    // so one call per (cinema, film) would have each call prune the previous one's work.
    // That is correct behaviour, and it is why a partial listing is so dangerous; see
    // `PartialReducePruneSpec`.
    Map(
      // "Duplicated" is on Multikino's board because the SEED put it there (the yearless
      // row). A cinema's listing is its whole board, so omitting it here would be Multikino
      // reporting that it has stopped showing the film — a correct prune, but not the
      // scenario this is testing.
      Multikino   -> Seq("Migrated", "Mixed", "Duplicated"),
      Helios      -> Seq("Unmigrated", "Respelled"),
      KinoMuranow -> Seq("Mixed"),
      KinoMuza    -> Seq("Duplicated")
    ).foreach { case (cinema, titles) =>
      cache.recordCinemaScrape(cinema, titles.map(title => CinemaMovie(
        movie = Movie(title, releaseYear = Some(2026)), cinema = cinema, posterUrl = None,
        filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = showtime)))
    }
    cache.canonicalizeBySanitize()

    withClue(s"a re-scrape+settle cycle lost showtimes: $settled -> ${totalShowtimes(screenings)}\n")(
      totalShowtimes(screenings) should be >= settled)
    withClue("the cycle emptied a film: ")(
      screenings.findAll().values.foreach(_.values.foreach(_ should not be empty)))
    // and no film ended up with nothing at all
    repository.findAll().foreach { row =>
      withClue(s"'${row.title}' (${row.year.getOrElse("—")}) ended the cycle with no cinemas: ")(
        row.record.data should not be empty)
    }
  }
}
