package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The settle moves rows around — folding duplicates onto one key, re-keying a yearless row
 * onto its TMDB year. Under the production storage split a film's SHOWTIMES do not live in
 * the row; they live in `screenings` under the film's id. So every one of those moves is a
 * rename that has to carry the showtimes with it, and any that doesn't destroys them.
 *
 * It happened twice in one day, at two call sites, and cost the boards: prod shed ~10k
 * upcoming showtimes per 30-minute settle in PL alone, films left intact and rebuilt only
 * by the next scrape, until both sites were fixed (762f04b4b, ba050574d).
 *
 * WHY NOTHING CAUGHT IT, AND WHAT CHANGED HERE. Two independent blind spots had to be
 * closed for this to be visible at unit speed:
 *
 *  1. THE FAKE COULDN'T EXPRESS IT. Every merge/re-key spec ran on a bare
 *     `InMemoryMovieRepository`, which stores showtimes INLINE in `MovieRecord.data`. A
 *     fold that unions two records carries them for free there, so the specs passed
 *     honestly while prod bled. The fake now takes an optional `ScreeningsRepository` and
 *     routes through the SAME pure helpers the Mongo repository uses (`stripShowtimes`,
 *     `showtimesOf`, `reStitch`, `stitch`), so it models the split instead of hiding it.
 *
 *  2. THE FIXTURES NEVER FOLDED. `CanonicalKeyFixpointSpec` feeds casing variants like
 *     "Zoo"/"ZOO", which sanitize to the SAME `CacheKey` and therefore land on one row
 *     from the start — no victim, no delete, nothing to lose. A fold with a victim needs
 *     two DIFFERENT keys that turn out to be one film, which is what the cases below
 *     build. Verified by reverting each fix in turn: with the fold migration removed this
 *     spec reports `showtimes before=1 after=0, deletes=1`.
 */
class SettlePreservesShowtimesSpec extends AnyFlatSpec with Matchers {

  private val when = LocalDateTime.of(2026, 6, 8, 18, 0)

  private def withShowtime(cinema: Source, title: String) =
    Map[Source, SourceData](cinema -> SourceData(title = Some(title), showtimes = Seq(Showtime(when, None))))

  private def fixture = {
    val screenings = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings))
    (screenings, repository, new CaffeineMovieCache(repository))
  }

  private def showtimeCount(s: InMemoryScreeningsRepository): Int =
    s.findAll().values.flatMap(_.values).map(_.size).sum

  // A FOLD: two rows under different keys turn out to be the same film (shared tmdbId).
  // One is persisted as the winner, the other deleted — and the loser is where the
  // showtimes are filed.
  "a duplicate fold" should "carry the victim's showtimes onto the winner" in {
    val (screenings, repository, cache) = fixture
    cache.put(CacheKey("Alpha", None),
      MovieRecord(tmdbId = Some(4242), data = withShowtime(Helios, "Alpha")))
    showtimeCount(screenings) shouldBe 1

    // Same film, different key — this write triggers the fold.
    cache.put(CacheKey("Alpha", Some(2026)),
      MovieRecord(tmdbId = Some(4242), data = Map[Source, SourceData](
        KinoMuza -> SourceData(title = Some("Alpha")))))

    withClue(s"rows=${cache.snapshot().map(r => (r.title, r.year))} deletes=${repository.deletes.size}: ")(
      showtimeCount(screenings) should be > 0)
  }

  // A RE-KEY: one row moves to a new key. Same rename, different call site.
  "a re-key" should "carry the film's showtimes to the new key" in {
    val (screenings, repository, cache) = fixture
    cache.put(CacheKey("Beta", None), MovieRecord(data = withShowtime(Helios, "Beta")))
    showtimeCount(screenings) shouldBe 1

    cache.rekey(CacheKey("Beta", None), CacheKey("Beta", Some(2026)), identity)

    withClue(s"rows=${cache.snapshot().map(r => (r.title, r.year))} deletes=${repository.deletes.size}: ")(
      showtimeCount(screenings) should be > 0)
    // …and specifically under the NEW id, not stranded under the old one.
    screenings.findForFilm(StoredMovieRecord.idFor("Beta", Some(2026))).values.flatten should not be empty
  }

  // The showtimes must MERGE, not overwrite: a fold unions two cinemas, so the winner has
  // to end up serving both venues' screenings rather than whichever wrote last.
  "a fold of two rows that BOTH have showtimes" should "keep both venues' screenings" in {
    val (screenings, _, cache) = fixture
    cache.put(CacheKey("Gamma", None),
      MovieRecord(tmdbId = Some(555), data = withShowtime(Helios, "Gamma")))
    cache.put(CacheKey("Gamma", Some(2026)),
      MovieRecord(tmdbId = Some(555), data = withShowtime(KinoMuza, "Gamma")))

    withClue(s"rows=${cache.snapshot().map(r => (r.title, r.year))}: ")(
      showtimeCount(screenings) shouldBe 2)
  }

  // The THIRD site, and the one that produced prod's 30-minute sawtooth. No merge and no
  // re-key: a single row whose stored spelling differs from the canonical one is rewritten
  // under the canonical string. `collapseCluster` did that by invalidating EVERY key and
  // re-`put`ting — and because `CacheKey` equality is normalised, invalidating the
  // canonical key deleted the SAME `_id` the `put` recreates. The delete cascaded to
  // `screenings`, and the re-stitch then read the id it had just emptied.
  //
  // Prod 2026-07-27: 735 of 941 rows delete+re-inserted under byte-identical ids every 30
  // minutes, each shedding its showtimes until the next scrape restored them.
  "a settle that only re-spells a row" should "not delete the row it is rewriting" in {
    val (screenings, repository, cache) = fixture
    // Stored all-caps; `canonical()` prefers "Zoo", so the row needs re-spelling.
    cache.put(CacheKey("ZOO", Some(2026)),
      MovieRecord(tmdbId = Some(31), data = withShowtime(Helios, "ZOO")))
    showtimeCount(screenings) shouldBe 1
    val deletesBefore = repository.deletes.size

    cache.canonicalizeBySanitize()

    withClue(s"rows=${cache.snapshot().map(r => (r.title, r.year))}: ")(
      showtimeCount(screenings) shouldBe 1)
    // …and it should not have deleted anything at all: rewriting a row in place is an
    // upsert, not a delete+insert. This is the churn half of the same bug.
    withClue("the settle delete+re-inserted a row it was only re-spelling: ")(
      repository.deletes.size shouldBe deletesBefore)
    // the re-spelling still happened
    cache.snapshot().map(_.title) shouldBe Seq("Zoo")
  }
}
