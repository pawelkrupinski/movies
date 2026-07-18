package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.TitleRuleSet

import java.time.LocalDateTime

/**
 * REGRESSION for the prod "Dzień objawienia" merge→split flap.
 *
 * Observed on prod /debug across ticks: one film (tmdbId 1275779, year 2026,
 * cacheKey `dzienobjawienia|2026`) appeared FULLY merged + enriched (177 cinemas,
 * ratings present) and later partial + UN-enriched (~65 cinemas, no ratings),
 * tick after tick. Several cinemas report the title in ALL-CAPS ("DZIEŃ
 * OBJAWIENIA", "Dzień Objawienia") — every variant `sanitize`s to the same
 * `dzienobjawienia`, so they merge into one row whose canonical spelling settles
 * deterministically to the non-shouting "Dzień objawienia".
 *
 * ── Root cause ────────────────────────────────────────────────────────────────
 * `collapseCluster`'s `needsFix` guard folded the cinemas' immutable SLOT titles
 * into its "anything to fix?" check. A SHOUTING slot ("DZIEŃ OBJAWIENIA")
 * `sanitize`-equals the canonical but never string-equals it — and a cinema's
 * reported title can never be re-written — so `needsFix` stayed permanently true
 * and the settled row was `delete`+`upsert`-ed on EVERY settle/hydrate tick. That
 * per-tick re-write re-keys the row and re-kicks its enrichment (freshness
 * invalidated), so the row repeatedly drops to an un-enriched partial state and
 * re-fills — the flap. (`f2dd5be1` fixed the identical churn for cross-LANGUAGE
 * slots but missed the same-language CASE-drift slot.)
 *
 * The fix: only an actual stored ROW key (which `collapseCluster` can re-key onto
 * the canonical) is a fix signal — never an immutable cinema slot. A converged
 * corpus must reach a fixpoint and stop writing.
 */
class DzienObjawieniaFlapSpec extends AnyFlatSpec with Matchers {

  private val TmdbId = 1275779
  private val Year   = Some(2026)

  private def cm(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title, releaseYear = Year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), None)))

  /** Resolve a row to the film's single TMDB id (every case variant → 1275779),
   *  as `MovieService`'s TMDB stage does — no fabricated second id. */
  private def resolve(existing: MovieRecord): MovieRecord =
    existing.copy(
      tmdbId = Some(TmdbId),
      data = existing.data + (Tmdb ->
        SourceData(title = Some("Dzień objawienia"), originalTitle = Some("Disclosure Day"), releaseYear = Year)))

  /** The real Würzburg venue and its real payload row (see the German case below).
   *  Title / originalTitle / yearlessness / first `startsAt` are verbatim from the
   *  checked-in A0263 capture — no invented spellings. */
  private val Wuerzburg     = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")
  private val MinionsTmdbId = 1489031

  private def minions: CinemaMovie =
    CinemaMovie(
      movie     = Movie("Minions & Monster", originalTitle = Some("Minions & Monsters")),
      cinema    = Wuerzburg,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 11, 11, 30), None)))

  "canonicalizeBySanitize" should
    "reach a fixpoint for a film whose cinemas SHOUT the title (no per-tick re-write)" in {
    val repo  = new InMemoryMovieRepository
    val cache = new CaffeineMovieCache(repo)

    // Two cinemas, one SHOUTING — both sanitize to `dzienobjawienia`, so they
    // merge into one row; the canonical spelling settles to "Dzień objawienia".
    cache.recordCinemaScrape(Helios,    Seq(cm(Helios,    "DZIEŃ OBJAWIENIA")))
    cache.recordCinemaScrape(Multikino, Seq(cm(Multikino, "Dzień objawienia")))
    cache.entries.foreach { case (k, e) => cache.settleResolved(k, resolve(e)) }

    cache.canonicalizeBySanitize()                          // first settle: the legitimate merge / re-key
    val writesAfterFirst = repo.upserts.size + repo.deletes.size

    cache.canonicalizeBySanitize()                          // second settle: nothing changed — must be a no-op
    val writesAfterSecond = repo.upserts.size + repo.deletes.size

    withClue(
      s"\nThe SHOUTING cinema slot re-tripped `needsFix`: the settled row was " +
      s"re-written ${writesAfterSecond - writesAfterFirst} more time(s) on a steady tick.\n" +
      s"stored = ${cache.snapshot().map(_.title)}\n") {
      writesAfterSecond shouldBe writesAfterFirst
    }
  }

  /** The German "Minions & Monster" flap — the same churn, caused by a POLISH rule
   *  leaking into a German corpus.
   *
   *  `f2dd5be1` fixed cross-LANGUAGE slot drift and the case above fixed
   *  same-language CASE drift. This one was upstream of both: the canonical tier's
   *  " & " → " i " unification is Polish ("i" = "and") but ran for every country,
   *  so the film CinemaxX Würzburg reports as "Minions & Monster" (see the recorded
   *  Filmstarts capture,
   *  `test/resources/fixtures/webedia-de/www.filmstarts.de/_/showtimes/theater-A0263/d-2026-07-11/p-1.json`)
   *  canonicalised to "Minions i Monster" and keyed `minionsimonster`. NO German
   *  cinema slot can ever produce that key, so the row's key and its own cinemas'
   *  spellings disagreed permanently and every settle re-canonicalised the row.
   *
   *  Observed in prod 2026-07-18 (confirmed against the live corpus):
   *  `kinowo_worker_showtimes{country="de",city="wurzburg"}` square-waved 115 ↔ 90,
   *  the film's 25 showtimes leaving and re-entering. The record's `updatedAt`
   *  advanced on SettleReaper's exact :22:35/:52:35 beat and its `web_screenings`
   *  went to 0 on each rewrite; the hourly :24 cinema scrape put them back. The row
   *  stayed `readyToProject` throughout — it was the SCREENINGS that were orphaned.
   *
   *  Under the country-scoped rule set the German key is the cinema's own spelling,
   *  so the settle converges. */
  it should "reach a fixpoint for a German title whose ' & ' must not become ' i '" in {
    TitleNormalizer.withRules(TitleRuleSet.forCountry("de")) {
      val repo  = new InMemoryMovieRepository
      val cache = new CaffeineMovieCache(repo)

      cache.recordCinemaScrape(Wuerzburg, Seq(minions))
      cache.entries.foreach { case (k, e) =>
        cache.settleResolved(k, e.copy(
          tmdbId = Some(MinionsTmdbId),
          data   = e.data + (Tmdb -> SourceData(
            title = Some("Minions & Monster"), originalTitle = Some("Minions & Monsters")))))
      }

      cache.canonicalizeBySanitize()                        // first settle: the legitimate re-key
      val writesAfterFirst = repo.upserts.size + repo.deletes.size

      cache.canonicalizeBySanitize()                        // second settle: must be a no-op
      val writesAfterSecond = repo.upserts.size + repo.deletes.size

      withClue(
        s"\nThe German row was re-written ${writesAfterSecond - writesAfterFirst} " +
        s"more time(s) on a steady tick.\nstored = ${cache.snapshot().map(r => (r.title, r.year))}\n") {
        writesAfterSecond shouldBe writesAfterFirst
      }
      // The key is the cinema's own spelling — no Polish conjunction injected.
      TitleNormalizer.sanitize("Minions & Monster") shouldBe "minionsmonster"
    }
  }
}
