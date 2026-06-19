package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
}
