package services.movies

import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRules, TitleRuleSet}

/** Pins the fix for the late-added-merge-rule data-loss bug: when a merge-key
 *  rule (a Canonical-tier unification — NOT a GlobalStructural decoration strip,
 *  which no longer feeds the key) is added AFTER two documents were written under
 *  distinct keys, they now collide on `CacheKey` (equality is `sanitize`, which
 *  runs the canonical tier). `rehydrate` must UNION the colliding rows, not
 *  last-write-wins drop one — otherwise a hydration silently loses one document's
 *  showtimes until the next scrape (and on the read-only web app, the row
 *  briefly serves half its cinemas). Reproduces prod: seed two distinct rows
 *  under the defaults, install a `/Kino Cafe` canonical unification so they
 *  collide, then hydrate. */
class CacheRehydrateUnionSpec extends AnyFlatSpec with Matchers {

  private def repositoryOf(rows: StoredMovieRecord*): MovieRepository = new MovieRepository {
    def enabled = true
    def findAll() = rows.toSeq
    def delete(t: String, y: Option[Int]) = ()
    def deleteById(id: String) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  // Thread-scoped (see TitleNormalizer.withRules): the rehydrate under test runs
  // synchronously on this thread at cache construction, and the scope keeps the
  // custom rule set from leaking into a suite running in parallel.
  private def withInstalledRules(rs: TitleRuleSet)(body: => Unit): Unit =
    TitleNormalizer.withRules(rs)(body)

  private def row(title: String, cinema: Source): StoredMovieRecord =
    StoredMovieRecord(title, Some(2025),
      MovieRecord(data = Map[Source, SourceData](
        cinema -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = Some(2025)))))

  private val decorated = row("Takie jest życie/Kino Cafe", CinemaCityKinepolis)
  private val base      = row("Takie jest życie",           Multikino)

  // Canonical-tier unification that didn't exist when the rows were written;
  // under it both titles sanitise to the same key. (A GlobalStructural strip
  // would NOT collide them — that tier feeds external lookups, not the key.)
  private val kinoCafeRule = TitleRule("test-kino-cafe", RuleScope.Canonical, None,
    """(?i)\s*/\s*Kino\s+Cafe\s*$""", "", applyAll = false, order = 100)

  "rehydrate" should "union two documents a late merge-key rule collides, not drop one" in {
    withInstalledRules(TitleRuleSet(TitleRules.all :+ kinoCafeRule)) {
      val cache = new CaffeineMovieCache(repositoryOf(decorated, base))   // rehydrates at construction
      cache.entries should have size 1
      // The union keeps BOTH cinemas; the old last-write-wins kept only the
      // last-iterated row's slot.
      cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    }
  }

  it should "leave non-colliding rows as separate entries (no spurious union)" in {
    // Same two rows, but WITHOUT the /Kino Cafe rule they don't collide.
    withInstalledRules(TitleRules.ruleSet) {
      val cache = new CaffeineMovieCache(repositoryOf(decorated, base))
      cache.entries should have size 2
    }
  }

  // The live duplicate-card / double-rating-run shape: different cinemas report ONE
  // film under different years, both already TMDB-resolved to the same id. They land
  // under distinct `CacheKey`s, so the raw hydrate `put` can't fold them (its
  // identity gate is bypassed on load). The hydrate is now a PURE LOAD, so they sit
  // separate until the periodic `SettleReaper` runs the whole-corpus `settle`
  // (`canonicalizeBySanitize`) — the cross-year collapse only that pass does. (The
  // settle is its own cluster-claimed tick now, not bolted onto the reload, so the
  // restart loop no longer resets it the way it once did.)
  private def resolvedRow(year: Int, cinema: Source): StoredMovieRecord =
    StoredMovieRecord("Kumotry", Some(year),
      MovieRecord(tmdbId = Some(777), data = Map[Source, SourceData](
        cinema -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(year)))))

  "settle after a pure load" should "collapse two same-tmdbId rows that differ only by year" in {
    val cache = new CaffeineMovieCache(repositoryOf(
      resolvedRow(2025, Multikino), resolvedRow(2026, CinemaCityKinepolis)))
    cache.entries should have size 2            // pure load leaves the cross-year split
    cache.canonicalizeBySanitize()              // the SettleReaper's settle collapses it
    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(Multikino, CinemaCityKinepolis)
  }

  // The "two copies of Kumotry" prod bug: ONE cinema reports the film at the
  // production year (2025) and never gets TMDB-resolved (no `tmdbId`); ANOTHER
  // reports it at the release year (2026) and resolves to TMDB id 1454157
  // (tmdbYear 2026). The unresolved 2025 row is within ±1 of the resolved
  // cluster's TMDB year, so clustering rule (2) must attach it — yet both rows
  // survive on /debug as `kumotry|2025` + `kumotry|2026`. Reproduces the exact
  // shape: a Tmdb slot carrying the resolved year drives `tmdbYear`.
  private def resolved2026Row(cinema: Source): StoredMovieRecord =
    StoredMovieRecord("Kumotry", Some(2026),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        cinema -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(2025)),
        Tmdb   -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(2026)))))

  private def unresolved2025Row(cinema: Source): StoredMovieRecord =
    StoredMovieRecord("Kumotry", Some(2025),
      MovieRecord(data = Map[Source, SourceData](
        cinema -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(2025)))))

  it should "attach an unresolved ±1-year row to its resolved same-title cluster" in {
    val cache = new CaffeineMovieCache(repositoryOf(
      unresolved2025Row(Multikino), resolved2026Row(CinemaCityKinepolis)))
    cache.entries should have size 2            // pure load
    cache.canonicalizeBySanitize()              // settle attaches the ±1-year row
    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(Multikino, CinemaCityKinepolis)
  }

  // A repository whose first `findAll` is empty (Mongo not ready at boot) then returns
  // the row. Without retry the boot hydrate gives up on the empty result and the
  // cache starts empty — the row only ever arrives if it's later re-written
  // (via the change stream), so a quiescent row stays Mongo-only and invisible
  // to the in-memory fold/settle. With retry enabled, boot waits Mongo out.
  private def flakeyRepository(row: StoredMovieRecord): MovieRepository = {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    new MovieRepository {
      def enabled = true
      def findAll() = if (calls.getAndIncrement() == 0) Seq.empty else Seq(row)
      def delete(t: String, y: Option[Int]) = ()
      def deleteById(id: String) = ()
      def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
      def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
      override def close() = ()
    }
  }

  "boot hydrate" should "retry an empty findAll (Mongo not ready) so quiescent rows still load" in {
    val cache = new CaffeineMovieCache(
      flakeyRepository(base), bootHydrateMaxAttempts = 5, bootHydrateRetryMillis = 20)
    cache.entries should have size 1   // boot retried past the empty first findAll
  }

  it should "give up after the configured attempts on a genuinely empty repository" in {
    val cache = new CaffeineMovieCache(repositoryOf(), bootHydrateMaxAttempts = 3, bootHydrateRetryMillis = 5)
    cache.entries should have size 0  // no rows, and it didn't hang
  }
}
