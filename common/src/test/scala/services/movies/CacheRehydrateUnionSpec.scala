package services.movies

import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleDefaults, TitleRuleSet}

/** Pins the fix for the late-added-merge-rule data-loss bug: when a merge-key
 *  rule (a Canonical-tier unification — NOT a GlobalStructural decoration strip,
 *  which no longer feeds the key) is added AFTER two docs were written under
 *  distinct keys, they now collide on `CacheKey` (equality is `sanitize`, which
 *  runs the canonical tier). `rehydrate` must UNION the colliding rows, not
 *  last-write-wins drop one — otherwise a hydration silently loses one doc's
 *  showtimes until the next scrape (and on the read-only web app, the row
 *  briefly serves half its cinemas). Reproduces prod: seed two distinct rows
 *  under the defaults, install a `/Kino Cafe` canonical unification so they
 *  collide, then hydrate. */
class CacheRehydrateUnionSpec extends AnyFlatSpec with Matchers {

  private def repoOf(rows: StoredMovieRecord*): MovieRepo = new MovieRepo {
    def enabled = true
    def findAll() = rows.toSeq
    def delete(t: String, y: Option[Int]) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def withInstalledRules(rs: TitleRuleSet)(body: => Unit): Unit = {
    val saved = TitleNormalizer.currentRules
    try { TitleNormalizer.installRules(rs); body }
    finally TitleNormalizer.installRules(saved)
  }

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

  "rehydrate" should "union two docs a late merge-key rule collides, not drop one" in {
    withInstalledRules(TitleRuleSet(TitleRuleDefaults.all :+ kinoCafeRule)) {
      val cache = new CaffeineMovieCache(repoOf(decorated, base))   // rehydrates at construction
      cache.entries should have size 1
      // The union keeps BOTH cinemas; the old last-write-wins kept only the
      // last-iterated row's slot.
      cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
    }
  }

  it should "leave non-colliding rows as separate entries (no spurious union)" in {
    // Same two rows, but WITHOUT the /Kino Cafe rule they don't collide.
    withInstalledRules(TitleRuleDefaults.ruleSet) {
      val cache = new CaffeineMovieCache(repoOf(decorated, base))
      cache.entries should have size 2
    }
  }

  // The live duplicate-card / double-rating-run bug at its source: different
  // cinemas report ONE film under different years, both already TMDB-resolved to
  // the same id. They land under distinct `CacheKey`s, so rehydrate's raw put
  // can't fold them (the `put` identity gate is bypassed on hydrate). They'd then
  // sit duplicated — each independently enriched — until the periodic `settle`,
  // which the worker's restart loop keeps resetting. rehydrate must collapse them
  // on load so the duplicate never reaches the read model or the rating pipeline.
  private def resolvedRow(year: Int, cinema: Source): StoredMovieRecord =
    StoredMovieRecord("Kumotry", Some(year),
      MovieRecord(tmdbId = Some(777), data = Map[Source, SourceData](
        cinema -> SourceData(title = Some("Kumotry"), rawTitle = Some("Kumotry"), releaseYear = Some(year)))))

  "rehydrate" should "collapse two same-tmdbId rows that differ only by year, on load" in {
    val cache = new CaffeineMovieCache(repoOf(
      resolvedRow(2025, Multikino), resolvedRow(2026, CinemaCityKinepolis)))
    cache.entries should have size 1
    cache.entries.head._2.cinemaData.keySet shouldBe Set(Multikino, CinemaCityKinepolis)
  }

  // A repo whose first `findAll` is empty (Mongo not ready at boot) then returns
  // the row. Without retry the boot hydrate gives up on the empty result and the
  // cache starts empty — the row only ever arrives if it's later re-written
  // (via the change stream), so a quiescent row stays Mongo-only and invisible
  // to the in-memory fold/settle. With retry enabled, boot waits Mongo out.
  private def flakeyRepo(row: StoredMovieRecord): MovieRepo = {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    new MovieRepo {
      def enabled = true
      def findAll() = if (calls.getAndIncrement() == 0) Seq.empty else Seq(row)
      def delete(t: String, y: Option[Int]) = ()
      def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
      def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
      override def close() = ()
    }
  }

  "boot hydrate" should "retry an empty findAll (Mongo not ready) so quiescent rows still load" in {
    val cache = new CaffeineMovieCache(
      flakeyRepo(base), bootHydrateMaxAttempts = 5, bootHydrateRetryMillis = 20)
    cache.entries should have size 1   // boot retried past the empty first findAll
  }

  it should "give up after the configured attempts on a genuinely empty repo" in {
    val cache = new CaffeineMovieCache(repoOf(), bootHydrateMaxAttempts = 3, bootHydrateRetryMillis = 5)
    cache.entries should have size 0  // no rows, and it didn't hang
  }
}
