package services.movies

import models.{CinemaCityKinepolis, MovieRecord, Multikino, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRules, TitleRuleSet}
import services.movies.SingleCountryNormalizer.titleNormalizer

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

  private def repositoryOf(rows: StoredMovieRecord*): MovieRepository =
    new StoredRowsRepository(rows.toSeq, titleNormalizer)

  // The rehydrate under test runs synchronously at cache construction, under the
  // rule set the cache HOLDS. Handing it the normalizer says so outright; this
  // used to install the rules in a thread-local scope and rely on the cache
  // reading whatever was ambient at the moment each key was built — which is
  // exactly the coupling that made a rule set impossible to scope per country.
  private def cacheUnder(rs: TitleRuleSet, rows: StoredMovieRecord*): CaffeineMovieCache =
    new CaffeineMovieCache(repositoryOf(rows*), normalizer = new TitleNormalizer(rs))

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
    val cache = cacheUnder(TitleRuleSet(TitleRules.all :+ kinoCafeRule), decorated, base)
    cache.entries should have size 1
    // The union keeps BOTH cinemas; the old last-write-wins kept only the
    // last-iterated row's slot.
    cache.entries.head._2.cinemaData.keySet shouldBe Set(CinemaCityKinepolis, Multikino)
  }

  /** Records the reconcile's writes and answers its survivor upserts with `outcomes`, in order. */
  private class ReconcileRecorder(rows: Seq[StoredMovieRecord], outcomes: WriteOutcome*)
      extends StoredRowsRepository(rows, titleNormalizer) {
    private var answers = outcomes.toList
    var deleted = Vector.empty[FilmId]
    override def upsert(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome = {
      super.upsert(id, key, e)
      val answer = answers.headOption.getOrElse(WriteOutcome.Written)
      answers = answers.drop(1)
      answer
    }
    override def delete(id: FilmId): WriteOutcome = { deleted :+= id; WriteOutcome.Written }
  }

  private val codecFailure = WriteOutcome.Failed(MovieRepository.Collection, "upsert", new RuntimeException("codec"))

  // The losers are deleted on the strength of the survivor carrying their union. A survivor
  // write that FAILED carries nothing, so deleting them would lose every field only they held.
  it should "keep the duplicate documents when the survivor's write fails" in {
    val repository = new ReconcileRecorder(Seq(decorated, base), codecFailure)
    new CaffeineMovieCache(repository, normalizer = new TitleNormalizer(TitleRuleSet(TitleRules.all :+ kinoCafeRule)))
    repository.upserts should have size 1
    repository.deleted shouldBe empty
  }

  // Only a decline for the LOSERS' identity is a reason to delete them first. Any other — a
  // client closing mid-shutdown — landed nothing either, and the write-time fold keeps its
  // losers on it; the reconcile deleted them and lost every field only they held.
  it should "keep the duplicate documents when the survivor's write is declined for another reason" in {
    val repository = new ReconcileRecorder(Seq(decorated, base), WriteOutcome.Declined("client-closing"))
    new CaffeineMovieCache(repository, normalizer = new TitleNormalizer(TitleRuleSet(TitleRules.all :+ kinoCafeRule)))
    repository.deleted shouldBe empty
  }

  it should "delete the losers once the survivor's write has landed" in {
    val repository = new ReconcileRecorder(Seq(decorated, base), WriteOutcome.Written)
    new CaffeineMovieCache(repository, normalizer = new TitleNormalizer(TitleRuleSet(TitleRules.all :+ kinoCafeRule)))
    repository.deleted should have size 1
  }

  // A survivor write DECLINED because a loser still holds the key or tmdbId is the ordinary
  // shape here — the losers are what hold them. Delete them, then write the survivor again.
  it should "delete the losers and rewrite the survivor when its write was declined for their identity" in {
    val repository = new ReconcileRecorder(Seq(decorated, base),
      WriteOutcome.Declined("identity-held-by-another-document"), WriteOutcome.Written)
    new CaffeineMovieCache(repository, normalizer = new TitleNormalizer(TitleRuleSet(TitleRules.all :+ kinoCafeRule)))
    repository.deleted should have size 1
    repository.upserts should have size 2
  }

  // The rewrite after the losers are gone can fail too. The cache then holds a union `movies`
  // never took, and every later scrape diffs against it — patching only what changed, so the
  // union's own fields never reach Mongo and the failure is never retried. The key goes, and
  // the next read finds the survivor as Mongo has it.
  it should "not keep the union resident when the survivor's rewrite fails after the losers are gone" in {
    val repository = new ReconcileRecorder(Seq(decorated, base), WriteOutcome.IdentityHeld, codecFailure)
    val cache = new CaffeineMovieCache(repository, normalizer = new TitleNormalizer(TitleRuleSet(TitleRules.all :+ kinoCafeRule)))
    repository.upserts should have size 2
    withClue("the union was never written, so the cache must not hold it: ")(cache.entries shouldBe empty)
  }

  it should "leave non-colliding rows as separate entries (no spurious union)" in {
    // Same two rows, but WITHOUT the /Kino Cafe rule they don't collide.
    val cache = cacheUnder(TitleRules.ruleSet, decorated, base)
    cache.entries should have size 2
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
      resolvedRow(2025, Multikino), resolvedRow(2026, CinemaCityKinepolis)), normalizer = titleNormalizer)
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
      unresolved2025Row(Multikino), resolved2026Row(CinemaCityKinepolis)), normalizer = titleNormalizer)
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
    new StoredRowsRepository(if (calls.getAndIncrement() == 0) Seq.empty else Seq(row), titleNormalizer)
  }

  "boot hydrate" should "retry an empty findAll (Mongo not ready) so quiescent rows still load" in {
    val cache = new CaffeineMovieCache(
      flakeyRepository(base), bootHydrateMaxAttempts = 5, bootHydrateRetryMillis = 20, normalizer = titleNormalizer)
    cache.entries should have size 1   // boot retried past the empty first findAll
  }

  it should "give up after the configured attempts on a genuinely empty repository" in {
    val cache = new CaffeineMovieCache(repositoryOf(), bootHydrateMaxAttempts = 3, bootHydrateRetryMillis = 5, normalizer = titleNormalizer)
    cache.entries should have size 0  // no rows, and it didn't hang
  }
}
