package tools

import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * A database name no other `it/` suite shares, for a spec that operates on the WHOLE
 * corpus.
 *
 * The it suites run in PARALLEL (`IntegrationTest / parallelExecution := true`) against
 * the one `MONGODB_DB`, and they keep out of each other's way by NAMING — a title prefix
 * per spec, a reserved imdbId, a cleanup that deletes only its own rows. That works for
 * every spec that reads and writes rows it named.
 *
 * It does not work for a spec that constructs a `CaffeineMovieCache`. The cache hydrates
 * the ENTIRE `movies` collection and then acts on it: the settle
 * (`backfillEmbeddedYears` / `canonicalizeBySanitize`) merges same-tmdbId year-variants
 * and DELETES the losers, `put` folds onto any sibling carrying the same tmdbId, and even
 * the plain hydrate reaps rows whose `_id` has drifted from their derived title. None of
 * those look at who owns a row. Proved against a shared database: seeding
 * `StagingFoldIntegrationSpec`'s two sentinels and running only
 * `RekeyScreeningsIntegrationSpec` logged `[movies.delete] film removed:
 * id=foldorphansitsentinel|2026` — a neighbour's rows destroyed, with their `screenings`
 * and `movie_slots` cascaded away behind them. In a parallel run that lands inside the
 * neighbour's test window often enough to flake it.
 *
 * So: a whole-corpus spec gets a corpus of its own. Naming discipline cannot help here,
 * because the operations under test are the ones that ignore names.
 */
object IntegrationCorpusDatabase {
  /** `<MONGODB_DB>_<suite>` — the configured database, suffixed per suite. Keeping the
   *  configured name as the PREFIX means the `IntegrationMongo` throwaway guard and the
   *  CI teardown still recognise it as a test database. */
  def named(suite: String): String = s"${Env.get("MONGODB_DB").getOrElse("kinowo")}_$suite"

  /**
   * Run `body` against this suite's own corpus and DROP that corpus afterwards — dropped
   * even when `body` throws, since the alternative is an orphan database per failed run.
   *
   * A suite that owns a whole database has no reason to delete its rows one id at a time:
   * dropping takes the `movies` rows, the `screenings` and `movie_slots` they cascade to,
   * and every index with them. The per-row `finally` blocks this replaced were only ever
   * approximating that, and they left the database itself behind — fifty of them had piled
   * up on the local replica set before anything dropped one.
   *
   * The drop is AWAITED. `WorkerWiringNormalizerIntegrationSpec` used to call
   * `drop().toFuture()` without awaiting it, so the JVM exited before the command reached
   * the server and `kinowo_it_wiring_*` survived every run — a drop that is merely started
   * is not a drop.
   *
   * Only ever call this for a database this suite alone owns, i.e. one named by [[named]].
   * The bare `MONGODB_DB` is SHARED by every other spec in both modules — `itAll` runs web
   * and worker in parallel with `IntegrationTest / parallelExecution := true` — so dropping
   * that one would delete a neighbour's corpus mid-run.
   */
  def withDatabase[A](uri: String, suite: String)(body: MongoDatabase => A): A = {
    IntegrationMongo.requireThrowaway(uri, Env.get(IntegrationMongo.OverrideVar).exists(v => v == "1" || v.equalsIgnoreCase("true")))
    val client = MongoClient(uri)
    try {
      val database = client.getDatabase(named(suite))
      try body(database)
      finally Await.result(database.drop().toFuture(), 60.seconds)
    } finally client.close()
  }
}
