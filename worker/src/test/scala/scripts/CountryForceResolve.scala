package scripts

import models.Country
import services.MongoConnection
import services.movies.MongoMovieRepository
import services.tasks.{EnqueueResult, EnrichTaskKeys, MongoTaskQueue, TaskType}

/**
 * One-shot operational tool: force a re-resolve of every row in a given
 * country's corpus, so the deployed worker (which enriches that country in its
 * OWN language) re-fetches TMDB `fullDetails` and overwrites content frozen from
 * a pre-fix resolve — e.g. the Polish synopsis + country names left on the UK/DE
 * deployments before the per-country enrichment-language fix.
 *
 * Enqueues one `force = true` ResolveTmdb task per row through the SAME
 * `MongoTaskQueue` the worker drains; the unique dedup key makes it idempotent /
 * safely re-runnable. NOTE (per the resolve path): a forced re-resolve strips the
 * row to its scraped data and re-runs the full title search, so for a handful of
 * ambiguous titles it can re-key/re-match — that is the standard `/debug` reenrich
 * behaviour, just applied in bulk.
 *
 * The country's database is taken from `Country` (NOT `MONGODB_DB`, which
 * `.env.local` pins to prod `kinowo`), so this can't accidentally touch Poland.
 *
 * Run against a prod tunnel:
 *   flyctl proxy 27017 -a kinowo-mongo                       # separate shell
 *   sbt "worker/Test/runMain scripts.CountryForceResolve uk" # or `de`
 */
object CountryForceResolve {
  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.UnitedKingdom)
    val dbName  = country.mongoDb
    val conn    = MongoConnection.fromEnvForDb(dbName, required = true)
    val db = conn.database.getOrElse {
      println(s"Could not open $dbName — is the Mongo tunnel up (flyctl proxy 27017 -a kinowo-mongo) and MONGODB_URI set?")
      sys.exit(1)
    }
    val repo  = new MongoMovieRepository(sharedDb = Some(db), fallbackToOwnInit = false)
    val queue = new MongoTaskQueue(Some(db))

    val rows = repo.findAll()
    println(s"${country.displayName} ($dbName): ${rows.size} rows · enqueuing force ResolveTmdb for each…")
    var added = 0
    var dup   = 0
    rows.foreach { r =>
      queue.enqueue(
        TaskType.ResolveTmdb,
        EnrichTaskKeys.resolveTmdbDedup(r.title, r.year),
        EnrichTaskKeys.resolveTmdbPayload(r.title, r.year, force = true)) match {
        case EnqueueResult.Added => added += 1
        case _                   => dup += 1
      }
    }
    println(s"done: $added enqueued, $dup already queued (dedup). The $dbName worker will drain them in ${country.language.toLanguageTag}.")
    conn.close()
    sys.exit(0)
  }
}
