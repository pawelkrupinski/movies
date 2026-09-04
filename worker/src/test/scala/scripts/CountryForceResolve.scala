package scripts

import models.Country
import services.MongoConnection
import services.movies.MongoMovieRepository
import services.tasks.MongoTaskQueue
import services.movies.SingleCountryNormalizer.titleNormalizer

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
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   sbt "worker/Test/runMain scripts.CountryForceResolve uk" # or `de`
 */
object CountryForceResolve {
  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.UnitedKingdom)
    val dbName  = country.mongoDb
    val conn    = MongoConnection.fromEnvForDb(dbName, required = true)
    val db = conn.database.getOrElse {
      println(s"Could not open $dbName — is the Mongo tunnel up (scripts/local-mirror/prod-tunnel.sh) and MONGODB_URI set?")
      sys.exit(1)
    }
    val repo  = new MongoMovieRepository(sharedDb = Some(db), fallbackToOwnInit = false, normalizer = titleNormalizer)
    val queue = new MongoTaskQueue(Some(db))

    val rows = repo.findAll()
    println(s"${country.displayName} ($dbName): ${rows.size} rows · enqueuing force ResolveTmdb for each…")
    val counts = ForceResolveEnqueue.all(queue, rows)
    println(s"done: ${counts.describe}. The $dbName worker will drain them in ${country.language.toLanguageTag}.")
    conn.close()
    sys.exit(0)
  }
}
