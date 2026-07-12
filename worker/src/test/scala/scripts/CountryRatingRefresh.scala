package scripts

import models.Country
import services.MongoConnection
import services.freshness.FreshnessKind
import services.movies.MongoMovieRepository
import services.tasks.{EnqueueResult, MongoTaskQueue, RatingTasks, TaskType}

/**
 * One-shot operational tool: enqueue a targeted RottenTomatoes rating refresh for
 * every film in a country's corpus that has a discovered `rottenTomatoesUrl` but
 * no `rottenTomatoes` score — the gap the RT scorecard-parser fix recovers. The
 * deployed worker drains each `RtRating` task, re-fetches the RT page (via Zyte in
 * prod) and now parses the `media-scorecard-json` `criticsScore.score`, writing the
 * score if changed. Idempotent (dedup key `rt|tmdb:<id>`); safe to re-run.
 *
 *   flyctl proxy 27017 -a kinowo-mongo
 *   sbt "worker/Test/runMain scripts.CountryRatingRefresh uk"   # or `pl`
 */
object CountryRatingRefresh {
  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.UnitedKingdom)
    val conn    = MongoConnection.fromEnvForDb(country.mongoDb, required = true)
    val db = conn.database.getOrElse {
      println(s"Could not open ${country.mongoDb} — is the tunnel up + MONGODB_URI set?"); sys.exit(1)
    }
    val repo  = new MongoMovieRepository(sharedDb = Some(db), fallbackToOwnInit = false)
    val queue = new MongoTaskQueue(Some(db))

    val affected = repo.findAll().filter(r =>
      r.record.tmdbId.isDefined && r.record.rottenTomatoesUrl.isDefined && r.record.rottenTomatoes.isEmpty)
    println(s"${country.mongoDb}: ${affected.size} films with an RT url but no score — enqueuing RtRating…")
    var added = 0; var dup = 0
    affected.foreach { r =>
      // `CacheKey` is `private[services]`, so build the same strings `RatingTasks`
      // would: the tmdbId-keyed RT dedup key + the title/year payload the handler decodes.
      val dedup   = s"${FreshnessKind.RtRating.label}|tmdb:${r.record.tmdbId.get}"
      val payload = Map(RatingTasks.TitleKey -> r.title, RatingTasks.YearKey -> r.year.map(_.toString).getOrElse(""))
      queue.enqueue(TaskType.RtRating, dedup, payload) match {
        case EnqueueResult.Added => added += 1
        case _                   => dup += 1
      }
    }
    println(s"done: $added enqueued, $dup already queued. The ${country.mongoDb} worker will drain them.")
    conn.close(); sys.exit(0)
  }
}
