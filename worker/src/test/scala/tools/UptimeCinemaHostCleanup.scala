package tools

import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import services.MongoConnection
import services.cinemas.CinemaScraperCatalog

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 *  One-shot: drop the redundant per-HOST uptime rows that cinema scrapes used
 *  to leave in `uptimeBuckets`.
 *
 *  Background: `MonitoringHttpFetch` once recorded every cinema scrape twice —
 *  once under the cinema's `displayName` (via `UptimeRecordingScraper`) and again
 *  under the request HOST (e.g. `kinomuranow.pl`). The host rows had no home on
 *  the uptime page's Cinemas/Enrichment sections, so they piled into "Other".
 *  Going forward those hosts are SUPPRESSED at the source (their union is
 *  `CinemaScraperCatalog.scrapeHosts`), but the already-written host docs linger
 *  until the bucket TTL expires them. This deletes them now so "Other" clears immediately.
 *
 *  We DELETE rather than re-attribute: a host maps many-to-one to cinemas
 *  (`www.bilety24.pl` fronts six venues), so a host row can't be folded into a
 *  single `displayName` — and the cinema's real history already lives under its
 *  `displayName`, so nothing is lost. Enrichment rows (`TMDB`, `Filmweb`, …),
 *  `displayName` rows, and `img: …` rows are untouched: their service strings
 *  aren't bare scrape hosts, so they never match the host set. Idempotent.
 *
 *  Reads `MONGODB_URI` / `MONGODB_DB` from env — source `.env.local` first:
 *
 *  ```
 *  set -a; . ./.env.local; set +a
 *  sbt 'worker/Test/runMain tools.UptimeCinemaHostCleanup'
 *  ```
 */
object UptimeCinemaHostCleanup {

  /** The host set to purge, single-sourced from the live scraper catalog so it
   *  can't drift from what `MonitoringHttpFetch` suppresses. A no-op `http` is
   *  fine: building the catalog only constructs client objects — it fetches
   *  nothing — and `scrapeHosts` reads their declared hosts. */
  def cinemaHosts: Set[String] = new CinemaScraperCatalog(new RealHttpFetch()).scrapeHosts

  def main(args: Array[String]): Unit = {
    val hosts = cinemaHosts
    val conn = MongoConnection.fromEnv(required = false)
    try {
      val db = conn.database.getOrElse {
        println("MONGODB_URI not set — nothing to do.")
        sys.exit(1)
      }
      val coll = db.getCollection("uptimeBuckets")

      val before = Await.result(coll.distinct[String]("service").toFuture(), 60.seconds).toSet
      val matched = before.intersect(hosts)
      println(s"${hosts.size} cinema host(s) in the catalog; ${matched.size} of them have uptime rows to drop.")

      val del = Await.result(
        coll.deleteMany(Filters.in("service", matched.toSeq*)).toFuture(),
        120.seconds)
      println(s"Deleted ${del.getDeletedCount} redundant per-host uptime doc(s) across ${matched.size} host(s).")
    } finally conn.close()
  }
}
