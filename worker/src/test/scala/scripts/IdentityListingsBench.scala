package scripts

import models.{Cinema, Country}
import services.identity.IdentityListingIntake
import services.movies.{InMemoryScrapeGuardLedger, TitleNormalizer}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}
import services.{MongoConnection, MongoRequirement}
import settings.{MongoDatabaseName, MongoUri}

import java.lang.management.ManagementFactory

/**
 * What a cut-over country's projection pays per tick to read its listings (`IdentityListingIntake.listings`):
 * wall time and allocation per pass over a COPY of a country's archive on the local :28017 server — never
 * the prod mirror itself, whose missing indexes the repository would create. Between passes it re-files
 * `churn` venues' listings under a new instant, as the scrapes landing in five minutes do.
 *
 *   mongosh … --eval 'db.getSiblingDB("kinowo_us_prod_mirror").cinema_scrapes.aggregate([{$out:{db:"kinowo_listings_bench",coll:"cinema_scrapes"}}])'
 *   sbt "worker/Test/runMain scripts.IdentityListingsBench kinowo_listings_bench us 6 70"
 *
 * The bench writes only its own database (the re-filed listings, the empty accepted-listings collection).
 */
object IdentityListingsBench {
  def main(args: Array[String]): Unit = {
    val database = args(0)
    val country  = args.lift(1).flatMap(Country.byCode).getOrElse(Country.UnitedStates)
    val passes   = args.lift(2).map(_.toInt).getOrElse(6)
    val churn    = args.lift(3).map(_.toInt).getOrElse(70)
    val conn     = new MongoConnection(Some(MongoUri("mongodb://127.0.0.1:28017/?directConnection=true")),
      MongoDatabaseName(database), MongoRequirement.Required)
    val archive  = new MongoScrapeArchiveRepository(conn.database)
    val accepted = new MongoScrapeArchiveRepository(conn.database, IdentityListingIntake.Collection)
    val intake   = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, TitleNormalizer.forCountry(country),
      maxRejections = 3, java.time.Clock.systemUTC(), services.movies.ScrapeLandingMetrics.noop)
    val live     = archive.contentStamps().keys.flatMap(Cinema.byDisplayName.get).toSeq
    val threads  = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val memory   = ManagementFactory.getMemoryMXBean
    def usedAfterGc() = { System.gc(); Thread.sleep(200); System.gc(); memory.getHeapMemoryUsage.getUsed }
    val baseline = usedAfterGc()
    var instant  = java.time.Instant.now()
    (1 to passes).foreach { n =>
      if (n > 1) live.take(churn).foreach { cinema =>
        instant = instant.plusMillis(1)
        archive.find(cinema).flatMap(_.lastSuccess).foreach(s =>
          archive.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), instant, s.listingComplete, s.films)))
      }
      val (alloc0, t0) = (threads.getTotalThreadAllocatedBytes, System.nanoTime)
      val listings     = intake.listings(live)
      val (alloc1, t1) = (threads.getTotalThreadAllocatedBytes, System.nanoTime)
      println(f"pass $n: ${listings.size} venues, ${listings.map(_._2.size).sum} listings — wall ${(t1 - t0) / 1e9}%.2fs, " +
        f"alloc ${(alloc1 - alloc0) / 1e6}%.0f MB")
    }
    val between = usedAfterGc()
    println(f"retained by the intake between passes: ${(between - baseline) / 1e6}%.0f MB")
    // What a projection tick holds while it works on the listings: one pass's result, live.
    val held = intake.listings(live)
    println(f"held by one pass's listings (${held.size} venues): ${(usedAfterGc() - between) / 1e6}%.0f MB")
    conn.close()
    sys.exit(0)
  }
}
