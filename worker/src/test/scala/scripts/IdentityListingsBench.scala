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
  private val clock = java.time.Clock.systemUTC()

  val commandLine: tools.CommandLine = tools.CommandLine("IdentityListingsBench", "Measures the identity listing intake's re-filing over a copied archive.")
    .withPositionals(tools.CommandLine.Positional("database", "the bench's own database on :28017 (required)"),
      tools.CommandLine.Positional("country", "the country whose title rules file the listings (default: us)"),
      tools.CommandLine.Positional("passes", "re-filing passes measured (default: 6)"),
      tools.CommandLine.Positional("churn", "listings churned per pass (default: 70)"))

  final case class Run(database: String, country: Country, passes: Int, churn: Int)

  def runOf(arguments: tools.CommandLine.Arguments): Run = Run(arguments.requiredPositional("database"),
    arguments.country("country").getOrElse(Country.UnitedStates), arguments.positionalInt("passes").getOrElse(6), arguments.positionalInt("churn").getOrElse(70))

  def main(args: Array[String]): Unit = {
    val Run(database, country, passes, churn) = runOf(commandLine.parseOrExit(args))
    val conn     = new MongoConnection(Some(MongoUri("mongodb://127.0.0.1:28017/?directConnection=true")),
      MongoDatabaseName(database), MongoRequirement.Required)
    val archive  = new MongoScrapeArchiveRepository(conn.database)
    val accepted = new MongoScrapeArchiveRepository(conn.database, IdentityListingIntake.Collection)
    val intake   = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, TitleNormalizer.forCountry(country),
      maxRejections = 3, clock, services.movies.ListingIntakeMetrics.noop)
    val live     = archive.contentStamps().keys.flatMap(Cinema.byDisplayName.get).toSeq
    val threads  = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val memory   = ManagementFactory.getMemoryMXBean
    def usedAfterGc() = { System.gc(); Thread.sleep(200); System.gc(); memory.getHeapMemoryUsage.getUsed }
    val baseline = usedAfterGc()
    var instant  = clock.instant()
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
    projectionsWholeRead(intake, live, () => usedAfterGc())
    conn.close()
    sys.exit(0)
  }

  /** What the projection's read (`projectedByVenue`) holds through its hourly whole read beside the identity model's own
   *  listings — the objects it adopted: worker-us's old gen filled at that read, 2026-10-24. One read each
   *  [[IdentityListingIntake.WholeReadEvery]] calls is whole; nothing moves between them here. */
  private def projectionsWholeRead(intake: IdentityListingIntake, live: Seq[Cinema], usedAfterGc: () => Long): Unit = {
    val model = intake.projectedByVenue(live).flatMap(_._2).map(_.listing)    // whole: the listings the model adopts
    intake.adopt(model)
    (2 to IdentityListingIntake.WholeReadEvery).foreach(_ => intake.projectedByVenue(live))
    val steady = usedAfterGc()
    val whole  = intake.projectedByVenue(live)                                  // whole again
    val during = usedAfterGc()
    val modelled = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[services.identity.Listing, java.lang.Boolean])
    model.foreach(modelled.add)
    val copies = whole.iterator.flatMap(_._2).count(p => !modelled.contains(p.listing))
    println(f"the projection's whole read, beside the model's ${model.size} listings: held ${(during - steady) / 1e6}%.0f MB more " +
      f"($copies listings not the model's object)")
    intake.adopt(model)
    println(f"after the next adoption: ${(usedAfterGc() - steady) / 1e6}%.0f MB over the steady state (${whole.size} venues)")
  }
}
