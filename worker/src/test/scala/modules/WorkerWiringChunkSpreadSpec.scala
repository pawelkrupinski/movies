package modules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.*
import settings.ScrapeChunkSpread
import tools.TestWiring

/** The scrape reaper sizes its outstanding-task budget by how long a chunked venue's fan-out is
 *  staggered, so it must read the SAME spread the chunk planner staggers by. It used to be wired
 *  to `ScrapeCadence.ChunkEnqueueSpread` directly, so a KINOWO_SCRAPE_CHUNK_SPREAD_MINUTES override
 *  moved the planner's spread while the reaper kept budgeting for the default. */
class WorkerWiringChunkSpreadSpec extends AnyFlatSpec with Matchers {

  "the worker's composition root" should "hand the scrape reaper the chunk planner's configured spread" in {
    val wiring = new WorkerWiring(Country.default) with TestWiring {
      override def scrapeChunkSpread: ScrapeChunkSpread = ScrapeChunkSpread(17.minutes)
    }
    try wiring.scrapeReaper.chunkSpread shouldBe ScrapeChunkSpread(17.minutes)
    finally wiring.stop()
  }
}
