package tools

import models.{Cinema, CinemaMovie, KinoMikro, KinoMuza, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import scala.jdk.CollectionConverters._

/** A pipeline leg's next-day walk lands its venues through the production runner side by side, as
 *  production's scrape pool does, rather than one after another: serially the US walk was ~66-75 s of
 *  Mongo round-trips on the convergence leg's critical path (run 37111868620). */
class PipelineLandingSpec extends AnyFlatSpec with Matchers {

  private def venue(c: Cinema)(listing: => Seq[CinemaMovie]): CinemaScraper = new CinemaScraper {
    val cinema: Cinema               = c
    def scrapeHosts: Set[String]     = Set.empty
    def fetch(): Seq[CinemaMovie]    = listing
  }

  "landPipeline" should "land venues side by side" in {
    val wiring   = new FixtureTestWiring("08-06-2026")
    val together = new CountDownLatch(2)
    val failed   = new ConcurrentLinkedQueue[String]()
    // Each venue's fetch waits for the other's: one after another, the first never sees the second.
    def meeting = { together.countDown(); if (!together.await(10, TimeUnit.SECONDS)) throw new IllegalStateException("landed alone"); Seq.empty }
    wiring.landPipeline(Seq(venue(KinoMikro)(meeting), venue(KinoMuza)(meeting)))(failed.add(_))
    failed.asScala shouldBe empty
  }

  it should "hand each venue that fails to the caller, and land the rest" in {
    val wiring = new FixtureTestWiring("08-06-2026")
    val failed = new ConcurrentLinkedQueue[String]()
    wiring.landPipeline(Seq(venue(Rialto)(throw new java.io.IOException("down")), venue(KinoMikro)(Seq.empty)))(failed.add(_))
    failed.asScala.toSeq shouldBe Seq("Kino Rialto: java.io.IOException: down")
  }
}
