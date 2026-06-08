package controllers

import models.Cinema
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers
import services.UptimeMonitor

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The /uptime page groups a cinema's deferred-detail enrichment health
 * ("<cinema>|enrichment") under the cinema as a sub-row, rather than letting it
 * fall into the "Other" section. `groupRows` is the pure grouping decision.
 */
class UptimeControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("uptime-controller-spec")
  private implicit val mat: Materializer = Materializer(sys)
  override def afterAll(): Unit = Await.result(sys.terminate(), 10.seconds)

  private val controller = new UptimeController(Helpers.stubControllerComponents(), new UptimeMonitor())
  private def fakeRow(n: String) = ServiceRow(n, Seq.empty)

  // A real cinema that the controller will place under "Cinemas" via Cinema.byCity.
  private val cinema = Cinema.byCity.head._2.head.displayName

  "groupRows" should "attach a cinema's |enrichment service as a sub-row, never in Other" in {
    val enrich = UptimeMonitor.enrichmentService(cinema)
    val active = Set(cinema, enrich, "Some Other Service")

    val (cinemasByCity, _, other) = controller.groupRows(active, fakeRow)

    val row = cinemasByCity.flatMap(_._2).find(_.name == cinema).get
    row.enrichment.map(_.name) shouldBe Some(enrich)         // grouped under its cinema
    other.map(_.name) should contain("Some Other Service")   // genuine "other" still shows
    other.map(_.name) should not contain enrich              // enrichment is NOT a standalone Other row
  }

  it should "leave a cinema without an active enrichment service un-subrowed" in {
    val (cinemasByCity, _, _) = controller.groupRows(Set(cinema), fakeRow)
    cinemasByCity.flatMap(_._2).find(_.name == cinema).get.enrichment shouldBe None
  }
}
