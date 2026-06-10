package controllers

import models.Cinema
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.UptimeMonitor
import services.fallback.{FallbackEvent, FilmwebFallbackState, InMemoryFilmwebFallbackStore}

import java.time.Instant
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

  private val fallbackStore = new InMemoryFilmwebFallbackStore
  private val controller = new UptimeController(Helpers.stubControllerComponents(), new UptimeMonitor(), fallbackStore)
  private def fakeRow(n: String) = ServiceRow(n, Seq.empty)

  private def fallbackState(name: String, active: Boolean) = FilmwebFallbackState(
    cinema = name, active = active, filmwebCinemaId = Some(2180),
    since = Some(Instant.ofEpochMilli(1_700_000_000_000L)), lastReason = Some("RuntimeException: down"),
    consecutiveFailures = if (active) 2 else 0,
    lastPrimaryProbeAt = Some(Instant.ofEpochMilli(1_700_000_100_000L)),
    nextPrimaryProbeAt = if (active) Some(Instant.ofEpochMilli(1_700_001_000_000L)) else None,
    updatedAt = Instant.ofEpochMilli(1_700_000_200_000L),
    history = List(FallbackEvent(Instant.ofEpochMilli(1_700_000_000_000L), FallbackEvent.Enter, "down"))
  )

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

  // Cinema City fetches each film's detail once per network and records its
  // health as a single "Cinema City Enrichment" entry — a standalone enrichment
  // service, not a per-venue sub-row and not adrift in "Other".
  it should "list the network-level Cinema City Enrichment as a standalone enrichment service" in {
    val (_, services, other) = controller.groupRows(Set("Cinema City Enrichment", "Some Other Service"), fakeRow)
    services.map(_.name) should contain("Cinema City Enrichment")
    other.map(_.name)    should not contain "Cinema City Enrichment"
  }

  // The chain-wide enrichment row leads the Global section, ahead of the
  // external rating sources.
  it should "render Cinema City Enrichment first among enrichment services" in {
    val (_, services, _) = controller.groupRows(Set("Cinema City Enrichment", "TMDB", "IMDb"), fakeRow)
    services.map(_.name).head shouldBe "Cinema City Enrichment"
  }

  "the /uptime/fallback page" should "list active fallbacks, recovered cinemas, and Filmweb-only-by-design venues" in {
    fallbackStore.put(fallbackState("Kino Praha", active = true))
    fallbackStore.put(fallbackState("Kino Iluzjon", active = false))
    fallbackStore.putFilmwebOnly(Set("Kino Astra"))

    val result = controller.fallback()(FakeRequest())
    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include ("Currently on fallback (1)")
    html should include ("Kino Praha")        // active
    html should include ("Recently recovered (1)")
    html should include ("Kino Iluzjon")       // recovered (inactive, has history)
    html should include ("Kino Astra")         // Filmweb-only by design
    html should include ("RuntimeException: down")  // the active row's reason rendered
  }
}
