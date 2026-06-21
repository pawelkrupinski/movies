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
  private val controller = new UptimeController(Helpers.stubControllerComponents(), TestAdminAction(), new UptimeMonitor(), fallbackStore)
  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)
  private def fakeRow(n: String) = ServiceRow(n, Seq.empty)

  /** A bar series, oldest→newest, from status keywords ("red"/"zero"/"green"/"empty"). */
  private def bars(statuses: String*): Seq[BarData] =
    statuses.zipWithIndex.map { case (s, i) => BarData(s"svc", i.toLong, "", "", "", s, 0, 0, 0, Seq.empty) }
  /** A `row` lookup that returns a crafted bar series for named services, else an empty row. */
  private def rowsFrom(crafted: Map[String, Seq[BarData]]): String => ServiceRow =
    n => ServiceRow(n, crafted.getOrElse(n, Seq.empty))

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

    val (_, _, cinemasByCity, _, other) = controller.groupRows(active, fakeRow)

    val row = cinemasByCity.flatMap(_._2).find(_.name == cinema).get
    row.enrichment.map(_.name) shouldBe Some(enrich)         // grouped under its cinema
    other.map(_.name) should contain("Some Other Service")   // genuine "other" still shows
    other.map(_.name) should not contain enrich              // enrichment is NOT a standalone Other row
  }

  it should "leave a cinema without an active enrichment service un-subrowed" in {
    val (_, _, cinemasByCity, _, _) = controller.groupRows(Set(cinema), fakeRow)
    cinemasByCity.flatMap(_._2).find(_.name == cinema).get.enrichment shouldBe None
  }

  // Cinema City fetches each film's detail once per network and records its
  // health as a single "Cinema City Enrichment" entry — a standalone enrichment
  // service, not a per-venue sub-row and not adrift in "Other".
  it should "list the network-level Cinema City Enrichment as a standalone enrichment service" in {
    val (_, _, _, services, other) = controller.groupRows(Set("Cinema City Enrichment", "Some Other Service"), fakeRow)
    services.map(_.name) should contain("Cinema City Enrichment")
    other.map(_.name)    should not contain "Cinema City Enrichment"
  }

  // The chain-wide enrichment row leads the Global section, ahead of the
  // external rating sources.
  it should "render Cinema City Enrichment first among enrichment services" in {
    val (_, _, _, services, _) = controller.groupRows(Set("Cinema City Enrichment", "TMDB", "IMDb"), fakeRow)
    services.map(_.name).head shouldBe "Cinema City Enrichment"
  }

  // ── Triage: failing / no-screenings lead, pulled out of the city group ───────
  private val city = Cinema.byCity.head._1

  "triage grouping" should "promote a cinema failing its last 3 scrapes into Failing, tagged with its city" in {
    val row = rowsFrom(Map(cinema -> bars("green", "red", "red", "red")))
    val (failing, zero, cinemasByCity, _, _) = controller.groupRows(Set(cinema), row)

    failing.map(_.row.name) should contain(cinema)
    failing.find(_.row.name == cinema).get.city shouldBe Some(city)
    zero.map(_.row.name) should not contain cinema
    cinemasByCity.flatMap(_._2).map(_.name) should not contain cinema   // removed from its city group
  }

  it should "promote a cinema returning no screenings into the No-screenings section" in {
    val row = rowsFrom(Map(cinema -> bars("zero", "zero", "zero")))
    val (failing, zero, cinemasByCity, _, _) = controller.groupRows(Set(cinema), row)
    zero.map(_.row.name) should contain(cinema)
    failing.map(_.row.name) should not contain cinema
    cinemasByCity.flatMap(_._2).map(_.name) should not contain cinema
  }

  it should "keep a healthy cinema in its city group, out of triage" in {
    val row = rowsFrom(Map(cinema -> bars("green", "green", "green")))
    val (failing, zero, cinemasByCity, _, _) = controller.groupRows(Set(cinema), row)
    failing shouldBe empty
    zero shouldBe empty
    cinemasByCity.flatMap(_._2).map(_.name) should contain(cinema)
  }

  it should "treat a lone red blip among greens as healthy (needs all of the last 3 red)" in {
    val row = rowsFrom(Map(cinema -> bars("green", "green", "red")))
    val (failing, _, cinemasByCity, _, _) = controller.groupRows(Set(cinema), row)
    failing.map(_.row.name) should not contain cinema
    cinemasByCity.flatMap(_._2).map(_.name) should contain(cinema)
  }

  it should "promote a failing enrichment service (no city) into Failing, not the enrichment section" in {
    val row = rowsFrom(Map("TMDB" -> bars("red", "red", "red")))
    val (failing, _, _, services, _) = controller.groupRows(Set("TMDB"), row)
    failing.map(_.row.name) should contain("TMDB")
    failing.find(_.row.name == "TMDB").get.city shouldBe None
    services.map(_.name) should not contain "TMDB"
  }

  it should "promote a cinema whose enrichment sub-row is failing, keeping the pair together" in {
    val enrich = UptimeMonitor.enrichmentService(cinema)
    val row = rowsFrom(Map(cinema -> bars("green", "green", "green"), enrich -> bars("red", "red", "red")))
    val (failing, _, cinemasByCity, _, _) = controller.groupRows(Set(cinema, enrich), row)

    failing.map(_.row.name) should contain(cinema)
    failing.find(_.row.name == cinema).get.row.enrichment.map(_.name) shouldBe Some(enrich)
    cinemasByCity.flatMap(_._2).map(_.name) should not contain cinema
  }

  "the rendered /uptime page" should "show a bespoke client's marker as just \"Custom\", with the city on hover" in {
    val failing = Seq(FlaggedRow(
      ServiceRow("Kino Rialto", bars("red", "red", "red"), tags = Set("custom:RialtoClient")),
      Some("Poznań")))
    val html = views.html.uptime(failing, Nil, Nil, Nil, Nil, Nil).body
    html should include ("Failing — last 3 scrapes")
    html should include ("""data-city="Poznań"""")           // city pops on name hover (instant tooltip)
    html should include ("tag-custom")                       // styled by kind
    html should include (">Custom<")                         // bespoke clients read just "Custom"
    html should include ("""title="RialtoClient"""")         // the actual class on hover
    html should not include "city-chip"                      // city is no longer a visible chip
  }

  it should "name the client for a shared marker, stripping the Client/Scraper suffix" in {
    val failing = Seq(FlaggedRow(
      ServiceRow("Kino Tatry", bars("red", "red", "red"), tags = Set("shared:FilmwebShowtimesClient")),
      None))
    val html = views.html.uptime(failing, Nil, Nil, Nil, Nil, Nil).body
    html should include ("tag-shared")
    html should include (">FilmwebShowtimes<")                 // suffix dropped for the chip
    html should include ("""title="FilmwebShowtimesClient"""") // full class on hover
  }

  "the /uptime page's Filmweb-fallback section" should "list only the cinemas currently on fallback — not recovered or by-design venues" in {
    fallbackStore.put(fallbackState("Kino Praha", active = true))
    fallbackStore.put(fallbackState("Kino Iluzjon", active = false))   // recovered (inactive, has history)

    val result = controller.index(adminSession)
    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include ("Filmweb fallback — currently on fallback (1)")
    html should include ("Kino Praha")              // active fallback shown
    html should include ("RuntimeException: down")  // the active row's reason rendered
    // The recovered table and Filmweb-only-by-design table were dropped …
    html should not include ("Recently recovered")
    html should not include ("Kino Iluzjon")        // recovered cinema no longer surfaced
    html should not include ("Filmweb-only by design")
  }

  // ── Auth gate: /uptime is an operational page, closed like the title-rules
  //    editor (login session + ADMIN_ALLOWLIST). ───────────────────────────────
  "the /uptime gate" should "401 an anonymous request" in {
    status(controller.index(FakeRequest())) shouldBe UNAUTHORIZED
  }

  it should "403 a logged-in user whose email is not on the allowlist" in {
    val outsider = new UptimeController(Helpers.stubControllerComponents(),
      TestAdminAction(allow = Set("someone-else@example.com")), new UptimeMonitor(), fallbackStore)
    status(outsider.index(adminSession)) shouldBe FORBIDDEN
  }

  it should "render /uptime for an allowlisted admin" in {
    status(controller.index(adminSession)) shouldBe OK
  }
}
