package views

import com.sun.net.httpserver.HttpExchange
import controllers.ReviewController
import models.Country
import modules.CspFilter
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout}
import play.api.test.{FakeRequest, Helpers}
import services.review._
import tools.{CdpPage, Chrome, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.{Await, ExecutionContext}

/**
 * The local review pages against the PUBLISHED review pages' design, in one headless Chrome: the published
 * template (`resources/review/reference-template.html`, verbatim, inside the artifact host's reset) filled by its
 * own `card()` from items equivalent to [[ReviewFixtures]], beside the Twirl page the real controller renders from
 * them — served with the headers the app sends (the real [[CspFilter]] over the controller's own), since a policy
 * that blocks the fonts is invisible to a page fetched without it.
 *
 * Every matched element must compute the same font, size, weight, line height, spacing, radius and colours, and
 * the page must load the three Google families the template does. Side-by-side screenshots land in
 * `web/target/review-screenshots/`.
 */
class ReviewDesignParitySpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private implicit val system: ActorSystem             = ActorSystem("review-design-parity")
  private implicit val materializer: Materializer      = Materializer(system)
  private implicit val executionContext: ExecutionContext = system.dispatcher

  private val now        = Instant.parse("2026-10-06T10:00:00Z")
  private val labels     = Files.createTempFile("labels", ".tsv")
  private val controller = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    Map(Country.Poland -> ReviewFixtures.source(now)), new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC))

  /** The published page as the artifact host serves it: its small reset, then the template verbatim. */
  private lazy val reference: String = {
    val template = new String(Files.readAllBytes(Paths.get(getClass.getResource("/review/reference-template.html").toURI)), StandardCharsets.UTF_8)
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">" +
      "<style>*, *::before, *::after { box-sizing: border-box; } body { margin: 0; }</style></head><body>" +
      template.replace("__TITLE__", "Review queue").replace("__LEDE__", "Listing clusters the resolver left unmatched.").replace("__SORT__", "1") +
      "</body></html>"
  }

  /** [[ReviewFixtures]]' held Kafka cluster as the template's items store holds one. */
  private val items: Seq[JsObject] = Seq(Json.obj(
    "id" -> "kafka", "title" -> "FRANZ KAFKA", "country" -> "pl", "confidence" -> 86,
    "members" -> Json.arr(Json.obj("venue" -> ReviewFixtures.Held.venue, "page" -> ReviewFixtures.Held.nativeId,
      "venueFacts" -> Json.obj("year" -> 2025, "directors" -> Json.arr("Agnieszka Holland"), "runtime" -> 127, "cast" -> Json.arr("Idan Weiss"),
        "catalogueIds" -> "bilety24:165208", "screenings" -> Json.obj("count" -> 1, "first" -> "2026-10-10 18:00", "last" -> "2026-10-10 18:00")))),
    "synopsis" -> "Biografia Kafki.",
    "expected" -> Json.obj("tmdb" -> 1157322, "imdb" -> "tt22963134", "title" -> "Franz", "year" -> 2025,
      "directors" -> Json.arr("Agnieszka Holland"), "runtime" -> 127, "overview" -> "Kafka's life."),
    "candidates" -> Json.arr(Json.obj("tmdb" -> 1703622, "title" -> "Royal Ballet and Opera: Macbeth", "year" -> 2026,
      "directors" -> Json.arr("Phyllida Lloyd"), "runtime" -> 180, "pct" -> 62)),
    "why" -> Json.arr("best rejected candidate 1157322 at 86.4% (title exact, year none)")))

  /** The local page with every header the app would send: the controller's, then the CSP filter's. */
  private def local(exchange: HttpExchange): Boolean =
    exchange.getRequestURI.getPath == "/local" && {
      val result = Await.result(new CspFilter().apply(_ => controller.queue(Some("pl"), 60, false)(FakeRequest()))(FakeRequest()),
        tools.SpecTimeouts.Io)
      val bytes = contentAsString(scala.concurrent.Future.successful(result)).getBytes(StandardCharsets.UTF_8)
      result.header.headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
      exchange.getResponseHeaders.add("Content-Type", "text/html; charset=UTF-8")
      exchange.sendResponseHeaders(result.header.status, bytes.length.toLong)
      val os = exchange.getResponseBody
      try os.write(bytes) finally os.close()
      true
    }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/reference" => reference }, dynamicRoute = local)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
    Files.deleteIfExists(labels): Unit
    system.terminate(): Unit
  }

  /** The elements both pages draw, each by the selector that finds its first instance in either. */
  private val Selectors = Seq("body", "h1", ".lede", ".count", ".filters button", ".filters button[aria-pressed=true]", ".card", ".raw",
    ".raw .tag", ".conf", ".card .meta", "dl.facts", "dl.facts dt", "dl.facts dd", ".syn", ".lead", ".lead .kind", ".cand .t b", ".cand .s",
    ".lead .cand", ".pct", ".card .btn", ".card .btn.yes", ".own input", "details.why", "details.why summary", ".listing .poster.empty")

  private val Properties = Seq("fontFamily", "fontSize", "fontWeight", "lineHeight", "letterSpacing", "padding", "borderRadius",
    "color", "backgroundColor", "textTransform")

  /** Each selector's computed style, the font family reduced to its first face. */
  private def styles(page: CdpPage): Map[String, Map[String, String]] =
    Selectors.map { sel =>
      val js = s"""(function(){ var e = document.querySelector(${Json.toJson(sel)}); if (!e) return null; var s = getComputedStyle(e);
        return { ${Properties.map(p => s"$p: s.$p").mkString(", ")} }; })()"""
      sel -> page.eval(js).asOpt[Map[String, String]].getOrElse(Map("missing" -> "no element")).map {
        case ("fontFamily", v) => "fontFamily" -> v.split(",").head.trim.stripPrefix("\"").stripSuffix("\"")
        case other             => other
      }
    }.toMap

  /** The font families the page has loaded a face of, once its fonts settle. */
  private def loadedFamilies(page: CdpPage): Set[String] = {
    page.eval("document.fonts.ready.then(function(){ return true; })")
    page.eval("Array.from(document.fonts).filter(function(f){ return f.status === 'loaded'; }).map(function(f){ return f.family.replace(/\"/g, ''); })")
      .as[Seq[String]].toSet
  }

  private val Families = Set("Archivo", "IBM Plex Sans", "IBM Plex Mono")

  private def shoot(page: CdpPage, name: String): Unit = {
    page.awaitRenderedFrame()
    ReviewPageSpec.save(page, s"$name.png")
  }

  private val Viewports = Seq(("desktop", 1280, 900, false), ("mobile", 390, 844, true))

  private def measure(page: CdpPage, name: String): Map[String, (Map[String, Map[String, String]], Set[String])] =
    Viewports.map { case (label, width, height, mobile) =>
      if (mobile) page.setViewport(width, height) else page.setDesktopViewport(width, height)
      page.awaitRenderedFrame()
      val families = loadedFamilies(page)
      val computed = styles(page)
      shoot(page, s"$name-$label")
      label -> (computed, families)
    }.toMap

  "the local review page" should "compute the published review page's fonts, sizes, spacing and colours, element for element" in {
    chrome match {
      case None => cancel("Chrome not installed — skipping the review design parity test")
      case Some(c) =>
        val published = c.openPage(server.baseUrl + "/reference") { page =>
          page.eval(s"state.items = ${Json.toJson(items)}; state.filter = 'all'; render(); notice('');")
          page.waitFor("!!document.querySelector('.card')")
          // the template filters to the open items; show the same filter state as the local page
          page.eval("document.getElementById('fOpen').setAttribute('aria-pressed', 'true')")
          measure(page, "reference")
        }
        val ours = c.openPage(server.baseUrl + "/local")(measure(_, "local"))
        for (label <- Viewports.map(_._1)) {
          val (want, wantFonts) = published(label)
          val (got, gotFonts)   = ours(label)
          withClue(s"[$label] ") {
            val differing = for {
              sel      <- Selectors
              property <- (want(sel).keySet ++ got(sel).keySet).toSeq.sorted
              if want(sel).get(property) != got(sel).get(property)
            } yield s"$sel $property: published ${want(sel).getOrElse(property, "-")}, local ${got(sel).getOrElse(property, "-")}"
            differing shouldBe empty
            // the reference loads them from Google; where it cannot (no network) there is nothing to match
            assume(Families.subsetOf(wantFonts), s"the published template loaded only $wantFonts — no network to Google Fonts?")
            gotFonts should contain allElementsOf Families
          }
        }
    }
  }
}
