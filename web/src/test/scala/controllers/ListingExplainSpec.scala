package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import play.twirl.api.HtmlFormat
import services.identity.ResolverDecision
import services.movies.ListingKey
import services.review._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.{Clock, Instant, ZoneOffset}

/** `/debug/review/listing`: one production listing explained — over REAL prod decisions and traces ([[ProdReviewSample]]),
 *  and a hand-built source for what the sample lacks (a listing served otherwise than decided, a judgement row). */
class ListingExplainSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {

  private def controller(mode: Mode, sources: Map[Country, ReviewSource], labels: Path = tempFile("labels", ".tsv")) =
    new ReviewController(Helpers.stubControllerComponents(), mode, sources, new ReviewAnswers(new InMemoryReviewAnswerStore), labels,
      Clock.fixed(Instant.parse("2026-10-06T12:00:00Z"), ZoneOffset.UTC))

  private val sample = controller(Mode.Dev,
    ProdReviewSample.Databases.map { case (db, country) => country -> (ProdReviewSample.source(db): ReviewSource) })

  private val rise  = ProdReviewSample.traces("kinowo_uk").find(_.listing.rawTitle == "Rise Fly Fishing Film Tour").get
  private def esc(text: String) = HtmlFormat.escape(text).body

  private def page(c: ReviewController, country: String, venue: Option[String], title: String): String = {
    val result = c.listing(country, venue, title)(FakeRequest())
    status(result) shouldBe OK
    contentAsString(result)
  }

  "a listing's explain page" should "show its take, the latest trace's stage, rules, runner-up and veto, and its cluster on the review pages" in {
    val html    = page(sample, "uk", Some(rise.listing.venue), rise.listing.rawTitle)
    val cluster = ProdReviewSample.decisions.collectFirst {
      case (country, d) if d.listings.contains(rise.listing) => ReviewCluster.of(country, d)
    }.get
    html should include("<h1>Rise Fly Fishing Film Tour</h1>")
    html should include(s"""<span class="tag">${cluster.basis}</span>""")
    html should include(s"<tr><th>stage</th><td>${rise.basis}")
    for (rule <- rise.rules) html should include(esc(rule))
    html should include(s"<tr><th>runner-up</th><td>${esc(rise.candidates.head)}</td></tr>")
    html should include(s"<tr><th>vetoed by</th><td>${esc(rise.vetoedBy.get)}</td></tr>")
    for (line <- rise.evidence) html should include(esc(line))               // the Why fold-out's trace, whole
    for (r <- rise.refusals) html should include(esc(s"${r.rule}: ${r.why}"))
    html should include(esc(s"/debug/review/why?country=uk&cluster=${cluster.id}"))
    html should include("no row in identity-judgements.tsv")
  }

  it should "hand over its take history as a LogsQL query of production's take-change lines, opened in VictoriaLogs" in {
    val html  = page(sample, "uk", Some(rise.listing.venue), rise.listing.rawTitle)
    val query = TakeHistory(Country.UnitedKingdom, rise.listing).query
    // the take-change lines' deciding stage and rule, score, runner-up and trigger are columns of the history
    Seq("trigger", "stage", "decided_by", "score", "runner_up_tmdb", "runner_up_score", "margin", "runner_up_won", "title_rules",
      "previous_decided_by").foreach(field => query should include(s", $field"))
    query should startWith(s"""_time:30d "identity take change" country:uk venue:="${rise.listing.venue}" listing:="Rise Fly Fishing Film Tour"""")
    for (field <- Seq("_time", "build", "kind", "tmdb_before", "tmdb_after", "basis", "rules", "runner_up")) query should include(field)
    html should include(s"""<pre class="logsql">${esc(query)}</pre>""")
    html should include(esc(TakeHistory(Country.UnitedKingdom, rise.listing).link))
    TakeHistory(Country.UnitedKingdom, rise.listing).link should startWith("https://logs.kinowo.net/select/vmui/#/?query=_time%3A30d%20")
    html should include(esc(s"scripts/listing-history uk --venue '${rise.listing.venue}' --title 'Rise Fly Fishing Film Tour' --since 30d"))
    html should include(esc(s"scripts/identity-slice uk --explain --venue '${rise.listing.venue}' --title 'Rise Fly Fishing Film Tour'"))
  }

  "a take-history query" should "quote a title's quotes and backslashes as LogsQL reads them, and a shell word's quotes" in {
    val key = ListingKey.Native("Kino \"Muza\"", "https://muza/1", """Rock 'n' Roll \ Live""")
    TakeHistory(Country.Poland, key).query should include("""venue:="Kino \"Muza\"" listing:="Rock 'n' Roll \\ Live"""")
    TakeHistory(Country.Poland, key).command should include("""--title 'Rock '\''n'\'' Roll \ Live'""")
  }

  // A listing the projection serves as another film than its cluster was decided, judged in the labels file.
  private val kafka   = ListingKey.Native("Kino Muza", "https://muza/kafka", "FRANZ KAFKA")
  private val elsewhere = ListingKey.Native("Kino Pod Baranami", "https://baranami/kafka", "Franz Kafka")
  private val handBuilt = Map[Country, ReviewSource](Country.Poland -> new InMemoryReviewSource(
    Seq(ResolverDecision(Seq(kafka, elsewhere), Some(1157322), 0.91, ResolverDecision.Basis.OwnMatch, Seq("own match 1157322 at 91.0%"))()),
    filmsHeld = Map(1157322 -> FilmCard(1157322, None, Some("Franz"), None, Some(2025), Nil, None, None, None)),
    servedHeld = Map(kafka -> Some(4321))))

  private def labelsWith(rows: String*): Path = {
    val path = tempFile("labels", ".tsv")
    Files.write(path, (LabelsTsv.Header +: rows).mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    path
  }

  it should "say when production serves another film than decided, and show the judgements filed on the listing" in {
    val labels = labelsWith(
      "pl\t*\tFRANZ KAFKA\ttmdb:1157322\tright\tknown\t\tFranz\t\t2026-10-18\treview page: Agnieszka Holland's",
      "pl\tKino Pod Baranami\tFRANZ KAFKA\ttmdb:1\twrong\tknown\t\t\t\t2026-10-18\tother venue's row",
      "uk\t*\tFRANZ KAFKA\ttmdb:2\twrong\tknown\t\t\t\t2026-10-18\tother country's row")
    val html = page(controller(Mode.Dev, handBuilt, labels), "pl", Some("Kino Muza"), "FRANZ KAFKA")
    html should include("Franz (tmdb:1157322)")
    html should include("""<div class="s warn">production serves it as""")
    html should include("tmdb:4321")
    html should include("""<td class="right">right</td>""")
    html should include("review page: Agnieszka Holland&#x27;s")
    html should not include "other venue&#x27;s row"
    html should not include "other country&#x27;s row"
    html should include("no trace of this listing in identity_traces")
  }

  it should "offer the listings whose title contains what was asked, when none is billed as it exactly" in {
    val html = page(controller(Mode.Dev, handBuilt), "pl", None, "kafka")
    html should include(esc(ListingExplain.path(Country.Poland, kafka)))
    html should include(esc(ListingExplain.path(Country.Poland, elsewhere)))
    page(controller(Mode.Dev, handBuilt), "pl", None, "Zzz") should include("no listing in pl is billed as")
  }

  it should "404 in production, and for a country no mirror holds" in {
    status(controller(Mode.Prod, handBuilt).listing("pl", None, "FRANZ KAFKA")(FakeRequest())) shouldBe NOT_FOUND
    status(controller(Mode.Dev, handBuilt).listing("de", None, "FRANZ KAFKA")(FakeRequest())) shouldBe NOT_FOUND
  }

  "a card's Why" should "link each member listing to its explain page" in {
    val cluster = ProdReviewSample.decisions.collectFirst {
      case (country, d) if d.listings.contains(rise.listing) => ReviewCluster.of(country, d)
    }.get
    contentAsString(sample.why("uk", cluster.id)(FakeRequest())) should include(
      s"""<a class="explain" href="${esc(ListingExplain.path(Country.UnitedKingdom, rise.listing))}">explain</a>""")
  }
}
