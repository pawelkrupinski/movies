package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.review._
import tools.SpecTimeouts

import java.nio.file.Files
import java.time.{Clock, Instant, ZoneOffset}

class ReviewControllerSpec extends AnyFlatSpec with Matchers {
  import ReviewFixtures._

  private val now   = Instant.parse("2026-10-06T10:00:00Z")
  private val clock = Clock.fixed(now, ZoneOffset.UTC)

  private def controller(mode: Mode, answers: ReviewAnswers = new ReviewAnswers(new InMemoryReviewAnswerStore),
                         labels: java.nio.file.Path = Files.createTempFile("labels", ".tsv")) =
    new ReviewController(Helpers.stubControllerComponents(), mode, Map(Country.Poland -> ReviewFixtures.source(now)), answers, labels, clock)

  "every review page" should "404 in production" in {
    val prod = controller(Mode.Prod)
    Seq(prod.queue(None, 60, false), prod.matchable(None, 0.5, None, 60, false), prod.recent(None, 48, 60, false), prod.history(),
      prod.exportLabels()).foreach(action => status(action(FakeRequest())) shouldBe NOT_FOUND)
    status(prod.answer()(FakeRequest().withBody(Json.obj()))) shouldBe NOT_FOUND
  }

  "the queue" should "render the unmatched clusters with the venue's facts, the candidates and the explanation" in {
    val html = contentAsString(controller(Mode.Dev).queue(Some("pl"), 60, false)(FakeRequest()))
    html should include("FRANZ KAFKA")
    html should include("""href="https://www.bilety24.pl/kino/967-franz-kafka-165208"""")
    html should include("Biografia Kafki.")                                 // the venue's synopsis
    html should include(s"""<img class="poster" src="${play.twirl.api.HtmlFormat.escape(tools.PosterProxy.proxy("https://bilety24/kafka.jpg"))}" alt="venue poster" loading="lazy"""")
    html should include("The venue gives no synopsis.")                     // Macbeth's venue gives none
    html should include("bilety24=165208")                                  // the feed's catalogue id, apart
    html should include("Kafka&#x27;s life.")                               // the candidate's overview
    html should include("86.4%")
    html should include("best rejected candidate 1157322")
    html should not include "Klondike"                                      // matched: not in the queue
  }

  it should "send the policy that lets it load the published review pages' Google fonts" in {
    val result = controller(Mode.Dev).queue(Some("pl"), 60, false)(FakeRequest())
    headers(result).get("Content-Security-Policy") shouldBe Some(modules.CspFilter.WithGoogleFonts)
  }

  it should "badge an unmatched card with its best candidate's probability, the no-match confidence named in the fold-out" in {
    val html = contentAsString(controller(Mode.Dev).queue(Some("pl"), 60, false)(FakeRequest()))
    html should include("""<span class="conf" title="best candidate">86.4%</span>""")
    html should not include "no match 14.0%"
    html should include("<li>BelowThreshold · no-match confidence 14.0%</li>")
    // a cluster with no candidate has no probability to badge
    html should not include "best 0.0%"
  }

  it should "describe a candidate the corpus lacks from the resolver's own TMDB record, the corpus's poster kept when it has one" in {
    def queueOver(films: Map[Int, FilmCard], records: Map[Int, FilmCard]) = contentAsString(new ReviewController(
      Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> new InMemoryReviewSource(Seq(heldDecision),
        filmsHeld = films, recordsHeld = records)), new ReviewAnswers(new InMemoryReviewAnswerStore),
      Files.createTempFile("labels", ".tsv"), clock).queue(Some("pl"), 60, false)(FakeRequest()))
    val record = FilmCard(kafka.tmdb, Some("tt22963134"), Some("Franz (TMDB)"), Some("Franz"), Some(2025), Seq("Agnieszka Holland"), Some(127), None, None)

    val recordOnly = queueOver(Map.empty, Map(kafka.tmdb -> record))
    recordOnly should include("<b>Franz (TMDB)</b>")
    recordOnly should include("2025 · Agnieszka Holland · 127 min")
    recordOnly should include("no TMDB poster")
    recordOnly should not include "not in this country&#x27;s corpus"

    val both = queueOver(Map(kafka.tmdb -> kafka), Map(kafka.tmdb -> record))
    both should include("<b>Franz (TMDB)</b>")
    both should include("""src="https://image.tmdb.org/t/p/w185/kafka.jpg"""")
    both should include("Kafka&#x27;s life.")
  }

  "the matchable page" should "show the basis and the veto's reason, filterable by basis" in {
    val c = controller(Mode.Dev)
    val all = contentAsString(c.matchable(Some("pl"), 0.5, None, 60, false)(FakeRequest()))
    all should include("BelowThreshold (1)")
    all should include("Vetoed (1)")
    all should include("denied: another house&#x27;s season production")
    val vetoed = contentAsString(c.matchable(Some("pl"), 0.5, Some("Vetoed"), 60, false)(FakeRequest()))
    vetoed should include("Macbeth")
    vetoed should not include "FRANZ KAFKA"
  }

  "the recently matched page" should "list the clusters matched in the window, by the slot write time" in {
    val html = contentAsString(controller(Mode.Dev).recent(Some("pl"), 48, 60, false)(FakeRequest()))
    html should include("Klondike")
    html should include("2026-10-06 09:00 UTC")
    contentAsString(controller(Mode.Dev).recent(Some("pl"), 0, 60, false)(FakeRequest())) should not include "Klondike"
  }

  "an answer" should "take its cluster out of the queue, an undo bring it back, and export to labels.tsv" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val labels  = Files.createTempFile("labels", ".tsv")
    val c       = controller(Mode.Dev, answers, labels)
    val card    = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head.payload(ReviewPage.Queue)
    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "right")))) shouldBe OK
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should not include "FRANZ KAFKA"
    contentAsString(c.queue(Some("pl"), 60, true)(FakeRequest())) should include("""class="btn yes right on" data-verdict="right">Right film""")

    val exported = contentAsJson(c.exportLabels()(FakeRequest()))
    (exported \ "added").as[Int] shouldBe 1
    LabelsTsv.read(labels) shouldBe Seq(LabelRow("pl", "Kino Opalenica", "FRANZ KAFKA", "tmdb:1157322", "right", "review page: right: Franz (2025)"))

    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "undo")))) shouldBe OK
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("FRANZ KAFKA")
    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "film")))) shouldBe BAD_REQUEST
  }

  it should "take a cluster the reviewer can't determine off the list, and label nothing for it" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val labels  = Files.createTempFile("labels", ".tsv")
    val c       = controller(Mode.Dev, answers, labels)
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("""data-verdict="unsure">Can't determine""")
    val card    = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head.payload(ReviewPage.Queue)
    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "unsure")))) shouldBe OK
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should not include "FRANZ KAFKA"

    val exported = contentAsJson(c.exportLabels()(FakeRequest()))
    (exported \ "added").as[Int] shouldBe 0
    (exported \ "summary").as[String] should not include "not exported"   // no film to label is the answer, not a gap
    LabelsTsv.read(labels) shouldBe empty
  }

  "every review page" should "read its countries side by side, not one after another" in {
    // Each country's read waits until the other's has started: read in turn, the first would wait out the barrier.
    val barrier = new java.util.concurrent.CyclicBarrier(2)
    def meeting(held: Seq[services.identity.ResolverDecision]): ReviewSource = new ReviewSource {
      private val source = new InMemoryReviewSource(held)
      export source.{decisions as _, *}
      def decisions(unmatchedOnly: Boolean) = { barrier.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); source.decisions(unmatchedOnly) }
    }
    val c = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
      Map(Country.Poland -> meeting(Seq(heldDecision)), Country.UnitedKingdom -> meeting(Nil)),
      new ReviewAnswers(new InMemoryReviewAnswerStore), Files.createTempFile("labels", ".tsv"), clock)
    val html = contentAsString(c.queue(None, 60, false)(FakeRequest()))
    html should not include "could not read"
    html should include("FRANZ KAFKA")
  }

  "the recently matched page" should "read the slot times while it reads the decisions" in {
    // Each read waits until the other has started: read in turn, the first would wait out the barrier.
    val barrier = new java.util.concurrent.CyclicBarrier(2)
    def meet[A](read: => A): A = { barrier.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); read }
    val meeting: ReviewSource = new ReviewSource {
      private val source = ReviewFixtures.source(now)
      export source.{decisions as _, updatedSince as _, *}
      def decisions(unmatchedOnly: Boolean) = meet(source.decisions(unmatchedOnly))
      def updatedSince(since: Instant)      = meet(source.updatedSince(since))
    }
    val html = contentAsString(new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> meeting),
      new ReviewAnswers(new InMemoryReviewAnswerStore), Files.createTempFile("labels", ".tsv"), clock).recent(Some("pl"), 48, 60, false)(FakeRequest()))
    html should not include "could not read"
    html should include("Klondike")
  }
}

