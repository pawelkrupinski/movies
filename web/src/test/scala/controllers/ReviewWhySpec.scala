package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.review._

import java.nio.file.Files
import java.time.{Clock, ZoneOffset}

/** A card's "Why" fold-out over REAL prod decisions and traces ([[ProdReviewSample]]): nothing of a listing's trace left out. */
class ReviewWhySpec extends AnyFlatSpec with Matchers {

  private val controller = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    ProdReviewSample.Databases.map { case (db, country) => country -> (ProdReviewSample.source(db): ReviewSource) },
    new ReviewAnswers(new InMemoryReviewAnswerStore), Files.createTempFile("labels", ".tsv"), Clock.fixed(ProdReviewSample.newestSlot, ZoneOffset.UTC))

  private val vetoed = ProdReviewSample.decisions.collectFirst {
    case (country, d) if d.members.exists(_.rawTitle == "Rise Fly Fishing Film Tour") => ReviewCluster.of(country, d)
  }.get
  private val trace = ProdReviewSample.traces("kinowo_uk").find(_.listing.rawTitle == "Rise Fly Fishing Film Tour").get

  "a card's Why" should "show every evidence line of its listings' traces, for and against, and every refusal with its veto rule" in {
    val result = controller.why(vetoed.country.code, vetoed.id)(FakeRequest())
    status(result) shouldBe OK
    val html = play.twirl.api.HtmlFormat.raw(contentAsString(result)).body
    trace.evidence should not be empty
    for (line <- trace.evidence) html should include(play.twirl.api.HtmlFormat.escape(line).body)
    html should include("""<ul class="for">""")
    html should include("""<ul class="against">""")
    for (r <- trace.refusals) html should include(play.twirl.api.HtmlFormat.escape(s"${r.rule}: ${r.why}").body)
    html should include(play.twirl.api.HtmlFormat.escape("best denied: Learned(runtime.delta >= 11 AND title in {none,overlap})").body)
    html should include("""<li class="veto">vetoed by""")
  }

  it should "say so of a cluster no country holds, and 404 in production" in {
    status(controller.why("uk", "no-such-cluster")(FakeRequest())) shouldBe NOT_FOUND
    status(new ReviewController(Helpers.stubControllerComponents(), Mode.Prod, Map(Country.Poland -> ReviewSource.empty),
      new ReviewAnswers(new InMemoryReviewAnswerStore), Files.createTempFile("labels", ".tsv"), Clock.systemUTC())
      .why("pl", vetoed.id)(FakeRequest())) shouldBe NOT_FOUND
  }

  "an evidence line" should "read as its feature and its signed weight" in {
    Contribution.parse("director=same_person +4.22") shouldBe Contribution("director=same_person", 4.22, "director=same_person +4.22")
    Contribution.parse("year.delta=31 -7.77").isFor shouldBe false
    Contribution.parse("season.delta=missing:listing +0.00").weight shouldBe 0.0
    Contribution.parse("odd line").weight shouldBe 0.0
  }
}
