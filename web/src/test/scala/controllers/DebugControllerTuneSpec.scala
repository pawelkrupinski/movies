package controllers

import tools.SpecClock.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * `/debug/tune` is the dev-only visual-tuning page. It must render (200) in
 * Dev/Test mode and 404 in Prod — the same `devOnly` gate as `/debug`. The
 * sample films are built in-process (`TuneSamples.films`), so the
 * page renders without any cache contents; the controller below is wired with
 * an empty repository on purpose to prove that.
 */
class DebugControllerTuneSpec extends AnyFlatSpec with Matchers {

  private def buildController(mode: Mode): DebugController =
    TestDebugController.build(Seq.empty, mode)._1

  "GET /debug/tune" should "render the tuning page in dev mode" in {
    val ctrl   = buildController(Mode.Dev)
    val result = ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe OK
    contentAsString(result) should include("tune-scope")
    // The real card partial rendered — the sample film's title is present.
    contentAsString(result) should include("Incepcja")
  }

  it should "render the pill / rating edge cases the page exists to tune" in {
    val ctrl = buildController(Mode.Dev)
    val html = contentAsString(ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune")))

    // RT below 60 → the `.rotten` red variant (Morbius, RT 15).
    html should include("rating-rt rotten")
    // A wide format token from the many-showtimes wrapping card.
    html should include("4DX")
    // The lone Metacritic bare-number pill (Aftersun) — and a card with no
    // ratings row at all (the preview / untitled film, enrichment = None).
    html should include("Aftersun")
    html should include("Niezatytułowany")
    // Programme-prefixed title kept verbatim (separate-row case).
    html should include("Kino Seniora")
  }

  "TuneSamples.films" should "cover the pill/rating edge cases" in {
    val films = TuneSamples.films

    // A card with no ratings at all (no ratings row content).
    films.exists { f =>
      val r = f.resolved.ratings
      r.imdb.isEmpty && r.metascore.isEmpty && r.rottenTomatoes.isEmpty && r.filmweb.isEmpty
    } shouldBe true
    // A rotten RT (below 60) and the widest values (10.0 / 100 / 100%).
    films.flatMap(_.resolved.ratings.rottenTomatoes).min should be < 60
    films.flatMap(_.resolved.ratings.imdb).max shouldBe 10.0
    // A card whose single cinema has many showtimes (pills wrap several rows).
    val maxSlotsInOneCinema = films
      .flatMap(_.showings.flatMap(_._2))
      .map(_.showtimes.size)
      .max
    maxSlotsInOneCinema should be >= 8
    // At least one no-booking showtime → the `<span>` badge variant.
    films.flatMap(_.showings.flatMap(_._2)).flatMap(_.showtimes).exists(_.bookingUrl.isEmpty) shouldBe true
  }

  it should "404 in production" in {
    val ctrl   = buildController(Mode.Prod)
    val result = ctrl.tune("poznan").apply(FakeRequest(GET, "/debug/tune"))

    status(result) shouldBe NOT_FOUND
  }

  /** The apex is Poland's deployment, so its unknown-city 404 was the one Polish answer
   *  `showtimes.cc` still gave: `/xyzzy/debug/tune` read "Nieznane miasto" while every
   *  other route there says "Unknown city". kinowo.net stays Polish. */
  it should "answer an unknown city on the showtimes.cc apex in English, on kinowo.net in Polish" in {
    val ctrl = buildController(Mode.Dev)
    def unknownOn(host: String) = ctrl.tune("xyzzy").apply(FakeRequest(GET, "/xyzzy/debug/tune").withHeaders("Host" -> host))

    status(unknownOn("showtimes.cc"))          shouldBe NOT_FOUND
    contentAsString(unknownOn("showtimes.cc")) shouldBe testsupport.TestMessages.forLang("en")("error.unknownCity", "xyzzy")
    contentAsString(unknownOn("kinowo.net"))   shouldBe testsupport.TestMessages.deployment("error.unknownCity", "xyzzy")
  }

  /** A dev-only page does not exist in production — for any city, known or not — so the
   *  dev gate answers before the slug is even looked up. */
  it should "404 as a dev-only endpoint in production even for an unknown city" in {
    val result = buildController(Mode.Prod).tune("xyzzy").apply(FakeRequest(GET, "/xyzzy/debug/tune").withHeaders("Host" -> "showtimes.cc"))
    status(result)          shouldBe NOT_FOUND
    contentAsString(result) shouldBe "dev-only endpoint"
  }

  "GET /debug/tune/movie" should "render the film tuning page in dev mode" in {
    val ctrl   = buildController(Mode.Dev)
    val result = ctrl.tuneFilm("poznan").apply(FakeRequest(GET, "/debug/tune/movie"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("tune-scope")
    // The real `_filmDetailContent` markup + a populated meta block.
    html should include("film-title")
    html should include("Christopher Nolan")
    // A film-specific slider var, and the sub-nav linking the sibling page.
    html should include("--film-title-font")
    html should include("/debug/tune")
  }

  it should "404 in production" in {
    val result = buildController(Mode.Prod).tuneFilm("poznan").apply(FakeRequest(GET, "/debug/tune/movie"))
    status(result) shouldBe NOT_FOUND
  }

  "TuneSamples.film" should "populate synopsis + cast + director for the meta blocks" in {
    val f = TuneSamples.film
    f.synopsis should not be empty
    f.cast should not be empty
    f.director should not be empty
  }
}
