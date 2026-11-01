package views

import com.sun.net.httpserver.HttpExchange
import controllers.ReviewController
import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout, redirectLocation, status}
import play.api.test.{FakeRequest, Helpers}
import services.review._
import tools.{CdpPage, Chrome, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.{Clock, Instant, ZoneOffset}

/**
 * `/debug/review` in real Chrome: the queue rendered by the real controller from a fixture, an answer
 * button clicked, the page's own script posting the card's payload to the real controller — the answer
 * stored, the card hidden, and a reload no longer listing the cluster. Then "Other film" with a pasted
 * link, and "Undo" bringing a card back.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class ReviewPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val now     = Instant.parse("2026-10-06T10:00:00Z")
  private val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
  private val labels  = Files.createTempFile("labels", ".tsv")
  // each answer a millisecond after the one before, as answers clicked apart are: the export written on each takes the
  // answers given since the one before it
  private val ticking = new Clock {
    private val ticks = new java.util.concurrent.atomic.AtomicLong
    override def getZone = ZoneOffset.UTC
    override def withZone(zone: java.time.ZoneId): Clock = this
    override def instant(): Instant = now.plusMillis(ticks.incrementAndGet())
  }
  private val controller = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    Map(Country.Poland -> ReviewFixtures.source(now)), answers, labels, ticking)

  /** The pages over the REAL prod sample ([[ProdReviewSample]]), every country at once. */
  private val sample = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    ProdReviewSample.Databases.map { case (db, country) => country -> (ProdReviewSample.source(db): ReviewSource) },
    new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(ProdReviewSample.newestSlot.plusSeconds(3600), ZoneOffset.UTC))

  /** A German cluster one member of which an aggregator's feed (Webedia) fills from its catalogue. */
  private val german = {
    import services.movies.ListingKey
    val fed = ListingKey.Published("Planken Lichtspiele Mannheim", "Queen", None, Nil)
    val own = ListingKey.Native("Atlantis Kino", "https://atlantis/queen", "Queen")
    new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Germany -> new InMemoryReviewSource(
      Seq(services.identity.ResolverDecision(Seq(fed, own), None, 0.2, services.identity.ResolverDecision.Basis.BelowThreshold, Nil)()),
      slotsHeld = Map(
        fed -> SlotFacts(VenueFacts(year = Some(2020), directors = Seq("Someone")), now),
        own -> SlotFacts(VenueFacts(year = Some(2019), directors = Seq("May el-Toukhy")), now)),
      feedsHeld = Map(("Planken Lichtspiele Mannheim", "Queen") ->
        ListingFeed(Seq(services.identity.CatalogueId("webedia", "279943")), 2, None, None)))),
      new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC))
  }

  /** A cluster whose venue lists a long cast, its feed's catalogue a short one, and whose candidate's stored TMDB record a cast too. */
  private val cast = {
    import services.movies.ListingKey
    val fed = ListingKey.Published("Planken Lichtspiele Mannheim", ReviewFixtures.Held.rawTitle, None, Nil)
    new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> new InMemoryReviewSource(
      Seq(ReviewFixtures.heldDecision.copy(members = Seq(ReviewFixtures.Held, fed))(ReviewFixtures.heldDecision.trace)),
      slotsHeld = Map(
        ReviewFixtures.Held -> SlotFacts(VenueFacts(year = Some(2025), cast = (1 to 7).map(i => s"Venue Actorwithaverylongname $i") :+ "<i>x</i>"), now),
        fed -> SlotFacts(VenueFacts(year = Some(2025), cast = Seq("Catalogue Actor")), now)),
      feedsHeld = Map((fed.venue, fed.rawTitle) -> ListingFeed(Seq(services.identity.CatalogueId("webedia", "1")), 1, None, None)),
      recordsHeld = Map(ReviewFixtures.kafka.tmdb -> ReviewFixtures.kafka.copy(cast = (1 to 6).map(i => s"TMDB Actor $i"))))),
      new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC))
  }

  /** A listing whose poster only its venue's last scrape (`identity_listings`) carries: no slot row, no venue page. */
  private val listedPoster = "https://biletyna.pl/file/get/id/414402"
  private val listed = {
    val key = services.movies.ListingKey.Native("Bielański Ośrodek Kultury", "https://biletyna.pl/dla-dzieci/Podrozniczek?eid=701872", "Podróżniczek")
    new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> new InMemoryReviewSource(
      Seq(services.identity.ResolverDecision(Seq(key), None, 0.9, services.identity.ResolverDecision.Basis.NoCandidate, Nil)()),
      feedsHeld = Map((key.venue, key.rawTitle) -> ListingFeed(Nil, 1, None, None, Some(listedPoster))))),
      new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC))
  }

  /** Prod's "Odblask | UFF": the resolver vetoed "Uff" and stored 16878 as the candidate that survived. */
  private val odblask = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    Map(Country.Poland -> ReviewFixtures.odblaskSource), new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC),
    // no stored read gives its candidates a poster: each is asked of TMDB, which answers every film as it answered 4935
    tmdbPosters = Some(new TmdbPosterLookup(tools.RoutingHttpFetch.getOnly(Seq("/3/movie/" -> TmdbPosterLookupSpec.HowlsAnswer)),
      settings.TmdbApiKey("k"))))

  /** A cluster of 46 cinemas, every one crediting a director the film it leans to doesn't have, and 4 with a film page
   *  of their own that credit nobody. */
  private val crowded = {
    val keys = (1 to 46).map(i => services.movies.ListingKey.Published(f"Cinema $i%02d", "Kandydaci śmierci", None, Seq("Richard Jones"))) ++
      (47 to 50).map(i => services.movies.ListingKey.Native(s"Cinema $i", s"https://cinema$i.example/kandydaci", "Kandydaci śmierci"))
    val film = FilmCard(1703629, None, Some("Kandydaci"), None, Some(2026), Seq("Maciej Kozłowski"), None, None, None)
    new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> new InMemoryReviewSource(
      Seq(services.identity.ResolverDecision(keys, None, 0.3, services.identity.ResolverDecision.Basis.BelowThreshold,
        Seq("best rejected candidate 1703629 at 52.3% (title overlap)"))()), filmsHeld = Map(film.tmdb -> film))),
      new ReviewAnswers(new InMemoryReviewAnswerStore), labels, Clock.fixed(now, ZoneOffset.UTC))
  }

  /** The review digest over its own judgements file: two agent judgements (one in PL, one in DE) and two candidates. */
  private val digestDir     = Files.createTempDirectory("digest")
  private val digestLabels  = digestDir.resolve("identity-judgements.tsv")
  private val digestAnswers = new ReviewAnswers(new InMemoryReviewAnswerStore)
  private val digest = {
    LabelsTsv.write(digestLabels, Seq(
      LabelRow("pl", "*", "Świąteczna zabawa z Peppą", "tmdb:1778515", "right",
        "agent judged 2026-10-22 (prodreg-peppa, flagged for the user): the PL billing of the 2026 cinema event",
        filmTitle = "Festive Fun with Peppa (2026)", judged = "2026-10-22"),
      LabelRow("de", "Metropolis Kino", "KIRSCHBLÜTEN - HANAMI", "tmdb:6461", "right",
        "agent judged 2026-10-22 (flagged for the user): Doris Dörrie's 2008 film, the venue's Cineamo id names it",
        filmTitle = "Kirschblüten – Hanami (2008)", judged = "2026-10-22")))
    Files.writeString(ReviewDigest.candidatesPath(digestLabels), ReviewDigest.CandidatesHeader + "\n" + Seq(
      "de\tRaststätte\tLatcho Drom\ttmdb:1247239\tLatcho Drom - Good Road (2024)\t2026-10-23\tlooks wrong: a 19′ short's record takes a famous feature's billing; Gatlif's Latcho Drom (1993) is tmdb:47105",
      "de\tKino im Druckluft\tSnake Eyes\ttmdb:1730437\tSnake Eyes (2026)\t2026-10-23\tlooks wrong: a 21′ short's record; De Palma's Snake Eyes (1998) is tmdb:8688").mkString("", "\n", "\n"))
    new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map.empty, digestAnswers, digestLabels, ticking)
  }
  private lazy val digestItems = ReviewDigest.pending(LabelsTsv.read(digestLabels),
    ReviewDigest.readCandidates(ReviewDigest.candidatesPath(digestLabels)), Set.empty)

  private def dynamic(exchange: HttpExchange): Boolean = {
    val path = exchange.getRequestURI.getPath
    lazy val body = Json.parse(new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8))
    val result = Option.when(exchange.getRequestMethod == "POST")(path).collect {
      case "/debug/review/answer" if (body \ "card" \ "page").asOpt[String].contains("digest") =>
        digest.answer()(FakeRequest("POST", path).withBody(body))
      case "/debug/review/answer" => controller.answer()(FakeRequest("POST", path).withBody(body))
      case "/debug/review/export" => controller.exportLabels()(FakeRequest("POST", path))
    }.orElse(Option.when(exchange.getRequestMethod == "GET")(path).collect {
      case poster if poster.startsWith("/debug/review/tmdb-poster/") =>
        odblask.tmdbPoster(poster.stripPrefix("/debug/review/tmdb-poster/").toInt, Some("pl"))(FakeRequest("GET", path))
    })
    result.foreach { r =>
      val bytes = contentAsString(r).getBytes(StandardCharsets.UTF_8)
      exchange.getResponseHeaders.add("Content-Type", "application/json")
      redirectLocation(r).foreach(exchange.getResponseHeaders.add("Location", _))
      exchange.sendResponseHeaders(status(r), bytes.length.toLong)
      val os = exchange.getResponseBody
      try os.write(bytes) finally os.close()
    }
    result.isDefined
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer(
      {
        case "/debug/review?country=pl" => contentAsString(controller.queue(Some("pl"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/debug/review?country=pl&answered=true" => contentAsString(controller.queue(Some("pl"), ReviewController.DefaultLimit, true)(FakeRequest()))
        case "/debug/review/recent?country=pl" => contentAsString(controller.recent(Some("pl"), 48, ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/de/review"               => contentAsString(german.queue(Some("de"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/cast/review"             => contentAsString(cast.queue(Some("pl"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/listed/review"           => contentAsString(listed.queue(Some("pl"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/crowded/review"          => contentAsString(crowded.queue(Some("pl"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case "/digest/review"           => contentAsString(digest.digest(None)(FakeRequest()))
        case "/odblask/review"          => contentAsString(odblask.queue(Some("pl"), ReviewController.DefaultLimit, false)(FakeRequest()))
        case why if why.startsWith("/debug/review/why?") =>
          val query = java.net.URI.create(why).getRawQuery.split("&").map(_.split("=", 2)).collect { case Array(k, v) =>
            k -> java.net.URLDecoder.decode(v, StandardCharsets.UTF_8) }.toMap
          contentAsString(sample.why(query("country"), query("cluster"))(FakeRequest()))
        case "/sample/listing"        =>
          val rise = ProdReviewSample.traces("kinowo_uk").find(_.listing.rawTitle == "Rise Fly Fishing Film Tour").get.listing
          contentAsString(sample.listing("uk", Some(rise.venue), rise.rawTitle)(FakeRequest()))
        case "/sample/review"         => contentAsString(sample.queue(Some("all"), 200, false)(FakeRequest()))
        case "/sample/review/matchable" => contentAsString(sample.matchable(Some("all"), 0.0, None, 200, false)(FakeRequest()))
        case "/sample/review/recent"    => contentAsString(sample.recent(Some("all"), 24 * 365, 200, false)(FakeRequest()))
      },
      dynamicRoute = dynamic)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
    Files.deleteIfExists(labels)
    Files.list(digestDir).forEach(Files.delete(_))
    Files.delete(digestDir)
  }

  /** Open `path` with `review/review-page.js` ([[tools.PageScripts]]) installed. */
  private def onPage(path: String)(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + path) { page =>
      page.useScript("review/review-page.js")
      body(page)
    }
    case None    => cancel("Chrome not installed — skipping /debug/review page test")
  }

  private def onQueue(body: CdpPage => Any): Unit = onPage("/debug/review?country=pl")(body)

  private def card(title: String) = s"__review.card(${Json.toJson(title)})"

  "the review queue" should "render each card in the published pages' design: venue facts, lead film, candidates, the sticky bar" in {
    onQueue { page =>
      page.evalString("getComputedStyle(document.querySelector('.bar')).position") shouldBe "sticky"
      page.evalString("document.getElementById('count').textContent") shouldBe "0 / 3 answered"
      page.evalString("document.querySelector('.filters button[aria-pressed=true]').textContent") shouldBe "To review"
      val kafka = card("FRANZ KAFKA")
      page.evalString(s"$kafka.querySelector('.tag').textContent") shouldBe "PL"
      page.evalString(s"$kafka.querySelector('.conf').textContent") shouldBe "86.4%"
      // the venue's poster beside what it says, its synopsis, and its cinema linked
      // its poster URL answers nothing here, so the "no poster" placeholder takes its place
      page.waitFor(s"!!$kafka.querySelector('.listing .poster.empty')")
      page.evalString(s"$kafka.querySelector('.listing .poster.empty').textContent") shouldBe "no venue poster"
      page.evalString(s"$kafka.querySelector('.listing dl.facts').textContent") should (include ("Year2025") and include ("Runtime127 min") and
        include ("Catalogue idsbilety24=165208") and include ("Screenings1 · 2026-10-10 18:00"))
      page.evalString(s"$kafka.querySelector('.syn').textContent") shouldBe "Biografia Kafki."
      page.evalString(s"$kafka.querySelector('.listing .meta a').getAttribute('href')") shouldBe ReviewFixtures.Held.nativeId
      // the film the model leaned to, with Right / Wrong; the rest of its candidates below, each with "This film"
      page.evalString(s"$kafka.querySelector('.lead .t b').textContent") shouldBe "Franz"
      page.evalString(s"$kafka.querySelector('.lead .pct').textContent") shouldBe "86.4%"
      page.evalInt(s"$kafka.querySelectorAll('.lead button[data-verdict=right], .lead button[data-verdict=wrong]').length") shouldBe 2
      page.evalString(s"$kafka.querySelector('details.why summary').textContent") shouldBe "Why the resolver decided this"
      // no poster, no synopsis, no candidate: said so rather than left blank
      val robaczki = card("Filmowe popołudnie dla dzieci: Robaczki")
      page.evalString(s"$robaczki.querySelector('.listing .poster.empty').textContent") shouldBe "no venue poster"
      page.evalString(s"$robaczki.querySelector('.syn.muted').textContent") shouldBe "The venue gives no synopsis."
      page.evalInt(s"$robaczki.querySelectorAll('.lead').length") shouldBe 0
      page.evalBool(s"$robaczki.querySelector('button[data-verdict=undo]').hidden") shouldBe true
    }
  }

  it should "show the venue's, the catalogue's and the candidate's cast cut short, a phone's width unscrolled, a tap unfolding the rest" in {
    onPage("/cast/review") { page =>
      page.setViewport(360, 740)
      val kafka = card("FRANZ KAFKA")
      val venue = s"$kafka.querySelector('.listing dl.facts:not(.catalogue) details.cast')"
      page.evalString(s"$venue.querySelector('summary').textContent") shouldBe
        (1 to 5).map(i => s"Venue Actorwithaverylongname $i").mkString(", ") + " +3 more"
      page.evalString(s"$venue.getAttribute('title')") should endWith("Venue Actorwithaverylongname 7, <i>x</i>")
      page.evalInt(s"$kafka.querySelectorAll('.listing i').length") shouldBe 0   // a name is text, never markup
      page.evalBool(s"$venue.open") shouldBe false
      page.evalString(s"getComputedStyle($venue.querySelector('.more')).display") should not be "none"
      page.eval(s"$venue.querySelector('summary').click()")
      page.waitFor(s"$venue.open")
      page.evalString(s"getComputedStyle($venue.querySelector('.more')).display") shouldBe "none"
      page.evalString(s"$venue.textContent") should include("Venue Actorwithaverylongname 6, Venue Actorwithaverylongname 7, <i>x</i>")
      page.evalString(s"$kafka.querySelector('dl.facts.catalogue .cast').textContent") shouldBe "Catalogue Actor"
      page.evalString(s"$kafka.querySelector('.lead .with summary').textContent") shouldBe
        (1 to 5).map(i => s"TMDB Actor $i").mkString(", ") + " +1 more"
      page.evalBool("document.documentElement.scrollWidth <= document.documentElement.clientWidth") shouldBe true
    }
  }

  it should "keep an aggregator's catalogue claims greyed in a block apart from what the venue says" in {
    onPage("/de/review") { page =>
      val queen = card("Queen")
      page.evalString(s"$queen.querySelector('.listing dl.facts:not(.catalogue)').textContent") should
        (include ("Year2019") and include ("DirectorMay el-Toukhy") and not include "Someone")
      page.evalString(s"$queen.querySelector('dl.facts.catalogue').previousElementSibling.textContent") shouldBe
        "The aggregator's catalogue entry says (not the venue):"
      page.evalString(s"$queen.querySelector('dl.facts.catalogue').textContent") should
        (include ("Year2020") and include ("DirectorSomeone") and include ("webedia=279943"))
      page.evalString(s"getComputedStyle($queen.querySelector('dl.facts.catalogue dd')).color") shouldBe
        page.evalString(s"getComputedStyle($queen.querySelector('dl.facts.catalogue dt')).color")
    }
  }

  it should "read a pasted link live, as the server's FilmRef.parse reads it" in {
    onQueue { page =>
      val kafka = card("FRANZ KAFKA")
      def typed(text: String): String = {
        page.eval(s"__review.typeOther('FRANZ KAFKA', ${Json.toJson(text)})")
        page.evalString(s"$kafka.querySelector('.hint').textContent")
      }
      Seq("https://www.themoviedb.org/movie/603-the-matrix", "https://m.imdb.com/title/tt0133093/", "tt0133093", "603",
        "https://www.filmweb.pl/film/Franz+Kafka-2025-10008278", "https://www.wikidata.org/wiki/Q83495", "Q83495",
        "rottentomatoes.com/m/the_matrix", "https://letterboxd.com/film/the-matrix/", "metacritic:the-matrix", "tmdb:0603",
        "filmweb:007", "https://www.themoviedb.org/tv/1399", "https://example.com/nothing", "imdb:123").foreach { input =>
        withClue(input) {
          typed(input) shouldBe FilmRef.parse(input).fold("Not a link or id I recognise.")(r => s"Reads as ${r.render}")
        }
      }
      typed("  ") shouldBe ""
    }
  }

  it should "mark a matched film's answer and count it, and filter answered cards in and out, remembering the filter" in {
    onPage("/debug/review/recent?country=pl") { page =>
      val klondike = card("Klondike")
      page.evalString(s"$klondike.querySelector('.lead .kind').textContent") shouldBe "Matched by OwnMatch · 2026-10-06 09:00 UTC"
      page.eval(s"$klondike.querySelector('.lead button[data-verdict=right]').click()")
      page.waitFor(s"$klondike.getAttribute('data-answered') === 'right'")
      page.evalBool(s"$klondike.querySelector('.lead button[data-verdict=right]').classList.contains('on')") shouldBe true
      page.evalBool(s"$klondike.classList.contains('done')") shouldBe true
      page.evalString("document.getElementById('count').textContent") shouldBe "1 / 1 answered"
      page.waitFor(s"$klondike.hidden === true")
      page.evalBool("document.getElementById('empty').hidden") shouldBe false

      page.eval("document.getElementById('fDone').click()")
      page.evalBool(s"$klondike.hidden") shouldBe false
      page.eval(s"$klondike.querySelector('button[data-verdict=undo]').click()")
      page.waitFor(s"$klondike.getAttribute('data-answered') === ''")
      page.evalString("document.getElementById('count').textContent") shouldBe "0 / 1 answered"
      answers.current() shouldBe empty

      page.reload()
      page.evalString("document.querySelector('.filters button[aria-pressed=true]').textContent") shouldBe "Answered"
      page.evalBool(s"$klondike.hidden") shouldBe true
      page.eval("document.getElementById('fOpen').click()")
      page.evalBool(s"$klondike.hidden") shouldBe false
    }
  }

  it should "load the answered cards when the filter remembered from another review page asks for them, never an empty page" in {
    onQueue { page =>
      page.eval(s"${card("FRANZ KAFKA")}.querySelector('button[data-verdict=event]').click()")
      page.waitFor(s"${card("FRANZ KAFKA")}.getAttribute('data-answered') === 'event'")
      // "Answered" picked on another review page: one key for all three
      page.eval("localStorage.setItem('review.filter', 'done')")
      page.reload()
      page.waitFor(s"location.search.indexOf('answered=true') >= 0 && document.readyState === 'complete' && !!${card("FRANZ KAFKA")}")
      page.evalBool(s"${card("FRANZ KAFKA")}.hidden") shouldBe false
      page.evalBool("document.getElementById('empty').hidden") shouldBe true
      // the answers and the origin's storage outlive this test: undo the answer and leave the default filter behind
      page.eval(s"${card("FRANZ KAFKA")}.querySelector('button[data-verdict=undo]').click()")
      page.waitFor(s"${card("FRANZ KAFKA")}.getAttribute('data-answered') === ''")
      answers.current() shouldBe empty
      page.eval("localStorage.removeItem('review.filter')")
    }
  }

  it should "store a clicked answer, hide its card, and leave it out of the next load" in {
    onQueue { page =>
      page.evalInt("document.querySelectorAll('.card').length") shouldBe 3
      page.eval(s"${card("FRANZ KAFKA")}.querySelector('button[data-verdict=event]').click()")
      page.waitFor(s"${card("FRANZ KAFKA")}.getAttribute('data-answered') === 'event'")
      page.waitFor(s"${card("FRANZ KAFKA")}.hidden === true")
      answers.current().map(a => (a.title, a.verdict, a.shown.map(_.ref.render))) shouldBe
        Seq(("FRANZ KAFKA", ReviewVerdict.Event, Some("tmdb:1157322")))

      page.reload()
      page.evalInt("document.querySelectorAll('.card').length") shouldBe 2
      page.evalString("document.querySelector('.summary').textContent") should include ("1 answered hidden")
      page.evalString("document.getElementById('count').textContent") shouldBe "1 / 3 answered"

      // the answered card isn't on this page: "All" loads the page that lists it
      page.eval("document.getElementById('fAll').click()")
      page.waitFor(s"location.search.indexOf('answered=true') >= 0 && document.readyState === 'complete' && !!${card("FRANZ KAFKA")}")
      page.evalBool(s"${card("FRANZ KAFKA")}.hidden") shouldBe false
      page.evalBool(s"${card("FRANZ KAFKA")}.querySelector('button[data-verdict=event]').classList.contains('on')") shouldBe true
      // and "To review" loads the page without it: that page's first `limit` cards can all be answered ones, which
      // hiding left an empty page while unanswered cards waited past the limit
      page.eval("document.getElementById('fOpen').click()")
      page.waitFor(s"location.search.indexOf('answered=true') < 0 && document.readyState === 'complete' && !!${card("Macbeth")}")
      page.evalBool(s"!!${card("FRANZ KAFKA")}") shouldBe false
      page.evalString("document.querySelector('.filters button[aria-pressed=true]').textContent") shouldBe "To review"
    }
  }

  it should "take an Other film link, refuse one that names no film, and undo an answer" in {
    onQueue { page =>
      val macbeth = card("Macbeth")
      page.eval(s"$macbeth.querySelector('input.other').value = 'https://example.com/nothing'")
      page.eval(s"$macbeth.querySelector('button[data-other]').click()")
      page.waitFor(s"$macbeth.querySelector('.status').classList.contains('is-error')")
      page.evalString(s"$macbeth.querySelector('.status').textContent") should include ("not a film link")

      page.eval(s"$macbeth.querySelector('input.other').value = 'https://www.imdb.com/title/tt0067372/'")
      page.eval(s"$macbeth.querySelector('button[data-other]').click()")
      page.waitFor(s"$macbeth.getAttribute('data-answered') === 'film'")
      answers.current().find(_.title == "Macbeth").flatMap(_.ref).map(_.render) shouldBe Some("imdb:tt0067372")

      page.eval(s"$macbeth.querySelector('button[data-verdict=undo]').click()")
      page.waitFor(s"$macbeth.getAttribute('data-answered') === ''")
      answers.current().find(_.title == "Macbeth") shouldBe None
    }
  }

  it should "write each answer into identity-judgements.tsv as it is clicked, with no export button, and say what it wrote" in {
    onQueue { page =>
      page.evalInt("document.querySelectorAll('#export-labels').length") shouldBe 0
      // the answers the cases above gave and took back, already in the file as they stand: no export run
      LabelsTsv.read(labels).map(r => (r.rawTitle, r.film, r.verdict)) shouldBe Seq(("FRANZ KAFKA", "tmdb:1157322", "wrong"))
      page.evalString("document.getElementById('export-summary').textContent") shouldBe ""
      val klondike = card("Klondike")
      page.evalBool(s"!!$klondike") shouldBe false                        // an own take: not the queue's
      val macbeth = card("Macbeth")
      def macbethRows = LabelsTsv.read(labels).filter(_.rawTitle == "Macbeth").map(r => (r.venue, r.film, r.verdict, r.note))
      page.eval(s"$macbeth.querySelector('input.other').value = 'https://www.imdb.com/title/tt0067372/'")
      page.eval(s"$macbeth.querySelector('button[data-other]').click()")
      page.waitFor("document.getElementById('export-summary').textContent.indexOf('1 added') >= 0")
      page.evalString("document.getElementById('export-summary').textContent") should startWith(s"$labels\nidentity-judgements.tsv: 1 added")
      macbethRows shouldBe Seq(("Kino Muza", "imdb:tt0067372", "right", "review page: hand label: imdb:tt0067372"))

      page.eval(s"$macbeth.querySelector('button[data-verdict=undo]').click()")
      page.waitFor("document.getElementById('export-summary').textContent.indexOf('taken back') >= 0")
      macbethRows shouldBe empty
    }
  }

  it should "say on each card how many listings an answer settles and why the resolver is unsure, the most first" in {
    onPage("/crowded/review") { page =>
      val crowd = card("Kandydaci śmierci")
      page.evalString(s"$crowd.querySelector('.priority .settles').textContent") shouldBe "settles 50 listings"
      page.evalString(s"$crowd.querySelector('.priority .unsure').textContent") shouldBe "52.3% unsure"
      page.evalString(s"$crowd.querySelector('.priority .reason').textContent") shouldBe "best candidate held below the cut at 52%"
    }
    // FRANZ KAFKA's listing is labelled by now (wrong for the film it shows): its answer settles nothing more, so it goes last
    onPage("/debug/review?country=pl&answered=true") { page =>
      page.evalString("Array.prototype.map.call(document.querySelectorAll('.card .raw-title'), function (t) { return t.textContent; }).join('|')") shouldBe
        "Macbeth|Filmowe popołudnie dla dzieci: Robaczki|FRANZ KAFKA"
      page.evalString(s"${card("FRANZ KAFKA")}.querySelector('.priority').textContent") should include("1 of 1 listings already labelled")
    }
  }

  it should "take a card the reviewer can't determine off the list, labelling nothing for it" in {
    onQueue { page =>
      val robaczki = card("Filmowe popołudnie dla dzieci: Robaczki")
      page.evalString(s"$robaczki.querySelector('button[data-verdict=unsure]').textContent") shouldBe "Can't determine"
      page.eval(s"$robaczki.querySelector('button[data-verdict=unsure]').click()")
      page.waitFor(s"$robaczki.getAttribute('data-answered') === 'unsure'")
      page.waitFor(s"$robaczki.hidden === true")
      val unsure = answers.current().filter(_.title.contains("Robaczki"))
      unsure.map(_.verdict) shouldBe Seq(ReviewVerdict.Unsure)
      unsure.flatMap(LabelsExport.rowsOf(_)) shouldBe empty
      // withdrawn again, so the cases after this one see the queue as they expect it
      page.eval(s"$robaczki.querySelector('button[data-verdict=undo]').click()")
      page.waitFor(s"$robaczki.getAttribute('data-answered') === ''")
    }
  }

  "a card whose best candidate was vetoed" should "lead with the candidate that survived, the vetoed one folded into a closed list it can still be picked from" in {
    onPage("/odblask/review") { page =>
      val odblask = card("Odblask | UFF")
      page.evalString(s"$odblask.querySelector('.lead .t b').textContent") shouldBe "Odblask"
      page.evalString(s"$odblask.querySelector('.lead').innerText") should not include "Uff"
      page.evalInt(s"$odblask.querySelectorAll('.head .conf').length") shouldBe 0   // 16878's probability is not stated
      val vetoed = s"$odblask.querySelector('details.vetoed')"
      page.evalBool(s"$vetoed.open") shouldBe false
      page.evalString(s"$vetoed.querySelector('summary').textContent") shouldBe "Vetoed candidates (1)"
      page.evalBool(s"$vetoed.querySelector('.cand.vetoed').checkVisibility()") shouldBe false
      page.evalString(s"$odblask.innerText") should not include "Uff"   // nothing of the vetoed film shows until opened
      page.eval(s"$vetoed.open = true")
      page.evalString(s"$vetoed.querySelector('.cand.vetoed .t b').textContent") shouldBe "Uff"
      page.evalString(s"$vetoed.querySelector('.pct').textContent") shouldBe "28.9%"
      page.evalString(s"$vetoed.querySelector('.denial').textContent") shouldBe
        "denied: its title names it only by a programme tag billed beside many titles"
      page.evalString(s"getComputedStyle($vetoed.querySelector('.denial')).color") shouldBe
        page.evalString("__review.resolvedColor('var(--bad)')")
      page.evalString(s"$vetoed.querySelector('button[data-verdict=film]').getAttribute('data-ref')") shouldBe "tmdb:850957"
      // the full reasoning keeps the veto line
      page.evalString(s"$odblask.querySelector('details.why').textContent") should include ("best vetoed candidate 850957 at 28.9%")
    }
  }

  "a card every candidate of which was vetoed" should "put no film forward and say so" in {
    onQueue { page =>
      val macbeth = card("Macbeth")
      page.evalString(s"$macbeth.querySelector('.lead').innerText") should include ("No candidate survived its vetoes")
      page.evalInt(s"$macbeth.querySelectorAll('.lead .cand, .lead button').length") shouldBe 0
      page.evalInt(s"$macbeth.querySelectorAll('.head .conf').length") shouldBe 0
      page.evalBool(s"$macbeth.querySelector('details.vetoed').open") shouldBe false
      page.evalString(s"$macbeth.querySelector('details.vetoed .cand .t b').textContent") shouldBe "Royal Ballet and Opera: Macbeth"
    }
  }

  "a card of 50 cinemas" should "fold its cinemas into one 'N cinemas' list, and its one grouped disagreement into a closed ⚠ at the bottom" in {
    onPage("/crowded/review") { page =>
      val card = "document.querySelector('.card')"
      page.evalString(s"$card.querySelector('.listing details.venues > summary').textContent") shouldBe "50 cinemas"
      page.evalBool(s"$card.querySelector('.listing details.venues').open") shouldBe false
      // the cinemas are listed once opened, from the card's own listings: none is sent twice
      page.evalInt(s"$card.querySelectorAll('.listing details.venues li').length") shouldBe 0
      page.eval(s"$card.querySelector('.listing details.venues').open = true")
      page.waitFor(s"$card.querySelectorAll('.listing details.venues li').length === 50")
      page.evalString(s"$card.querySelector('.listing details.venues li').textContent") shouldBe "Cinema 01"
      page.evalString(s"__review.links($card, '.listing details.venues li a')") shouldBe
        (47 to 50).map(i => s"Cinema $i https://cinema$i.example/kandydaci _blank").mkString("|")
      // nothing of the disagreement shows until the ⚠ is opened
      page.evalString(s"$card.innerText") should not include "is directed by"
      page.evalString(s"$card.lastElementChild.tagName + ' ' + $card.lastElementChild.className") shouldBe "DETAILS checks"
      page.evalString(s"$card.querySelector('details.checks > summary').textContent") shouldBe "⚠ 1"
      page.eval(s"$card.querySelector('details.checks').open = true")
      page.evalInt(s"$card.querySelectorAll('details.checks > ul > li').length") shouldBe 1
      page.evalString(s"$card.querySelector('details.checks').innerText") should include (
        "46 cinemas credit Richard Jones; Kandydaci (2026) is directed by Maciej Kozłowski")
      page.evalString(s"$card.querySelector('details.checks').innerText") should not include "Cinema 07"
      page.eval(s"$card.querySelector('details.checks details.venues').open = true")
      page.evalInt(s"$card.querySelectorAll('details.checks details.venues li').length") shouldBe 46
    }
  }

  "a card's Why" should "load its listings' whole traces when opened: every evidence line for and against, and the veto rule" in {
    onPage("/sample/review/matchable") { page =>
      val trace = ProdReviewSample.traces("kinowo_uk").find(_.listing.rawTitle == "Rise Fly Fishing Film Tour").get
      val fishing = card("Rise Fly Fishing Film Tour")
      // nothing is read until it is opened
      page.evalString(s"$fishing.querySelector('details.why .traces').innerHTML") shouldBe ""
      page.eval(s"$fishing.querySelector('details.why').open = true")
      page.waitFor(s"!!$fishing.querySelector('details.why .trace')")
      val text = page.evalString(s"$fishing.querySelector('details.why').innerText")
      for (line <- trace.evidence) text should include(line)
      text should include("best denied: Learned(runtime.delta >= 11 AND title in {none,overlap})")
      page.evalString(s"getComputedStyle($fishing.querySelector('details.why .for li')).color") shouldBe
        page.evalString("getComputedStyle(document.documentElement).getPropertyValue('--ok').trim() && getComputedStyle(document.querySelector('.btn.yes')).color")
      page.evalInt(s"$fishing.querySelectorAll('details.why .against li').length") should be > 0
      page.evalBool(s"__review.anyMentions($fishing, 'details.why li.veto', 'best denied')") shouldBe true
    }
  }

  "a venue's poster" should "show from its scraped listing alone, at the address the poster proxy gives it" in {
    onPage("/listed/review") { page =>
      // the poster's address answers with a 1×1 PNG: no network, and positive proof the browser asked IT — biletyna's
      // own host, which refuses the weserv proxy (`PosterProxy`)
      val png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
      page.onEvent("Fetch.requestPaused") { p =>
        page.send("Fetch.fulfillRequest", Json.obj("requestId" -> (p \ "requestId").as[String], "responseCode" -> 200,
          "responseHeaders" -> Json.arr(Json.obj("name" -> "Content-Type", "value" -> "image/png")), "body" -> png))
      }
      page.send("Fetch.enable", Json.obj("patterns" -> Json.arr(Json.obj("urlPattern" -> tools.PosterProxy.proxy(listedPoster)))))
      page.reload()
      page.waitFor("__review.loaded(document.querySelector('.listing img.poster'))")
      page.evalString("document.querySelector('.listing img.poster').src") shouldBe tools.PosterProxy.proxy(listedPoster)
    }
  }

  "a candidate's TMDB poster" should "load from TMDB's own address, looked up live when no stored read gives one" in {
    onPage("/odblask/review") { page =>
      // TMDB's image host answers with a 1×1 PNG: no network, and positive proof the lookup's redirect led the browser there
      val png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
      val asked = new java.util.concurrent.atomic.AtomicBoolean(false)
      page.onEvent("Fetch.requestPaused") { p =>
        asked.set(true)
        page.send("Fetch.fulfillRequest", Json.obj("requestId" -> (p \ "requestId").as[String], "responseCode" -> 200,
          "responseHeaders" -> Json.arr(Json.obj("name" -> "Content-Type", "value" -> "image/png")), "body" -> png))
      }
      page.send("Fetch.enable", Json.obj("patterns" -> Json.arr(Json.obj("urlPattern" -> TmdbPosterLookupSpec.HowlsPoster))))
      page.reload()
      val lead = "document.querySelector('.card img.poster[alt=\"TMDB poster\"]')"
      page.waitFor(s"__review.loaded($lead)")
      page.evalString(s"$lead.getAttribute('src')") shouldBe "/debug/review/tmdb-poster/16878?country=pl"
      asked.get shouldBe true
    }
  }

  "every review page over the real prod sample" should "render its cards whole, each with a payload its buttons can post" in {
    Seq("/sample/review" -> 15, "/sample/review/matchable" -> 15, "/sample/review/recent" -> 1).foreach { case (path, atLeast) =>
      onPage(path) { page =>
        withClue(path) {
          page.evalInt("document.querySelectorAll('.card').length") should be >= atLeast
          page.evalInt("document.querySelectorAll('.notice.warn').length") shouldBe 0
          // every card's payload parses and names its cluster, title and listings
          page.evalBool("__review.payloadsWhole()") shouldBe true
        }
      }
    }
    onPage("/sample/review/matchable") { page =>
      ReviewPageSpec.save(page, "review-matchable-desktop.png")
      page.setViewport(400, 900)
      page.evalString("getComputedStyle(document.querySelector('.cands .cand')).gridTemplateColumns").split(" ").length shouldBe 2
      page.evalBool("document.documentElement.scrollWidth <= window.innerWidth") shouldBe true
      ReviewPageSpec.save(page, "review-matchable-mobile.png")
    }
    onPage("/sample/review/matchable") { page =>
      page.evalString("document.querySelector('.controls').textContent") should include ("Vetoed")
      page.eval("document.querySelector('.card button[data-verdict=wrong]').click()")
      page.waitFor("document.querySelector('.card').getAttribute('data-answered') === 'wrong'")
    }
  }

  "the review digest" should "list the takes awaiting the user by country, and record right, not a film and skip as the user's word" in {
    def item(raw: String) = s"document.querySelector('.card[data-cluster=\"${digestItems.find(_.rawTitle == raw).get.id}\"]')"
    onPage("/digest/review") { page =>
      page.evalString("Array.from(document.querySelectorAll('.country-group')).map(g => g.getAttribute('data-country') + ':' + g.querySelectorAll('.card').length).join(' ')") shouldBe
        "pl:1 de:3"
      page.evalString("document.getElementById('count').textContent") shouldBe "0 / 4 answered"
      val hanami = item("KIRSCHBLÜTEN - HANAMI")
      page.evalString(s"$hanami.querySelector('.said').textContent") shouldBe "agent judged right · 2026-10-22"
      page.evalString(s"$hanami.querySelector('.film-title').textContent") shouldBe "Kirschblüten – Hanami (2008)"
      page.evalString(s"$hanami.querySelector('.evidence').textContent") should include("Doris Dörrie's 2008 film")
      page.evalString(s"$hanami.querySelector('a.explain').getAttribute('href')") should startWith("/debug/review/listing?country=de&venue=Metropolis+Kino")
      page.evalString(s"$hanami.querySelector('.cand .s a').getAttribute('href')") shouldBe "https://www.themoviedb.org/movie/6461"
      page.evalString(s"${item("Latcho Drom")}.querySelector('.said').textContent") shouldBe "flagged · 2026-10-23"
      page.evalInt(s"$hanami.querySelectorAll('button[data-verdict=right], button[data-verdict=wrong], button[data-verdict=event], button[data-skip]').length") shouldBe 4
      page.eval("document.querySelector('.country-group').scrollIntoView()")
      ReviewPageSpec.save(page, "review-digest-desktop.png")

      // right: the agent's judgement becomes the user's
      page.eval(s"$hanami.querySelector('button[data-verdict=right]').click()")
      page.waitFor(s"$hanami.getAttribute('data-answered') === 'right'")
      LabelsTsv.read(digestLabels).find(_.film == "tmdb:6461").get.note should startWith("review page: right: Kirschblüten – Hanami (2008) (was: right: agent judged")
      // not a film: the candidate's take judged wrong, as the user's
      val latcho = item("Latcho Drom")
      page.eval(s"$latcho.querySelector('button[data-verdict=event]').click()")
      page.waitFor(s"$latcho.getAttribute('data-answered') === 'event'")
      LabelsTsv.read(digestLabels).find(_.rawTitle == "Latcho Drom").map(r => (r.verdict, r.note)) shouldBe
        Some(("wrong", "review page: not a film: Latcho Drom - Good Road (2024)"))
      page.evalString("document.getElementById('count').textContent") shouldBe "2 / 4 answered"
      page.evalString("document.getElementById('export-summary').textContent") should include("1 added")
      // skip: to the end of its country's list, nothing recorded
      val before = digestAnswers.history().size
      val snake  = item("Snake Eyes")
      page.eval(s"$snake.querySelector('button[data-skip]').click()")
      page.evalBool(s"$snake.classList.contains('skipped') && $snake.parentNode.lastElementChild === $snake") shouldBe true
      digestAnswers.history().size shouldBe before

      page.setViewport(400, 900)
      page.evalBool("document.documentElement.scrollWidth <= window.innerWidth") shouldBe true
      page.eval(s"$hanami.scrollIntoView()")
      ReviewPageSpec.save(page, "review-digest-mobile.png")
    }
    // answered takes leave the digest; the skipped one stays
    onPage("/digest/review") { page =>
      page.evalString("Array.from(document.querySelectorAll('.card .raw-title')).map(t => t.textContent).join('|')") shouldBe
        "Świąteczna zabawa z Peppą|Snake Eyes"
    }
  }

  "a listing's explain page" should "render in the review pages' design, whole at a phone's width" in {
    onPage("/sample/listing") { page =>
      page.evalString("getComputedStyle(document.querySelector('.card.listing')).borderRadius") shouldBe "10px"
      page.evalString("getComputedStyle(document.querySelector('.explain h1')).fontFamily") should include("Archivo")
      // one column: the sections stack, the title is not squeezed beside them
      page.evalString("getComputedStyle(document.querySelector('.card.listing')).gridTemplateColumns").split(" ").length shouldBe 1
      page.evalInt("document.querySelectorAll('.card.listing table tr').length") should be >= 3
      page.evalString("document.querySelector('pre.logsql').textContent") should include("identity take change")
      ReviewPageSpec.save(page, "review-listing-desktop.png")
      page.setViewport(400, 900)
      page.evalBool("document.documentElement.scrollWidth <= window.innerWidth") shouldBe true
      ReviewPageSpec.save(page, "review-listing-mobile.png")
    }
  }
}

object ReviewPageSpec {
  /** A screenshot of the page as it stands, under `web/target/review-screenshots/` — for a human to look at. */
  def save(page: CdpPage, name: String): Unit = {
    val dir = java.nio.file.Paths.get("web/target/review-screenshots").toAbsolutePath
    Files.createDirectories(dir)
    Files.write(dir.resolve(name), java.util.Base64.getDecoder.decode(page.screenshot())): Unit
  }
}
