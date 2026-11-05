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

class ReviewControllerSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {
  import ReviewFixtures._

  private val now   = Instant.parse("2026-10-06T10:00:00Z")
  private val clock = Clock.fixed(now, ZoneOffset.UTC)

  private def controller(mode: Mode, answers: ReviewAnswers = new ReviewAnswers(new InMemoryReviewAnswerStore),
                         labels: java.nio.file.Path = tempFile("labels", ".tsv"), clock: Clock = clock,
                         tmdbPosters: Option[TmdbPosterLookup] = None) =
    new ReviewController(Helpers.stubControllerComponents(), mode, Map(Country.Poland -> ReviewFixtures.source(now)), answers, labels, clock,
      tmdbPosters = tmdbPosters)

  "every review page" should "404 in production" in {
    val prod = controller(Mode.Prod)
    Seq(prod.queue(None, 60, false), prod.matchable(None, 0.5, None, 60, false), prod.recent(None, 48, 60, false), prod.history(),
      prod.exportLabels()).foreach(action => status(action(FakeRequest())) shouldBe NOT_FOUND)
    status(prod.answer()(FakeRequest().withBody(Json.obj()))) shouldBe NOT_FOUND
    status(controller(Mode.Prod, tmdbPosters = Some(howlsLookup)).tmdbPoster(4935, Some("pl"))(FakeRequest())) shouldBe NOT_FOUND
  }

  "an answer with no labels file configured" should "be kept, refused for the file with the fix named, and write nowhere" in {
    val labels  = tempDirectory("unconfigured").resolve("identity-judgements.tsv")
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val c = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> ReviewFixtures.source(now)), answers, labels, clock,
      labelsWritable = false)
    val card = ReviewDigest.pending(Seq(LabelRow("pl", "Kino Opalenica", "Ruchomy zamek Hauru", "tmdb:4935", "right",
      "agent judged 2026-10-22 (test): x", judged = "2026-10-22")), Nil, Set.empty).head.payload
    val reply = contentAsJson(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "right"))))
    (reply \ "exportError").as[String] should include("REVIEW_LABELS_FILE is not set")
    answers.history() should have size 1
    status(c.exportLabels()(FakeRequest())) shouldBe INTERNAL_SERVER_ERROR
    Files.exists(labels) shouldBe false
  }

  // TMDB's recorded answer for "Howl's Moving Castle" (4935): the poster a card shows when no stored read gives one
  private def howlsLookup = TmdbPosterLookupSpec.lookup

  "the review digest" should "404 in production, and list the takes awaiting the user by country, each film named and its listing explained" in {
    val labels = tempDirectory("digest").resolve("identity-judgements.tsv")
    LabelsTsv.write(labels, Seq(LabelRow("pl", "Kino Opalenica", "Ruchomy zamek Hauru", "tmdb:4935", "right",
      "agent judged 2026-10-22 (test): Miyazaki's, the venue's year 2004", judged = "2026-10-22")))
    Files.writeString(ReviewDigest.candidatesPath(labels), ReviewDigest.CandidatesHeader +
      "\nde\tRaststätte\tLatcho Drom\ttmdb:1247239\tLatcho Drom - Good Road (2024)\t2026-10-23\tlooks wrong: a 19′ short\n")
    status(controller(Mode.Prod, labels = labels).digest(None)(FakeRequest())) shouldBe NOT_FOUND
    val dev  = controller(Mode.Dev, labels = labels, tmdbPosters = Some(howlsLookup))
    val html = contentAsString(dev.digest(None)(FakeRequest()))
    html.indexOf("Polska") should (be > 0 and be < html.indexOf("Deutschland"))
    html should include("<b class=\"film-title\">Howl&#x27;s Moving Castle (2004)</b>")   // the row files no title: TMDB's
    html should include("<b class=\"film-title\">Latcho Drom - Good Road (2024)</b>")
    html should include("agent judged right · 2026-10-22")
    html should include("flagged · 2026-10-23")
    html should include("looks wrong: a 19′ short")
    html should include("/debug/review/listing?country=de&amp;venue=Rastst%C3%A4tte&amp;title=Latcho")
    html should include("/debug/review/tmdb-poster/1247239?country=de")
    contentAsString(dev.digest(Some("de"))(FakeRequest())) should not include "Ruchomy zamek Hauru"
    // answered: the agent's row becomes the user's, and leaves the digest
    val item = ReviewDigest.pending(LabelsTsv.read(labels), Nil, Set.empty).head
    status(dev.answer()(FakeRequest().withBody(Json.obj("card" -> item.payload, "verdict" -> "right")))) shouldBe OK
    LabelsTsv.read(labels).head.note should startWith("review page: right: tmdb:4935 (was: right: agent judged")
    contentAsString(dev.digest(None)(FakeRequest())) should (not include "Ruchomy zamek Hauru" and include("Latcho Drom"))
  }

  "a candidate's TMDB poster" should "redirect to the poster TMDB names for it, and be not found when TMDB names none or no key is set" in {
    val looked = controller(Mode.Dev, tmdbPosters = Some(howlsLookup)).tmdbPoster(4935, Some("pl"))(FakeRequest())
    status(looked) shouldBe SEE_OTHER
    redirectLocation(looked) shouldBe Some(TmdbPosterLookupSpec.HowlsPoster)
    status(controller(Mode.Dev, tmdbPosters = Some(howlsLookup)).tmdbPoster(1575247, Some("pl"))(FakeRequest())) shouldBe NOT_FOUND
    status(controller(Mode.Dev).tmdbPoster(4935, Some("pl"))(FakeRequest())) shouldBe NOT_FOUND
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
      tempFile("labels", ".tsv"), clock).queue(Some("pl"), 60, false)(FakeRequest()))
    val record = FilmCard(kafka.tmdb, Some("tt22963134"), Some("Franz (TMDB)"), Some("Franz"), Some(2025), Seq("Agnieszka Holland"), Some(127), None, None)

    val recordOnly = queueOver(Map.empty, Map(kafka.tmdb -> record))
    recordOnly should include("<b>Franz (TMDB)</b>")
    recordOnly should include("2025 · Agnieszka Holland · 127 min")
    // no poster stored for it: the card asks for one live, the "no TMDB poster" placeholder taking its place should none load
    recordOnly should include("""src="/debug/review/tmdb-poster/1157322?country=pl"""")
    queueOver(Map.empty, Map(kafka.tmdb -> record.copy(poster = Some("https://image.tmdb.org/t/p/w185/hashed.jpg")))) should
      include("""src="https://image.tmdb.org/t/p/w185/hashed.jpg"""")   // the poster the poster evidence hashed
    recordOnly should not include "not in this country&#x27;s corpus"

    val both = queueOver(Map(kafka.tmdb -> kafka), Map(kafka.tmdb -> record))
    both should include("<b>Franz (TMDB)</b>")
    both should include("""src="https://image.tmdb.org/t/p/w185/kafka.jpg"""")
    both should include("Kafka&#x27;s life.")
  }

  "a card" should "show the cast the venue, its feed's catalogue and each candidate film state, cut short past five names" in {
    val fed     = services.movies.ListingKey.Published("Planken Lichtspiele Mannheim", "FRANZ KAFKA", None, Nil)
    val venue   = (1 to 7).map(i => s"Venue Actor $i") :+ "<b>Bold</b> & Co"
    val catalog = Seq("Catalogue Actor 1", "Catalogue Actor 2")
    val tmdb    = (1 to 6).map(i => s"TMDB Actor $i")
    val html = contentAsString(new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> new InMemoryReviewSource(
      Seq(heldDecision.copy(members = Seq(Held, fed))(heldDecision.trace)),
      slotsHeld = Map(Held -> SlotFacts(VenueFacts(cast = venue), now), fed -> SlotFacts(VenueFacts(year = Some(2025), cast = catalog), now)),
      feedsHeld = Map((fed.venue, fed.rawTitle) -> ListingFeed(Seq(services.identity.CatalogueId("webedia", "1")), 1, None, None)),
      recordsHeld = Map(kafka.tmdb -> kafka.copy(cast = tmdb)))), new ReviewAnswers(new InMemoryReviewAnswerStore),
      tempFile("labels", ".tsv"), clock).queue(Some("pl"), 60, false)(FakeRequest()))
    // the venue's: its first five, the other three behind "+3 more", every name escaped
    html should include("<summary>Venue Actor 1, Venue Actor 2, Venue Actor 3, Venue Actor 4, Venue Actor 5 <span class=\"more\">+3 more</span></summary>")
    html should include("Venue Actor 6, Venue Actor 7, &lt;b&gt;Bold&lt;/b&gt; &amp; Co")
    html should not include "<b>Bold</b>"
    // the catalogue's, in its own block: two names, nothing to cut
    html should include("<dt>Cast</dt><dd><span class=\"cast\">Catalogue Actor 1, Catalogue Actor 2</span></dd>")
    // the candidate film's, from the resolver's stored TMDB record
    html should include("TMDB Actor 1, TMDB Actor 2, TMDB Actor 3, TMDB Actor 4, TMDB Actor 5 <span class=\"more\">+1 more</span>")
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

  "an answer" should "take its cluster out of the queue, an undo bring it back, each written into identity-judgements.tsv as it is given" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val labels  = tempFile("labels", ".tsv")
    val c       = controller(Mode.Dev, answers, labels)
    val card    = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head.payload(ReviewPage.Queue)
    val reply = contentAsJson(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "right"))))
    (reply \ "exported").as[String] should startWith("identity-judgements.tsv: 1 added")
    (reply \ "path").as[String] shouldBe labels.toString
    // in the file at once, in the review page's own words: no export step
    LabelsTsv.read(labels) shouldBe Seq(LabelRow("pl", "Kino Opalenica", "FRANZ KAFKA", "tmdb:1157322", "right", "review page: right: Franz (2025)"))
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should not include "FRANZ KAFKA"
    contentAsString(c.queue(Some("pl"), 60, true)(FakeRequest())) should include("""class="btn yes right on" data-verdict="right">Right film""")
    (contentAsJson(c.exportLabels()(FakeRequest())) \ "alreadyUsed").as[Int] shouldBe 1   // written once: not again

    // the undo a minute later takes the undone answer's row back as it is given
    val later  = controller(Mode.Dev, answers, labels, Clock.fixed(now.plusSeconds(60), ZoneOffset.UTC))
    val undone = contentAsJson(later.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "undo"))))
    (undone \ "exported").as[String] should include("1 row of withdrawn answers taken back")
    LabelsTsv.read(labels) shouldBe empty
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("FRANZ KAFKA")
    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "film")))) shouldBe BAD_REQUEST
  }

  it should "be kept when the labels file cannot be written, the response saying why" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val labels  = tempDirectory("labels")                    // a directory: no file can be read or written there
    val card    = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head.payload(ReviewPage.Queue)
    val result  = controller(Mode.Dev, answers, labels).answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "right")))
    status(result) shouldBe OK
    (contentAsJson(result) \ "exportError").as[String] should startWith(s"not written to $labels")
    answers.current().map(_.title) shouldBe Seq("FRANZ KAFKA")
  }

  "the queue" should "rank by the listings an answer settles, and say on each card how many and why the resolver is unsure" in {
    val crowded = services.identity.ResolverDecision((1 to 6).map(i => services.movies.ListingKey.Published(s"Kino $i", "Kandydaci śmierci", None, Nil)),
      None, 0.48, services.identity.ResolverDecision.Basis.BelowThreshold, Seq("best rejected candidate 1703629 at 52.3% (title overlap)"))()
    val pooled  = services.identity.ResolverDecision(Seq(services.movies.ListingKey.Published("Kino Z", "Dolly", None, Nil)), Some(500), 0.7,
      services.identity.ResolverDecision.Basis.PooledMatch, Nil)()
    val c = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
      Map(Country.Poland -> new InMemoryReviewSource(Seq(heldDecision, crowded, matchedDecision, pooled))),
      new ReviewAnswers(new InMemoryReviewAnswerStore), tempFile("labels", ".tsv"), clock)
    val html = contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest()))
    val order = Seq("Kandydaci śmierci", "FRANZ KAFKA", "Dolly").map(t => html.indexOf(s"""<span class="raw-title">$t</span>"""))
    order should not contain -1
    order shouldBe order.sorted
    html should include("""<span class="settles">settles 6 listings</span> · <span class="unsure">52.3% unsure</span>""")
    html should include("""<span class="reason">best candidate held below the cut at 52%</span>""")
    html should include("""<span class="reason">PooledMatch take at 70%</span>""")
    html should not include "Klondike"                                       // an own take the stages agree on
  }

  it should "take a cluster the reviewer can't determine off the list, and label nothing for it" in {
    val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
    val labels  = tempFile("labels", ".tsv")
    val c       = controller(Mode.Dev, answers, labels)
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("""data-verdict="unsure">Can't determine""")
    val card    = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head.payload(ReviewPage.Queue)
    val reply = contentAsJson(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "unsure"))))
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should not include "FRANZ KAFKA"

    (reply \ "exported").as[String] should startWith("identity-judgements.tsv: 0 added")
    (reply \ "exported").as[String] should not include "not exported"   // no film to label is the answer, not a gap
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
      new ReviewAnswers(new InMemoryReviewAnswerStore), tempFile("labels", ".tsv"), clock)
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
      new ReviewAnswers(new InMemoryReviewAnswerStore), tempFile("labels", ".tsv"), clock).recent(Some("pl"), 48, 60, false)(FakeRequest()))
    html should not include "could not read"
    html should include("Klondike")
  }

  "a card" should "read its listings, pages, feeds and films side by side, not one after another" in {
    // Each of the five reads waits until the others have started: read in turn, the first would wait out the barrier.
    val barrier = new java.util.concurrent.CyclicBarrier(5)
    def meet[A](read: => A): A = { barrier.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); read }
    val meeting: ReviewSource = new ReviewSource {
      private val source = ReviewFixtures.source(now)
      export source.{slots as _, venuePages as _, feeds as _, films as _, filmRecords as _, *}
      def slots(listingKeys: Seq[String])          = meet(source.slots(listingKeys))
      def venuePages(urls: Seq[String])            = meet(source.venuePages(urls))
      def feeds(listings: Seq[(String, String)])   = meet(source.feeds(listings))
      def films(tmdbIds: Seq[Int])                 = meet(source.films(tmdbIds))
      def filmRecords(tmdbIds: Seq[Int])           = meet(source.filmRecords(tmdbIds))
    }
    val card = ReviewCards.build(meeting, Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil, new ReviewAnswers.Index(Nil)).head
    card.members.map(_.venue) shouldBe Seq("Kino Opalenica")
  }

  "every review page" should "show the clusters of the source's latest decisions read, not those it kept from an earlier one" in {
    var held = Seq(heldDecision)
    val rereading: ReviewSource = new ReviewSource {
      private val source = ReviewFixtures.source(now)
      export source.{decisions as _, *}
      def decisions(unmatchedOnly: Boolean) = held                                   // the same read until it changes
    }
    val c = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> rereading),
      new ReviewAnswers(new InMemoryReviewAnswerStore), tempFile("labels", ".tsv"), clock)
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("FRANZ KAFKA")
    held = Seq(vetoedDecision)
    val html = contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest()))
    html should include("Macbeth")
    html should not include "FRANZ KAFKA"
  }

  "every review page" should "show 20 cards unless asked for more, the routes' default the one its links leave out" in {
    val routes = scala.io.Source.fromResource("routes").getLines().filter(_.contains("controllers.ReviewController.")).toSeq
    val limits = routes.flatMap(line => """limit: Int \?= (\w+)""".r.findFirstMatchIn(line).map(_.group(1)))
    limits should have size 3                                                   // the queue, the matchable page, the recent page
    limits.distinct shouldBe Seq(ReviewController.DefaultLimit.toString)
    ReviewController.DefaultLimit shouldBe 20
  }
}

