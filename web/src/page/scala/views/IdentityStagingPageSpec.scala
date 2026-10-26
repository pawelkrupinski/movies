package views

import com.sun.net.httpserver.HttpExchange
import controllers.IdentityStagingController
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout, status}
import play.api.test.{FakeRequest, Helpers}
import services.identity.staging.{IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedFact, StagedListing, StagedNode, StagedQuestion, StagedTake, StagingBeat}
import tools.{CdpPage, Chrome, MutableClock, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.time.Instant

/**
 * `/debug/identity/staging` in real Chrome: every country's section on the one page, each cluster a row that opens onto
 * its questions and their outcomes, and the page following what the workers publish next by its own polls — a row
 * left open stays open across them.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class IdentityStagingPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  private val staging = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private val controller = new IdentityStagingController(Helpers.stubControllerComponents(), play.api.Mode.Dev, Seq(staging),
    models.Country.Poland, clock, pollMillis = 100)
  private def admin(path: String) = FakeRequest("GET", path)

  private def question(text: String, outcome: String, attempts: Int = 0) =
    StagedQuestion("FamilySearch", "imdb", text, outcome, attempts, Option.when(attempts > 0)(clock.millis() + 600000), Option.when(attempts > 0)("imdb.com"))
  private def cluster(key: String, venue: String, title: String, questions: Seq[StagedQuestion], resolvedAt: Option[Long] = None) =
    StagedCluster(key, Seq(StagedListing(venue, title)), 1, StagedTake(None, None, "NoEvidence", 0.0), 0, onTmdb = false,
      questions, questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since = clock.millis() - 120000,
      resolvedAt = resolvedAt, resolution = resolvedAt.map(_ => StagedCluster.Settled))
  private def beat(n: Int) = StagingBeat(clock.millis(), n, n, 0, Map("FamilySearch" -> n), 0)

  private val diuna = cluster("a1", "Kino Muza", "Diuna",
    Seq(question("imdb|title|Diuna", StagedQuestion.Failed, attempts = 3), question("wiki|title|Diuna", StagedQuestion.NotAttempted)))

  /** The page's polls, answered by the controller, its query string and all. */
  private def data(exchange: HttpExchange): Boolean = exchange.getRequestURI.getPath == "/debug/identity/staging/data" && {
    val since  = Option(exchange.getRequestURI.getRawQuery).map(q => java.net.URLDecoder.decode(q.stripPrefix("since="), StandardCharsets.UTF_8))
    val result = controller.data(since)(admin("/debug/identity/staging/data"))
    val bytes  = contentAsString(result).getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(status(result), bytes.length.toLong)
    val os = exchange.getResponseBody
    try os.write(bytes) finally os.close()
    true
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer(
      { case "/debug/identity/staging" => contentAsString(controller.index(admin("/debug/identity/staging"))) },
      dynamicRoute = data)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onPage(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/identity/staging")(body)
    case None    => cancel("Chrome not installed — skipping /debug/identity/staging page test")
  }

  "the staging page" should "show every country at once, open a cluster onto its questions, and follow the next publish live" in {
    staging.publish("pl", Seq(diuna), Nil, beat(1))
    staging.publish("de", Seq(cluster("b1", "Babylon", "Der Pate", Seq(question("imdb|title|Der Pate", StagedQuestion.Answered)))), Nil, beat(1))
    onPage { page =>
      page.waitFor("document.querySelectorAll('section.country').length === " + models.Country.all.size)
      page.evalString("document.querySelector('section[data-country=pl] .pill.staging').textContent") should include ("1")
      page.evalString("document.querySelector('section[data-country=de] details.cluster summary').textContent") should include ("Der Pate")
      page.evalString("document.querySelector('section[data-country=uk]').textContent") should include ("no snapshot yet")
      page.evalString("document.querySelector('section[data-country=pl] details.cluster .body.meta').textContent") should include ("1/2 attempted")

      page.eval("document.querySelector('section[data-country=pl] details.cluster summary').click()")
      page.waitFor("document.querySelector('section[data-country=pl] details.cluster').open")
      page.evalInt("document.querySelectorAll('section[data-country=pl] details.cluster[open] tr.question').length") shouldBe 2
      page.evalString("document.querySelector('section[data-country=pl] tr.question .o-failed').textContent") should include ("failed 3×")

      // the next publish: Diuna stops waiting, and arrives under "Decided and changed" by the page's own poll, still open
      clock.advanceSeconds(20)
      staging.publish("pl", Seq(diuna.copy(resolvedAt = Some(clock.millis()), resolution = Some(StagedCluster.Settled))), Nil, beat(0))
      page.waitFor("document.querySelector('section[data-country=pl]').textContent.indexOf('settled') >= 0")
      page.evalString("document.querySelector('section[data-country=pl]').textContent") should include ("nothing waits")
      page.eval("document.querySelector('section[data-country=pl] details.cluster').open").as[Boolean] shouldBe true
    }
  }

  // The UK saw "node Native(Southsea Cinema & Arts Centre,https://southseacinema.savoysystems.co.uk/…,Legend)": the
  // resolver's last explanation line names its cluster's first listing by the key's case-class text.
  it should "name a listing key's fields one per line where a why line ends with one" in {
    val page = "https://southseacinema.savoysystems.co.uk/SouthseaCinema.dll/TSelectItems.waSelectItemsPrompt.TcsWebMenuItem_687.TcsWebTab_688.TcsProgramme_174366"
    val take = StagedTake(Some(1), Some("Legend"), "OwnMatch", 0.9, explanation = Seq("own match 1 at 90.0%",
      s"node Native(Southsea Cinema & Arts Centre,$page,Legend)", "node Published(Kino Muza,Belle, the film,Some(2013),List(Amma Asante))"))
    staging.publish("es", Nil, Nil, beat(0), resolved = Seq(cluster("k1", "Southsea Cinema & Arts Centre", "Legend", Nil, resolvedAt = Some(clock.millis()))
      .copy(take = take, resolution = Some(StagedCluster.Decided))), keptFor = java.time.Duration.ofDays(7))
    onPage { page_ =>
      val row = "section[data-country=es] details.cluster[data-id=\"es|k1\"]"
      page_.waitFor(s"document.querySelector('$row') !== null")
      page_.eval(s"document.querySelector('$row > summary').click()")
      page_.waitFor(s"document.querySelector('$row').open")
      def item(i: Int) = page_.evalString(s"""Array.from(document.querySelectorAll('$row ul.why')[0].children[$i].querySelectorAll('.key'))
        .map(e => e.textContent).join('\\n')""")
      item(1).linesIterator.toSeq shouldBe Seq("venue Southsea Cinema & Arts Centre", s"page $page", "title Legend")
      page_.evalString(s"document.querySelectorAll('$row ul.why')[0].children[1].querySelector('.key a').getAttribute('href')") shouldBe page
      // a link reads quietly: the page's steel blue, not the browser's bright default
      page_.evalString(s"getComputedStyle(document.querySelector('$row ul.why .key a')).color") shouldBe "rgb(127, 151, 173)"
      item(2).linesIterator.toSeq shouldBe Seq("venue Kino Muza", "title Belle, the film", "year 2013", "directors Amma Asante")
      // one per line: each field its own block
      page_.evalString(s"getComputedStyle(document.querySelector('$row ul.why .key')).display") shouldBe "block"
      item(0) shouldBe ""   // a line naming no key is left as it is
    }
  }

  it should "show why a decided cluster was matched, and no question counts when it asked none" in {
    val take = StagedTake(Some(1234), Some("Groundswell"), "OwnMatch", 0.96,
      explanation = Seq("Ojai Playhouse/Groundswell: best tmdb 1234 at 96.0%", "kept apart: tmdb 99 (year 1987)", "… and 3 more"))
    staging.publish("us", Nil, Nil, beat(0), resolved = Seq(cluster("c1", "Ojai Playhouse", "Groundswell", Nil, resolvedAt = Some(clock.millis()))
      .copy(take = take, resolution = Some(StagedCluster.Decided))), keptFor = java.time.Duration.ofDays(7))
    onPage { page =>
      val row = "section[data-country=us] details.cluster"
      page.waitFor(s"document.querySelector('$row') !== null")
      page.evalString(s"document.querySelector('$row summary').textContent") should include ("Groundswell · OwnMatch 96%")
      page.evalString(s"document.querySelector('$row .body.meta').textContent") should not include ("answered")
      page.eval(s"document.querySelector('$row summary').click()")
      page.waitFor(s"document.querySelector('$row').open")
      page.evalInt(s"document.querySelectorAll('$row ul.why li').length") shouldBe 3
      page.evalString(s"document.querySelector('$row ul.why').textContent") should include ("best tmdb 1234 at 96.0%")
    }
  }

  // The US saw "settled · Royal Ballet & Opera 2026/27: Swan Lake · Broadcast 91%" and nothing before it: the take's
  // unshrinkable line took the whole row, squeezing the listing's title to nothing.
  it should "keep a resolved row's listing in view beside a long take, and name a row with no listing by its take" in {
    val take = StagedTake(Some(1702782), Some("Royal Ballet & Opera 2026/27: Swan Lake"), "Broadcast", 0.91)
    val swan = cluster("d1", "Galaxy Theatres Gig Harbour", "Royal Ballet: Swan Lake", Nil, resolvedAt = Some(clock.millis()))
      .copy(take = take, listingCount = 2)
    staging.publish("es", Nil, Nil, beat(0), resolved = Seq(swan, swan.copy(key = "d2", listings = Nil, listingCount = 0)),
      keptFor = java.time.Duration.ofDays(7))
    onPage { page =>
      def title(key: String) = s"document.querySelector('section[data-country=es] details.cluster[data-id=\"es|$key\"] summary .title')"
      page.waitFor(s"${title("d2")} !== null")
      page.eval("document.getElementById('countries').style.width = '460px'")   // one column, as on a laptop's half screen
      page.evalString(s"${title("d1")}.textContent") should include ("Royal Ballet: Swan Lake")
      page.evalInt(s"Math.round(${title("d1")}.getBoundingClientRect().width)") should be > 100
      page.evalString(s"${title("d2")}.textContent") should include ("(no listings) Royal Ballet & Opera 2026/27: Swan Lake")
    }
  }

  it should "lay every fact a move was decided on side by side: the listing's, each film's, each trace before and now" in {
    def facts(named: (String, String)*) = named.map(StagedFact.apply)
    val listing = StagedListing("Kino Amok", "Belle", facts("year" -> "2021", "directors" -> "Mamoru Hosoda", "poster" -> "https://kino.example/belle.jpg"),
      formerly = facts("year" -> "2021", "poster" -> "https://kino.example/belle.jpg"))
    val other   = StagedListing("Rialto", "Belle", facts("year" -> "2021"))
    val before  = StagedTake(Some(1), Some("Belle"), "OwnMatch", 0.71, filmFacts = facts("tmdb" -> "1", "year" -> "2013", "directors" -> "Amma Asante"),
      nodes = Seq(StagedNode("Kino Amok", "Belle", facts("decided by" -> "resolver:title", "score" -> "71.0%", "measure year.gap" -> "8"))))
    val after   = StagedTake(Some(2), Some("Belle"), "Agreed", 0.92, filmFacts = facts("tmdb" -> "2", "year" -> "2021", "directors" -> "Mamoru Hosoda"),
      nodes = Seq(StagedNode("Kino Amok", "Belle", facts("decided by" -> "agreement:director", "score" -> "92.0%", "measure year.gap" -> "0"))))
    val moved   = cluster("f1@4/changed/1", "Kino Amok", "Belle", Nil, resolvedAt = Some(clock.millis()))
      .copy(listings = Seq(listing, other), listingCount = 2, take = after, resolution = Some(StagedCluster.Changed), was = Some(1), before = Some(before),
        trigger = Some("scrape"))
    staging.publish("pl", Nil, Nil, beat(0), resolved = Seq(moved), keptFor = java.time.Duration.ofDays(7))
    onPage { page =>
      val row = "section[data-country=pl] details.cluster[data-id=\"pl|f1@4/changed/1\"]"
      page.waitFor(s"document.querySelector('$row') !== null")
      page.eval(s"document.querySelector('$row > summary').click()")
      page.waitFor(s"document.querySelector('$row').open")
      def table(i: Int) = page.evalString(s"""Array.from(document.querySelectorAll('$row table.facts')[$i].rows)
        .map(r => Array.from(r.cells).map(c => c.textContent + (c.classList.contains('differs') ? '*' : '')).join('|')).join('\\n')""")
      table(0).linesIterator.toSeq shouldBe Seq(
        "|listing before|listing now|before: Belle|now: Belle",
        "year|2021*|2021*|2013*|2021*",
        "poster|https://kino.example/belle.jpg|https://kino.example/belle.jpg|—|—",
        "directors|—|Mamoru Hosoda*|Amma Asante*|Mamoru Hosoda*",
        "tmdb|—|—|1*|2*")
      val changed = page.evalString(s"Array.from(document.querySelectorAll('$row .line.changed li')).map(e => e.textContent).join('\\n')")
      changed.linesIterator.toSeq shouldBe Seq(
        "set off by: scrape",
        "listing Belle @ Kino Amok: directors — → Mamoru Hosoda",
        "decision: film Belle (2013) → Belle (2021); basis OwnMatch → Agreed; decided by resolver:title → agreement:director")
      page.evalString(s"document.querySelector('$row .line.trace .label').textContent") should include ("trace · Belle @ Kino Amok")
      table(1).linesIterator.toSeq shouldBe Seq("|before|now", "decided by|resolver:title*|agreement:director*", "score|71.0%*|92.0%*", "measure year.gap|8*|0*")
      page.evalString(s"document.querySelector('$row details.listing-facts summary').textContent") should include ("the other 1 listings' facts")
    }
  }

  it should "list only the picked types, each saying on hover what it means, and say what a moved take was before and why, beside what it is now and why" in {
    val before = StagedTake(Some(1), Some("Belle (2013)"), "OwnMatch", 0.71,
      explanation = Seq("best tmdb 1 at 71.0%", "decided by resolver:title-year", "runner-up then: tmdb 2 Belle (2021) — the film taken now"))
    val after  = StagedTake(Some(2), Some("Belle (2021)"), "Agreed", 0.92, explanation = Seq("agreed: imdb, wiki name tmdb 2"))
    val moved  = cluster("e1@3/changed/1", "Kino Amok", "Belle", Nil, resolvedAt = Some(clock.millis()))
      .copy(take = after, resolution = Some(StagedCluster.Changed), was = Some(1), before = Some(before))
    val decided = cluster("e2@3/decided", "Babylon", "Oppenheimer", Nil, resolvedAt = Some(clock.millis()))
      .copy(take = StagedTake(Some(872585), Some("Oppenheimer"), "OwnMatch", 0.97), resolution = Some(StagedCluster.Decided))
    staging.publish("de", Seq(cluster("e3", "Babylon", "Der Pate", Seq(question("imdb|title|Der Pate", StagedQuestion.NotAttempted)))), Nil,
      beat(1), resolved = Seq(moved, decided), keptFor = java.time.Duration.ofDays(7))
    onPage { page =>
      val section = "section[data-country=de]"
      def rows = page.evalInt(s"document.querySelectorAll('$section details.cluster').length")
      def titles = page.evalString(s"Array.from(document.querySelectorAll('$section details.cluster summary .title')).map(e => e.textContent).join('|')")
      page.waitFor(s"document.querySelector('$section details.cluster[data-id=\"de|e1@3/changed/1\"]') !== null")
      titles should (include ("Belle") and include ("Oppenheimer") and include ("Der Pate"))
      val all = rows
      def hover(t: String) = page.evalString(s"document.querySelector('#filters button.filter[data-type=\"$t\"]').title")
      hover("staging") should include ("still waits")
      hover("changed") should include ("one film to another")
      hover("joined") should include ("already on the site")
      page.evalInt("Array.from(document.querySelectorAll('#filters button.filter')).filter(b => !b.title).length") shouldBe 0

      page.eval("document.querySelector('#filters button.filter[data-type=changed]').click()")
      page.waitFor(s"document.querySelectorAll('$section details.cluster').length === 1")
      titles should include ("Belle")
      page.evalString(s"document.querySelector('$section').textContent") should include ("none of the picked types")
      page.evalString("location.hash") shouldBe "#only=changed"
      page.evalString(s"document.querySelector('$section details.cluster summary > .meta').textContent") should include (
        "changed · Belle (2013) → Belle (2021) · Agreed 92%")

      page.eval(s"document.querySelector('$section details.cluster summary').click()")
      page.waitFor(s"document.querySelector('$section details.cluster').open")
      // each kind of data its own hue: the listing's title, its venue, the film it takes now, the one it took before
      def color(selector: String) = page.evalString(s"getComputedStyle(document.querySelector('$section details.cluster $selector')).color")
      Seq("summary .title", "summary .venue", "summary .film", "summary .move-from").map(color).distinct should have size 4
      page.evalString(s"document.querySelector('$section details.cluster .line.from').textContent") should include ("Belle (2013) (tmdb 1) · OwnMatch 71%")
      page.evalString(s"document.querySelector('$section details.cluster .line.to').textContent") should include ("Belle (2021) · Agreed 92%")
      val whys = page.evalString(s"Array.from(document.querySelectorAll('$section details.cluster ul.why')).map(e => e.textContent).join('|')")
      whys should (include ("decided by resolver:title-year") and include ("the film taken now") and include ("agreed: imdb, wiki name tmdb 2"))

      page.eval("document.querySelector('#filters button.filter[data-type=staging]').click()")   // a second type picked beside it
      page.waitFor(s"document.querySelectorAll('$section details.cluster').length >= 2")
      titles should (include ("Belle") and include ("Der Pate") and not include ("Oppenheimer"))
      page.eval("document.querySelector('#filters button.filter[data-type=\"\"]').click()")       // all
      page.waitFor(s"document.querySelectorAll('$section details.cluster').length === $all")
    }
  }

  it should "list listings joining a match quietly, as joined, and say how many events the worker was never told" in {
    val joined = cluster("c1@7/joined", "Abbeygate Bury St Edmunds", "NAZA", Nil, resolvedAt = Some(clock.millis()))
      .copy(listingCount = 86, resolution = Some(StagedCluster.Joined))
    staging.publish("uk", Seq(joined), Nil, beat(0).copy(dropped = Some(42)))
    onPage { page =>
      page.waitFor("document.querySelector('section[data-country=uk] details.cluster') !== null")
      page.evalString("document.querySelector('section[data-country=uk] details.cluster').className") should include ("quiet")
      page.evalString("document.querySelector('section[data-country=uk] details.cluster summary .r-joined').textContent") shouldBe "joined"
      page.evalString("document.querySelector('section[data-country=uk] details.cluster summary .title').textContent") should include ("+85 joined")
      page.evalString("document.querySelector('section[data-country=uk] .pill.dropped').textContent") should include ("42 events dropped")
      // quiet: its title drawn dimmer than a decision's
      page.evalString("getComputedStyle(document.querySelector('section[data-country=uk] details.cluster summary .title')).color") shouldBe
        "rgb(138, 138, 138)"
    }
  }

  it should "list the countries one under the other across the page, one alone when picked, or all in one list by time" in {
    clock.advanceSeconds(60)
    val older = cluster("t1", "Ojai Playhouse", "Anora", Nil, resolvedAt = Some(clock.millis())).copy(resolution = Some(StagedCluster.Decided))
    staging.publish("us", Nil, Nil, beat(0), resolved = Seq(older), keptFor = java.time.Duration.ofDays(7))
    clock.advanceSeconds(10)
    val newer = cluster("t2", "Kino Muza", "Anora", Nil, resolvedAt = Some(clock.millis())).copy(resolution = Some(StagedCluster.Decided))
    staging.publish("pl", Nil, Nil, beat(0), resolved = Seq(newer), keptFor = java.time.Duration.ofDays(7))
    onPage { page =>
      page.waitFor("document.querySelectorAll('section.country').length === " + models.Country.all.size)
      page.waitFor("document.querySelector('section[data-country=pl] details.cluster[data-id=\"pl|t2\"]') !== null")
      // one under the other, each the page's whole width
      page.eval("""(() => { const all = Array.from(document.querySelectorAll('section.country')), width = document.getElementById('countries').clientWidth;
        return all.every((s, i) => s.offsetWidth === width && (i === 0 || s.offsetTop >= all[i - 1].offsetTop + all[i - 1].offsetHeight)); })()""")
        .as[Boolean] shouldBe true

      page.eval("document.querySelector('#country-filters button.filter[data-country=us]').click()")
      page.waitFor("document.querySelectorAll('section.country').length === 1")
      page.evalString("document.querySelector('section.country').dataset.country") shouldBe "us"
      page.evalString("location.hash") shouldBe "#country=us"

      page.eval("document.querySelector('#country-filters button.filter[data-country=all]').click()")
      page.waitFor("document.querySelector('section[data-country=all]') !== null")
      page.evalInt("document.querySelectorAll('section.country').length") shouldBe 1
      // every country's rows in one list, each tagged with its country: the waits, then the decisions newest first
      def ids = page.evalString("Array.from(document.querySelectorAll('section[data-country=all] details.cluster')).map(e => e.dataset.id).join(' ')")
      ids should include ("pl|t2 us|t1")
      page.evalString("document.querySelector('section[data-country=all] details.cluster[data-id=\"pl|t2\"] summary .cc').textContent") shouldBe "pl"
      page.evalString("location.hash") shouldBe "#country=all"

      page.eval("document.querySelector('#country-filters button.filter[data-country=\"\"]').click()")   // by country
      page.waitFor("document.querySelectorAll('section.country').length === " + models.Country.all.size)
    }
  }

  it should "split an own match's line into its fields and signals, one a line, and link each film, page and answer out" in {
    val line = "'Ladies Night - Przepis na święta' [2026] {Magdalena Nieć} ×23: own match 1601776 at 94.6% by favoured-calibrated " +
      "(venues.corroborating=29(+5.06) director=same_person(+4.22) search.rank=1(+3.48) year.delta=0(+3.40) popularity.log2=-1(-2.04))"
    val page = "https://kino.example/ladies-night"
    val take = StagedTake(Some(1601776), Some("Ladies Night"), "OwnMatch", 0.946, explanation = Seq(line),
      filmFacts = Seq(StagedFact("tmdb", "1601776"), StagedFact("imdb", "tt3456789")))
    val ladies = cluster("l1", "Kino Muza", "Ladies Night - Przepis na święta",
      Seq(question("imdb|record|tt3456789", StagedQuestion.Answered), question("filmweb|title|Ladies Night", StagedQuestion.Answered)),
      resolvedAt = Some(clock.millis()))
      .copy(take = take, resolution = Some(StagedCluster.Decided),
        listings = Seq(StagedListing("Kino Muza", "Ladies Night - Przepis na święta", facts = Seq(StagedFact("page", page)))))
    staging.publish("uk", Nil, Nil, beat(0), resolved = Seq(ladies), keptFor = java.time.Duration.ofDays(7))
    onPage { p =>
      val row = "section[data-country=uk] details.cluster[data-id=\"uk|l1\"]"
      p.waitFor(s"document.querySelector('$row') !== null")
      p.eval(s"document.querySelector('$row > summary').click()")
      p.waitFor(s"document.querySelector('$row').open")
      val keys = p.evalString(s"Array.from(document.querySelector('$row ul.why li').querySelectorAll('.key')).map(e => e.textContent).join('\\n')")
      keys.linesIterator.toSeq shouldBe Seq("listing 'Ladies Night - Przepis na święta'", "year 2026", "directors Magdalena Nieć",
        "listings 23", "film tmdb 1601776 at 94.6%", "by favoured-calibrated", "signals ",
        "venues.corroborating 29 +5.06", "director same_person +4.22", "search.rank 1 +3.48", "year.delta 0 +3.40", "popularity.log2 -1 -2.04")
      p.evalString(s"document.querySelector('$row ul.why .key.signal:last-child .weight').className") shouldBe "weight against"
      def hrefs(selector: String) =
        p.evalString(s"Array.from(document.querySelectorAll('$row $selector a')).map(a => a.href).join(' ')")
      hrefs("ul.why") should include ("https://www.themoviedb.org/movie/1601776")
      hrefs(".line.to") should include ("https://www.themoviedb.org/movie/1601776")
      hrefs("tr.question") should (include ("https://www.imdb.com/title/tt3456789/") and include ("https://www.filmweb.pl/search#/film?query=Ladies%20Night"))
      hrefs("table.facts") should (include ("https://www.themoviedb.org/movie/1601776") and include ("https://www.imdb.com/title/tt3456789/") and include (page))
      hrefs(".line:first-child") should include (page)   // the listing's own page
    }
  }
}
