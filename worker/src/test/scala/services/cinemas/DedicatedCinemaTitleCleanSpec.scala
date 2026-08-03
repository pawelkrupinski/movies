package services.cinemas

import tools.HttpFetch
import models.{KinoBajka, KinoCyfroweKino, KinoFarys, KinoNaStarowce, KinoOskard, KinoStaryMlyn}
import services.titlerules.{ExtraTitleRules, TitleRules, TitleRuleSet}
import services.movies.TitleNormalizer
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.{Bilety24Client, Bilety24OrganizerClient, CyfroweKinoClient, KinoBajkaClient, KinoKijowClient, SystemBiletowyClient}

import java.time.YearMonth

/** Proves the three dedicated clients route their scraped title through
 *  their injected normalizer's `cinemaClean("<slug>", …)`, so the fourth-wave per-cinema
 *  rules ([[ExtraTitleRules.perCinemaRules]]) actually fire at ingestion.
 *
 *  Load-bearing pair: with only the seed rules the venue junk survives into
 *  `Movie.title` (fail-before); with the extras installed it's stripped
 *  (pass-after). The rule strings themselves are covered by `ExtraTitleRulesSpec`;
 *  this spec is the CODE half — that each client actually calls `cinemaClean`. */
class DedicatedCinemaTitleCleanSpec extends AnyFlatSpec with Matchers {

  private val NoopHttp: HttpFetch = new HttpFetch {
    def get(url: String): String = ""
    def post(url: String, body: String, contentType: String): String = ""
  }

  // Two rule sets held as ORDINARY instances and passed to the client under test.
  // Not `TitleNormalizer.withRules`: the clients now take their normalizer as a
  // constructor/method parameter, so a thread-local swap would not reach them and
  // both halves of each load-bearing pair would silently assert the same thing.
  private val withExtras = new TitleNormalizer(TitleRuleSet(TitleRules.all ++ ExtraTitleRules.all))
  private val seedOnly   = new TitleNormalizer(TitleRules.ruleSet)

  // The repertoire ships as a `data-dane` JSON blob on `<div id="rep2">`; the SPS
  // screening-code lives in the film's `t` title, which cinemaClean must strip.
  private val bajkaHtml =
    """<div id="rep2" data-dane='{"buy":"https://book/1","dni":{"2026-08-20":[{"t":"TOY STORY 5 2D DUB. SPS","s":[{"g":"18:00","h":"nb","x":0}]}]}}'></div>"""

  private def bajkaTitles(titles: TitleNormalizer) =
    new KinoBajkaClient(NoopHttp, KinoBajka, titles = titles).parseHtml(bajkaHtml).map(_.movie.title)

  "KinoBajkaClient" should "strip the '2D DUB. SPS' screening-code suffix via cinemaClean" in {
    bajkaTitles(withExtras) shouldBe Seq("TOY STORY 5")
  }
  it should "be load-bearing — the seed rules alone leave the suffix in place" in {
    bajkaTitles(seedOnly) shouldBe Seq("TOY STORY 5 2D DUB. SPS")
  }

  private val cyfroweHtml =
    """<div class="amy-movie-item">
      |  <h3 class="amy-movie-field-title"><a href="/f/ts5">Premiera! toy story 5</a></h3>
      |  <div class="entry-showtime"><div class="st-item">
      |    <div class="st-title"><label>20/08/2026</label></div>
      |    <ul><li>18:00</li></ul>
      |  </div></div>
      |</div>""".stripMargin

  private def cyfroweTitles(titles: TitleNormalizer) =
    CyfroweKinoClient.parse(cyfroweHtml, KinoCyfroweKino, titles).map(_.movie.title)

  "CyfroweKinoClient" should "strip the 'Premiera!' prefix via cinemaClean" in {
    val titles = cyfroweTitles(withExtras)
    titles should have size 1
    titles.head.toLowerCase shouldBe "toy story 5"
  }
  it should "be load-bearing — the seed rules alone keep the 'Premiera!' prefix" in {
    cyfroweTitles(seedOnly).head.toLowerCase should startWith("premiera!")
  }

  private val kijowHtml =
    """<div class="cd-timeline-content eventlist">
      |  <span class="cd-date">20 sie 18:00</span>
      |  <h2>20 sie 18:00 - Diabeł ubiera się u Prady 2 Napisy PL</h2>
      |  <a class="btn-badge2" href="/MSI/Default.aspx?id=1">Bilety</a>
      |</div>""".stripMargin

  private def kijowTitles(titles: TitleNormalizer) =
    KinoKijowClient.parseDocument(kijowHtml, YearMonth.of(2026, 8), titles).map(_.title)

  "KinoKijowClient" should "strip the 'Napisy PL' subtitle suffix via cinemaClean" in {
    kijowTitles(withExtras) shouldBe Seq("Diabeł ubiera się u Prady 2")
  }
  it should "be load-bearing — the seed rules alone leave the 'Napisy PL' suffix" in {
    kijowTitles(seedOnly) shouldBe Seq("Diabeł ubiera się u Prady 2 Napisy PL")
  }

  // ── shared portal clients: per-cinema cleanup keyed by the derived Cinema.slug ──

  "Cinema.slug" should "derive the kebab-case id the portal rules key on" in {
    KinoOskard.slug shouldBe "kino-oskard"
    KinoStaryMlyn.slug shouldBe "kino-stary-mlyn"
    KinoNaStarowce.slug shouldBe "kino-na-starowce"
    KinoFarys.slug shouldBe "kino-farys"
  }

  private val oskardHtml =
    """<div class="title-name" title="Following/Kino Cafe">Following/Kino Cafe</div>
      |<a class="b24-button" title="Kup bilet - Film: Following/Kino Cafe - 2026-08-20 18:00 - Konin" href="/buy/1">Kup</a>""".stripMargin

  private def oskardTitle(titles: TitleNormalizer) =
    Bilety24Client.parseEvent(oskardHtml, KinoOskard, "https://oskard.example", "ev1", titles).map(_.movie.title)

  "Bilety24Client (Kino Oskard)" should "strip the '/Kino Cafe' venue suffix via cinemaClean" in {
    oskardTitle(withExtras) shouldBe Some("Following")
  }
  it should "be load-bearing — the seed rules alone leave '/Kino Cafe' in place" in {
    oskardTitle(seedOnly) shouldBe Some("Following/Kino Cafe")
  }

  private val staryMlynHtml =
    """<a title="Film: Toy Story 5 sensoryczny - 2026-08-20 18:00 - Zgierz" href="/buy/1">Kup</a>"""

  private def staryMlynTitles(titles: TitleNormalizer) =
    Bilety24OrganizerClient.parse(staryMlynHtml, KinoStaryMlyn, titles).map(_.movie.title)

  "Bilety24OrganizerClient (Kino Stary Młyn)" should "strip the 'sensoryczny' suffix via cinemaClean" in {
    staryMlynTitles(withExtras) shouldBe Seq("Toy Story 5")
  }
  it should "be load-bearing — the seed rules alone leave 'sensoryczny' in place" in {
    staryMlynTitles(seedOnly) shouldBe Seq("Toy Story 5 sensoryczny")
  }

  private def systemBiletowyHtml(title: String) =
    s"""<div class="event-item" data-date="2026-08-20" data-time="18:00">
       |  <h3 class="event-title">$title</h3>
       |  <a href="/kup-bilet/1">Kup</a>
       |</div>""".stripMargin

  "SystemBiletowyClient (Na Starówce)" should "strip the 'akcja lato w kinie' campaign suffix" in {
    SystemBiletowyClient.parse(systemBiletowyHtml("Toy story 5 akcja lato w kinie"), KinoNaStarowce, "https://s.example", withExtras)
      .map(_.movie.title) shouldBe Seq("Toy story 5")
  }

  "SystemBiletowyClient (Kino Farys)" should "fix the 'Tot story 5' source typo" in {
    SystemBiletowyClient.parse(systemBiletowyHtml("Tot story 5"), KinoFarys, "https://s.example", withExtras)
      .map(_.movie.title) shouldBe Seq("Toy Story 5")
  }
  it should "be load-bearing — the seed rules alone leave the typo" in {
    SystemBiletowyClient.parse(systemBiletowyHtml("Tot story 5"), KinoFarys, "https://s.example", seedOnly)
      .map(_.movie.title) shouldBe Seq("Tot story 5")
  }
}
