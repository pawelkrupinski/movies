package services.cinemas

import models.{KinoBajka, KinoCyfroweKino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ExtraTitleRules
import services.movies.TitleNormalizer
import services.titlerules.{TitleRuleDefaults, TitleRuleSet}
import tools.HttpFetch

import java.time.YearMonth

/** Proves the three dedicated clients route their scraped title through
 *  `TitleNormalizer.cinemaClean("<slug>", …)`, so the fourth-wave per-cinema
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

  private def withExtras[A](body: => A): A = {
    val saved = TitleNormalizer.currentRules
    try { TitleNormalizer.installRules(TitleRuleSet(TitleRuleDefaults.all ++ ExtraTitleRules.all)); body }
    finally TitleNormalizer.installRules(saved)
  }
  private def seedOnly[A](body: => A): A = {
    val saved = TitleNormalizer.currentRules
    try { TitleNormalizer.installRules(TitleRuleDefaults.ruleSet); body }
    finally TitleNormalizer.installRules(saved)
  }

  private val bajkaHtml =
    """<div class="screening-day" id="screening-20260820">
      |  <div class="screening-item">
      |    <div class="title-age-group"><h4><a href="/film/ts5">TOY STORY 5 2D DUB. SPS</a></h4></div>
      |    <div class="screening-link" data-url="https://book/1"><span class="time">18:00</span></div>
      |  </div>
      |</div>""".stripMargin

  private def bajkaTitles() =
    new KinoBajkaClient(NoopHttp, KinoBajka).parseHtml(bajkaHtml).map(_.movie.title)

  "KinoBajkaClient" should "strip the '2D DUB. SPS' screening-code suffix via cinemaClean" in {
    withExtras(bajkaTitles() shouldBe Seq("TOY STORY 5"))
  }
  it should "be load-bearing — the seed rules alone leave the suffix in place" in {
    seedOnly(bajkaTitles() shouldBe Seq("TOY STORY 5 2D DUB. SPS"))
  }

  private val cyfroweHtml =
    """<div class="amy-movie-item">
      |  <h3 class="amy-movie-field-title"><a href="/f/ts5">Premiera! toy story 5</a></h3>
      |  <div class="entry-showtime"><div class="st-item">
      |    <div class="st-title"><label>20/08/2026</label></div>
      |    <ul><li>18:00</li></ul>
      |  </div></div>
      |</div>""".stripMargin

  private def cyfroweTitles() =
    CyfroweKinoClient.parse(cyfroweHtml, KinoCyfroweKino).map(_.movie.title)

  "CyfroweKinoClient" should "strip the 'Premiera!' prefix via cinemaClean" in {
    withExtras {
      val titles = cyfroweTitles()
      titles should have size 1
      titles.head.toLowerCase shouldBe "toy story 5"
    }
  }
  it should "be load-bearing — the seed rules alone keep the 'Premiera!' prefix" in {
    seedOnly(cyfroweTitles().head.toLowerCase should startWith("premiera!"))
  }

  private val kijowHtml =
    """<div class="cd-timeline-content eventlist">
      |  <span class="cd-date">20 sie 18:00</span>
      |  <h2>20 sie 18:00 - Diabeł ubiera się u Prady 2 Napisy PL</h2>
      |  <a class="btn-badge2" href="/MSI/Default.aspx?id=1">Bilety</a>
      |</div>""".stripMargin

  private def kijowTitles() =
    KinoKijowClient.parseDocument(kijowHtml, YearMonth.of(2026, 8)).map(_.title)

  "KinoKijowClient" should "strip the 'Napisy PL' subtitle suffix via cinemaClean" in {
    withExtras(kijowTitles() shouldBe Seq("Diabeł ubiera się u Prady 2"))
  }
  it should "be load-bearing — the seed rules alone leave the 'Napisy PL' suffix" in {
    seedOnly(kijowTitles() shouldBe Seq("Diabeł ubiera się u Prady 2 Napisy PL"))
  }
}
