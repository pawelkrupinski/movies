package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.DecorationDiscovery._
import services.identity.TitleDecorations

/** The decoration discovery's own decisions on venue titles from the unmatched clusters' fixture: what it mines, the
 *  rule it writes, what it refuses to strip, how it judges a moved take, and what it writes into the rules. */
class DecorationDiscoverySpec extends AnyFlatSpec with Matchers {

  // PL venues' unmatched titles (test/resources/fixtures/identity-unmatched/pl.json.gz) carrying a programme banner
  private val banner = Seq(
    "Kino Nowe Horyzonty" -> "Binti Edukacja Młode Horyzonty",
    "Kino Nowe Horyzonty" -> "Fritzi – przyjaźń bez granic Edukacja Młode Horyzonty",
    "Kino Pod Baranami"   -> "Pinokio Edukacja Mlode Horyzonty",
    "Kino Nowe Horyzonty" -> "Kino Konesera: Róża",
    "Kino Nowe Horyzonty" -> "Kino Konesera: Ojczyzna",
    "Kino Nowe Horyzonty" -> "Kino Konesera: Lalka")

  "Mining" should "propose a run recurring around three different titles" in {
    val mined = mine(banner, Seq("Binti", "Pinokio"), TitleDecorations.None, minRemainders = 3)
    mined.map(c => c.side -> c.text) should contain allOf ("suffix" -> "edukacja mlode horyzonty", "prefix" -> "kino konesera")
    // the shorter runs only ever bill inside those two: one proposal each, not one per word
    mined.map(c => c.side -> c.text) should contain noneOf ("suffix" -> "mlode horyzonty", "suffix" -> "horyzonty", "prefix" -> "kino")
  }

  it should "never propose a run a known film title starts or ends with" in {
    val edges = Edges(Seq("Kill Bill: Vol. 2", "Konesera"))
    edges.stripsRealTitle("suffix", Seq("vol", "2")) shouldBe true
    edges.stripsRealTitle("prefix", Seq("kill")) shouldBe true
    edges.stripsRealTitle("prefix", Seq("vol", "2")) shouldBe false
    edges.stripsRealTitle("prefix", Seq("kino", "konesera")) shouldBe false
    // "Kino Konesera" is a decoration, unless a film is titled so: then it is part of a real title
    mine(banner, Seq("Kino Konesera"), TitleDecorations.None, minRemainders = 3).map(_.text) should not contain "kino konesera"
  }

  it should "not propose what the resolver's decorations already strip" in {
    val known = TitleDecorations(Set(Seq("kino", "konesera")), Set.empty)
    mine(banner, Nil, known, minRemainders = 3).map(_.text) should not contain "kino konesera"
  }

  "The rule" should "strip the run in every spelling it was billed in, and nothing that only resembles it" in {
    val c = Candidate("suffix", Seq("edukacja", "mlode", "horyzonty"), 3, 2, Seq("binti"))
    val p = ruleFor(c, banner.map(_._2).filter(t => carries(c, t))).get
    p.rule.id shouldBe "xtra-discovered-suffix-edukacja-mlode-horyzonty"
    p.rule("Binti Edukacja Młode Horyzonty") shouldBe "Binti"
    p.rule("Pinokio Edukacja Mlode Horyzonty") shouldBe "Pinokio"
    p.rule("Pinokio | EDUKACJA MŁODE HORYZONTY") shouldBe "Pinokio"
    p.rule("Edukacja Młode Horyzonty") shouldBe "Edukacja Młode Horyzonty"
    p.rule("Binti Edukacja Młode Horyzontyści") shouldBe "Binti Edukacja Młode Horyzontyści"
  }

  it should "strip a prefix with its separator, never the whole title" in {
    val c = Candidate("prefix", Seq("kino", "konesera"), 3, 1, Seq("roza"))
    val p = ruleFor(c, banner.map(_._2).filter(t => carries(c, t))).get
    p.rule("Kino Konesera: Róża") shouldBe "Róża"
    p.rule("KINO KONESERA - Lalka") shouldBe "Lalka"
    p.rule("Kino Konesera") shouldBe "Kino Konesera"
    p.rule("Kino Koneserami") shouldBe "Kino Koneserami"
  }

  "Judging" should "tell a gain from a loss, a switch and a wrong take" in {
    judge("tmdb:1", "tmdb:1", billsSeveral = false, labelled = None) shouldBe None
    judge("", "tmdb:1", billsSeveral = false, labelled = Some(true)) shouldBe Some(Right)
    judge("", "tmdb:1", billsSeveral = false, labelled = Some(false)) shouldBe Some(Wrong)
    judge("", "tmdb:1", billsSeveral = false, labelled = None) shouldBe Some(Unjudged)
    judge("", "tmdb:1", billsSeveral = true, labelled = Some(true)) shouldBe Some(Wrong)
    judge("tmdb:1", "", billsSeveral = false, labelled = None) shouldBe Some(Lost)
    judge("tmdb:1", "tmdb:2", billsSeveral = false, labelled = Some(true)) shouldBe Some(Switched)
  }

  "An evaluation" should "be kept only on a right take with nothing wrong, switched or unanswered" in {
    val p = ruleFor(Candidate("prefix", Seq("kino", "konesera"), 3, 1, Nil), Seq("Kino Konesera: Róża")).get
    def e(verdicts: Seq[String], unanswered: Int = 0) =
      Evaluation(p, 1, verdicts.map(v => Change("pl", "v", "t", "", "", v)), unanswered)
    e(Seq(Right, Lost, Unjudged)).kept shouldBe true
    e(Seq(Right, Wrong)).kept shouldBe false
    e(Seq(Right, Switched)).kept shouldBe false
    e(Seq(Lost, Unjudged)).kept shouldBe false
    e(Seq(Right), unanswered = 1).kept shouldBe false
  }

  "Applying" should "insert the kept rules above the marker, with the titles they were measured on" in {
    val p = ruleFor(Candidate("prefix", Seq("kino", "konesera"), 3, 1, Nil), Seq("Kino Konesera: Róża")).get
    val source = s"  val discovered: Seq[Discovered] = Seq(\n    $Marker\n  )\n"
    val out = splice(source, Seq(p))
    out should include ("Discovered(TitleRule(\"xtra-discovered-prefix-kino-konesera\", GlobalStructural")
    out should include ("Seq(\"Kino Konesera: Róża\" -> \"Róża\"))")
    out.indexOf("xtra-discovered") should be < out.indexOf(Marker)
    report(Seq(Evaluation(p, 1, Seq(Change("pl", "Kino", "Kino Konesera: Róża", "", "tmdb:1 Róża (2026)", Right)), 0)), "fixture") should
      include ("| kept | prefix | `kino konesera` |")
  }

  "The unmatched clusters' fixture" should "measure a proposal that touches no listing as nothing" in {
    val capture = tools.UnmatchedClusters.read(tools.UnmatchedClusters.fixturePath(models.Country.Spain))
    val bench   = new Bench(new CaptureReplay(capture, None), Nil)
    val p = ruleFor(Candidate("prefix", Seq("zzz", "qqq"), 3, 1, Nil), Seq("Zzz Qqq: Film")).get
    bench.evaluate(p) shouldBe None
    bench.lexicon should not be empty
  }
}
