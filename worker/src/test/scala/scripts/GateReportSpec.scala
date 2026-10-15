package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.GateReport.{Cand, Evaluation, Known, Run}

/** The synopsis gate's rule — never accept on plot text alone — and what a run reads off its command line. */
class GateReportSpec extends AnyFlatSpec with Matchers {

  private def cand(title: String, year: Option[Int] = Some(2020), directors: Set[String] = Set.empty, cast: Set[String] = Set.empty,
                   characters: Set[String] = Set.empty, text: String = "") = Cand(title, s"u/$title", title, year, text, directors, cast, characters)
  private val row = Known(directors = Set("Agnieszka Holland"), cast = Set("Maja Ostaszewska", "Jalal Altawil"), characters = Set("Julia"),
    year = Some(2020), queries = Seq("Zielona granica"))
  private def evaluated(c: Cand, sim: Double = 0.0, known: Known = row, mode: String = "imdb-eng") = GateReport.evaluate(mode, "", sim, known, c)

  "The gate" should "accept an exact title or a director overlap as corroborated" in {
    val exact = evaluated(cand("Zielona Granica"))
    (exact.eligible, exact.corroborated, exact.via) shouldBe (true, true, "exact-title")
    val directed = evaluated(cand("Green Border", directors = Set("Agnieszka Holland")))
    (directed.eligible, directed.via) shouldBe (true, "director")
  }

  it should "accept plot text only beside a character name or two cast, and never on its own" in {
    evaluated(cand("Green Border"), sim = 0.9).eligible shouldBe false
    evaluated(cand("Green Border", text = "Julia crosses the forest"), sim = 0.1).via shouldBe "character-name"
    evaluated(cand("Green Border", cast = Set("Maja Ostaszewska", "Jalal Altawil")), sim = 0.1).eligible shouldBe true
    evaluated(cand("Green Border", cast = Set("Maja Ostaszewska", "Jalal Altawil")), sim = GateReport.Tau / 2).eligible shouldBe false
  }

  it should "reject a director contradiction, and a year off by more than one unless the title is exact" in {
    evaluated(cand("Zielona granica", directors = Set("Andrzej Wajda"))).eligible shouldBe false
    evaluated(cand("Green Border", year = Some(2017), directors = Set("Agnieszka Holland"))).eligible shouldBe false
    evaluated(cand("Zielona granica", year = Some(2017))).eligible shouldBe true
  }

  it should "look for the candidate's character names in the row's own synopsis when it resolves TMDB" in {
    GateReport.evaluate("tmdb-pl", "Julia, aktywistka", 0.1, row.copy(characters = Set.empty, cast = Set.empty),
      cand("Green Border", characters = Set("Julia"))).characters shouldBe Seq("Julia")
  }

  "Deciding" should "take a corroborated candidate before a more similar one, and the most similar abstained only when emitting all" in {
    def e(title: String, sim: Double, exact: Boolean, cast: Int = 0) =
      Evaluation(cand(title), sim, Nil, Seq.fill(cast)("c"), Nil, exact, yearContradicts = false, directorContradicts = false)
    GateReport.decide(Seq(e("domain", 0.9, exact = false, cast = 2), e("exact", 0.2, exact = true)), emitAll = false)
      .map { case (best, accepted) => best.candidate.title -> accepted } shouldBe Some("exact" -> true)
    GateReport.decide(Seq(e("low", 0.01, exact = false), e("high", 0.5, exact = false)), emitAll = false) shouldBe None
    GateReport.decide(Seq(e("low", 0.01, exact = false), e("high", 0.5, exact = false)), emitAll = true)
      .map { case (best, accepted) => best.candidate.title -> accepted } shouldBe Some("high" -> false)
  }

  "The synopsis similarity" should "score the same plot above an unrelated one" in {
    val docs = Seq("A girl crosses the forest border with her family", "A detective solves a murder in the city")
    val idf  = GateReport.buildIdf(docs, "en")
    def sim(a: String, b: String) = GateReport.cosine(GateReport.vec(a, "en", idf), GateReport.vec(b, "en", idf))
    sim(docs.head, "The family crosses a forest border") should be > sim(docs.head, docs(1))
    GateReport.cosine(Map.empty, Map("a" -> 1.0)) shouldBe 0.0
  }

  "A row's search titles" should "add the title without parentheses, each pipe part and what follows a colon" in {
    GateReport.stripVariants("Kino Konesera: Lalka (2D) | napisy") shouldBe
      Seq("Kino Konesera: Lalka | napisy", "Kino Konesera: Lalka (2D)", "napisy", "Lalka (2D) | napisy")
  }

  "Its command line" should "read the ratings mode, the output and the cap, and refuse a cap that is no number" in {
    def run(args: String*) = GateReport.runOf(GateReport.commandLine.parse(args).toOption.get)
    run() shouldBe Run(ratings = false, GateReport.DefaultOut, None)
    run("ratings") shouldBe Run(ratings = true, GateReport.DefaultRatingsOut, None)
    run("ratings").modes shouldBe Seq("imdb-eng", "filmweb-pl")
    run("o.jsonl", "20") shouldBe Run(ratings = false, "o.jsonl", Some(20))
    an[tools.CommandLine.InvalidArguments] should be thrownBy run("o.jsonl", "all")
    an[tools.CommandLine.InvalidArguments] should be thrownBy run("o.jsonl", "20", "extra")
    GateReport.commandLine.parse(Seq("--cap", "20")).isLeft shouldBe true
  }
}
