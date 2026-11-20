package tools

import kinowo.build.{Coupling, CouplingSimulation}
import kinowo.build.Coupling.{Exemption, Exemptions, Graph, Type}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A move's coupling measured before the move (project/CouplingSimulation.scala, `scripts/coupling-report --simulate`):
 *  the coupling report's functions over its graph with classes renamed into other packages, folded into another class,
 *  and layer rows added, over a hand-built graph. */
class CouplingSimulationSpec extends AnyFlatSpec with Matchers {

  private def t(name: String, isAbstract: Boolean = false) = Type(name, "common", isAbstract)
  // services.movies holds a store (abstract), the service over it and a reading every outside package uses
  private val graph = Graph(
    Seq(t("models.Film"), t("services.movies.Store", isAbstract = true), t("services.movies.Service"), t("services.movies.Reading"),
      t("services.movies.Helper"), t("services.users.Users"), t("services.cards.Cards")),
    Set("services.movies.Service" -> "services.movies.Store", "services.movies.Service" -> "services.movies.Reading",
      "services.movies.Service" -> "services.movies.Helper", "services.movies.Reading" -> "models.Film",
      "services.users.Users" -> "services.movies.Reading", "services.cards.Cards" -> "services.movies.Reading",
      "services.cards.Cards" -> "services.movies.Service", "services.movies.Helper" -> "models.Film"))
  private val table = Seq("layer\t1\tmodels\tdata", "layer\t3\tservices\tthe application",
    "rank\t1\tservices.movies\tthe corpus", "rank\t2\tservices.users\tusers", "rank\t2\tservices.cards\tcards")
  private def moves(lines: String*) = CouplingSimulation.Moves.parse(lines, graph)

  "a moves file" should "rename each class it names, a pattern's every class, a fold under its host, and keep the layer rows" in {
    val m = moves("# the reading leaves", "services.movies.Reading\tservices.evidence", "services.movies.H*\tinto services.movies.Service",
      "rank\t0\tservices.evidence\tthe evidence")
    m.renames shouldBe Map("services.movies.Reading" -> "services.evidence.Reading", "services.movies.Helper" -> "services.movies.Service")
    m.folds shouldBe Set("services.movies.Helper")
    m.layerRows shouldBe Seq("rank\t0\tservices.evidence\tthe evidence")
    CouplingSimulation.renamed(m)("services.movies.Reading$") shouldBe "services.evidence.Reading$"
    CouplingSimulation.renamed(m)("services.movies.Reading.Row") shouldBe "services.evidence.Reading.Row"
    CouplingSimulation.renamed(m)("services.movies.Helper") shouldBe "services.movies.Service.Helper"
    CouplingSimulation.renamed(m)("services.movies.Readings") shouldBe "services.movies.Readings"
  }

  it should "refuse a class no production class is, a host that is none, and a row of no known shape" in {
    an[IllegalArgumentException] should be thrownBy moves("services.movies.Nothing\tservices.evidence")
    an[IllegalArgumentException] should be thrownBy moves("services.movies.Helper\tinto services.movies.Nothing")
    an[IllegalArgumentException] should be thrownBy moves("services.movies.Helper")
  }

  "a moved graph" should "measure the package a class leaves and the one it enters as the report would after the move" in {
    val after = CouplingSimulation.moved(graph, moves("services.movies.Reading\tservices.evidence"))
    val byName = Coupling.packages(after).map(p => p.name -> p).toMap
    (byName("services.evidence").types, byName("services.evidence").ca, byName("services.evidence").ce) shouldBe ((1, 3, 1))
    (byName("services.movies").types, byName("services.movies").ca) shouldBe ((3, 1))
    after.known should contain("services.users.Users" -> "services.evidence.Reading")
  }

  "a folded class" should "give its host the union of both fan-outs, each collaborator once" in {
    val after = CouplingSimulation.moved(graph, moves("services.movies.Service\tinto services.movies.Helper"))
    Coupling.fans(after)("services.movies.Helper")._2 shouldBe 3 // Store, Reading and Film; Service itself is gone
    after.byName.contains("services.movies.Service") shouldBe false
    after.known should contain("services.cards.Cards" -> "services.movies.Helper")
  }

  "a rank row" should "replace the row ranking its package" in {
    CouplingSimulation.layered(table, Seq("rank\t3\tservices.movies\tmoved up")).rankOf("services.movies") shouldBe Some(3)
  }

  "the report" should "show each changed package, the baseline rows that change, the cycles after and the affected fan-outs" in {
    val exemptions = Exemptions(Seq(Exemption("class", "services.movies.Reading", "exempt", "o", "a reading")))
    val report = CouplingSimulation.report(graph, table, exemptions,
      moves("services.movies.Reading\tservices.evidence", "rank\t0\tservices.evidence\tthe evidence"))
    report should include("1 classes moved, 0 folded into another, 1 layer-table rows added.")
    report should include("| `services.evidence` | - -> 1 |")
    report should include("`ce` `services.evidence`: (none) -> 1")
    report should not include "`ce` `services.users`"
    report should include("## Package cycles after (0)")
    report should include("| `services.movies.Reading` | `services.evidence.Reading` | 1 | exempt |")
    // without its rank row the new package has none in an ordered layer
    CouplingSimulation.report(graph, table, Exemptions.empty, moves("services.movies.Reading\tservices.evidence")) should
      include("`rank-violation` `services.evidence->?`: (none) -> 0")
  }
}
