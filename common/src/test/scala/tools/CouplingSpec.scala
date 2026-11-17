package tools

import kinowo.build.Coupling
import kinowo.build.Coupling.{Graph, Type}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The coupling numbers the report shows and the ratchet holds (`project/Coupling.scala`, `sbt couplingReport`), over a
 *  hand-built class graph: what Zinc's record is folded into (project/CouplingAnalysis.scala reads the record itself);
 *  and the checked-in baseline (scripts/ci/coupling_ratchet.py compares the measured rows with it) in the rows' shape,
 *  its limits the build's. */
class CouplingSpec extends AnyFlatSpec with Matchers {

  private def t(name: String, module: String = "common", isAbstract: Boolean = false) = Type(name, module, isAbstract)

  // models <-> services.movies is a cycle; services.movies -> tools is not; worker.Main uses all three
  private val graph = Graph(
    Seq(t("models.Film"), t("models.Codec", isAbstract = true), t("services.movies.MovieService"),
      t("services.movies.MovieRepository", isAbstract = true), t("tools.Slugify", "base"), t("worker.Main", "worker")),
    Set("models.Film" -> "models.Codec", "models.Codec" -> "services.movies.MovieRepository",
      "services.movies.MovieService" -> "models.Film", "services.movies.MovieService" -> "tools.Slugify",
      "services.movies.MovieService" -> "services.movies.MovieRepository",
      "worker.Main" -> "services.movies.MovieService", "worker.Main" -> "models.Film", "worker.Main" -> "tools.Slugify",
      "worker.Main" -> "worker.Main", "worker.Main" -> "scala.Option"))

  "a class's package" should "be its top two segments, `<root>` for none" in {
    Coupling.packageOf("services.identity.agreement.AgreementStage") shouldBe "services.identity"
    Coupling.packageOf("models.Film") shouldBe "models"
    Coupling.packageOf("AppLoader") shouldBe "<root>"
    Coupling.fullPackageOf("services.identity.agreement.AgreementStage") shouldBe "services.identity.agreement"
  }

  "an object's member" should "be named as a class's member: an object and its class are one type" in {
    Coupling.normalize("clients.TmdbClient$.Page") shouldBe "clients.TmdbClient.Page"
    Coupling.normalize("models.Film") shouldBe "models.Film"
  }

  "a nested class" should "count as its top-level class" in {
    Coupling.topLevel(Set("a.Outer", "a.Outer.Inner", "a.Outer.Inner.Deeper", "a.Other")) shouldBe
      Map("a.Outer" -> "a.Outer", "a.Outer.Inner" -> "a.Outer", "a.Outer.Inner.Deeper" -> "a.Outer", "a.Other" -> "a.Other")
  }

  "the known edges" should "leave out self-references and classes outside the graph" in {
    graph.known should not contain ("worker.Main" -> "worker.Main")
    graph.known should not contain ("worker.Main" -> "scala.Option")
    graph.known.size shouldBe 8
  }

  "module edges" should "count the class references across each module pair" in {
    Coupling.moduleEdges(graph) shouldBe Map(("common", "base") -> 1, ("worker", "common") -> 2, ("worker", "base") -> 1)
  }

  "package stats" should "give Ca and Ce as distinct outside classes, and I, A, D from them" in {
    val byName = Coupling.packages(graph).map(p => p.name -> p).toMap
    val movies = byName("services.movies")
    movies.ce shouldBe 2 // models.Film, tools.Slugify
    movies.cePackages shouldBe 2 // models, tools
    movies.ca shouldBe 2 // models.Codec, worker.Main
    movies.instability shouldBe 0.5
    movies.abstractness shouldBe 0.5
    movies.distance shouldBe 0.0
    val base = byName("tools")
    (base.ca, base.ce, base.instability, base.abstractness, base.distance) shouldBe ((2, 0, 0.0, 0.0, 1.0))
    byName("worker").instability shouldBe 1.0
    (byName("worker").ce, byName("worker").cePackages) shouldBe ((3, 3))
  }

  "a package's Ce in packages" should "not rise when a dependency is split into roles in the same package" in {
    def ce(edges: Set[(String, String)]) = {
      val g = Graph(Seq(t("services.a.User"), t("services.b.Store", isAbstract = true), t("services.b.Reader", isAbstract = true),
        t("services.b.Writer", isAbstract = true)), edges)
      Coupling.packages(g).find(_.name == "services.a").map(p => (p.ce, p.cePackages))
    }
    ce(Set("services.a.User" -> "services.b.Store")) shouldBe Some((1, 1))
    ce(Set("services.a.User" -> "services.b.Reader", "services.a.User" -> "services.b.Writer")) shouldBe Some((2, 1))
  }

  "a package cycle" should "be a strongly connected component, with every edge inside it" in {
    val cycles = Coupling.cycles(graph)
    cycles.map(_.packages) shouldBe Seq(Seq("models", "services.movies"))
    cycles.head.edges.map(_._1) shouldBe Seq("models" -> "services.movies", "services.movies" -> "models")
    cycles.head.edges.head._2 shouldBe Seq("models.Codec" -> "services.movies.MovieRepository")
  }

  "the components" should "find every cycle of a graph, a long one and a self-contained pair alike" in {
    Coupling.components(Seq("a", "b", "c", "d", "e", "f"),
      Set("a" -> "b", "b" -> "c", "c" -> "a", "c" -> "d", "e" -> "f", "f" -> "e")) shouldBe
      Seq(Seq("a", "b", "c"), Seq("e", "f"))
    Coupling.components(Seq("a", "b"), Set("a" -> "b")) shouldBe empty
  }

  "fan-in and fan-out" should "count distinct production classes" in {
    val fans = Coupling.fans(graph)
    fans("worker.Main") shouldBe ((0, 3))
    fans("models.Film") shouldBe ((2, 1))
    fans("tools.Slugify") shouldBe ((2, 0))
  }

  "a split package" should "be a full package with classes in two modules" in {
    val split = Graph(Seq(t("tools.Slugify", "base"), t("tools.HttpFetch", "http"), t("tools.Digest", "base"), t("models.Film")), Set.empty)
    Coupling.splitPackages(split) shouldBe Seq("tools" -> Seq("base" -> 2, "http" -> 1))
  }

  "the baseline rows" should "hold the limits, the module and cycle edges, every package's Ce and the fan-outs at the limit" in {
    Coupling.baselineRows(graph, fanOutLimit = 3, newPackageCeLimit = 15) shouldBe Seq(
      "limit\tfanout\t3", "limit\tnew-package-ce\t15",
      "module-edge\tcommon->base\t1", "module-edge\tworker->base\t1", "module-edge\tworker->common\t2",
      "cycle-edge\tmodels->services.movies\t1", "cycle-edge\tservices.movies->models\t1",
      "ce\tmodels\t1", "ce\tservices.movies\t2", "ce\ttools\t0", "ce\tworker\t3",
      "fanout\tservices.movies.MovieService\t3", "fanout\tworker.Main\t3")
  }

  "the report" should "name an edge used but not declared, and a declared one no class uses" in {
    val report = Coupling.report(graph, Map("worker" -> Set("common"), "common" -> Set("base", "geodata")))
    report should include("`worker -> base` (1 refs) is used but not declared")
    report should not include ("`worker -> common` (2 refs) is used but not declared")
    report should include("| `services.movies` | common | 2 | 1 | 2 | 2 | 2 | 0.50 | 0.50 | 0.00 |")
    report should include("1. `models`, `services.movies`")
  }

  "the checked-in baseline" should "be the rows' shape, one row a key, its limits the build's" in {
    val lines = Files.readAllLines(RepoRoot.file("test/resources/coupling-baseline.tsv").toPath).asScala.toSeq
    lines.take(Coupling.BaselineHeader.size) shouldBe Coupling.BaselineHeader
    val rows = lines.filterNot(l => l.startsWith("#") || l.trim.isEmpty).map(_.split("\t", -1).toSeq)
    all(rows.map(_.size)) shouldBe 3
    rows.map(_.head).distinct.toSet should contain allElementsOf Set("limit", "module-edge", "ce")
    rows.map(_.head).toSet -- Set("limit", "module-edge", "cycle-edge", "ce", "fanout") shouldBe empty
    all(rows.map(_(2))) should fullyMatch regex "[0-9]+"
    rows.map(r => (r(0), r(1))).distinct.size shouldBe rows.size
    rows.collect { case Seq("limit", key, value) => key -> value.toInt }.toMap shouldBe
      Map("fanout" -> Coupling.DefaultFanOutLimit, "new-package-ce" -> Coupling.DefaultNewPackageCeLimit)
  }
}
