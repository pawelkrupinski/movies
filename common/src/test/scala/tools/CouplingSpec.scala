package tools

import kinowo.build.{ClassReferences, Coupling}
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

  it should "be the longest deeper package measured on its own that holds it, its top two segments when none does" in {
    val units = Set("services.identity.agreement", "services.identity.agreement.votes", "services.identity.staging")
    Coupling.packageOf("services.identity.agreement.AgreementStage", units) shouldBe "services.identity.agreement"
    Coupling.packageOf("services.identity.agreement.votes.Vote", units) shouldBe "services.identity.agreement.votes"
    Coupling.packageOf("services.identity.agreement.inner.Helper", units) shouldBe "services.identity.agreement"
    Coupling.packageOf("services.identity.IdentityResolver", units) shouldBe "services.identity"
    Coupling.packageOf("services.identity.projection.FilmPlans", units) shouldBe "services.identity"
    Coupling.packageOf("services.identityadapters.TakeReads", units) shouldBe "services.identityadapters"
  }

  // services.identity <-> services.identity.agreement: one package, no cycle, until the agreement is measured on its own
  private val deeper = Graph(
    Seq(t("services.identity.vocabulary.Question", "identity"), t("services.identity.Resolver", "identity"),
      t("services.identity.agreement.Stage", "identity"), t("services.identity.agreement.Family", "identity"), t("models.Film")),
    Set("services.identity.vocabulary.Question" -> "services.identity.agreement.Family", "services.identity.agreement.Stage" -> "services.identity.Resolver",
      "services.identity.Resolver" -> "models.Film"))

  "a deeper package with a row of its own" should "be measured as a package: its Ca, Ce and D, and the cycles through it" in {
    Coupling.packages(deeper).map(_.name) shouldBe Seq("models", "services.identity")
    Coupling.cycles(deeper) shouldBe empty
    val split = deeper.copy(units = Set("services.identity.agreement"))
    val byName = Coupling.packages(split).map(p => p.name -> p).toMap
    byName.keySet shouldBe Set("models", "services.identity", "services.identity.agreement")
    (byName("services.identity.agreement").types, byName("services.identity.agreement").ca, byName("services.identity.agreement").ce) shouldBe ((2, 1, 1))
    (byName("services.identity").ca, byName("services.identity").ce, byName("services.identity").cePackages) shouldBe ((1, 2, 2))
    Coupling.cycles(split).map(_.packages) shouldBe Seq(Seq("services.identity", "services.identity.agreement"))
    Coupling.baselineRows(split) should contain("cycle-edge\tservices.identity->services.identity.agreement\t1")
    Coupling.edgeRows(split) should contain(
      "services.identity.agreement->services.identity\tservices.identity.agreement.Stage->services.identity.Resolver")
  }

  it should "need an exemption row of its own: its parent's `package` row covers it only while it is measured inside the parent" in {
    // services.metrics.web: concrete series every tier reads (D 1). Without a row of its own it is services.metrics, which
    // the `package services.metrics` row exempts; once measured apart, that row (one package, never a prefix) no longer
    // covers it -- the row neither lapses for the parent nor widens to the child unseen: the child takes its own row
    val metrics = Graph(Seq(t("services.metrics.web.PageSeries"), t("services.metrics.web.CacheSeries"), t("services.metrics.Registry"),
        t("controllers.Home", "web")),
      Set("controllers.Home" -> "services.metrics.web.PageSeries", "controllers.Home" -> "services.metrics.web.CacheSeries",
        "controllers.Home" -> "services.metrics.Registry"))
    val exempt = Coupling.Exemptions.parse(Seq("package\tservices.metrics\texempt\tcoupling\tmetric bundles: concrete series, none swapped"))
    def distanceRows(g: Graph) = Coupling.baselineRows(g, exemptions = exempt, distanceMinTypes = 2).filter(_.startsWith("distance\t"))
    distanceRows(metrics) shouldBe empty
    distanceRows(metrics.copy(units = Set("services.metrics.web"))) shouldBe Seq("distance\tservices.metrics.web\t100")
    val both = Coupling.Exemptions.parse(Seq("package\tservices.metrics\texempt\tcoupling\tmetric bundles: concrete series, none swapped",
      "package\tservices.metrics.web\texempt\tcoupling\tthe web tier's metric bundles: concrete series, none swapped"))
    Coupling.baselineRows(metrics.copy(units = Set("services.metrics.web")), exemptions = both, distanceMinTypes = 1)
      .filter(_.startsWith("distance\t")) shouldBe empty
  }

  it should "take its rank from its own row, and be held to it" in {
    val table = Coupling.Layers.parse(Seq("layer\t1\tmodels\td", "layer\t2\tservices\ta", "rank\t0\tservices.identity\tthe model",
      "rank\t1\tservices.identity.agreement\tover the model"))
    table.units shouldBe Set("services.identity.agreement")
    Coupling.Layers.parse(Seq("layer\t2\tservices\ta", "layer\t0\tservices.identity.low\tb")).units shouldBe Set("services.identity.low")
    Coupling.rankViolations(deeper.copy(units = table.units), table) shouldBe
      Seq(("services.identity", "services.identity.agreement") -> Seq("services.identity.vocabulary.Question" -> "services.identity.agreement.Family"))
    Coupling.rankViolations(deeper, table) shouldBe empty
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

  "the baseline rows" should "hold the limits, the module and cycle edges, every package's Ce and the fan-outs past the limit" in {
    Coupling.baselineRows(graph, fanOutLimit = 2, newPackageCeLimit = 15, distanceMinTypes = 100) shouldBe Seq(
      "limit\tfanout\t2", "limit\tfanout-measure\t4", "limit\tnew-package-ce\t15", "limit\tdistance\t20", "limit\tdistance-min-types\t100",
      "limit\tdistance-measure\t2",
      "module-edge\tcommon->base\t1", "module-edge\tworker->base\t1", "module-edge\tworker->common\t2",
      "cycle-edge\tmodels->services.movies\t1", "cycle-edge\tservices.movies->models\t1",
      "ce\tmodels\t1", "ce\tservices.movies\t2", "ce\ttools\t0", "ce\tworker\t3",
      "fanout\tservices.movies.MovieService\t3", "fanout\tworker.Main\t3")
  }

  // a scraper using three of its contract's four members and one class besides; a registry naming five scrapers
  private val contracted = Graph(
    Seq(t("services.cinemas.pl.HeliosClient"), t("services.cinemas.common.CinemaScraper", isAbstract = true), t("models.Cinema"),
      t("models.CinemaMovie"), t("models.Showtime"), t("tools.Slugify"), t("services.cinemas.pl.venues.WarszawaVenues"),
      t("services.identity.Film")),
    Set("services.cinemas.pl.HeliosClient" -> "services.cinemas.common.CinemaScraper", "services.cinemas.pl.HeliosClient" -> "models.Cinema",
      "services.cinemas.pl.HeliosClient" -> "models.CinemaMovie", "services.cinemas.pl.HeliosClient" -> "tools.Slugify",
      "services.identity.Film" -> "models.Cinema", "services.identity.Film" -> "models.CinemaMovie", "services.identity.Film" -> "models.Showtime",
      "services.cinemas.pl.venues.WarszawaVenues" -> "services.cinemas.pl.HeliosClient", "services.cinemas.pl.venues.WarszawaVenues" -> "models.Cinema",
      "services.cinemas.pl.venues.WarszawaVenues" -> "tools.Slugify"))
  private val exemptions = Coupling.Exemptions.parse(Seq(
    "# kind\tpattern\tvalue\towner\twhy",
    "contract\tservices.cinemas.*\tscrape\tscrapers\tthe scrape contract",
    "member\tservices.cinemas.common.CinemaScraper\tscrape\tscrapers\tentry", "member\tmodels.Cinema\tscrape\tscrapers\tvenue",
    "member\tmodels.CinemaMovie\tscrape\tscrapers\tfilm", "member\tmodels.Showtime\tscrape\tscrapers\tscreening",
    "registry\tservices.cinemas.*Venues\texempt\tscrapers\ta venue roster",
    "foundation\tmodels\texempt\tcoupling\tvalues"))

  "a declared contract" should "count as ONE dependency of its users, and as its members of everyone else" in {
    val held = Coupling.heldFanOut(contracted, exemptions)
    held("services.cinemas.pl.HeliosClient") shouldBe 2 // the contract, tools.Slugify
    held("services.identity.Film") shouldBe 3 // not a user: Cinema, CinemaMovie, Showtime each count
    Coupling.heldFanOut(contracted, Coupling.Exemptions.empty)("services.cinemas.pl.HeliosClient") shouldBe 4
  }

  "an exemption" should "take a class out of the fan-out limit, a package out of the distance limit, by pattern" in {
    exemptions.fanOutLimit("services.cinemas.pl.venues.WarszawaVenues", 9) shouldBe None
    exemptions.fanOutLimit("services.cinemas.pl.HeliosClient", 9) shouldBe Some(9)
    exemptions.distanceLimit("models", 20) shouldBe None
    exemptions.distanceLimit("services.identity", 20) shouldBe Some(20)
    Coupling.Exemptions.matches("modules.*", "modules") shouldBe true
    Coupling.Exemptions.matches("modules.*", "modules.wiring.HttpWiring") shouldBe true
    Coupling.Exemptions.matches("modules.*", "modulesX") shouldBe false
    Coupling.Exemptions.parse(Seq("config\tsettings.ProcessConfiguration\t90\tcoupling\tthe typed-config root"))
      .fanOutLimit("settings.ProcessConfiguration", 9) shouldBe Some(90)
    val byRole = Coupling.Exemptions.parse(Seq("package\tservices.metrics\texempt\tcoupling\tmetric bundles: concrete series, none swapped"))
    byRole.distanceLimit("services.metrics", 20) shouldBe None
    byRole.distanceLimit("services.movies", 20) shouldBe Some(20)
    byRole.fanOutLimit("services.metrics", 9) shouldBe Some(9) // a package's distance, never a class's fan-out
  }

  "the baseline rows" should "freeze a class past the limit unless exempt, and a large package past the distance limit unless exempt" in {
    val rows = Coupling.baselineRows(contracted, fanOutLimit = 1, exemptions = exemptions, distanceLimit = 20, distanceMinTypes = 3)
    rows should contain("fanout\tservices.cinemas.pl.HeliosClient\t2")
    rows should contain("fanout\tservices.identity.Film\t3")
    rows.filter(_.contains("WarszawaVenues")) shouldBe empty // a registry
    rows.filter(_.startsWith("distance\t")) shouldBe Seq("distance\tservices.cinemas\t33") // models (3 types, D 1) is a foundation; services.cinemas: A 1/3, I 1
    Coupling.baselineRows(contracted, fanOutLimit = 1, distanceMinTypes = 3).filter(_.startsWith("distance\t")) shouldBe
      Seq("distance\tmodels\t100", "distance\tservices.cinemas\t33")
  }

  "the exemption table" should "refuse an unknown kind, a value neither exempt nor a limit, a row without its why, and a half contract" in {
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("rooot\tmodules.*\texempt\to\tw"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("root\tmodules.*\tmany\to\tw"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("root\tmodules.*\texempt\to\t "))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("root\tmodules.*\texempt\to"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("package\tservices.*\texempt\to\tw")) // one package
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("class\tservices.*\texempt\to\tw")) // one class
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("member\tmodels.Cinema\tscrape\to\tw"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("contract\tservices.cinemas.*\tscrape\to\tw"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("contract-size\tscrape\t12\to\tw")) // no such contract
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("contract\tservices.cinemas.*\tscrape\to\tw",
      "member\tmodels.Cinema\tscrape\to\tw", "contract-size\tscrape\texempt\to\tw"))
  }

  "a class exemption" should "exempt one named class from the fan-out limit, carrying the day it was added" in {
    val table = Coupling.Exemptions.parse(Seq("class\tservices.identity.Clustering\texempt\tidentity\tpipeline stage: clusters a " +
      "family's members; needs the evidence, edges and solver it sequences\t2026-11-19"))
    table.fanOutLimit("services.identity.Clustering", 9) shouldBe None
    table.fanOutLimit("services.identity.Voting", 9) shouldBe Some(9)
    table.rows.map(_.added) shouldBe Seq("2026-11-19")
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("class\tservices.identity.*\texempt\to\tw\t2026-11-19"))
    an[IllegalArgumentException] should be thrownBy Coupling.Exemptions.parse(Seq("class\tservices.identity.Voting\texempt\to\tw\tyesterday"))
  }

  "the checked-in exemption table" should "parse, exempt the roots, registries, config root and foundations, and declare the scrape contract" in {
    val table = Coupling.Exemptions.parse(Files.readAllLines(RepoRoot.file("test/resources/coupling-exemptions.tsv").toPath).asScala.toSeq)
    Seq("modules.WorkerMain", "modules.wiring.HttpWiring", "services.cinemas.pl.venues.KrakowVenues", "services.cinemas.CinemaScraperCatalog",
      "settings.ProcessConfiguration").map(table.fanOutLimit(_, 9)) shouldBe Seq(None, None, None, None, None)
    Seq("models", "tools", "settings", "services.titles", "modules.wiring").map(table.distanceLimit(_, 20)) shouldBe Seq(None, None, None, None, None)
    table.fanOutLimit("services.identity.IdentityResolver", 9) shouldBe Some(9)
    table.distanceLimit("services.movies", 20) shouldBe Some(20)
    table.contractsOf("services.cinemas.pl.HeliosClient").map(c => c._1 -> c._2.size) shouldBe Seq("scrape" -> 12)
    table.contractsOf("services.identity.IdentityResolver") shouldBe empty
  }

  "the resolver vocabulary, projection, served and task-handler contracts" should "be gone: under the collaborators-only count every member was data, or a lone collaborator" in {
    val table = Coupling.Exemptions.parse(Files.readAllLines(RepoRoot.file("test/resources/coupling-exemptions.tsv").toPath).asScala.toSeq)
    table.contracts.map(_._1) should contain noneOf ("resolver-vocabulary", "projection", "served", "task-handler")
  }

  "the registries that hold only data" should "be exempt no more: the city and country tables and the Polish roster registry count no collaborator past the limit" in {
    val table = Coupling.Exemptions.parse(Files.readAllLines(RepoRoot.file("test/resources/coupling-exemptions.tsv").toPath).asScala.toSeq)
    Seq("models.City", "models.Country", "services.cinemas.pl.PolishVenueRegistry").map(table.fanOutLimit(_, 9)) shouldBe Seq(Some(9), Some(9), Some(9))
  }

  "the fan-out rows" should "give every class's held fan-out, one at or below the limit included, with what it counts" in {
    // the classes it references, folded as the count folds them (a contract it uses one `contract:<name>`): what a
    // `Coupling: split of <X>` claim compares with X's frozen reference set
    Coupling.fanOutRows(contracted, exemptions) should contain allOf ("services.cinemas.pl.HeliosClient\t2\tcontract:scrape,tools.Slugify",
      "services.identity.Film\t3\tmodels.Cinema,models.CinemaMovie,models.Showtime", "models.Showtime\t0\t")
    Coupling.fanOutRows(contracted, exemptions).size shouldBe contracted.types.size
  }

  "the agreement sources contract" should "be one dependency of the stage's builder and the pipeline it fills, and its members one by one for a stage" in {
    val table = Coupling.Exemptions.parse(Files.readAllLines(RepoRoot.file("test/resources/coupling-exemptions.tsv").toPath).asScala.toSeq)
    val members = Seq("services.identity.agreement.FamilyAnswers", "services.identity.IdentityLookups",
      "services.identity.agreement.AgreementVerdicts", "services.identity.agreement.AnswerChanges",
      "services.identity.vocabulary.PosterAnswers", "services.identity.vocabulary.CatalogueAnswers", "services.titles.TitleNormalizer",
      "services.identity.agreement.AgreementKeys")
    table.contracts.find(_._1 == "agreement-sources").map(_._3.toSet) shouldBe Some(members.toSet)
    val composition = "services.identity.agreement.AgreementStageComposition"
    val pipeline    = "services.identity.agreement.AgreementPipeline"
    val evidence    = "services.identity.agreement.AgreementEvidence" // a stage reading some of the same types
    val takes       = "services.identity.agreement.AgreementTakes"
    val graph = Graph((composition +: pipeline +: evidence +: takes +: members).map(t(_)),
      (members.map(composition -> _) ++ members.map(pipeline -> _) ++ members.take(2).map(evidence -> _) :+ (pipeline -> takes)).toSet)
    val held = Coupling.heldFanOut(graph, table)
    held(composition) shouldBe 1 // the contract
    held(pipeline) shouldBe 2    // the contract, the takes
    held(evidence) shouldBe 2    // no discount: two members
    table.contractsOf(evidence) shouldBe empty
  }

  "the edge rows" should "give every class reference with its package edge, a package's own included" in {
    Coupling.edgeRows(graph) shouldBe Seq(
      "models->models\tmodels.Film->models.Codec",
      "models->services.movies\tmodels.Codec->services.movies.MovieRepository",
      "services.movies->models\tservices.movies.MovieService->models.Film",
      "services.movies->services.movies\tservices.movies.MovieService->services.movies.MovieRepository",
      "services.movies->tools\tservices.movies.MovieService->tools.Slugify",
      "worker->models\tworker.Main->models.Film",
      "worker->services.movies\tworker.Main->services.movies.MovieService",
      "worker->tools\tworker.Main->tools.Slugify")
  }

  "the report" should "name an edge used but not declared, and a declared one no class uses" in {
    val report = Coupling.report(graph, Map("worker" -> Set("common"), "common" -> Set("base", "geodata")))
    report should include("`worker -> base` (1 refs) is used but not declared")
    report should not include ("`worker -> common` (2 refs) is used but not declared")
    report should include("| `services.movies` | common | 2 | 1 | 2 | 2 | 2 | 0.50 | 0.50 | 0.00 |")
    report should include("1. `models`, `services.movies`")
  }

  private val layers = Coupling.Layers.parse(Seq(
    "# a comment", "", "layer\t0\ttools\tutilities", "layer\t1\tmodels\tthe data", "layer\t2\tservices\tthe application",
    "layer\t3\tworker\tthe composition root"))

  "a package's layer" should "be the longest prefix row naming it, none when no row does" in {
    layers.layerOf("services.movies") shouldBe Some(2)
    layers.layerOf("models") shouldBe Some(1)
    layers.layerOf("modelsx") shouldBe None
    Coupling.Layers.parse(Seq("layer\t2\tservices\tx", "layer\t0\tservices.low\ty")).layerOf("services.low") shouldBe Some(0)
  }

  "the layering" should "forbid a package edge into a higher layer and a package without one, unless allowed with a why" in {
    Coupling.layerViolations(graph, layers) shouldBe Seq(("models", "services.movies") -> Seq("models.Codec" -> "services.movies.MovieRepository"))
    val allowing = Coupling.Layers.parse(Seq("layer\t0\ttools\tu", "layer\t1\tmodels\td", "layer\t2\tservices\ta",
      "allow\tmodels->services.movies\tthe codec reads the repository"))
    Coupling.layerViolations(graph, allowing) shouldBe Seq(("worker", "?") -> Nil)
    Coupling.baselineRows(graph, fanOutLimit = 3, layers = Some(allowing)).last shouldBe "layer-violation\tworker->?\t0"
  }

  // services.a -> services.b and services.b -> services.c inside layer 2, with services.b ranked above services.a: the
  // first edge points up inside the layer
  private val ordered = Graph(Seq(t("services.a.A"), t("services.b.B"), t("services.c.C"), t("services.d.D"), t("models.M")),
    Set("services.a.A" -> "services.b.B", "services.b.B" -> "services.c.C", "services.c.C" -> "models.M", "services.d.D" -> "models.M"))
  private val ranked = Coupling.Layers.parse(Seq("layer\t1\tmodels\tthe data", "layer\t2\tservices\tthe application",
    "rank\t0\tservices.c\tthe bottom", "rank\t1\tservices.a\tover c", "rank\t2\tservices.b\tover a", "rank\t0\tservices.d\tbeside c"))

  "the order inside a layer" should "forbid an edge to a higher rank, naming it with its class references" in {
    Coupling.rankViolations(ordered, ranked) shouldBe Seq(("services.a", "services.b") -> Seq("services.a.A" -> "services.b.B"))
    Coupling.layerViolations(ordered, ranked) shouldBe empty
  }

  it should "allow an edge down, or to the same rank, and an upward one an `allow` row names" in {
    val flat = Coupling.Layers.parse(Seq("layer\t1\tmodels\td", "layer\t2\tservices\ta", "rank\t0\tservices.a\tx",
      "rank\t0\tservices.b\tx", "rank\t0\tservices.c\tx", "rank\t0\tservices.d\tx"))
    Coupling.rankViolations(ordered, flat) shouldBe empty
    val allowing = Coupling.Layers.parse(Seq("layer\t1\tmodels\td", "layer\t2\tservices\ta", "rank\t0\tservices.c\tx",
      "rank\t1\tservices.a\tx", "rank\t2\tservices.b\tx", "rank\t0\tservices.d\tx", "allow\tservices.a->services.b\tthe one seam"))
    Coupling.rankViolations(ordered, allowing) shouldBe empty
  }

  it should "name a package of an ordered layer that has no rank, and leave an unordered layer alone" in {
    val missing = Coupling.Layers.parse(Seq("layer\t1\tmodels\td", "layer\t2\tservices\ta", "rank\t0\tservices.c\tx",
      "rank\t2\tservices.a\tx", "rank\t1\tservices.b\tx"))
    Coupling.rankViolations(ordered, missing) shouldBe Seq(("services.d", "?") -> Nil)
    Coupling.baselineRows(ordered, layers = Some(missing)).last shouldBe "rank-violation\tservices.d->?\t0"
    missing.ordered(1) shouldBe false
    missing.rankOf("models") shouldBe None
  }

  it should "say in the report which direction is allowed" in {
    Coupling.report(ordered, Map.empty, layers = Some(ranked)) should include(
      "- `services.a -> services.b` points up inside layer 2 (rank 2.1 -> 2.2; the allowed direction is `services.b -> services.a`)")
  }

  "the layer table" should "refuse a row of another shape and an allowed edge without its why" in {
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("layer\tzero\ttools\tu"))
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("allow\tmodels->services\t "))
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("lyer\t0\ttools\tu"))
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("layer\t2\tservices\ta", "rank\tx\tservices.a\tu"))
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("layer\t2\tservices\ta", "rank\t0\tservices.a\t "))
    an[IllegalArgumentException] should be thrownBy
      Coupling.Layers.parse(Seq("layer\t2\tservices\ta", "rank\t0\tservices.a\tu", "rank\t1\tservices.a\tv"))
    an[IllegalArgumentException] should be thrownBy Coupling.Layers.parse(Seq("layer\t2\tservices\ta", "rank\t0\tclients\tu"))
  }

  "the checked-in layer table" should "parse, and name every package's layer" in {
    val table = Coupling.Layers.parse(Files.readAllLines(RepoRoot.file("test/resources/coupling-layers.tsv").toPath).asScala.toSeq)
    Seq("tools", "models", "settings", "services.identity", "clients", "controllers", "modules.wiring").flatMap(table.layerOf) shouldBe
      Seq(0, 1, 2, 3, 3, 4, 5)
    // the application and the composition roots are ordered: the identity model under the cinemas that build its
    // readings, its worker adapters over them; the wirings under the roots
    Seq(3, 5).forall(table.ordered) shouldBe true
    Seq("services.identity", "services.cinemas", "services.tasks", "services.identityadapters").flatMap(table.rankOf) shouldBe
      Seq("services.identity", "services.cinemas", "services.tasks", "services.identityadapters").flatMap(table.rankOf).sorted.distinct
    table.rankOf("modules.wiring").get should be < table.rankOf("modules").get
  }

  // ── the same-file uses (ClassReferences: the fixture's class files, compiled into this test configuration) ──

  private val fixture = "tools.couplingfixture"
  private def compiled(pkg: String): Seq[ClassReferences.Compiled] = {
    val dir = java.nio.file.Paths.get(getClass.getResource("/" + pkg.replace('.', '/')).toURI)
    Files.list(dir).iterator.asScala.toSeq.filter(_.toString.endsWith(".class")).map(p => ClassReferences.read(Files.readAllBytes(p)))
  }
  /** One source's binary names (a class and its object) -> its top-level classes. */
  private def tops(pkg: String, names: String*): Map[String, String] =
    names.flatMap(n => Seq(s"$pkg.$n" -> s"$pkg.$n", s"$pkg.$n$$" -> s"$pkg.$n")).toMap
  private val together = s"$fixture.together"
  private val togetherTops = tops(together, "Schedule", "Showtimes", "Listing", "Venue", "Alone", "Event", "Started", "Stopped")
  private lazy val sameFile = Coupling.sameFileEdges(compiled(together), togetherTops)

  "a class's uses of the top-level classes of its own file" should "count, through a field, a lambda and an anonymous class" in {
    sameFile.filter(_._1 == s"$together.Schedule") shouldBe
      Set("Showtimes", "Listing", "Venue").map(n => s"$together.Schedule" -> s"$together.$n")
  }

  it should "leave out a class using none of them, and a sealed parent's compiler-written mirror of its children" in {
    sameFile.filter(_._1 == s"$together.Alone") shouldBe empty
    sameFile.filter(_._1 == s"$together.Event") shouldBe empty
    sameFile should contain allOf (s"$together.Started" -> s"$together.Event", s"$together.Stopped" -> s"$together.Event")
    sameFile.filter { case (from, to) => from == to } shouldBe empty
  }

  "a local or an anonymous class" should "fold into its nearest enclosing class of the source, any other name into none" in {
    Coupling.foldBinary(s"$together.Schedule$$$$anon$$1", togetherTops) shouldBe Some(s"$together.Schedule")
    Coupling.foldBinary(s"$together.Venue$$Local$$1", togetherTops) shouldBe Some(s"$together.Venue")
    Coupling.foldBinary("scala.Option", togetherTops) shouldBe None
    Coupling.foldBinary(s"$fixture.split.Schedule", togetherTops) shouldBe None
  }

  "moving a collaborator into a class's file" should "not lower its fan-out" in {
    val split = s"$fixture.split"
    def graph(pkg: String, zinc: Set[(String, String)], sameFileEdges: Set[(String, String)]) =
      Graph(Seq("Schedule", "Showtimes", "Listing", "Venue").map(n => t(s"$pkg.$n")), zinc ++ sameFileEdges)
    // split: Zinc records each use across files, and the one-class file adds none
    val zinc = Set("Showtimes", "Listing", "Venue").map(n => s"$split.Schedule" -> s"$split.$n")
    val apart = graph(split, zinc, Coupling.sameFileEdges(compiled(split), tops(split, "Schedule")))
    // together: Zinc records none of them, the class files all
    val joined = graph(together, Set.empty, sameFile)
    Coupling.fans(apart)(s"$split.Schedule")._2 shouldBe 3
    Coupling.fans(joined)(s"$together.Schedule")._2 shouldBe 3
    Coupling.baselineRows(joined, fanOutLimit = 2).filter(_.startsWith("fanout")) shouldBe Seq(s"fanout\t$together.Schedule\t3")
  }

  // ── a file's imports, which Zinc charges to its last class ──

  private val imports = s"$fixture.imports"
  private val importTops = tops(imports, "Booking", "Ledger", "Tally")
  private val splitPackage = s"$fixture.split"
  private val splitTops = tops(splitPackage, "Listing", "Venue", "Showtimes", "Schedule")
  private def inImports(name: String) = s"$imports.$name"
  private def inSplit(name: String) = s"$splitPackage.$name"

  "the class a file's imports are charged to" should "be its last top-level definition, or its top-level definitions' wrapper" in {
    val source = new String(Files.readAllBytes(RepoRoot.file("common/src/test/scala/tools/couplingfixture/imports/Charged.scala").toPath), "UTF-8")
    Coupling.importOwner(source, importTops.values.toSet) shouldBe Some(inImports("Tally"))
    Coupling.importOwner("package a\n\nimport b.C\n\nfinal class First\ndef helper: Int = 1\nfinal class Last\n",
      Set("a.First", "a.Last", "a.Charged$package")) shouldBe Some("a.Charged$package")
    Coupling.importOwner("package a\n\n@deprecated(\"x\") private[a] sealed abstract class Only\n// object Mentioned\n", Set("a.Only")) shouldBe Some("a.Only")
    Coupling.importOwner("package a\n", Set("a.Gone")) shouldBe None
  }

  "a file's imports" should "be charged to the classes using them, not to the last class of the file" in {
    val (booking, ledger, tally) = (inImports("Booking"), inImports("Ledger"), inImports("Tally"))
    val (venue, listing, showtimes) = (inSplit("Venue"), inSplit("Listing"), inSplit("Showtimes"))
    // Zinc's record of Charged.scala: each import's real use, and both imports charged to Tally, the last class -- with
    // a tree-only use of Tally's that its class file does not show and no other class of the file accounts for
    val zinc = Set(booking -> venue, ledger -> listing, tally -> venue, tally -> listing, tally -> showtimes)
    val uses = Coupling.classFileUses(compiled(imports), importTops, splitTops)
    uses shouldBe Map(booking -> Set(venue), ledger -> Set(listing), tally -> Set(listing))
    val corrected = Coupling.withoutImportCharge(zinc, tally, uses(tally), importTops.values.toSet)
    corrected shouldBe Set(booking -> venue, ledger -> listing, tally -> listing, tally -> showtimes)
    Coupling.fans(Graph(Seq(booking, ledger, tally, venue, listing, showtimes).map(n => t(n)), corrected))(tally)._2 shouldBe 2
  }

  // ── data or collaborator (Coupling.Collaborators: the fixture's class files, couplingfixture/kinds/Kinds.scala) ──

  private val kinds = s"$fixture.kinds"
  private val kindNames = Seq("Point", "Origin", "Colour", "Figure", "Circle", "Blank", "Slug", "Units", "Score", "Celsius", "NotFound",
    "Booking", "Title", "Stamp", "Counter", "Memo", "Env", "Draft", "YearCap", "Normalizer", "Store", "MapStore", "Port", "Rule",
    "EmptyRule", "Wiring", "Tickets", "Source", "Fixed", "Live", "Node", "Numbering")
  private lazy val kindReasons: Map[String, String] =
    Coupling.Collaborators.classify(compiled(kinds), Coupling.foldBinary(_, tops(kinds, kindNames*))).map { case (k, v) => k.stripPrefix(s"$kinds.") -> v }

  "a data type" should "be a case class or object, an enum, a sealed data hierarchy, a value class, an exception, a record-like class or an object of pure functions" in {
    Seq("Point", "Origin", "Colour", "Figure", "Circle", "Blank", "Slug", "Units", "Score", "Celsius", "NotFound", "Node")
      .filter(kindReasons.contains) shouldBe empty
  }

  it should "include a pure strategy: an object of pure functions implementing a trait, and so the trait" in {
    Seq("Rule", "EmptyRule").filter(kindReasons.contains) shouldBe empty
  }

  it should "stay data holding data, with a lazy val of its fields, and with a method taking a collaborator (its caller counts that)" in {
    Seq("Booking", "Title", "Stamp").filter(kindReasons.contains) shouldBe empty
  }

  "a collaborator" should "be a type holding state: a var, a field written after construction, a mutable collection, a clock" in {
    kindReasons("Counter") shouldBe s"state: $kinds.Counter$$.count"
    kindReasons("Memo") should startWith("state:")
    kindReasons("Normalizer") shouldBe s"state: $kinds.Normalizer.last" // TitleNormalizer's memo caches
    kindReasons("YearCap") shouldBe s"state: $kinds.YearCap.clock" // LatestTitleYear's clock
  }

  it should "be a type reading the world, and a case class carrying a closure" in {
    kindReasons("Env") shouldBe s"effect: $kinds.Env$$ calls java.util.UUID#randomUUID"
    kindReasons("Draft") shouldBe s"behaviour: $kinds.Draft.rowsOf is a function" // ProjectionDraft's closures, which read the store
  }

  it should "be an interface: a trait implemented by a collaborator, or by nothing" in {
    kindReasons("Store") shouldBe s"implemented by $kinds.MapStore"
    kindReasons("MapStore") should startWith("state:")
    kindReasons("Port") shouldBe s"an interface: $kinds.Port has no implementation"
  }

  it should "be a type holding a collaborator, reading an object that reads the world, or a hierarchy one of whose implementations is one" in {
    kindReasons("Wiring") should startWith("holds")
    kindReasons("Tickets") shouldBe s"reads the object $kinds.Env$$, which reaches java.util.UUID#randomUUID"
    kindReasons.keySet should not contain "Numbering" // Counter's var is Counter's state, not its reader's
    kindReasons("Source") shouldBe s"implemented by $kinds.Live"
    kindReasons.keySet should not contain "Fixed"
  }

  "a use of a nested type" should "be judged by that type: a nested data type no coupling, a call of its object's own function one" in {
    val nested = s"$fixture.nested"
    // binary name -> named type (a nested one by itself, an object apart from its class: Coupling.Collaborators.unitOf)
    val unitOfBinary = Map(s"$nested.Measures$$" -> "Measures", s"$nested.Measures$$Film" -> "Measures.Film", s"$nested.Measures$$Film$$" -> "Measures.Film",
      s"$nested.FilmCount" -> "FilmCount", s"$nested.Relating" -> "Relating").map { case (b, n) => b -> Coupling.Collaborators.unitOf(b, s"$nested.$n") }
    unitOfBinary(s"$nested.Measures$$Film$$") shouldBe s"$nested.Measures.Film$$"
    val topOf = (u: String) => if (u.contains(".Measures")) s"$nested.Measures" else u.stripSuffix("$")
    val topOfBinary = unitOfBinary.map { case (b, u) => b -> topOf(u) }
    val classes = compiled(nested)
    val reasons = Coupling.Collaborators.classify(classes, Coupling.foldBinary(_, unitOfBinary))
    reasons(s"$nested.Measures$$") should startWith("state:")
    reasons.keySet.filter(_.contains("Film")) shouldBe empty // a lazily computed reading of its own fields is no state
    val uses = classes.flatMap(c => Coupling.foldBinary(c.name, topOfBinary).map(_ -> c.uses.flatMap(Coupling.foldBinary(_, unitOfBinary))))
      .groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val (count, relating, measures) = (s"$nested.FilmCount", s"$nested.Relating", s"$nested.Measures")
    val edges = Set(count -> measures, relating -> measures)
    // Zinc records the object too for a use of its nested type: the class files say which was used
    val zinc = Map((count -> measures) -> Set(measures, s"$nested.Measures.Film"), (relating -> measures) -> Set(measures, s"$nested.Measures.Film"))
    uses(count) should contain(s"$nested.Measures.Film")
    uses(count) should not contain s"$nested.Measures$$"
    Coupling.Collaborators.coupledEdges(edges, uses, zinc, topOf, reasons.contains) shouldBe Set(relating -> measures)
    // a use no class file shows (an inline method) falls back to Zinc's record, then to the type itself
    Coupling.Collaborators.coupledEdges(edges, Map.empty, zinc, topOf, reasons.contains) shouldBe edges
    Coupling.Collaborators.coupledEdges(edges, Map.empty, Map((count -> measures) -> Set(s"$nested.Measures.Film")), topOf, reasons.contains) shouldBe
      Set(relating -> measures)
  }

  "a segregated interface" should "cost nothing: a collaborator and an interface it implements are ONE dependency, a type's own supertypes none" in {
    // TitleNormalizer implements TitleForms; Resolver holds a normalizer and hands it on where TitleForms is expected
    val (normalizer, forms, resolver, cache) = ("services.titles.TitleNormalizer", "services.titles.TitleForms",
      "services.identity.Resolver", "services.movies.MovieCache")
    val supertypes = Coupling.supertypeClosure(Map(normalizer -> Set(forms), forms -> Set.empty))
    val g = Graph(Seq(t(normalizer), t(forms, isAbstract = true), t(resolver), t(cache, isAbstract = true)),
      Set(resolver -> normalizer, resolver -> forms, resolver -> cache, normalizer -> forms), supertypes = supertypes)
    Coupling.heldFanOut(g, Coupling.Exemptions.empty)(resolver) shouldBe 2 // the normalizer (as its forms too), the cache
    Coupling.heldFanOut(g, Coupling.Exemptions.empty)(normalizer) shouldBe 0 // its own interface is no collaborator
    // a class using the interface alone still counts it
    Coupling.withoutSupertypes(resolver, Set(forms, cache), supertypes) shouldBe Set(forms, cache)
    Coupling.supertypeClosure(Map("a.C" -> Set("a.B"), "a.B" -> Set("a.A"))) shouldBe Map("a.C" -> Set("a.B", "a.A"), "a.B" -> Set("a.A"))
  }

  "fan-out" should "count only the collaborators a class uses, a data type none" in {
    val g = Graph(Seq(t("services.a.User"), t("services.b.Store", isAbstract = true), t("models.Film"), t("models.Cinema")),
      Set("services.a.User" -> "services.b.Store", "services.a.User" -> "models.Film", "services.a.User" -> "models.Cinema",
        "models.Film" -> "models.Cinema"), data = Set("models.Film", "models.Cinema"))
    Coupling.fans(g)("services.a.User") shouldBe ((0, 1))
    Coupling.fans(g)("models.Film") shouldBe ((0, 0))
    Coupling.heldTargets(g, Coupling.Exemptions.empty)("services.a.User") shouldBe Set("services.b.Store")
    Coupling.packageEdges(g).keySet shouldBe Set("services.a" -> "services.b", "services.a" -> "models") // still a dependency
    Coupling.collaboratorRows(g.copy(collaboratorWhy = Map("services.b.Store" -> "an interface"))) should contain allOf (
      "models.Film\tdata\t", "services.b.Store\tcollaborator\tan interface")
  }

  "a descriptor or a generic signature" should "name its classes, not its type variables or primitives" in {
    ClassReferences.Signatures.types("(ILa/B;[[Lc/D;)Le/F;") shouldBe Seq("a/B", "c/D", "e/F")
    ClassReferences.Signatures.types("<T:Ljava/lang/Object;Lx:La/Bound;>(TT;Ljava/util/List<+La/Item;>;)TLx;") shouldBe
      Seq("java/lang/Object", "a/Bound", "java/util/List", "a/Item")
    ClassReferences.Signatures.types("La/Outer<TT;>.Inner<La/Arg;>;") shouldBe Seq("a/Outer", "a/Outer$Inner", "a/Arg")
  }

  "the checked-in baseline" should "be the rows' shape, one row a key, its limits the build's" in {
    val lines = Files.readAllLines(RepoRoot.file("test/resources/coupling-baseline.tsv").toPath).asScala.toSeq
    lines.take(Coupling.BaselineHeader.size) shouldBe Coupling.BaselineHeader
    val rows = lines.filterNot(l => l.startsWith("#") || l.trim.isEmpty).map(_.split("\t", -1).toSeq)
    all(rows.map(_.size)) shouldBe 3
    rows.map(_.head).distinct.toSet should contain allElementsOf Set("limit", "module-edge", "ce")
    rows.map(_.head).toSet -- Set("limit", "module-edge", "ce", "fanout", "distance", "distance-cap") shouldBe empty // no cycle-edge: the graph has none
    all(rows.map(_(2))) should fullyMatch regex "[0-9]+"
    rows.map(r => (r(0), r(1))).distinct.size shouldBe rows.size
    // every frozen distance row carries its ceiling (scripts/ci/coupling_ratchet.py, THE CEILING), at most 0.02 above the
    // row; a row a re-measure took again (`limit distance-measure`) may sit above its kept ceiling, until it falls back under
    val distance = rows.collect { case Seq("distance", pkg, d) => pkg -> d.toInt }.toMap
    val caps = rows.collect { case Seq("distance-cap", pkg, c) => pkg -> c.toInt }.toMap
    caps.keySet shouldBe distance.keySet
    all(distance.map { case (pkg, d) => caps(pkg) - d }) should be <= 2
    rows.collect { case Seq("limit", key, value) => key -> value.toInt }.toMap shouldBe
      Map("fanout" -> Coupling.DefaultFanOutLimit, "fanout-measure" -> Coupling.FanOutMeasure, "new-package-ce" -> Coupling.DefaultNewPackageCeLimit,
        "distance" -> Coupling.DefaultDistanceLimit, "distance-min-types" -> Coupling.DefaultDistanceMinTypes,
        "distance-measure" -> Coupling.PackageMeasure)
  }
}
