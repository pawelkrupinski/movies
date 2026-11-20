package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

/**
 * EACH TIER'S PUBLISH GATE COVERS EVERYTHING ITS IMAGE IS BUILT FROM.
 *
 * A tier's image is published (and so deployed) only when its `changed-<tier>` filter in `main.yml` matched a path
 * changed since the tier's `deployed-<tier>` marker ([[K8sTierPathGatingSpec]]). The filter is hand-kept, the image is
 * not: `test/resources/fixtures/identity-judgements.tsv`, bundled into the worker as a resource, was missing from the
 * worker's filter, so an edit to it reached prod only with the next unrelated worker deploy. A path the image holds that
 * its filter misses ships late, silently.
 *
 * The image's inputs come from the build (`project/ImageInputs.scala`: every main source and resource of the
 * deployable's compile closure, the repository files its build bundles, the build definition and the Dockerfile),
 * staged as `image-inputs/<tier>.txt` among these specs' resources. The filter is the real one out of `main.yml`, read
 * with the matcher's semantics ([[ChangedPathsFilter]]; [[K8sTierPathGatingIntegrationSpec]] proves them the same as
 * the action's `matches.sh` over these same paths).
 */
class ImagePathGateSpec extends AnyFlatSpec with Matchers {

  Seq("worker" -> K8sTierPathGatingSpec.workerFilter, "web" -> K8sTierPathGatingSpec.webFilter).foreach {
    case (tier, filter) =>
      s"the $tier tier's publish gate" should "match every path its image is built from" in {
        val missed = ImagePathGateSpec.imageInputs(tier).filterNot(ChangedPathsFilter(filter).matches)
        withClue(s"main.yml's `changed-$tier` filter misses paths the $tier image is built from, so a change to them " +
            s"would not publish it -- add a pattern covering each: ")(missed shouldBe empty)
      }
  }

  "the worker's image inputs" should "hold the resources its build bundles from outside any module" in {
    ImagePathGateSpec.imageInputs("worker") should contain allOf (
      "test/resources/fixtures/identity-judgements.tsv",
      "infra/jvm/worker.options",
      "infra/nix/files/heap-dumps.sh",
      "scrapers/src/main/scala/services/cinemas/CinemaScraperCatalog.scala",
      "Dockerfile",
      "build.sbt")
    ImagePathGateSpec.imageInputs("worker") should not contain "infra/jvm/web.options"
    withClue("a test-only dependency (testkit, itkit) ships nothing: ")(
      ImagePathGateSpec.imageInputs("worker").filter(path => path.startsWith("testkit/") || path.startsWith("itkit/")) shouldBe empty)
    withClue("the web app's code is no worker input: ")(
      ImagePathGateSpec.imageInputs("worker").filter(_.startsWith("web/")) shouldBe empty)
  }

  /** The self-test: the worker's filter as it stood before identity-judgements.tsv was added is caught. */
  "the check" should "catch a filter that misses a bundled resource" in {
    val withoutJudgements = K8sTierPathGatingSpec.workerFilter.filterNot(_.startsWith("test/resources/"))
    ImagePathGateSpec.imageInputs("worker").filterNot(ChangedPathsFilter(withoutJudgements).matches) should
      contain("test/resources/fixtures/identity-judgements.tsv")
  }

  /**
   * A BUILD-ONLY FILE PUBLISHES NOTHING. `project/` is the build definition, and most of it builds the dist -- the
   * dependencies, the resource generators, the plugins -- but some of it only measures or tests the code: the coupling
   * and test-impact reports, the forked-test and classpath guards, `sbt run`'s hooks. A change there ships an image of
   * the same bytes and still restarted every worker into a full identity re-resolve (d7a733703, `project/Coupling.scala`,
   * 2026-11-20). The build names those files ([[ImageInputs]]'s `buildOnly`), so the image inputs leave them out, and
   * both tiers' filters exclude exactly them.
   */
  Seq("worker" -> K8sTierPathGatingSpec.workerFilter, "web" -> K8sTierPathGatingSpec.webFilter).foreach {
    case (tier, filter) =>
      s"the $tier tier's publish gate" should "exclude exactly the build definition the build calls build-only" in {
        val excluded = filter.map(_.trim).filter(_.startsWith("!project/")).map(_.drop(1)).sorted
        withClue(s"main.yml's `changed-$tier` excludes are not the build's build-only files (project/ImageInputs.scala): ")(
          excluded shouldBe ImagePathGateSpec.buildOnly.sorted)
      }

      it should "skip a build-only change and publish an app source, resource or build change" in {
        val gate = ChangedPathsFilter(filter)
        gate.matches("project/Coupling.scala") shouldBe false
        gate.matches("project/TestImpact.scala") shouldBe false
        Seq("common/src/main/scala/models/Movie.scala", "common/src/main/resources/title-rules.tsv",
          "project/Dependencies.scala", "project/plugins.sbt", "project/build.properties", "build.sbt")
          .foreach(path => withClue(s"$path: ")(gate.matches(path) shouldBe true))
      }

      s"the $tier image's build" should "read no build-only file" in {
        ImagePathGateSpec.imageInputs(tier).filter(ImagePathGateSpec.buildOnly.contains) shouldBe empty
        val stageDefinitions = ImagePathGateSpec.stageDefinitions(tier)
        withClue("the staged dist's task graph holds no setting (an empty list means the build staged nothing): ")(
          stageDefinitions should not be empty)
        withClue(s"a setting the $tier dist is staged from is defined in a build-only file -- it builds the image: ")(
          stageDefinitions.map(_.path).filter(ImagePathGateSpec.buildOnly.contains).distinct shouldBe empty)
        val calls = stageDefinitions.filterNot(d => d.path.startsWith("project/")).flatMap { definition =>
          val lines = BuildCode.setting(RepoFile.read(definition.path).linesIterator.toVector, definition.from, definition.to)
            .map(BuildCode.strip)
          lines.flatMap(line => ImagePathGateSpec.buildOnlyNames.filter(BuildCode.names(line, _)))
            .map(name => s"${definition.path}:${definition.from} calls $name").toSeq
        }
        withClue(s"a setting the $tier dist is staged from calls a build-only file's code: ")(calls.distinct shouldBe empty)
      }
  }

  "the build definition the images are built from" should "call no build-only file's code" in {
    val built = RepoFile.listed("project")(_.getName.endsWith(".scala")).map(file => s"project/${file.getName}")
      .filterNot(ImagePathGateSpec.buildOnly.contains)
    built should contain("project/Dependencies.scala")
    val calls = built.flatMap { path =>
      RepoFile.read(path).linesIterator.map(BuildCode.strip)
        .flatMap(line => ImagePathGateSpec.buildOnlyNames.filter(BuildCode.names(line, _)).map(name => s"$path calls $name")).toSeq
    }
    withClue("an image-building file of project/ uses a build-only one, so a change there can change the image: ")(
      calls.distinct shouldBe empty)
  }

  "the build-only list" should "name only files of project/ that exist" in {
    ImagePathGateSpec.buildOnly should not be empty
    ImagePathGateSpec.buildOnly.foreach { path =>
      withClue(s"$path: ")(path should startWith("project/"))
      withClue(s"$path is gone: ")(java.nio.file.Files.isRegularFile(java.nio.file.Paths.get(path)) shouldBe true)
    }
  }

  "the build-only check" should "catch a build-only name used in code and pass one in a comment" in {
    BuildCode.names(BuildCode.strip("  .settings(ForkedTests.settings(Test))"), "ForkedTests") shouldBe true
    BuildCode.names(BuildCode.strip("  // see ForkedTests"), "ForkedTests") shouldBe false
    BuildCode.names(BuildCode.strip(" * (ForkedTests.scala)"), "ForkedTests") shouldBe false
    BuildCode.names(BuildCode.strip("  MyForkedTestsX.go"), "ForkedTests") shouldBe false
    val build = Vector("lazy val a = (project in file(\"a\"))", "  .settings(", "    Compile / x := {", "      Coupling.go()",
      "    },", "    Compile / y := 1,", "  )")
    BuildCode.setting(build, 3, 3) shouldBe build.slice(2, 4)
    BuildCode.setting(build, 6, 6) shouldBe Vector("    Compile / y := 1,")
  }

  "the filter's reading" should "follow matches.sh: `dir/**` a prefix, anything else exact, any `!` match excluding" in {
    val filter = ChangedPathsFilter(Seq("worker/**", "!worker/src/test/**", "build.sbt", "# a comment", "- 'Dockerfile'"))
    filter.matches("worker/src/main/scala/A.scala") shouldBe true
    filter.matches("worker/src/test/scala/ASpec.scala") shouldBe false
    filter.matches("build.sbt") shouldBe true
    filter.matches("build.sbt.bak") shouldBe false
    filter.matches("Dockerfile") shouldBe true
    filter.matches("workers/x") shouldBe false
  }
}

object ImagePathGateSpec {

  /** The build definition the build calls build-only: no image is built from it (`image-inputs/build-only.txt`). */
  lazy val buildOnly: Vector[String] = resourceLines("build-only")

  /** The top-level names the build-only files define, which no image-building code may use. */
  lazy val buildOnlyNames: Vector[String] = buildOnly.flatMap { path =>
    "(?m)^(?:final |sealed |private |abstract )*(?:object|class|trait|case class) ([A-Za-z0-9_]+)".r
      .findAllMatchIn(RepoFile.read(path)).map(_.group(1))
  }.distinct

  /** Where each setting `tier`'s staged dist is built from is defined: a repository file and its lines
   *  (`image-inputs/<tier>-stage-definitions.txt`, staged by the build). */
  final case class Definition(path: String, from: Int, to: Int)

  def stageDefinitions(tier: String): Vector[Definition] = resourceLines(s"$tier-stage-definitions").map { line =>
    line.split('\t') match {
      case Array(path, from, to) => Definition(path, from.toInt, to.toInt)
      case _                     => throw new IllegalStateException(s"not `<path>\\t<from>\\t<to>`: $line")
    }
  }

  /** The paths the build says `tier`'s image is built from (`image-inputs/<tier>.txt`, staged by the build). */
  def imageInputs(tier: String): Vector[String] = resourceLines(tier)

  private def resourceLines(name: String): Vector[String] = {
    val resource = Option(getClass.getResource(s"/image-inputs/$name.txt"))
      .getOrElse(throw new IllegalStateException(s"no image-inputs/$name.txt among the test resources: the build stages it"))
    val source = Source.fromURL(resource, "UTF-8")
    try source.getLines().filter(_.nonEmpty).toVector
    finally source.close()
  }
}

/** A `changed-paths` filter set, read as `.github/actions/changed-paths/matches.sh` reads it: lines trimmed, a leading
 *  `- ` and surrounding quotes dropped, blank and `#` lines ignored; a pattern ending `/` and two stars matches anything under that directory, anything
 *  else exactly that path; a path matches when an include does and no `!` exclude does. */
final case class ChangedPathsFilter(patterns: Seq[String]) {

  private val normalised = patterns.map(_.trim.replaceFirst("^-\\s*", "").replaceAll("^['\"]|['\"]$", ""))
    .filter(line => line.nonEmpty && !line.startsWith("#"))

  private val (excludes, includes) = normalised.partition(_.startsWith("!")) match {
    case (excluded, included) => (excluded.map(_.drop(1)), included)
  }

  private def under(path: String, pattern: String): Boolean =
    if (pattern.endsWith("/**")) path.startsWith(pattern.stripSuffix("**")) else path == pattern

  def matches(path: String): Boolean = includes.exists(under(path, _)) && !excludes.exists(under(path, _))
}

/** A line of build code as the build-only check reads it: its comment (`//`, or a Scaladoc or block comment line) gone. */
object BuildCode {
  def strip(line: String): String = {
    val trimmed = line.trim
    if (trimmed.startsWith("*") || trimmed.startsWith("/*")) "" else line.replaceFirst("//.*$", "")
  }

  /** The lines of the setting sbt places at lines `from`..`to` (1-based) of a `.sbt` file: sbt records a setting's first
   *  line only, so its body runs on through the lines indented deeper than its first. */
  def setting(lines: Vector[String], from: Int, to: Int): Vector[String] = {
    def indent(line: String) = line.takeWhile(_ == ' ').length
    val depth = indent(lines(from - 1))
    val body  = lines.drop(to).takeWhile(line => line.trim.isEmpty || indent(line) > depth)
    lines.slice(from - 1, to) ++ body
  }

  /** Whether `code` uses the top-level name `name` as a whole word. */
  def names(code: String, name: String): Boolean = s"(?<![A-Za-z0-9_])${java.util.regex.Pattern.quote(name)}(?![A-Za-z0-9_])".r.findFirstIn(code).isDefined
}
