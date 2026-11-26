package tools

import kinowo.build.ReaderNames
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.SourceTrees

import java.nio.file.Files

/** How test code reaches a module's source tree (`<module>/src/...`) other than through a declaration,
 *  `testsupport.SourceTrees("worker/src/main")`.
 *
 *  WHY. The test-impact selection (project/TestImpact.scala) runs a lint walking `worker/src/main` on a change to
 *  `MovieService.scala` only because the lint DECLARES the tree: no class dependency records the walk, and the
 *  selection follows nothing else under a source tree (project/ReaderNames.scala, `declaredTrees`). Spotting path
 *  literals instead missed one new shape every review round (2026-11-16/17: a module in a variable,
 *  `Paths.get(module, "src", "test")`; `Path.of("x").resolve("src")`; `Path.of(".", "identity", "src")`; `File(...)`
 *  without `new`). So every shape that names or builds a source tree is refused here, and a declaration holds string
 *  literals only, which the build reads as they are.
 *
 *  A literal naming ONE file by its whole path (`"worker/src/main/scala/A.scala"`) is no walk: the selection reads it
 *  as that file's reader. */
object SourceTreeWalks {

  enum Rule(val why: String) {
    case UndeclaredTree    extends Rule("names a source tree outside a SourceTrees declaration")
    case TreeFromParts     extends Rule("builds a source path from parts in a path call")
    case InterpolatedPath  extends Rule("builds a source path by interpolation")
    case DeclaredByValue   extends Rule("declares source trees by anything but string literals")
    case RenamedDeclaration extends Rule("renames SourceTrees, which the build reads by its name")
  }

  final case class Offense(rule: Rule, line: Int, literal: String) {
    def show(file: String): String = s"$file:$line \"$literal\" ${rule.why}"
  }

  /** A literal that could be a path: no whitespace, nothing but path and interpolation characters. */
  private val PathLike = """[\w$.{}()*/-]+""".r
  /** A module-rooted source tree or path: `worker/src`, `$module/src/main`, `./identity/src/test/scala`. */
  private val ModuleRooted = """(?:\./)?[\w$.{}*-]+/src(?:/.*)?""".r
  private val SourceSegment = """(?:^|.*/)src(?:/.*)?""".r
  /** A call building or opening a path: what a tree's parts pass through. */
  private val PathCall = """(?:\bPaths\s*\.\s*get|\bPath\s*\.\s*of|\bFile|\bresolve|\bresolveSibling|\bfile|\bFiles\s*\.\s*\w+)\s*\(""".r
  private val Renamed = """\bSourceTrees\s*=>""".r

  def offenses(text: String, rules: Set[Rule] = Rule.values.toSet): Seq[Offense] = {
    val code         = ReaderNames.code(text)
    val declarations = ReaderNames.argumentSpans(code, ReaderNames.Declaration)
    val calls        = ReaderNames.argumentSpans(code, PathCall)
    def within(spans: Seq[(Int, Int)], at: Int) = spans.exists { case (open, close) => open < at && at < close }
    def lineOf(at: Int) = code.substring(0, at).count(_ == '\n') + 1
    val literals = ReaderNames.literalSpans(text).flatMap { case (start, _, literal) =>
      val pathLike = PathLike.matches(literal)
      val lastName = literal.stripSuffix("/").split('/').lastOption.getOrElse("")
      val rule =
        if (!pathLike || within(declarations, start)) None
        else if (ModuleRooted.matches(literal) && (!lastName.contains('.') || literal.contains('*')) && rules(Rule.UndeclaredTree))
          Some(Rule.UndeclaredTree)
        else if (within(calls, start) && SourceSegment.matches(literal) && !(ModuleRooted.matches(literal) && !literal.contains('$')) &&
          rules(Rule.TreeFromParts)) Some(Rule.TreeFromParts)
        else if (literal.contains('$') && SourceSegment.matches(literal) && rules(Rule.InterpolatedPath)) Some(Rule.InterpolatedPath)
        else None
      rule.map(Offense(_, lineOf(start), literal))
    }
    val byValue = declarations.collect {
      case (open, close) if rules(Rule.DeclaredByValue) && code.substring(open + 1, close).exists(c => !c.isWhitespace && c != ',' && c != '"') =>
        Offense(Rule.DeclaredByValue, lineOf(open), text.substring(open + 1, close).trim)
    }
    val renamed = Renamed.findAllMatchIn(code).filter(_ => rules(Rule.RenamedDeclaration)).map(m => Offense(Rule.RenamedDeclaration, lineOf(m.start), "SourceTrees =>"))
    (literals ++ byValue ++ renamed).sortBy(_.line)
  }

  /** Every test-side source: each module's spec, integration, page and fixture trees and the test-support modules. */
  val TestSide: SourceTrees = SourceTrees("*/src/test", "*/src/it", "*/src/page", "*/src/fixtures", "testkit/src/main", "itkit/src/main")
}

class SourceTreeWalksLintSpec extends AnyFlatSpec with Matchers {
  import SourceTreeWalks.{Offense, Rule, offenses}

  /** file -> (WHY its literals naming a source tree are no walk, those literals). Each is TEXT a path is compared
   *  to, or a path in another repository, never one this repository's test code opens. */
  private val AllowedByFile: Map[String, (String, Set[String])] = Map(
    "identity/src/test/scala/services/identity/ReaderNamesSpec.scala" ->
      ("the selection's own spec: the declarations and globs it expects back, compared as text",
        Set("common/src/main", "worker/src/fixtures", "web/src/main/twirl", "*/src/*/scala", "worker/src/main/scala/*.scala")),
    "testkit/src/main/scala/settings/HarnessConfiguration.scala" ->
      ("the share-card generator's default OUTPUT directory: written by an operator's run, read by no test",
        Set("web/src/main/assets/img")),
    "worker/src/it/scala/deploy/ConvergenceBisectSpec.scala" ->
      ("a file committed in the spec's own scratch repository", Set("worker/src/main/Change$n.scala")),
    "worker/src/it/scala/deploy/ConvergenceDispatchGateSpec.scala" ->
      ("a probe path fed to the workflow's path filter, never opened", Set("$dir/src/main/Probe.scala", "$dir/src/fixtures/Probe.scala")),
    "repochecks/src/test/scala/deploy/DiscoveryPullRequestArtefactsSpec.scala" ->
      ("a path the discovery workflow's `git add` names, compared as text", Set("geodata/src/main/resources/generated")),
    "repochecks/src/test/scala/deploy/SharedMachineStateLintSpec.scala" ->
      ("a prefix that classifies a walked path as test code; the walk itself is declared", Set("testkit/src/")),
    "worker/src/test/scala/scripts/IdentityCapture.scala" ->
      ("prefixes a change's paths are matched against (which changes move a decision), nothing read",
        Set("identity/src/main/scala/services/identity/", "identity/src/main/resources/identity-",
          "common/src/main/scala/services/titles/rules/", "identity/src/main/scala/services/identity/agreement/")),
    "worker/src/test/scala/tools/NoDecisionKeyedByMutableIdSpec.scala" ->
      ("prefixes of the allowlist's field ids (`<path>#<field>`); the walk is the declared Roots",
        Set("identity/src/main/scala/services/identity/", "worker/src/main/scala/services/identityadapters/",
          "worker/src/main/scala/services/tmdb/", "worker/src/main/scala/services/agreementasks/", "worker/src/main/scala/services/identitystaging/", "worker/src/main/scala/services/lookupfill/", "worker/src/main/scala/services/identityvenuepages/", "worker/src/main/scala/services/identityprojection/", "worker/src/main/scala/services/identitymodel/")),
    "worker/src/test/scala/tools/NoSwallowedFailureSpec.scala" ->
      ("prefixes a scanned file is matched against (which code reads an upstream); the walk is ScalaSourceScan's MainRoots",
        Set("scrapers/src/main/scala/services/cinemas/", "worker/src/main/scala/services/enrichment/", "common/src/main/scala/tools/",
          "scrapers/src/main/scala/tools/")),
  )
  private val Allowed: Map[(String, String), String] =
    AllowedByFile.toSeq.flatMap { case (file, (why, literals)) => literals.map(file -> _ -> why) }.toMap

  private def rulesOf(text: String, rules: Set[Rule] = Rule.values.toSet) = offenses(text, rules).map(_.rule)

  // every shape a review round found the literal-spotting miss, planted
  "the lint" should "refuse a module's tree passed in a variable" in {
    rulesOf("""val dir = Paths.get(module, "src", if (it) "it" else "test")""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""modules.filter(module => Paths.get(module.getName, "src", "main").toFile.isDirectory)""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""modules.map(module => s"$module/src/main")""") shouldBe Seq(Rule.UndeclaredTree)
  }

  it should "refuse a tree resolved, or listed from the root, in parts" in {
    rulesOf("""Files.walk(Path.of("x").resolve("src"))""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""Files.list(Path.of(".")).map(_.resolve("src/main"))""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""Files.walk(Path.of(".", "identity", "src"))""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""Files.walk(Paths.get(module + "/src/test"))""") shouldBe Seq(Rule.TreeFromParts)
  }

  it should "refuse a File, with or without new, over a source tree" in {
    rulesOf("""val src = File("identity", "src/main")""") shouldBe Seq(Rule.TreeFromParts)
    rulesOf("""val src = new java.io.File("identity/src/main/scala")""") shouldBe Seq(Rule.UndeclaredTree)
    rulesOf("""val src = File("identity/src/main/scala")""") shouldBe Seq(Rule.UndeclaredTree)
  }

  it should "refuse a tree held in a value, a glob, and a file path built by interpolation" in {
    rulesOf("""private val Roots = Seq("common/src/main", "worker/src/fixtures")""") shouldBe Seq(Rule.UndeclaredTree, Rule.UndeclaredTree)
    rulesOf("""val all = "*/src/*/scala";""") shouldBe Seq(Rule.UndeclaredTree)
    rulesOf("""val one = s"identity/src/main/scala/services/identity/$name.scala";""") shouldBe Seq(Rule.InterpolatedPath)
  }

  it should "refuse a declaration by value, and a renamed one" in {
    rulesOf("""SourceTrees(roots*)""") shouldBe Seq(Rule.DeclaredByValue)
    rulesOf("""SourceTrees(s"$module/src/main")""") shouldBe Seq(Rule.DeclaredByValue)
    rulesOf("""import testsupport.{SourceTrees => Trees}""") shouldBe Seq(Rule.RenamedDeclaration)
  }

  it should "pass a declaration, a whole file path, and text that is no path" in {
    rulesOf("""val Main = SourceTrees("worker/src/main", "*/src/*/scala")""") shouldBe empty
    rulesOf("""SourceTrees.apply("worker/src/test")""") shouldBe empty
    rulesOf("""Files.readString(Path.of("worker/src/main/scala/services/corpus/MovieService.scala"))""") shouldBe empty
    rulesOf("""element.attr("src") shouldBe "/poster.jpg";""") shouldBe empty
    rulesOf("""fail("belongs in an it tree (or web/src/page)")""") shouldBe empty
    rulesOf("""// Paths.get(module, "src")""") shouldBe empty
    rulesOf("val snippet = \"\"\"Paths.get(module, \"src\")\"\"\"; ") shouldBe empty
  }

  // the mutation proof: each rule catches a shape no other rule does, so dropping any one lets a walk through
  "every rule" should "be the only one catching its planted shape" in {
    val planted = Map(
      Rule.UndeclaredTree     -> """private val Roots = Seq("worker/src/main")""",
      Rule.TreeFromParts      -> """Files.walk(Paths.get(module, "src"))""",
      Rule.InterpolatedPath   -> """val one = s"$module/src/main/A.scala";""",
      Rule.DeclaredByValue    -> """SourceTrees(roots*)""",
      Rule.RenamedDeclaration -> """import testsupport.{SourceTrees => Trees}""")
    planted.keySet shouldBe Rule.values.toSet
    planted.foreach { case (rule, shape) =>
      withClue(s"$rule: ")(rulesOf(shape) shouldBe Seq(rule))
      withClue(s"without $rule: ")(rulesOf(shape, Rule.values.toSet - rule) shouldBe empty)
    }
  }

  /** This spec: its allowlist and examples quote the very shapes it refuses; its own walk is declared. */
  private val ThisSpec = "common/src/test/scala/tools/SourceTreeWalksLintSpec.scala"

  private lazy val found: Seq[(String, Offense)] = SourceTreeWalks.TestSide.scalaFiles.filterNot(_.toString == ThisSpec).flatMap { file =>
    offenses(Files.readString(file)).map(file.toString -> _)
  }

  "test code" should "reach a module's source tree only through a SourceTrees declaration" in {
    SourceTreeWalks.TestSide.missing shouldBe empty
    SourceTreeWalks.TestSide.scalaFiles.size should be > 1000
    val offending = found.filterNot { case (file, offense) => Allowed.contains(file -> offense.literal) }
    withClue("declare the trees a test walks: testsupport.SourceTrees(\"<module>/src/<config>\") (string literals only)\n")(
      offending.map { case (file, offense) => offense.show(file) } shouldBe empty)
  }

  "the allowlist" should "hold only literals the lint still finds, each with its why" in {
    Allowed.foreach { case (_, why) => why.trim should not be empty }
    (Allowed.keySet -- found.map { case (file, offense) => file -> offense.literal }.toSet) shouldBe empty
  }

  // the repository's own sources, as the build scans them (TestImpact: every module's `src/`)
  private lazy val sources: Seq[ReaderNames.Source] =
    (SourceTrees("*/src").scalaFiles ++ SourceTrees("*/src").files(".java")).map(f => ReaderNames.source(f.toString, Files.readString(f)))

  private def readersOf(path: String): Seq[String] = ReaderNames.walkers(path, sources)

  // review rounds 5-8: each found a walker the literal-spotting missed (a module in a variable, `.resolve("src")`, ...)
  "a change in a module" should "select every source declaring a tree that holds it" in {
    readersOf("common/src/main/scala/clients/TmdbJson.scala") should contain allOf (
      "common/src/test/scala/testsupport/ModuleSources.scala", "worker/src/test/scala/tools/ScalaSourceScan.scala")
    readersOf("scrapers/src/main/scala/services/cinemas/pl/HeliosClient.scala") should contain allOf (
      "worker/src/test/scala/tools/ScalaSourceScan.scala", "scrapers/src/test/scala/services/cinemas/NoWallClockInClientsSpec.scala",
      "worker/src/test/scala/tools/ListingCompletenessSpec.scala", "common/src/test/scala/testsupport/ModuleSources.scala")
    readersOf("worker/src/main/scala/services/corpus/MovieService.scala") should contain (
      "worker/src/test/scala/services/evidence/ListingConstraintsRoutingSpec.scala")
    readersOf("identity/src/main/scala/services/identity/Authority.scala") should contain (
      "identity/src/test/scala/services/identity/ProvenanceSpec.scala")
    readersOf("repochecks/src/test/scala/deploy/RepoFileSpec.scala") should contain (
      "repochecks/src/test/scala/deploy/StressAndEmulatorScheduleSpec.scala")
  }

  it should "select every declarer in the repository, for a file under each tree it declares" in {
    val files = SourceTrees("*/src").files("").map(_.toString)
    val declarers = sources.filter(_.declared.nonEmpty)
    declarers.size should be > 50
    val unselected = for {
      declarer <- declarers
      tree     <- declarer.declared
      file     <- files.find(ReaderNames.covers(tree, _)).toSeq
      if !readersOf(file).contains(declarer.path)
    } yield s"${declarer.path}: $tree ($file)"
    unselected shouldBe empty
  }
}
