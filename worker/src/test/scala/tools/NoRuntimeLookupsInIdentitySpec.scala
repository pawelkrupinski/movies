package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/**
 * Identity code reads no input the rules versions cannot see: no reflection, no class loaded by name, no environment
 * variable or system property, no file, and no classpath resource but through the one seam that reads the checked-in
 * artefacts ([[services.identity.ClasspathResource]], whose every resource the rules closure digests:
 * `IdentityRuntimeInputsSpec`).
 *
 * Why (docs/design/identity-no-drift.md): a stored family or agreement verdict stands while its rules version and its
 * receipt stand. The versions are digests of the compiled closure (`IdentityRulesSources`); a decision that reads a
 * setting, an environment variable or a class picked by name at run time moves neither when that input changes, so the
 * stored state goes stale — exactly what the principle forbids. Such an input comes in through the composition root as
 * a constructor parameter, where `IdentityRuntimeInputsSpec` holds it to a version or a receipt.
 *
 * Scope: the identity module's main sources and the worker's identity adapters. Each allow-list entry names why its
 * read cannot reach a decision; an entry whose file no longer reads it fails as stale.
 */
class NoRuntimeLookupsInIdentitySpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{code, read, scalaFiles}

  private val Roots = Seq("identity/src/main", "worker/src/main/scala/services/identity")

  /** A runtime lookup, by what it reads. */
  private val Lookups: Seq[(String, scala.util.matching.Regex)] = Seq(
    "reflection"            -> """\bjava\.lang\.reflect\b|\bscala\.reflect\.runtime\b|\.getDeclared(?:Method|Field|Constructor)s?\(|\.get(?:Method|Field|Constructor)s?\(""".r,
    "a class loaded by name" -> """\bClass\.forName\(|\bServiceLoader\b|\.loadClass\(""".r,
    "the environment"       -> """\bsys\.(?:env|props)\b|\bSystem\.(?:getenv|getProperty|getProperties)\b""".r,
    "a classpath resource"  -> """\.getResource(?:AsStream)?\(|\bSource\.fromResource\(""".r,
    "a file"                -> """\bSource\.fromFile\(|\bFiles\.(?:read\w*|lines|newBufferedReader|newInputStream)\(|\bnew\s+(?:java\.io\.)?File(?:InputStream|Reader)?\(""".r,
    "an identity hash"      -> """\bSystem\.identityHashCode\(""".r)

  /** file -> why its lookup cannot reach a decision. Shrink it by moving the read behind a constructor parameter. */
  private val Allowlist: Map[String, String] = Map(
    "identity/src/main/scala/services/identity/ClasspathResource.scala" ->
      ("the one reader of the checked-in artefacts (weights, decorations, tables, the version stamps); every resource it " +
        "is handed is in a rules closure (IdentityRuntimeInputsSpec)"),
    "identity/src/main/scala/services/identity/FamilyScope.scala" ->
      ("TitleInputs hashes one resolve's shared decorations instance by identity, an in-process memo key whose equals " +
        "compares the same instance (eq): never persisted, never in a version or a receipt"))

  private[tools] def lookupsIn(line: String): Seq[String] =
    Lookups.collect { case (what, pattern) if pattern.findFirstIn(code(line)).isDefined => what }

  private lazy val files: Seq[Path] = scalaFiles(Roots)

  "the lookup matcher" should "name each kind of runtime lookup, and not a comment naming one" in {
    lookupsIn("""val k = Class.forName("services.identity.X")""") shouldBe Seq("a class loaded by name")
    lookupsIn("""val m = classOf[X].getDeclaredMethod("f")""") shouldBe Seq("reflection")
    lookupsIn("""val loaders = ServiceLoader.load(classOf[Rule])""") shouldBe Seq("a class loaded by name")
    lookupsIn("""val on = sys.env.get("IDENTITY_FILL").contains("1")""") shouldBe Seq("the environment")
    lookupsIn("""val on = System.getenv("IDENTITY_FILL")""") shouldBe Seq("the environment")
    lookupsIn("""val n = sys.props.getOrElse("identity.batch", "500")""") shouldBe Seq("the environment")
    lookupsIn("""val in = getClass.getResourceAsStream("/weights.json")""") shouldBe Seq("a classpath resource")
    lookupsIn("""val rows = Source.fromFile("titles.tsv").getLines()""") shouldBe Seq("a file")
    lookupsIn("""val bytes = Files.readAllBytes(path)""") shouldBe Seq("a file")
    lookupsIn("""val h = System.identityHashCode(x)""") shouldBe Seq("an identity hash")
    lookupsIn("""  // Class.forName and sys.env are banned here""") shouldBe empty
    lookupsIn("""val name = getClass.getSimpleName""") shouldBe empty
  }

  "Identity code" should "make no runtime lookup outside the allow-list" in {
    files.nonEmpty shouldBe true
    val offenders = files.filterNot(p => Allowlist.contains(p.toString)).flatMap { path =>
      read(path).linesIterator.zipWithIndex.flatMap { case (line, index) =>
        lookupsIn(line).map(what => s"$path:${index + 1}: $what: ${line.trim}")
      }
    }
    withClue(
      "These identity lines read an input no rules version or receipt sees. Hand the value in as a constructor " +
        "parameter from the composition root (and hold it to a version or a receipt), or allow-list the file with " +
        "why it cannot reach a decision:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "keep every allow-list entry pointing at a file that still makes such a lookup" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot { file =>
      val path = Paths.get(file)
      Files.exists(path) && read(path).linesIterator.exists(line => lookupsIn(line).nonEmpty)
    }
    withClue("Allow-listed but no longer making a runtime lookup: drop the entry:\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }
}
