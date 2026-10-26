package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{MongoCollections, ResolveRole}

import java.nio.file.{Path, Paths}

import ScalaSourceScan.{argumentsAt, codeOf, scalaFiles, topLevelParts}

/**
 * Every input the identity path reads at run time is in the dependency key of what read it, or compiled into a rules
 * version (docs/design/identity-no-drift.md: state is persisted inputs, or caches keyed by their FULL dependency set).
 * A stored family or agreement verdict stands while its rules version and its receipt or slice digest stand; an input
 * in neither changes under it and leaves it stale. Each rule is derived from the code, so a new input fails here until
 * it says how it is keyed:
 *
 *  1. Collections. Every collection the identity path opens — its sources, the worker's identity adapters, the wiring
 *     that builds the model and the stage, and the stores those name — carries a [[ResolveRole]] in the registry
 *     ([[MongoCollections]]): an input and the key it is in, or the path's own state and why it decides nothing.
 *  2. Resources. Every resource a source in a rules closure names is digested by that closure: a checked-in table or
 *     weights file read at run time moves the rules version when it changes.
 *  3. Configuration. Every value the composition root hands the decision makers (the agreement stage, the model) is
 *     either in their version (`AgreementStage.decidedUnder`, `ResolveInputs.rulesVersion`), read into a receipt or
 *     digest, or decides nothing (asking, budgets, metrics); a versioned one must be named where the version is built.
 *
 * Runtime lookups that bypass constructor parameters (reflection, the environment) are banned outright by
 * `NoRuntimeLookupsInIdentitySpec`.
 */
class IdentityRuntimeInputsSpec extends AnyFlatSpec with Matchers {

  // ── 1. collections ─────────────────────────────────────────────────────────

  private val IdentityRoots = Seq("identity/src/main", "worker/src/main/scala/services/identity")
  private val Wiring        = "worker/src/main/scala/modules"

  /** The wiring that builds the identity path: all of the identity projection's, and the worker's definitions that build
   *  the model and its lookups. */
  private val IdentityWiring     = Paths.get(s"$Wiring/wiring/IdentityProjectionWiring.scala")
  private val WorkerWiring       = Paths.get(s"$Wiring/WorkerWiring.scala")
  private val WorkerIdentityDefs = Seq("identityModel", "storedLookups", "cutoverLookups", "venueDetailLookups", "identityQuestionMemory",
    "identityTmdbLayer", "venuePageIndex", "newVenuePageStore")

  private val Definition =
    """(?m)^([ \t]*)(?:@\w+\s+)*(?:override\s+)?(?:(?:protected|private)(?:\[\w+\])?\s+)?(?:lazy\s+val|def|val|var)\s+(\w+)""".r

  /** Each definition in `source`: its name and its text, up to the next definition no deeper than it. */
  private def definitions(source: String): Seq[(String, String)] = {
    val found = Definition.findAllMatchIn(source).toVector
    found.zipWithIndex.map { case (m, i) =>
      val depth = m.group(1).length
      val end   = found.drop(i + 1).find(_.group(1).length <= depth).fold(source.length)(_.start)
      m.group(2) -> source.substring(m.start, end)
    }
  }

  /** The wiring's values, by name, that build a Mongo store: what an identity definition naming one opens. */
  private lazy val wiredStores: Map[String, Seq[String]] =
    scalaFiles(Seq(Wiring)).flatMap(p => definitions(codeOf(p))).filter(_._2.contains("new Mongo"))
      .groupBy(_._1).map { case (name, defs) => name -> defs.map(_._2) }

  /** The text of the identity path's code: its sources, and the wiring's identity definitions with the stores they name. */
  private lazy val identityCode: Seq[String] = {
    val worker = definitions(codeOf(WorkerWiring)).toMap
    val missing = WorkerIdentityDefs.filterNot(worker.contains)
    require(missing.isEmpty, s"WorkerWiring defines none of ${missing.mkString(", ")}: name the definitions that build the identity path")
    val wiring = codeOf(IdentityWiring) +: WorkerIdentityDefs.map(worker)
    val named  = wiring.flatMap(text => """\b([a-z]\w*)\b""".r.findAllMatchIn(text).map(_.group(1))).distinct
    scalaFiles(IdentityRoots).map(codeOf) ++ wiring ++ named.flatMap(wiredStores.getOrElse(_, Nil))
  }

  private lazy val opened: Set[String] = CollectionNames.openedIn(identityCode)

  "The identity path's collections" should "be found by the scan, the stores the wiring hands it included" in {
    opened should contain allOf ("identity_listings", "cinema_scrapes", "identity_pins", "tmdb_films", "identity_family_answers",
      "identity_agreements", "identity_model_families")
  }

  they should "each be registered as an input with the key it is in, or as the path's own state with why it decides nothing" in {
    val registered   = MongoCollections.all.map(c => c.name -> c).toMap
    val unregistered = opened.filterNot(registered.contains)
    val unkeyed      = opened.filter(registered.contains).filter(name => registered(name).resolve.isEmpty)
    withClue(s"Opened by the identity path but not in services.MongoCollections: ${unregistered.toSeq.sorted.mkString(", ")}\n") {
      unregistered shouldBe empty
    }
    withClue(
      "Opened by the identity path with no ResolveRole. Give its entry `.resolveInput(key)` — the receipt, slice digest or " +
        "rules version its content is in, so a filing of it reopens what read it (make it so if it is in none) — or " +
        s"`.resolveState(why)`, why it decides nothing a whole resolve would not: ${unkeyed.toSeq.sorted.mkString(", ")}\n") {
      unkeyed shouldBe empty
    }
  }

  they should "be the only collections with a ResolveRole, so none is kept for a collection the path no longer opens" in {
    val stale = MongoCollections.all.filter(_.resolve.isDefined).map(_.name).filterNot(opened)
    withClue(s"A ResolveRole on a collection the identity path does not open: drop it: ${stale.sorted.mkString(", ")}\n") {
      stale shouldBe empty
    }
  }

  "The registry's resolve inputs" should "each name the key they are in" in {
    MongoCollections.all.flatMap(c => c.resolve.collect { case ResolveRole.Input(key, _) if key.trim.isEmpty => c.name }) shouldBe empty
    MongoCollections.all.flatMap(c => c.resolve.collect { case ResolveRole.State(why) if why.trim.isEmpty => c.name }) shouldBe empty
  }

  // ── 2. resources ───────────────────────────────────────────────────────────

  /** Every copy of `resource` on the classpath. */
  private def copies(resource: String): Seq[java.net.URL] =
    scala.jdk.CollectionConverters.EnumerationHasAsScala(getClass.getClassLoader.getResources(resource)).asScala.toSeq

  private def closureOf(name: String): Seq[String] =
    copies(s"${services.identity.ClasspathResource.Stamps}/$name-sources.txt").headOption.fold(Seq.empty[String])(url =>
      scala.util.Using.resource(scala.io.Source.fromURL(url, "UTF-8"))(_.getLines().toVector))

  // A stamp left in another module's output by an older build (common generated them until the resolver moved into its
  // own module) shadows the build's own wherever that module comes first on the classpath: the rules version a worker
  // reads stops moving, and no stored family or verdict is decided again when the rules change.
  "Every rules version stamp" should "be on the classpath once, so no stale copy can shadow the build's own" in {
    val stamps = Seq("identity-rules", "agreement-rules", "venue-slot").flatMap(name => Seq(s"$name-version.txt", s"$name-sources.txt"))
      .map(stamp => s"${services.identity.ClasspathResource.Stamps}/$stamp")
    val twice  = stamps.map(stamp => stamp -> copies(stamp)).filter(_._2.size != 1)
    withClue(s"Not exactly one copy on the classpath (a stale build output? clean that module):\n${twice.map { case (stamp, urls) =>
      s"$stamp: ${urls.mkString(", ")}" }.mkString("\n")}\n") {
      twice shouldBe empty
    }
  }

  private lazy val closures: Map[String, Seq[String]] = Seq("identity-rules", "agreement-rules").map(n => n -> closureOf(n)).toMap

  private val ResourceName = """"/?([\w.-]+(?:/[\w.-]+)*\.(?:json|tsv|txt|csv))"""".r
  /** The build's own stamps, generated beside the closure (`build.sbt`): the version, and the paths it digests. */
  private val Generated    = """[\w-]+-(?:version|sources)\.txt""".r

  "Every resource a rules closure's source names" should "be digested by that closure" in {
    closures.values.foreach(_ should not be empty)
    val undigested = closures.toSeq.flatMap { case (closure, paths) =>
      val resources = paths.filter(_.contains("/src/main/resources/")).map(_.replaceFirst("^.*?/src/main/resources/", "")).toSet
      // a source outside the repository's module trees (a dependency's, recorded by Zinc as it found it) is not ours to read
      paths.filter(p => p.endsWith(".scala") && java.nio.file.Files.exists(Paths.get(p))).flatMap { source =>
        ResourceName.findAllMatchIn(codeOf(Paths.get(source))).map(_.group(1))
          .filterNot(name => Generated.matches(name) || resources(name))
          .map(name => s"$closure: $source names $name")
      }
    }.distinct
    withClue(
      "A source the rules version digests names a resource the version does not digest: the resolver reads it at run " +
        "time and no stored family or verdict reopens when it changes. Move it under identity/, common/ or geodata/'s " +
        s"src/main/resources:\n${undigested.mkString("\n")}\n") {
      undigested shouldBe empty
    }
  }

  // ── 3. configuration ───────────────────────────────────────────────────────

  private def parameters(path: String, header: String): Seq[String] = {
    val source = codeOf(Paths.get(path))
    val at     = source.indexOf(header)
    require(at >= 0, s"$path declares no `$header`")
    topLevelParts(argumentsAt(source, source.indexOf('(', at))).map(_.trim).filter(_.nonEmpty)
      .map(p => """(?:(?:private\[\w+\]|val|var|override)\s+)*(\w+)\s*:""".r.findFirstMatchIn(p).get.group(1))
  }

  private def definitionOf(path: String, name: String): String =
    definitions(codeOf(Paths.get(path))).collectFirst { case (`name`, text) => text }
      .getOrElse(throw new NoSuchElementException(s"$path defines no $name"))

  private val Stage = "identity/src/main/scala/services/identity/agreement/AgreementStage.scala"

  /** Each of the agreement stage's parameters, and how it can move a verdict. */
  private val StageParameters: Map[String, StageParameter] = {
    import StageParameter.*
    Map(
      "families"    -> Versioned, "identities" -> Versioned, "listedOn" -> Versioned, "calibration" -> Versioned, "rules" -> Versioned,
      "venues"      -> Read("each listing's venue detail is in its listings' digest (digestOf)"),
      "normalizer"  -> Read("compiled: TitleNormalizer and its title rules are in the closure, the country's being its database's"),
      "tmdbOf"      -> Read("each agreed IMDb id's find is in the receipt"),
      "tmdb"        -> Read("every TMDB read the fill makes is in the receipt (Noting)"),
      "posters"     -> Read("every poster hash read is in the receipt"),
      "catalogue"   -> Read("every catalogue answer read is in the receipt"),
      "attempted"   -> Read("the question memory: each attempt settled on is in the receipt (tried|...)"),
      "clock"       -> Read("the year it reads is in the receipt (YearRead)"),
      "stored"      -> Read("the verdicts: the cache this key keeps"),
      "changes"     -> Inert("which answers were filed: when to read a receipt again, never what it decides"),
      "attemptsMarked" -> Inert("how many attempts were marked: when to read the receipts again"),
      "ask"         -> Inert("asking"), "askNow" -> Inert("asking, inline"), "metrics" -> Inert("metrics"),
      "staging"     -> Inert("the staging screen"), "keyStore" -> Inert("names: no verdict is found by its key"),
      "correctionPostersAtOnce" -> Inert("a budget: delays a decision, never moves its result (no-drift item 21)"),
      "correctionsPerApply"     -> Inert("a budget: delays a decision, never moves its result (no-drift item 21)"),
      "backfillPerApply"        -> Inert("a budget: delays a decision, never moves its result"))
  }

  "The agreement stage's parameters" should "each say how they can move a verdict" in {
    val declared = parameters(Stage, "final class AgreementStage")
    withClue("AgreementStage takes a parameter this spec does not classify (or no longer takes one it does): say whether " +
      "it is in decidedUnder (Versioned), read into the receipt or listings' digest (Read), or decides nothing (Inert)\n") {
      declared.toSet shouldBe StageParameters.keySet
    }
  }

  they should "be named where decidedUnder is built, each one versioned" in {
    val version   = definitionOf(Stage, "decidedUnder")
    val unnamed   = StageParameters.collect { case (name, StageParameter.Versioned) if !s"\\b$name\\b".r.findFirstIn(version).isDefined => name }
    withClue(s"Versioned, yet decidedUnder does not read: ${unnamed.toSeq.sorted.mkString(", ")}\n") { unnamed shouldBe empty }
  }

  private val Inputs = "identity/src/main/scala/services/identity/ResolveInputs.scala"

  "The model's resolve inputs" should "each be in the rules version its families are kept under, which the inputs build themselves" in {
    val fields  = parameters(Inputs, "final case class ResolveInputs")
    val version = definitionOf(Inputs, "rulesVersion")
    fields.filterNot(field => s"\\b$field\\b".r.findFirstIn(version).isDefined) shouldBe empty
    definitionOf(Inputs, "incremental") should include ("rules = rulesVersion(code)")
  }
}

/** How a decision maker's parameter can move what it decides. */
enum StageParameter {
  /** In its version: a change decides every stored verdict again. */
  case Versioned
  /** Read into the receipt or the digest a stored verdict stands by: `how`. */
  case Read(how: String)
  /** Decides nothing: `why`. */
  case Inert(why: String)
}
