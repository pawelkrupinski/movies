package kinowo.build

/**
 * COUPLING, MEASURED FROM THE COMPILER'S RECORD (`sbt couplingReport`, scripts/coupling-report): the production code's
 * class graph -- every top-level class of a module's `src/main`, and the class->class dependencies Zinc records between
 * them (a name used, a type inferred, a parent, an implicit; `project/CouplingAnalysis.scala` reads them) -- folded into
 * the numbers the coupling ratchet (scripts/ci/coupling_ratchet.py) holds every change to:
 *
 *  - MODULES: the cross-module edges, weighted by the class->class references across them;
 *  - PACKAGES (a class's package cut to its top two segments: `services.identity`, `models`): afferent coupling Ca (the
 *    classes outside the package depending on one inside), efferent coupling Ce (the classes outside it that its classes
 *    depend on; and, held by the ratchet, the outside PACKAGES they depend on -- splitting a trait into roles, which raises
 *    the class count without a new dependency, leaves it), instability I = Ce / (Ca + Ce), abstractness A (traits and abstract classes / all its types) and the
 *    distance from the main sequence D = |A + I - 1|; and the package CYCLES, the strongly connected components of the
 *    package graph, with every package edge inside one (its weight, and the class references that make it);
 *  - CLASSES: fan-in and fan-out (distinct production classes referencing it / referenced by it);
 *  - SPLIT PACKAGES: a (full) package whose classes live in two or more modules.
 *
 * A nested class counts as its top-level class (`Outer.Inner` is `Outer`): a class's own pieces are no coupling. Two
 * top-level classes of ONE source file are two classes: Zinc records no dependency between them (its compiler bridge
 * drops a dependency on the source being compiled), so their uses of each other are read from their class files
 * ([[sameFileEdges]], project/ClassReferences.scala) -- moving a collaborator into a class's file lowers no fan-out.
 * And Zinc charges every import of a file to the file's LAST top-level class ([[importOwner]]), on top of each real use:
 * that class keeps only the dependencies its class files confirm or no other class of the file accounts for
 * ([[withoutImportCharge]]) -- a helper class appended to a file inherits none of the file's imports.
 *
 * Compiled twice: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec) -- so it is written in the
 * syntax both accept.
 */
object Coupling {

  /** A top-level production class: its name, its module, whether it is a trait or an abstract class. */
  final case class Type(name: String, module: String, isAbstract: Boolean)

  /** The graph: every type, the dependencies between them (`from` uses `to`; both types' names), the types that are
   *  DATA ([[Collaborators]]: no coupling to depend on), why each other type is a collaborator, and each type's
   *  supertypes among the types ([[supertypeClosure]]). */
  final case class Graph(types: Seq[Type], edges: Set[(String, String)], data: Set[String] = Set.empty,
                         collaboratorWhy: Map[String, String] = Map.empty, coupledEdges: Option[Set[(String, String)]] = None,
                         supertypes: Map[String, Set[String]] = Map.empty) {
    val byName: Map[String, Type] = types.map(t => t.name -> t).toMap
    /** The edges between two distinct known types. */
    val known: Set[(String, String)] = edges.filter { case (from, to) => from != to && byName.contains(from) && byName.contains(to) }
    /** The known edges into a COLLABORATOR: what a class's fan-out and fan-in count -- `coupledEdges` when measured
     *  ([[Collaborators.coupledEdges]]: a use of a nested type judged by that type), else every edge into a type not in
     *  `data`. Cycles, layers, module edges and a package's Ca, Ce and D read every known edge: a data type's users do
     *  make its package harder to change (a field renamed breaks them), and Ca/Ce over collaborators alone, against an
     *  abstractness over every type, put 10 more packages past the distance limit (2026-11-19). */
    val coupled: Set[(String, String)] = coupledEdges.fold(known.filterNot { case (_, to) => data(to) })(known.intersect)
  }

  /** A class's full package: its name less the last segment (`<root>` for none). */
  def fullPackageOf(name: String): String = { val i = name.lastIndexOf('.'); if (i < 0) "<root>" else name.substring(0, i) }

  /** A class's package as the report groups it: the full package cut to its top two segments. */
  def packageOf(name: String): String = {
    val full = fullPackageOf(name)
    if (full == "<root>") full else full.split('.').take(2).mkString(".")
  }

  /** A class's name without the `$` Zinc records a member of an object under (`clients.TmdbClient$.Page` is
   *  `clients.TmdbClient.Page`): an object and its class are one type. */
  def normalize(name: String): String = name.split('.').map(_.stripSuffix("$")).mkString(".")

  /** The top-level class of each name in `names` (one source's classes, normalized): a name whose prefix is another of
   *  them is nested. */
  def topLevel(names: Set[String]): Map[String, String] = {
    def outer(name: String): String = {
      val i = name.lastIndexOf('.')
      if (i < 0) name else { val prefix = name.substring(0, i); if (names(prefix)) outer(prefix) else name }
    }
    names.map(n => n -> outer(n)).toMap
  }

  /** The top-level class a compiled class (a binary name: `a.Outer$Inner`, `a.Outer$`) folds into, given one source's
   *  binary names and their top-level classes: its own entry, else -- a local or an anonymous class (`a.Outer$$anon$1`,
   *  `a.Outer$Local$1`) -- the nearest enclosing one, cut at each `$` from the right. None: not one of the source's. */
  def foldBinary(binary: String, tops: Map[String, String]): Option[String] = {
    var name = binary
    while (!tops.contains(name) && name.lastIndexOf('$') > 0) name = name.substring(0, name.lastIndexOf('$'))
    tops.get(name)
  }

  /** The dependencies between the top-level classes of ONE source file, which Zinc does not record: each compiled class's
   *  uses (ClassReferences, its class file) folded as Zinc's record is -- a nested, companion, anonymous or local class
   *  into its top-level class --, the self-edges dropped. `tops`: the source's binary names -> top-level classes. */
  def sameFileEdges(compiled: Seq[ClassReferences.Compiled], tops: Map[String, String]): Set[(String, String)] =
    classFileUses(compiled, tops, Map.empty).toSeq.flatMap { case (from, uses) => uses.map(from -> _) }.toSet

  /** The top-level classes each top-level class of ONE source uses, read from its class files (ClassReferences) and
   *  folded as Zinc's record is: `tops` folds the source's own binary names, `others` every other production binary
   *  name (a name neither folds -- a library class -- is left out), the self-uses dropped. */
  def classFileUses(compiled: Seq[ClassReferences.Compiled], tops: Map[String, String], others: Map[String, String]): Map[String, Set[String]] =
    compiled.flatMap { c =>
      foldBinary(c.name, tops).toSeq.flatMap(from => c.uses.toSeq.flatMap(u => foldBinary(u, tops).orElse(foldBinary(u, others))).filter(_ != from).map(from -> _))
    }.groupBy(_._1).map { case (from, uses) => from -> uses.map(_._2).toSet }

  private val TopLevelDefinition =
    """(?m)^(?:@\w+(?:\([^)\n]*\))?\s+)*(?:(?:final|sealed|abstract|case|implicit|open|transparent|private(?:\[\w+\])?|protected(?:\[\w+\])?)\s+)*(?:class|trait|object|enum)\s+(\w+)""".r

  /** The class Zinc charges a source's imports to. Scala 3's dependency extraction charges a dependency outside every
   *  class -- a top-level import's -- to the LAST top-level definition of the file (`firstClassOrModule`, which folds the
   *  tree keeping the last one it meets): the top-level definitions' wrapper `<file>$package` when the source has one (the
   *  compiler appends it after every class), else the last class, trait, object or enum the source text defines at its
   *  first column. `tops`: the source's top-level classes. None: the text defines none of them there. */
  def importOwner(source: String, tops: Set[String]): Option[String] =
    tops.find(_.endsWith("$package")).orElse {
      TopLevelDefinition.findAllMatchIn(source).map(_.group(1)).toSeq.lastOption
        .flatMap(simple => tops.find(t => t == simple || t.endsWith("." + simple)))
    }

  /** Zinc's edges with one source's import charge taken off. Zinc charges every import of the file to its
   *  [[importOwner]], so an edge of the owner's is the import's, not a use of it, when the owner's own class files do not
   *  use the class (`confirmed`: [[classFileUses]] of it) and another of the file's top-level classes (`classes`) is
   *  recorded using it -- the import's real user (an unused import fails the build, so every import has one). The
   *  owner's edges its class files confirm, or no other class of the file accounts for, stay; so does every other edge. */
  def withoutImportCharge(zinc: Set[(String, String)], owner: String, confirmed: Set[String], classes: Set[String]): Set[(String, String)] = {
    val usedByOthers: Set[String] = zinc.collect { case (from, to) if from != owner && classes(from) => to }
    zinc.filterNot { case (from, to) => from == owner && !confirmed(to) && usedByOthers(to) }
  }

  // ── collaborators ──

  /**
   * DATA OR COLLABORATOR (the owner, 2026-11-19: "ignore data types and other non-collaborators, only count
   * collaborators"): which production types a dependency on couples a class to. Read from every production class file
   * ([[ClassReferences.Shape]]), each class folded into its named type; a type is a COLLABORATOR when any of its
   * classes (the class, its companion object, the classes nested in it that are no named type of their own -- a nested
   * type is judged by itself) is one of:
   *
   *  - a class holding STATE: a field some method other than a constructor writes (a `var`, a hand-rolled cache; a lazy
   *    val's own machinery aside), or a field of a mutable or effectful library type ([[StatefulTypes]]: a mutable
   *    collection, a cache, an atomic, a clock, a stream, a logger, a client) -- `TitleNormalizer`'s memo caches,
   *    `LatestTitleYear`'s clock;
   *  - a class carrying BEHAVIOUR: an instance field of a function type (`ProjectionDraft`'s closures that read the store);
   *  - a class doing IO or reading the world: a method body calling one of [[EffectCalls]] (the clock, the environment,
   *    files, the network, a database, randomness, threads, logging, metrics);
   *  - a trait or abstract class with no production implementation (an interface: `MovieRepository` is implemented, by
   *    classes that hold a database, so it is one anyway), or with an implementation that is a collaborator;
   *  - a class holding a collaborator (a field of its type), reading an object that reads the world (itself, or through
   *    an object it reads: a `MODULE$` read), or extending one that is a collaborator by itself (not merely as the
   *    parent of a collaborator sibling). Reading an object that only keeps private state -- a memo of a pure function
   *    (`TitleTextMeasures.Qualifiers`' memo, `CreditRelation`'s thread-local transliterator) -- passes nothing on: the
   *    reader's results stay a function of its inputs, and a class calling that object directly counts it.
   *
   * Every other type is DATA: immutable, effect-free, and holding only data -- case classes and case objects, enums,
   * hierarchies (sealed or not) of such, value classes, record-like final classes of vals (`EvidenceNode`), objects of
   * constants and pure functions (opaque types and type aliases live in one; a string helper of `tools`), pure strategy
   * objects. A lazy val is data: computed once from its owner's fields, then never written again -- immutable, no IO.
   * A method of a data type may take a collaborator as a parameter: its caller holds that collaborator, and counts it.
   * The verdict is the greatest fixpoint: a type is data unless a rule above, or a collaborator it holds, reads or
   * extends, or one implementing it, makes it one.
   */
  object Collaborators {
    private def any(prefixes: Seq[String])(name: String): Boolean = prefixes.exists(p => name == p || name.startsWith(p))

    /** Library types whose instance is state or a door to the world: a field of one is state. */
    val StatefulTypes: Seq[String] = Seq("scala.collection.mutable.", "scala.collection.Iterator", "scala.collection.concurrent.",
      "java.util.concurrent.", "java.util.ArrayList", "java.util.LinkedList", "java.util.HashMap", "java.util.LinkedHashMap",
      "java.util.TreeMap", "java.util.HashSet", "java.util.LinkedHashSet", "java.util.TreeSet", "java.util.ArrayDeque",
      "java.util.Map", "java.util.List", "java.util.Set", "java.util.Collection", "java.util.Deque", "java.util.Queue",
      "java.util.Iterator", "java.util.Random", "java.util.Timer", "java.lang.ThreadLocal", "java.lang.Thread",
      "java.lang.StringBuilder", "java.lang.Process", "java.time.Clock", "java.time.InstantSource", "scala.util.Random",
      "scala.concurrent.", "java.io.InputStream", "java.io.OutputStream", "java.io.Reader", "java.io.Writer",
      "java.io.PrintStream", "java.io.BufferedReader", "java.io.RandomAccessFile", "java.nio.channels.",
      "java.net.http.", "java.net.Socket", "java.net.ServerSocket", "java.net.URLConnection", "java.net.HttpURLConnection",
      "com.mongodb.", "org.mongodb.", "org.bson.codecs.", "com.github.benmanes.caffeine.", "io.prometheus.", "io.micrometer.",
      "org.slf4j.Logger", "play.api.Logger", "play.api.libs.ws.", "sttp.", "akka.", "org.apache.pekko.")

    /** Library types whose instance is a function: an instance field of one is behaviour. */
    val FunctionTypes: Seq[String] = (0 to 22).map(n => s"scala.Function$n") ++ Seq("scala.PartialFunction", "java.util.function.")

    /** The calls that reach the world on their own (`owner#method`; a prefix ending in `.` or `#` names a package or
     *  every method of a class): the static entry points -- the system clock, the environment, files, the network, a
     *  database, randomness, threads, logging, metrics. A cache's get is none: a cache is state, its holder's. A call on a handle the caller was given (`clock.instant()` on
     *  an injected `Clock`, `random.nextInt`) is none: the handle's holder holds that state, and counts it. A classpath
     *  resource read (`getResourceAsStream`, `Source.fromResource`) is none either: a table shipped with the code is a
     *  constant. */
    val EffectCalls: Seq[String] = Seq("java.lang.System#currentTimeMillis", "java.lang.System#nanoTime",
      "java.lang.System#getenv", "java.lang.System#getProperty", "java.lang.System#getProperties",
      "java.lang.System#setProperty", "java.lang.System#exit", "java.lang.Runtime#", "java.lang.ProcessBuilder#",
      "java.lang.Thread#sleep", "java.lang.Thread#start",
      "java.time.Instant#now()", "java.time.LocalDate#now()", "java.time.LocalDateTime#now()", "java.time.LocalTime#now()",
      "java.time.ZonedDateTime#now()", "java.time.OffsetDateTime#now()", "java.time.Year#now()", "java.time.YearMonth#now()",
      "java.time.Clock#system",
      "java.util.UUID#randomUUID", "java.util.Random#<init>", "scala.util.Random$#", "java.util.concurrent.Executors#",
      "java.util.concurrent.CompletableFuture#supplyAsync", "java.util.concurrent.CompletableFuture#runAsync",
      "java.util.concurrent.ThreadLocalRandom#", "java.util.concurrent.TimeUnit#sleep",
      "java.nio.file.Files#", "java.io.File#exists", "java.io.File#isFile", "java.io.File#isDirectory", "java.io.File#list",
      "java.io.File#listFiles", "java.io.File#delete", "java.io.File#mkdir", "java.io.File#mkdirs", "java.io.File#length",
      "java.io.File#lastModified", "java.io.File#createNewFile", "java.io.File#renameTo", "java.io.FileInputStream#",
      "java.io.FileOutputStream#", "java.io.FileReader#", "java.io.FileWriter#", "java.io.RandomAccessFile#",
      "java.io.PrintStream#print", "scala.Predef$#print", "scala.Console$#print", "scala.io.Source$#fromFile",
      "scala.io.Source$#fromURL", "scala.io.StdIn", "java.net.http.", "java.net.URL#open", "java.net.Socket",
      "java.net.InetAddress#", "com.mongodb.", "org.mongodb.", "io.prometheus.",
      "io.micrometer.", "org.slf4j.", "play.api.Logger", "play.api.libs.ws.", "sttp.", "scala.concurrent.Future$#apply",
      "scala.concurrent.Await", "scala.concurrent.package$#blocking")

    private val ImplementedBy = "implemented by"

    /** Field names of a lazy val's own machinery (Scala 3: `x$lzy1`, `OFFSET$_m_0`, `0bitmap$1`; 2: `bitmap$0`). */
    private def lazyMachinery(field: String): Boolean =
      field.contains("$lzy") || field.startsWith("OFFSET$") || field.contains("bitmap$")

    private def isModule(c: ClassReferences.Compiled): Boolean =
      c.name.endsWith("$") && c.shape.fields.exists(f => f.name == "MODULE$" && f.isStatic)
    private def isAnonymous(c: ClassReferences.Compiled): Boolean =
      c.name.contains("$anon") || c.name.matches(".*\\$[A-Za-z_]\\w*\\$\\d+")
    private def isAbstractType(c: ClassReferences.Compiled): Boolean = c.shape.isInterface || c.shape.isAbstract

    /** Why ONE class file makes its type a collaborator by itself, none when it does not. `all`: every production class
     *  by binary name. */
    def ownReason(c: ClassReferences.Compiled, all: Map[String, ClassReferences.Compiled]): Option[String] = {
      val s = c.shape
      // a field holding an object (an `export`ed object's alias) is a read of that object, judged as one, no state
      def holdsObject(f: ClassReferences.Field): Boolean = f.types.nonEmpty && f.types.forall(t => all.get(t).exists(isModule))
      val fields = s.fields.filterNot(f => lazyMachinery(f.name) || f.name == "MODULE$" || holdsObject(f))
      val state = fields.find(f => s.rewritten(f.name) || f.types.exists(any(StatefulTypes)))
      val effect = s.calls.toSeq.sorted.find(any(EffectCalls))
      // an anonymous class's fields are what its enclosing code captured: that code is judged itself
      val function = if (isModule(c) || isAnonymous(c)) None else fields.find(f => !f.isStatic && f.types.exists(any(FunctionTypes)))
      state.map(f => s"state: ${c.name}.${f.name}")
        .orElse(effect.map(call => s"effect: ${c.name} calls $call"))
        .orElse(function.map(f => s"behaviour: ${c.name}.${f.name} is a function"))
    }

    /** Every production type's verdict: its name -> why it is a collaborator, absent for a DATA type. `compiled`: every
     *  production class file; `topOf`: a binary name's top-level production type (None: a library class). */
    def classify(compiled: Seq[ClassReferences.Compiled], topOf: String => Option[String]): Map[String, String] = {
      val all: Map[String, ClassReferences.Compiled] = compiled.map(c => c.name -> c).toMap
      val byTop: Map[String, Seq[ClassReferences.Compiled]] = compiled.flatMap(c => topOf(c.name).map(_ -> c)).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      // each production abstract class's direct implementations
      val implementations: Map[String, Seq[ClassReferences.Compiled]] = compiled
        .flatMap(c => (c.shape.superName +: c.shape.interfaces).filter(all.contains).map(_ -> c)).groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      val reason = scala.collection.mutable.Map.empty[String, String]
      byTop.foreach { case (top, classes) =>
        classes.iterator.flatMap(c => ownReason(c, all)).toSeq.headOption.foreach(r => reason(top) = r)
        classes.filter(isAbstractType).foreach { a =>
          if (!reason.contains(top) && implementations.getOrElse(a.name, Nil).isEmpty) reason(top) = s"an interface: ${a.name} has no implementation"
        }
      }
      def collaborator(binary: String, from: String): Boolean = topOf(binary).exists(t => t != from && reason.contains(t))
      // the types that read the world, themselves or through an object they read: what a read of an object passes on
      val effectful = scala.collection.mutable.Map.empty[String, String] // a type -> the effect call it reaches
      byTop.foreach { case (top, classes) =>
        classes.iterator.flatMap(c => c.shape.calls.toSeq.sorted.find(any(EffectCalls))).toSeq.headOption.foreach(call => effectful(top) = call)
      }
      def objectsRead(c: ClassReferences.Compiled): Seq[String] =
        (c.shape.modules.toSeq ++ c.shape.fields.flatMap(_.types).filter(t => all.get(t).exists(isModule))).distinct.sorted
      var spreading = true
      while (spreading) {
        spreading = false
        byTop.foreach { case (top, classes) =>
          if (!effectful.contains(top)) classes.iterator.flatMap(objectsRead).flatMap(m => topOf(m).filter(t => t != top && effectful.contains(t)))
            .toSeq.headOption.foreach { t => effectful(top) = effectful(t); spreading = true }
        }
      }
      var changed = true
      while (changed) {
        changed = false
        byTop.foreach { case (top, classes) =>
          if (!reason.contains(top)) {
            val why: Option[String] = classes.iterator.flatMap { c =>
              c.shape.fields.iterator.flatMap(f => f.types.filterNot(t => all.get(t).exists(isModule)).find(collaborator(_, top))
                .map(t => s"holds ${c.name}.${f.name}: $t")) ++
                // an object read passes on only its reading of the world: an object's private memo is no state of its caller's
                objectsRead(c).iterator.find(m => collaborator(m, top) && topOf(m).exists(effectful.contains))
                  .map(m => s"reads the object $m, which reaches ${topOf(m).map(effectful).getOrElse("")}") ++
                // a parent that is a collaborator by itself, not merely as a sibling's parent (`Fixed` beside `Live`)
                (c.shape.superName +: c.shape.interfaces).find(p => collaborator(p, top) && topOf(p).exists(t => !reason(t).startsWith(ImplementedBy)))
                  .map(p => s"extends $p") ++
                (if (isAbstractType(c)) implementations.getOrElse(c.name, Nil).find(i => collaborator(i.name, top))
                  .map(i => s"$ImplementedBy ${i.name}") else None)
            }.toSeq.headOption
            why.foreach { w => reason(top) = w; changed = true }
          }
        }
      }
      reason.toMap
    }

    /** The named type a compiled class is (`binary`, its source name `name`): the source name, an object's (its module
     *  class, `X$`) with a `$` -- an object is judged apart from its companion class: calling `Qualifiers.split` uses no
     *  memo a `Qualifiers` instance holds. */
    def unitOf(binary: String, name: String): String = normalize(name) + (if (binary.endsWith("$")) "$" else "")

    /** The DATA types among `types`: those [[classify]] finds no reason in. */
    def data(types: Seq[String], reasons: Map[String, String]): Set[String] = types.filterNot(reasons.contains).toSet

    /**
     * The edges that couple: `from` -> `to` (top-level types) counts when what `from` uses of `to` is a collaborator --
     * judged per TYPE used, a nested type by itself (`IdentityMeasures.Film`, a case class, is data even though
     * `IdentityMeasures`' own functions make it a collaborator; a call of those functions counts it). What `from` uses
     * of `to`: the types of `to` (`unitsOf(to)`) its class files name (`classFileUses(from)`: the types they name,
     * folded to their nearest named type), else -- a use the class files do not show (an inline method, a type alias)
     * -- the ones Zinc records (`zincUses((from, to))`: source names, each its class and its object), else `to` itself,
     * class and object. `collaborator`: whether a named type ([[unitOf]]) is one ([[classify]] over every named type).
     */
    def coupledEdges(edges: Set[(String, String)], classFileUses: Map[String, Set[String]], zincUses: Map[(String, String), Set[String]],
                     topOf: String => String, collaborator: String => Boolean): Set[(String, String)] =
      edges.filter { case e @ (from, to) =>
        val seen = classFileUses.getOrElse(from, Set.empty[String]).filter(u => topOf(u) == to)
        val used = if (seen.nonEmpty) seen else zincUses.getOrElse(e, Set.empty[String]).flatMap(u => Set(u, u + "$")).filter(u => topOf(u) == to)
        (if (used.nonEmpty) used else Set(to, to + "$")).exists(collaborator)
      }
  }

  // ── modules ──

  /** Cross-module edges: (from module, to module) -> the class->class references across them. */
  def moduleEdges(g: Graph): Map[(String, String), Int] =
    g.known.toSeq.map { case (from, to) => (g.byName(from).module, g.byName(to).module) }.filter { case (a, b) => a != b }
      .groupBy(identity).map { case (k, v) => k -> v.size }

  // ── packages ──

  final case class PackageStats(name: String, types: Int, abstracts: Int, ca: Int, ce: Int, cePackages: Int, modules: Seq[String]) {
    def instability: Double = if (ca + ce == 0) 0.0 else ce.toDouble / (ca + ce)
    def abstractness: Double = if (types == 0) 0.0 else abstracts.toDouble / types
    def distance: Double = math.abs(abstractness + instability - 1)
  }

  def packages(g: Graph): Seq[PackageStats] = {
    val pkg = g.types.map(t => t.name -> packageOf(t.name)).toMap
    val cross = g.known.toSeq.filter { case (from, to) => pkg(from) != pkg(to) }
    val ce = cross.groupBy { case (from, _) => pkg(from) }.map { case (p, es) => p -> es.map(_._2).toSet.size }
    val ca = cross.groupBy { case (_, to) => pkg(to) }.map { case (p, es) => p -> es.map(_._1).toSet.size }
    val cePackages = cross.groupBy { case (from, _) => pkg(from) }.map { case (p, es) => p -> es.map(e => pkg(e._2)).toSet.size }
    g.types.groupBy(t => pkg(t.name)).toSeq.map { case (p, ts) =>
      PackageStats(p, ts.size, ts.count(_.isAbstract), ca.getOrElse(p, 0), ce.getOrElse(p, 0), cePackages.getOrElse(p, 0), ts.map(_.module).distinct.sorted)
    }.sortBy(_.name)
  }

  /** Package edges: (from package, to package) -> the class->class references making it. */
  def packageEdges(g: Graph): Map[(String, String), Seq[(String, String)]] =
    g.known.toSeq.map { case e @ (from, to) => (packageOf(from), packageOf(to)) -> e }.filter { case ((a, b), _) => a != b }
      .groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sorted }

  /** Every class->class dependency, one `<from package>-><to package><TAB><from class>-><to class>` line each (a
   *  package's own, `p->p`, too: what a move out of it must take along), sorted: the evidence behind the rows
   *  (scripts/coupling-report --out keeps it as edges.tsv; the ratchet names a refused edge's references from it). */
  def edgeRows(g: Graph): Seq[String] =
    g.known.toSeq.map { case (x, y) => s"${packageOf(x)}->${packageOf(y)}\t$x->$y" }.sorted

  /** Every class's held fan-out ([[heldTargets]]), one `<class><TAB><fan-out><TAB><target>,<target>...` line each,
   *  sorted: what a `Coupling: split of <X>` claim holds -- each piece's count, and the union of what the pieces and X's
   *  remainder reference -- when a piece, or what X keeps, is at or below the limit and so has no baseline row
   *  (scripts/ci/coupling_ratchet.py's `--fanouts`; `--update` records each frozen class's targets as its reference set). */
  def fanOutRows(g: Graph, exemptions: Exemptions): Seq[String] =
    heldTargets(g, exemptions).toSeq.sortBy(_._1).map { case (c, ts) => s"$c\t${ts.size}\t${ts.toSeq.sorted.mkString(",")}" }

  /** Every named type's verdict (a nested type's too, an object's as `X$`), one `<type><TAB>data|collaborator<TAB><why>`
   *  line each, sorted: what a dependency on it
   *  costs a class's fan-out ([[Collaborators]]; scripts/coupling-report --out keeps it as collaborators.tsv). */
  def collaboratorRows(g: Graph): Seq[String] =
    (if (g.data.isEmpty && g.collaboratorWhy.isEmpty) g.types.map(_.name) else (g.data ++ g.collaboratorWhy.keys).toSeq).distinct.sorted
      .map(t => if (g.data(t)) s"$t\tdata\t" else s"$t\tcollaborator\t${g.collaboratorWhy.getOrElse(t, "")}")

  /** The strongly connected components of a directed graph with two or more nodes (Tarjan), each sorted, sorted. */
  def components(nodes: Seq[String], edges: Set[(String, String)]): Seq[Seq[String]] = {
    val next = edges.toSeq.groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sorted }
    var index = 0
    val indexOf = scala.collection.mutable.Map.empty[String, Int]
    val low = scala.collection.mutable.Map.empty[String, Int]
    val stack = scala.collection.mutable.ArrayBuffer.empty[String]
    val onStack = scala.collection.mutable.Set.empty[String]
    val out = Seq.newBuilder[Seq[String]]
    def visit(v: String): Unit = {
      indexOf(v) = index; low(v) = index; index += 1
      stack += v; onStack += v
      next.getOrElse(v, Nil).foreach { w =>
        if (!indexOf.contains(w)) { visit(w); low(v) = math.min(low(v), low(w)) }
        else if (onStack(w)) low(v) = math.min(low(v), indexOf(w))
      }
      if (low(v) == indexOf(v)) {
        val members = Seq.newBuilder[String]
        var w = ""
        while (w != v) { w = stack.remove(stack.size - 1); onStack -= w; members += w }
        val all = members.result()
        if (all.size > 1) out += all.sorted
      }
    }
    (nodes ++ edges.toSeq.flatMap { case (a, b) => Seq(a, b) }).distinct.sorted.foreach(v => if (!indexOf.contains(v)) visit(v))
    out.result().sortBy(_.mkString(" "))
  }

  /** A package cycle: its packages, and every package edge inside it with the class references making it. */
  final case class Cycle(packages: Seq[String], edges: Seq[((String, String), Seq[(String, String)])])

  def cycles(g: Graph): Seq[Cycle] = {
    val edges = packageEdges(g)
    components(packages(g).map(_.name), edges.keySet).map { members =>
      val in = members.toSet
      Cycle(members, edges.toSeq.filter { case ((a, b), _) => in(a) && in(b) }.sortBy { case ((a, b), refs) => (refs.size, a, b) })
    }
  }

  // ── layers ──

  /** THE LAYERING (test/resources/coupling-layers.tsv, [[Layers.parse]]): every package's layer, bottom (0) up, the
   *  order of the packages inside a layer (their ranks, bottom (0) up), and the few package edges allowed to point up
   *  anyway, each with its why. A package may depend only on its own layer or one below, and inside an ORDERED layer
   *  (one any `rank` row names) only on its own rank or a lower one; a package no row places is a violation too, so a
   *  new package takes a place before it lands. With the package graph acyclic, the ranks are its order made explicit:
   *  an edge that would close a cycle is an upward edge first, named with the direction allowed. */
  final case class Layers(rows: Seq[(String, Int)], allowed: Set[(String, String)], ranks: Map[String, Int] = Map.empty) {
    /** A package's layer: the longest `prefix` row naming it (`services` names `services.movies`), None for none. */
    def layerOf(pkg: String): Option[Int] =
      rows.filter { case (prefix, _) => pkg == prefix || pkg.startsWith(prefix + ".") }.sortBy(-_._1.length).headOption.map(_._2)
    /** A package's rank inside its layer: its own `rank` row (the package itself, never a prefix), None for none. */
    def rankOf(pkg: String): Option[Int] = ranks.get(pkg)
    /** Whether `layer` orders its packages: some `rank` row names a package of it. */
    def ordered(layer: Int): Boolean = ranks.keys.exists(p => layerOf(p).contains(layer))
  }

  object Layers {
    /** The table's rows: `layer<TAB><n><TAB><package prefix><TAB><why>`, `rank<TAB><n><TAB><package><TAB><why>` and
     *  `allow<TAB><from>-><to><TAB><why>`; `#` lines and blank lines are comments. A row of any other shape, an `allow`
     *  without its why, a package ranked twice or a ranked package no `layer` row places is an error. */
    def parse(lines: Seq[String]): Layers = {
      val layers = Seq.newBuilder[(String, Int)]
      val allowed = Set.newBuilder[(String, String)]
      val ranks = scala.collection.mutable.LinkedHashMap.empty[String, Int]
      lines.zipWithIndex.foreach { case (line, index) =>
        val n: Int = index + 1
        val cols: Array[String] = line.split("\t", -1)
        def bad(why: String): Nothing = throw new IllegalArgumentException(s"coupling-layers.tsv line $n: $why")
        if (line.trim.isEmpty || line.startsWith("#")) ()
        else if (cols(0) == "layer" && cols.length == 4) {
          if (cols(1).isEmpty || !cols(1).forall(_.isDigit)) bad(s"layer `${cols(1)}` is not a number")
          layers += ((cols(2), cols(1).toInt))
        } else if (cols(0) == "rank" && cols.length == 4) {
          if (cols(1).isEmpty || !cols(1).forall(_.isDigit)) bad(s"rank `${cols(1)}` is not a number")
          if (cols(3).trim.isEmpty) bad(s"the rank of ${cols(2)} has no why")
          if (ranks.contains(cols(2))) bad(s"${cols(2)} is ranked twice")
          ranks(cols(2)) = cols(1).toInt
        } else if (cols(0) == "allow" && cols.length == 3) {
          if (cols(2).trim.isEmpty) bad(s"the allowed edge ${cols(1)} has no why")
          val ends: Array[String] = cols(1).split("->", -1)
          if (ends.length != 2 || ends(0).isEmpty || ends(1).isEmpty) bad(s"`${cols(1)}` is not <from>-><to>")
          allowed += ((ends(0), ends(1)))
        } else bad("not a `layer` or `allow` row")
      }
      val table = Layers(layers.result(), allowed.result(), ranks.toMap)
      ranks.keys.filter(p => table.layerOf(p).isEmpty).foreach(p => throw new IllegalArgumentException(s"coupling-layers.tsv: $p is ranked but no `layer` row places it"))
      table
    }
  }

  /** The package edges the layering forbids, with the class references making each: an edge into a higher layer that
   *  no `allow` row names, and (as `<package>->?`) every package no `layer` row names. */
  def layerViolations(g: Graph, layers: Layers): Seq[((String, String), Seq[(String, String)])] = {
    val unlayered: Seq[((String, String), Seq[(String, String)])] =
      packages(g).map(_.name).filter(p => layers.layerOf(p).isEmpty).map(p => ((p, "?"), Seq.empty[(String, String)]))
    def up(a: String, b: String): Boolean = {
      val from: Option[Int] = layers.layerOf(a)
      val to: Option[Int] = layers.layerOf(b)
      from.isDefined && to.isDefined && to.get > from.get && !layers.allowed((a, b))
    }
    val upward: Seq[((String, String), Seq[(String, String)])] = packageEdges(g).toSeq.filter { case ((a, b), _) => up(a, b) }
    (unlayered ++ upward).sortBy { case ((a, b), _) => (a, b) }
  }

  /** The package edges the order inside a layer forbids, with the class references making each: inside an ordered
   *  layer, an edge to a package of a higher rank that no `allow` row names, and (as `<package>->?`) every package of it
   *  no `rank` row names. */
  def rankViolations(g: Graph, layers: Layers): Seq[((String, String), Seq[(String, String)])] = {
    val unranked: Seq[((String, String), Seq[(String, String)])] = packages(g).map(_.name)
      .filter(p => layers.layerOf(p).exists(layers.ordered) && layers.rankOf(p).isEmpty).map(p => ((p, "?"), Seq.empty[(String, String)]))
    def up(a: String, b: String): Boolean = {
      val (la, lb, ra, rb) = (layers.layerOf(a), layers.layerOf(b), layers.rankOf(a), layers.rankOf(b))
      la.isDefined && la == lb && ra.isDefined && rb.isDefined && rb.get > ra.get && !layers.allowed((a, b))
    }
    val upward: Seq[((String, String), Seq[(String, String)])] = packageEdges(g).toSeq.filter { case ((a, b), _) => up(a, b) }
    (unranked ++ upward).sortBy { case ((a, b), _) => (a, b) }
  }

  // ── classes ──

  /** Per class: (fan-in, fan-out) over distinct production classes, a dependency on a DATA type no coupling (fan-out
   *  counts the collaborators a class uses; a data type's fan-in is 0). */
  def fans(g: Graph): Map[String, (Int, Int)] = {
    val out = g.coupled.groupBy(_._1).map { case (k, v) => k -> v.size }
    val in  = g.coupled.groupBy(_._2).map { case (k, v) => k -> v.size }
    g.types.map(t => t.name -> ((in.getOrElse(t.name, 0), out.getOrElse(t.name, 0)))).toMap
  }

  /** Per class: its fan-out as the limit counts it -- every member of a declared contract the class is a user of
   *  ([[Exemptions]]: `contract` / `member` rows) counted as ONE dependency, every other class as one each. With no
   *  contract it is the plain fan-out of [[fans]]. */
  def heldFanOut(g: Graph, exemptions: Exemptions): Map[String, Int] = heldTargets(g, exemptions).map { case (c, ts) => c -> ts.size }

  /** Per class: what its held fan-out counts -- the COLLABORATORS it references (a data type none), a declared contract it uses
   *  folded into one `contract:<name>`. A `Coupling: split of <X>` claim holds the pieces' union of these to X's.
   *  A type's own supertypes are no collaborator of it, and a supertype of another collaborator it uses is that same
   *  collaborator ([[withoutSupertypes]]: segregating an interface out of a class costs its users nothing). */
  def heldTargets(g: Graph, exemptions: Exemptions): Map[String, Set[String]] = {
    val out = g.coupled.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
    g.types.map { t =>
      val contracts = exemptions.contractsOf(t.name)
      t.name -> withoutSupertypes(t.name, out.getOrElse(t.name, Set.empty[String]), g.supertypes)
        .map(to => contracts.find(_._2(to)).fold(to)(c => "contract:" + c._1))
    }.toMap
  }

  /** `targets` of `from` less `from`'s own supertypes and every supertype of another target: a class depending on
   *  `TitleNormalizer` and on `TitleForms`, an interface TitleNormalizer implements, depends on ONE collaborator (it
   *  hands its normalizer on as its forms), and `TitleNormalizer` implementing `TitleForms` is no dependency of its
   *  own. `supertypes`: each type's supertypes among the production types, transitively. */
  def withoutSupertypes(from: String, targets: Set[String], supertypes: Map[String, Set[String]]): Set[String] = {
    val own = supertypes.getOrElse(from, Set.empty[String])
    val implied = targets.flatMap(t => supertypes.getOrElse(t, Set.empty[String]))
    targets.filterNot(t => own(t) || implied(t))
  }

  /** Each type's supertypes among the production types, transitively (`direct`: each type's own parents). */
  def supertypeClosure(direct: Map[String, Set[String]]): Map[String, Set[String]] = {
    def up(t: String, seen: Set[String]): Set[String] =
      direct.getOrElse(t, Set.empty[String]).filterNot(seen).flatMap(p => up(p, seen + p) + p)
    direct.keys.map(t => t -> (up(t, Set(t)) - t)).toMap
  }

  // ── exemptions ──

  /** One row of the exemption table (test/resources/coupling-exemptions.tsv): what it names (`pattern`: a class or package
   *  name, `*` any run of characters, `p.*` also `p` itself), its `kind` and `value` (`exempt` or a limit of its own, or,
   *  for a contract's rows, the contract's name), whose it is and why, and the day it was added (`added`, an optional
   *  sixth column: every fan-out row added since 2026-11-19 carries one, scripts/ci/coupling_ratchet.py holds it). */
  final case class Exemption(kind: String, pattern: String, value: String, owner: String, why: String, added: String = "") {
    def matches(name: String): Boolean = Exemptions.matches(pattern, name)
  }

  /** THE EXEMPTIONS: the classes and packages the fan-out and distance limits do not hold (or hold to a limit of their
   *  own), and the declared contracts whose members a user's fan-out counts as one. */
  final case class Exemptions(rows: Seq[Exemption]) {
    private def limitOf(kinds: Set[String], name: String, default: Int): Option[Int] =
      rows.find(r => kinds(r.kind) && r.matches(name)).fold(Option(default))(r => if (r.value == "exempt") None else Some(r.value.toInt))
    /** A class's fan-out limit: None when exempt. */
    def fanOutLimit(cls: String, default: Int): Option[Int] = limitOf(Exemptions.FanOutKinds, cls, default)
    /** A package's distance limit (hundredths): None when exempt. */
    def distanceLimit(pkg: String, default: Int): Option[Int] = limitOf(Exemptions.DistanceKinds, pkg, default)
    /** The contracts `cls` is a user of, each with its members. */
    def contractsOf(cls: String): Seq[(String, Set[String])] =
      rows.filter(r => r.kind == "contract" && r.matches(cls)).map(_.value).distinct.sorted
        .map(name => name -> rows.filter(r => r.kind == "member" && r.value == name).map(_.pattern).toSet)
    /** Every contract: its name, its users' patterns, its members. */
    def contracts: Seq[(String, Seq[String], Seq[String])] =
      rows.filter(_.kind == "contract").map(_.value).distinct.sorted.map { name =>
        (name, rows.filter(r => r.kind == "contract" && r.value == name).map(_.pattern),
          rows.filter(r => r.kind == "member" && r.value == name).map(_.pattern).sorted)
      }
  }

  object Exemptions {
    /** `class`: one class past the fan-out limit by its role, an agent's exemption (the owner, 2026-11-19): its why names
     *  the role and the collaborators it needs (scripts/ci/coupling_ratchet.py lints it), listed weekly for the owner to
     *  strike (`scripts/coupling-report --new-exemptions --since 7d`). */
    val FanOutKinds: Set[String] = Set("root", "registry", "config", "class")
    val DistanceKinds: Set[String] = Set("root", "foundation")
    val ContractKinds: Set[String] = Set("contract", "member")
    /** `contract-size`: a contract declared before the member cap (scripts/ci/coupling_ratchet.py's CONTRACT_MAX_MEMBERS,
     *  which holds it), grandfathered at its size -- `pattern` the contract, `value` its members at most. */
    val ContractSizeKind = "contract-size"
    val empty: Exemptions = Exemptions(Nil)

    def matches(pattern: String, name: String): Boolean =
      (pattern.endsWith(".*") && name == pattern.dropRight(2)) ||
        name.matches(pattern.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*"))

    /** The table's rows, `kind<TAB>pattern<TAB>limit|exempt|contract<TAB>owner<TAB>why[<TAB>added]` (`added` a date,
     *  YYYY-MM-DD); `#` and blank lines are comments.
     *  An unknown kind, a value that is neither `exempt` nor a number (for an exempting kind), a member of a contract no
     *  `contract` row has users for (or the reverse), or a row without its owner or why is an error. */
    def parse(lines: Seq[String]): Exemptions = {
      val rows = lines.zipWithIndex.flatMap { case (line, index) =>
        val n: Int = index + 1
        def bad(why: String): Nothing = throw new IllegalArgumentException(s"coupling-exemptions.tsv line $n: $why")
        if (line.trim.isEmpty || line.startsWith("#")) None
        else {
          val cols: Array[String] = line.split("\t", -1)
          if (cols.length != 5 && cols.length != 6) bad("not `kind<TAB>pattern<TAB>limit|exempt<TAB>owner<TAB>why[<TAB>added]`")
          val row = Exemption(cols(0), cols(1), cols(2), cols(3), cols(4), if (cols.length == 6) cols(5) else "")
          if (cols.length == 6 && !row.added.matches("""\d{4}-\d{2}-\d{2}""")) bad(s"`${row.added}` is no date (YYYY-MM-DD)")
          if (row.kind == "class" && row.pattern.contains("*")) bad("a `class` row names one class, no `*`")
          if (!(FanOutKinds ++ DistanceKinds ++ ContractKinds + ContractSizeKind)(row.kind)) bad(s"unknown kind `${row.kind}`")
          if (row.pattern.isEmpty) bad("no pattern")
          if (!ContractKinds(row.kind) && row.value != "exempt" && !(row.value.nonEmpty && row.value.forall(_.isDigit)))
            bad(s"`${row.value}` is neither `exempt` nor a limit")
          if (ContractKinds(row.kind) && row.value.trim.isEmpty) bad("a contract row names its contract")
          if (row.kind == ContractSizeKind && !(row.value.nonEmpty && row.value.forall(_.isDigit))) bad("a `contract-size` row is a number")
          if (row.owner.trim.isEmpty || row.why.trim.isEmpty) bad(s"${row.pattern} has no owner or no why")
          Some(row)
        }
      }
      val users = rows.filter(_.kind == "contract").map(_.value).toSet
      val members = rows.filter(_.kind == "member").map(_.value).toSet
      (users ++ members).foreach { c =>
        if (!users(c)) throw new IllegalArgumentException(s"coupling-exemptions.tsv: contract $c has members but no `contract` row naming its users")
        if (!members(c)) throw new IllegalArgumentException(s"coupling-exemptions.tsv: contract $c has users but no `member` rows")
      }
      rows.filter(_.kind == ContractSizeKind).map(_.pattern).filterNot(users).foreach { c =>
        throw new IllegalArgumentException(s"coupling-exemptions.tsv: a `contract-size` row for $c, which is no declared contract")
      }
      Exemptions(rows)
    }
  }

  /** Split packages: a full package whose classes live in 2+ modules -> (module, class count) per module. */
  def splitPackages(g: Graph): Seq[(String, Seq[(String, Int)])] =
    g.types.groupBy(t => fullPackageOf(t.name)).toSeq.map { case (p, ts) =>
      p -> ts.groupBy(_.module).toSeq.map { case (m, in) => m -> in.size }.sortBy(_._1)
    }.filter(_._2.size > 1).sortBy(_._1)

  // ── the baseline ──

  /** The fan-out above which a class needs a row of its own: a frozen baseline row (it may only fall) or an exemption. */
  val DefaultFanOutLimit = 9
  /** How fan-out is measured, held as a limit row so a measurement fix re-takes the frozen rows (coupling_ratchet.py's
   *  `refrozen`, with its `Coupling:` trailer): 1, Zinc's record alone (a class's uses of the top-level classes of its own
   *  file uncounted); 2, those counted ([[sameFileEdges]]); 3, a file's imports no longer charged to its last class
   *  ([[withoutImportCharge]]); 4, only COLLABORATORS counted ([[Collaborators]]: a data type, nested ones judged by
   *  themselves, is no coupling). */
  val FanOutMeasure = 4
  /** The outside packages a package with no row may depend on. */
  val DefaultNewPackageCeLimit = 8
  /** The distance from the main sequence, in hundredths, above which a package needs a frozen row or an exemption. */
  val DefaultDistanceLimit = 20
  /** The types a package needs before its distance is held: a smaller one's D swings on one class. */
  val DefaultDistanceMinTypes = 8

  /** A package's distance D in hundredths, as the baseline holds it. */
  def hundredths(d: Double): Int = math.round(d * 100).toInt

  /** The ratchet's rows (TAB-separated `kind key value`, sorted): what scripts/ci/coupling_ratchet.py compares. A class
   *  past its fan-out limit ([[heldFanOut]]) and a package of `distanceMinTypes` types or more past the distance limit get
   *  a row, unless an exemption covers them. */
  def baselineRows(g: Graph, fanOutLimit: Int = DefaultFanOutLimit, newPackageCeLimit: Int = DefaultNewPackageCeLimit,
                   layers: Option[Layers] = None, exemptions: Exemptions = Exemptions.empty,
                   distanceLimit: Int = DefaultDistanceLimit, distanceMinTypes: Int = DefaultDistanceMinTypes): Seq[String] = {
    val limits = Seq(s"limit\tfanout\t$fanOutLimit", s"limit\tfanout-measure\t$FanOutMeasure", s"limit\tnew-package-ce\t$newPackageCeLimit",
      s"limit\tdistance\t$distanceLimit", s"limit\tdistance-min-types\t$distanceMinTypes")
    val modules = moduleEdges(g).toSeq.sorted.map { case ((a, b), n) => s"module-edge\t$a->$b\t$n" }
    val cycleEdges = cycles(g).flatMap(_.edges).map { case ((a, b), refs) => s"cycle-edge\t$a->$b\t${refs.size}" }.sorted
    val pkgs = packages(g)
    val ce = pkgs.map(p => s"ce\t${p.name}\t${p.cePackages}")
    val fanout = heldFanOut(g, exemptions).toSeq
      .collect { case (c, out) if exemptions.fanOutLimit(c, fanOutLimit).exists(out > _) => s"fanout\t$c\t$out" }.sorted
    val distance = pkgs.collect {
      case p if p.types >= distanceMinTypes && exemptions.distanceLimit(p.name, distanceLimit).exists(hundredths(p.distance) > _) =>
        s"distance\t${p.name}\t${hundredths(p.distance)}"
    }.sorted
    val layerRows = layers.toSeq.flatMap(l => layerViolations(g, l)).map { case ((a, b), refs) => s"layer-violation\t$a->$b\t${refs.size}" }
    val rankRows = layers.toSeq.flatMap(l => rankViolations(g, l)).map { case ((a, b), refs) => s"rank-violation\t$a->$b\t${refs.size}" }
    limits ++ modules ++ cycleEdges ++ ce ++ fanout ++ distance ++ layerRows ++ rankRows
  }

  val BaselineHeader: Seq[String] = Seq(
    "# THE COUPLING BASELINE (scripts/coupling-report --update writes it; scripts/ci/coupling_ratchet.py holds every push to it).",
    "# Production code only (*/src/main), from Zinc's class-dependency record and, between the classes of one source file,",
    "# their class files (project/Coupling.scala). One row a fact,",
    "# `kind<TAB>key<TAB>value`:",
    "#   limit        fanout: a class without a fanout row (or an exemption) has at most this fan-out; fanout-measure: how",
    "#                fan-out is counted (2: a class's uses of its own file's top-level classes too; 3: a file's imports",
    "#                charged to the classes using them, not to its last class; 4: only collaborators, a data type none);",
    "#                new-package-ce: a",
    "#                package without a ce row at most this Ce; distance (hundredths) and distance-min-types: a package of",
    "#                that many types or more without a distance row (or an exemption) has at most this D",
    "#   module-edge  <from>-><to>, the class references across it: a new pair is a new cross-module edge kind",
    "#   cycle-edge   <from>-><to>, a package edge inside a package cycle (a strongly connected component): never held here --",
    "#                the package graph has no cycle, and the ratchet fails on any such row, in the code or in this file",
    "#   ce           <package>, its efferent coupling in packages (distinct outside packages its classes use): it may not rise",
    "#   fanout       <class>, its fan-out (distinct collaborators it uses, a declared contract's members one) past the",
    "#                limit: FROZEN -- it may only fall; a new row is never admitted, only an exemption",
    "#                (test/resources/coupling-exemptions.tsv, with `Coupling: <why>`) lets a new class past the limit",
    "#   distance     <package>, its D in hundredths past the limit: frozen the same way",
    "#   distance-cap <package>, its frozen distance row's CEILING (hundredths): its value on 2026-11-19 + 2; a split's",
    "#                +0.02 stops there, a fall lowers it with the row (min(ceiling, row + 2)) and nothing raises it;",
    "#                scripts/ci/coupling_ratchet.py update writes it (not couplingReport, whose rows carry none)",
    "#   layer-violation <from>-><to>, a package edge into a higher layer (test/resources/coupling-layers.tsv): never",
    "#                held here -- the ratchet fails on any (cut it, or allow it in the layer table with its why)",
    "#   rank-violation <from>-><to>, inside an ordered layer an edge to a higher-ranked package, or <package>->? for a",
    "#                package of it with no `rank` row: never held here either -- the ratchet fails on any",
    "# A row added or raised lands only in a commit carrying `Coupling: <why>` (CLAUDE.md, Design rules); a fall is",
    "# recorded by --update with no trailer. Module-edge weights are information: only the pair is held.")

  // ── the human report ──

  private def f2(d: Double): String = { val r = math.round(d * 100); s"${r / 100}.${"%02d".format(r % 100)}" }
  private def table(header: Seq[String], rows: Seq[Seq[String]]): Seq[String] =
    Seq(header.mkString("| ", " | ", " |"), header.map(_ => "---").mkString("|", "|", "|")) ++ rows.map(_.mkString("| ", " | ", " |"))

  /** The markdown summary. `declared`: each module's direct Compile dependencies (build.sbt's dependsOn). */
  def report(g: Graph, declared: Map[String, Set[String]], top: Int = 20, layers: Option[Layers] = None,
             exemptions: Exemptions = Exemptions.empty): String = {
    val modules = g.types.map(_.module).distinct.sorted
    val medges = moduleEdges(g)
    val pkgs = packages(g)
    val cyc = cycles(g)
    val fan = fans(g)
    val out = Seq.newBuilder[String]
    out += "# Coupling report"
    out += ""
    out += s"Production code (`*/src/main`): ${g.types.size} top-level classes in ${pkgs.size} packages across ${modules.size} modules, " +
      s"${g.known.size} class->class dependencies (${medges.values.sum} of them across modules)."
    out += ""
    out += "## Modules"
    out += ""
    out += "Rows depend on columns; a cell is the class->class references across that edge."
    out += ""
    out ++= table("from \\ to" +: modules, modules.map(a => a +: modules.map(b => if (a == b) "." else medges.get((a, b)).map(_.toString).getOrElse(""))))
    val transitive = medges.keySet.toSeq.sorted.filterNot { case (a, b) => declared.getOrElse(a, Set.empty)(b) }
    val unused = declared.toSeq.flatMap { case (a, bs) => bs.toSeq.map(a -> _) }.filter { case (a, b) => modules.contains(a) && modules.contains(b) && !medges.contains((a, b)) }.sorted
    out += ""
    out += "Unexpected edges:"
    out += ""
    if (transitive.isEmpty && unused.isEmpty) out += "- none"
    transitive.foreach { case (a, b) => out += s"- `$a -> $b` (${medges((a, b))} refs) is used but not declared: it reaches `$b` only through another module's dependsOn" }
    unused.foreach { case (a, b) => out += s"- `$a -> $b` is declared (dependsOn) but no production class uses it" }
    out += ""
    out += "## Packages"
    out += ""
    out += "Ca: outside classes using the package; Ce: outside classes it uses (Ce pkgs: outside packages, what the ratchet holds); I = Ce/(Ca+Ce); A = abstract types / types; D = |A+I-1|. Sorted by D, then Ce."
    out += ""
    out ++= table(Seq("package", "modules", "types", "abstract", "Ca", "Ce", "Ce pkgs", "I", "A", "D"),
      pkgs.sortBy(p => (-math.round(p.distance * 1000), -p.ce, p.name)).map(p =>
        Seq(s"`${p.name}`", p.modules.mkString(" "), p.types.toString, p.abstracts.toString, p.ca.toString, p.ce.toString, p.cePackages.toString, f2(p.instability), f2(p.abstractness), f2(p.distance))))
    out += ""
    out += s"## Package cycles (${cyc.size})"
    out += ""
    if (cyc.isEmpty) out += "None."
    cyc.zipWithIndex.foreach { case (c, i) =>
      out += s"${i + 1}. ${c.packages.map(p => s"`$p`").mkString(", ")} -- ${c.edges.size} edges inside it, lightest first (the cheapest to cut):"
      c.edges.foreach { case ((a, b), refs) =>
        val sample = refs.take(3).map { case (x, y) => s"`$x` -> `$y`" }.mkString(", ")
        out += s"   - `$a -> $b`: ${refs.size} refs (${sample}${if (refs.size > 3) ", ..." else ""})"
      }
    }
    layers.foreach { l =>
      val bad = layerViolations(g, l)
      val badRank = rankViolations(g, l)
      def sample(refs: Seq[(String, String)]): String =
        refs.take(3).map { case (x, y) => s"`$x` -> `$y`" }.mkString(", ") + (if (refs.size > 3) ", ..." else "")
      def place(p: String): String = l.layerOf(p).map(n => l.rankOf(p).fold(s"$n")(r => s"$n.$r")).getOrElse("?")
      out += ""
      out += s"## Layers (${bad.size + badRank.size} violations)"
      out += ""
      out += "Each package's layer and, inside an ordered layer, its rank (test/resources/coupling-layers.tsv: `layer.rank`), bottom " +
        "up; a package depends only on its own layer or below, and inside an ordered layer only on its own rank or below."
      out += ""
      out ++= table(Seq("layer", "packages"), pkgs.groupBy(p => place(p.name)).toSeq
        .sortBy { case (n, _) => n.split('.').map(x => if (x == "?") Int.MaxValue else x.toInt).toSeq match { case Seq(a) => (a, -1); case Seq(a, b) => (a, b); case _ => (Int.MaxValue, 0) } }
        .map { case (n, ps) => Seq(n, ps.map(p => s"`${p.name}`").sorted.mkString(" ")) })
      bad.foreach { case ((a, b), refs) =>
        out += (if (b == "?") s"- `$a` has no layer" else s"- `$a -> $b` points up: ${refs.size} refs (${sample(refs)})")
      }
      badRank.foreach { case ((a, b), refs) =>
        out += (if (b == "?") s"- `$a` has no rank in its layer (${place(a)})"
                else s"- `$a -> $b` points up inside layer ${place(a).takeWhile(_ != '.')} (rank ${place(a)} -> ${place(b)}; the allowed direction is `$b -> $a`): ${refs.size} refs (${sample(refs)})")
      }
    }
    out += ""
    def ranked(title: String, key: ((Int, Int)) => Int): Unit = {
      out += title
      out += ""
      out ++= table(Seq("class", "module", "fan-in", "fan-out"),
        fan.toSeq.sortBy { case (c, f) => (-key(f), c) }.take(top).map { case (c, (in, o)) => Seq(s"`$c`", g.byName(c).module, in.toString, o.toString) })
      out += ""
    }
    ranked(s"## Top $top classes by fan-in", _._1)
    ranked(s"## Top $top classes by fan-out", _._2)
    ranked(s"## God hubs\n\nHigh fan-in AND high fan-out: the top $top by the smaller of the two.", f => math.min(f._1, f._2))
    val held = heldFanOut(g, exemptions)
    val heldOver = held.toSeq.filter { case (c, o) => exemptions.fanOutLimit(c, DefaultFanOutLimit).exists(o > _) }
    out += s"## Limits: fan-out ${DefaultFanOutLimit}, distance ${f2(DefaultDistanceLimit / 100.0)}"
    out += ""
    out += "A class's held fan-out counts each declared contract it uses as one dependency; the exemptions " +
      "(test/resources/coupling-exemptions.tsv) take the composition roots, the registries, the typed-config root and the foundation packages out."
    out += ""
    out += s"Classes past the fan-out limit (not exempt): ${heldOver.size} -- " +
      Seq(9, 14, 20, 30).map(k => s">$k: ${held.toSeq.count { case (c, o) => o > k && exemptions.fanOutLimit(c, DefaultFanOutLimit).isDefined }}").mkString(", ")
    out += ""
    val far = pkgs.filter(p => p.types >= DefaultDistanceMinTypes && exemptions.distanceLimit(p.name, DefaultDistanceLimit).exists(hundredths(p.distance) > _))
    out += s"Packages of ${DefaultDistanceMinTypes}+ types past the distance limit (not exempt): " +
      (if (far.isEmpty) "none" else far.sortBy(-_.distance).map(p => s"`${p.name}` ${f2(p.distance)}").mkString(", "))
    out += ""
    exemptions.contracts.foreach { case (name, users, members) =>
      val uses = g.known.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
      val using = g.types.count(t => users.exists(u => Exemptions.matches(u, t.name)) && uses.getOrElse(t.name, Set.empty[String]).exists(members.contains))
      out += s"Contract `$name` (${members.size} members, one dependency for ${users.map(u => s"`$u`").mkString(" ")}; $using classes use it): " +
        members.map(m => s"`$m`").mkString(", ")
      out += ""
    }
    out += "## Split packages"
    out += ""
    val split = splitPackages(g)
    if (split.isEmpty) out += "None."
    split.foreach { case (p, ms) => out += s"- `$p`: ${ms.map { case (m, n) => s"$m ($n)" }.mkString(", ")}" }
    out += ""
    out.result().mkString("\n")
  }
}
