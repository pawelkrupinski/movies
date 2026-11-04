import sbt._
import xsbti.VirtualFileRef

/**
 * TEST-IMPACT SELECTION (`identityImpact`, scripts/ci/identity_impact.py): the test classes a change can affect, found
 * STRUCTURALLY from the compiler's own record — every class Zinc records as depending on a changed class (a name used,
 * a type inferred, a parent, an implicit), transitively, across every module's main, fixtures and test sources. A spec
 * no changed class reaches compiles to the same bytecode against the same classes, so it decides as it did; the rest of
 * what it can read is DATA, which is followed by NAME:
 *
 *  - a changed source (`.scala`, `.java`) seeds its own classes;
 *  - any other changed file seeds the classes of every source whose STRING LITERALS name it: its file name, its stem
 *    (a hyphenated one also by each leading run of two or more of its words: `cinema-scrapes-hard-clusters-pl.json.gz`
 *    is read through `s"cinema-scrapes-$key"`), or a directory between its resource root and it
 *    (`identity-unmatched/pl.json.gz` through `Path.of(..., "identity-unmatched")`);
 *  - a changed path a generated identity stamp digests (`<name>-sources.txt`, `IdentityRulesSources`) changes that
 *    stamp, read by name like any resource;
 *  - a resource or fixture no literal names has an UNKNOWN reader, and so has a deleted source (its old dependents are
 *    not on record) and a change to the build: every test is affected (`ALL <why>`);
 *  - an integration or page source (`src/it/`, `src/page/`) is on no unit spec's class path: it affects none.
 *
 * What a selection cannot see is a class reached only by reflection (a module named in a config string), a file read
 * through a name built from no literal, or process-global state one spec leaves for another; the replay and mutation
 * checks (scripts/identity-impact-check) are the evidence that none of those matters to the identity specs.
 */
object TestImpact {

  /** One compile analysis: a module's directory and the configuration it compiled. */
  final case class Unit(module: String, analysis: xsbti.compile.CompileAnalysis)

  /** The repository path of a file Zinc recorded (`${BASE}/worker/src/...`, or absolute under `repo`). */
  def repoPath(repo: File, ref: VirtualFileRef): String = {
    val id   = ref.id
    val root = repo.getAbsolutePath + "/"
    if (id.startsWith("${BASE}/")) id.stripPrefix("${BASE}/")
    else if (id.startsWith(root)) id.stripPrefix(root)
    else id
  }

  /** The string literals of a source, their contents only: what a reader names a file by, never its comments. */
  def literals(text: String): Seq[String] = {
    val out = Seq.newBuilder[String]
    var i = 0
    val n = text.length
    while (i < n) {
      if (text.startsWith("//", i)) { val e = text.indexOf('\n', i); i = if (e < 0) n else e }
      else if (text.startsWith("/*", i)) { val e = text.indexOf("*/", i + 2); i = if (e < 0) n else e + 2 }
      else if (text.startsWith("\"\"\"", i)) {
        val e = text.indexOf("\"\"\"", i + 3); val end = if (e < 0) n else e
        out += text.substring(i + 3, end); i = end + 3
      } else if (text.charAt(i) == '"') {
        var j = i + 1
        while (j < n && text.charAt(j) != '"' && text.charAt(j) != '\n') j += (if (text.charAt(j) == '\\') 2 else 1)
        out += text.substring(i + 1, math.min(j, n)); i = j + 1
      } else if (text.charAt(i) == '\'' && i + 2 < n && (text.charAt(i + 2) == '\'' || text.charAt(i + 1) == '\\')) {
        val e = text.indexOf('\'', i + 2); i = if (e < 0) n else e + 1 // a character literal: '"' is no string
      } else i += 1
    }
    out.result()
  }

  private val ResourceRoots = Seq("src/main/resources/", "src/test/resources/", "src/it/resources/", "src/page/resources/",
    "src/fixtures/resources/", "test/resources/fixtures/", "test/resources/")

  /** Whether a path is data some spec may read off the class path or the fixtures tree. */
  def isResource(path: String): Boolean = ResourceRoots.exists(r => path.startsWith(r) || path.contains("/" + r))

  /** The names a reader may name `path` by (the scaladoc above). */
  def tokens(path: String): Set[String] = {
    val name  = path.substring(path.lastIndexOf('/') + 1)
    val stem  = name.takeWhile(_ != '.')
    val words = stem.split("[-_]").toSeq
    val runs  = (2 until words.size).map(k => words.take(k).mkString("-")).filter(_.length >= 6)
    val under = ResourceRoots.flatMap { r =>
      val i = path.indexOf(r)
      if (i == 0 || (i > 0 && path.charAt(i - 1) == '/')) Some(path.substring(i + r.length)) else None
    }.sortBy(_.length).headOption.getOrElse(path)
    val dirs  = under.split('/').toSeq.dropRight(1)
    (Set(name, under) ++ runs ++ dirs ++ (if (stem.length >= 4) Set(stem) else Set.empty)).filter(_.nonEmpty)
  }

  /**
   * THE REVERSE OF [[select]]: every path a change to which [[select]] would let reach a class of `roots` (repository paths
   * of sources) -- what `roots` can read, by the same rules: the sources its classes reach through every dependency Zinc
   * keeps, transitively, across every unit; the data files their string literals name (by [[tokens]]); every data file no
   * source's literal names (its reader is unknown); the build; and, of a stamp a reached literal names, every path it
   * digests. A path [[select]] would not let reach `roots` is none of these, so a commit whose `reach` holds the same
   * content as another's runs `roots`' classes on the same code and the same data. `exclude`d paths are left out (what the
   * caller keys apart); `tracked` is every file the repository tracks (`git ls-files`). Sorted repository paths.
   */
  def reach(repo: File, units: Seq[Unit], stamps: Seq[File], tracked: Seq[String], roots: Set[String],
            exclude: String => Boolean): Seq[String] = {
    val all = units.map(u => u -> u.analysis.asInstanceOf[sbt.internal.inc.Analysis].relations)
    val classesOf: Map[String, Set[String]] = all.flatMap { case (_, r) =>
      r.classes.forwardMap.toSeq.map { case (src, cls) => repoPath(repo, src) -> cls.toSet }
    }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val sourceOf: Map[String, Set[String]] =
      classesOf.toSeq.flatMap { case (src, cls) => cls.map(_ -> src) }.groupBy(_._1).map { case (k, v) => k -> v.map(_._2).toSet }
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, r) =>
      r.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    val missing = roots -- classesOf.keySet
    require(missing.isEmpty, s"reach: roots no analysis compiled: ${missing.mkString(", ")}")
    def depends(cls: String): Set[String] = all.flatMap { case (_, r) =>
      r.internalClassDep.forward(cls) ++ r.externalClassDep.forward(cls).map(c => sourceNameOf.getOrElse(c, c))
    }.toSet.filter(sourceOf.contains)
    var classes = roots.flatMap(classesOf); var frontier = classes
    while (frontier.nonEmpty) { val next = frontier.flatMap(depends) -- classes; classes ++= next; frontier = next }
    val reached = classes.flatMap(sourceOf) ++ roots
    val sources: Seq[(String, Set[String])] = (repo * "*").get.filter(_.isDirectory).flatMap { module =>
      ((module / "src") ** ("*.scala" || "*.java")).get
    }.map(f => IO.relativize(repo, f).get -> literals(IO.read(f)).toSet)
    val reachedLiterals = sources.collect { case (src, lits) if reached(src) => lits }.flatten.toSet
    val anyLiterals     = sources.flatMap(_._2).toSet
    // every tracked file but a source: one a reached literal names, and data no literal names
    val data = tracked.filterNot(p => p.endsWith(".scala") || p.endsWith(".java")).filter { path =>
      val names = tokens(path)
      names.exists(reachedLiterals) || (isResource(path) && !names.exists(anyLiterals))
    }
    val build = (repo * "*.sbt").get ++ (repo / "project" * ("*.scala" || "*.sbt" || "*.properties")).get
    val stamped = stamps.filter(_.exists).filter(list => tokens(list.getName.stripSuffix("-sources.txt") + "-version.txt").exists(reachedLiterals))
      .flatMap(IO.readLines(_)).filter(_.nonEmpty)
    (reached.toSeq ++ data ++ build.map(f => IO.relativize(repo, f).get) ++ stamped).distinct.filterNot(exclude).sorted
  }

  /** The verdict: `ALL <why>` lines (every test), else the affected test classes as `<module> <class>` lines. */
  def select(repo: File, units: Seq[Unit], tests: Map[String, Seq[String]], stamps: Seq[File],
             changed: Seq[String]): Seq[String] = {
    val all = units.map(u => u -> u.analysis.asInstanceOf[sbt.internal.inc.Analysis].relations)
    val classesOf: Map[String, Set[String]] = all.flatMap { case (u, r) =>
      r.classes.forwardMap.toSeq.map { case (src, cls) => repoPath(repo, src) -> cls.toSet }
    }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, r) =>
      r.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    val known = classesOf.values.flatten.toSet
    // dependent <- dependency, every module's analysis at once: a class Zinc records another module's binary name for
    // is that module's source name
    val dependents: Map[String, Set[String]] = all.flatMap { case (_, r) =>
      r.internalClassDep.all.toSeq ++ r.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }
    }.collect { case (from, to) if known(to) => to -> from }.groupBy(_._1).map { case (k, v) => k -> v.map(_._2).toSet }

    val why = Seq.newBuilder[String]
    val seeds = Set.newBuilder[String]
    lazy val sources: Seq[(String, Set[String])] = (repo * "*").get.filter(_.isDirectory).flatMap { module =>
      ((module / "src") ** ("*.scala" || "*.java")).get
    }.map(f => IO.relativize(repo, f).get -> literals(IO.read(f)).toSet)
    def readers(path: String): Seq[String] = {
      val names = tokens(path)
      sources.collect { case (src, lits) if lits.exists(names) => src }
    }
    // a changed path a stamp digests changes the stamp
    val stamped = stamps.filter(_.exists).flatMap { list =>
      val digested = IO.readLines(list).toSet
      if (changed.exists(digested)) {
        val name = list.getName.stripSuffix("-sources.txt")
        Seq(s"$name-version.txt", s"$name-sources.txt")
      } else Nil
    }
    (changed ++ stamped).distinct.foreach { path =>
      if ((path.endsWith(".sbt") && !path.contains("/")) || path.startsWith("project/")) why += s"ALL the build changed: $path"
      else if (path.endsWith(".scala") || path.endsWith(".java")) {
        classesOf.get(path) match {
          case Some(cls) => seeds ++= cls
          case None if new File(repo, path).exists && !path.matches("[^/]+/src/.*") => () // not compiled by a module
          // an integration or page spec's source: on no unit test's class path (those configurations extend Test)
          case None if path.matches("[^/]+/src/(it|page)/.*") => ()
          case None if new File(repo, path).exists => why += s"ALL a source no analysis compiled: $path"
          case None => why += s"ALL a source deleted or renamed (its old dependents are not on record): $path"
        }
      } else {
        val by = readers(path)
        if (by.isEmpty && isResource(path)) why += s"ALL a resource no literal names (its reader is unknown): $path"
        by.foreach(src => seeds ++= classesOf.getOrElse(src, Set.empty))
      }
    }
    val reasons = why.result()
    if (reasons.nonEmpty) reasons.distinct
    else {
      var affected = seeds.result(); var frontier = affected
      while (frontier.nonEmpty) { val next = frontier.flatMap(c => dependents.getOrElse(c, Set.empty[String])) -- affected; affected ++= next; frontier = next }
      tests.toSeq.sortBy(_._1).flatMap { case (module, names) => names.filter(affected).sorted.map(n => s"$module $n") }
    }
  }
}
