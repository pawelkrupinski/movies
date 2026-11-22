import sbt._
import xsbti.VirtualFileRef

/**
 * TEST-IMPACT SELECTION (`identityImpact`, scripts/ci/identity_impact.py): the test classes a change can affect, found
 * STRUCTURALLY from the compiler's own record — every class Zinc records as depending on a changed class (a name used,
 * a type inferred, a parent, an implicit), transitively, across every module's main, fixtures and test sources, and,
 * between the classes of ONE source file (which Zinc does not record), every use their class files make of each other
 * (project/SameFileEdges.scala: a concrete spec extending an abstract one of its own file, a class using its file's
 * helper object) -- the coupling report's same-file edges. A spec
 * no changed class reaches compiles to the same bytecode against the same classes, so it decides as it did; the rest of
 * what it can read is DATA, which is followed by NAME:
 *
 *  - a changed source (`.scala`, `.java`) seeds its own classes;
 *  - any other changed file seeds the classes of every source whose STRING LITERALS name it: its file name, its stem
 *    (a hyphenated one also by each leading run of two or more of its words: `cinema-scrapes-hard-clusters-pl.json.gz`
 *    is read through `s"cinema-scrapes-$key"`), or a directory between its resource root and it
 *    (`identity-unmatched/pl.json.gz` through `Path.of(..., "identity-unmatched")`) -- `ReaderNames.tokens`;
 *  - a changed file under a module's source tree (`<module>/src/...`), a source included, also seeds every source
 *    DECLARING a tree that holds it (`testsupport.SourceTrees("worker/src/main")` or a glob of every module's `src/<config>/scala`:
 *    `ReaderNames.declaredTrees`) and every source whose literal is its very path: a spec walking `worker/src/main` for
 *    a call reads every source under it, which no class dependency records. A test reaches a source tree only through
 *    such a declaration (SourceTreeWalksLintSpec), so the declarations are every walk;
 *  - any other changed file also seeds every source whose literals name its path or a directory above it
 *    (`ReaderNames.treeNames`: a walk of `test/resources/fixtures/flicks`);
 *  - a changed path a generated identity stamp digests (`<name>-sources.txt`, `IdentityRulesSources`) changes that
 *    stamp, read by name like any resource;
 *  - a file only the build reads is what the build makes of it (`ReaderNames.madeFrom`: the stamp carry table, each
 *    stamp's `-version.txt`);
 *  - a BOOKKEEPING file (`ReaderNames.Bookkeeping`: the coupling and quality baselines and tables, the AOT class
 *    list) seeds nothing: only tools outside the selection read one;
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

  /** A compiled class file's bytes (sbt's `fileConverter`: `ref => Files.readAllBytes(converter.toPath(ref))`). */
  type ClassFiles = VirtualFileRef => Array[Byte]

  /** `(dependency, dependent)` pairs between the classes of one source file, which Zinc does not record, read from the
   *  file's class files (SameFileEdges), for every source of every unit. */
  private def sameFileEdges(relations: Seq[sbt.internal.inc.Relations], classFiles: ClassFiles): Seq[(String, String)] =
    relations.flatMap { r =>
      r.classes.forwardMap.toSeq.collect { case (src, names) if names.size > 1 =>
        kinowo.build.SameFileEdges.of(names.toSet, names.map(n => n -> r.productClassName.forward(n).toSet).toMap)(
          r.products(src).toSeq.map(p => kinowo.build.ClassReferences.read(classFiles(p))))
      }.flatten
    }

  /** The repository path of a file Zinc recorded (`${BASE}/worker/src/...`, or absolute under `repo`). */
  def repoPath(repo: File, ref: VirtualFileRef): String = {
    val id   = ref.id
    val root = repo.getAbsolutePath + "/"
    if (id.startsWith("${BASE}/")) id.stripPrefix("${BASE}/")
    else if (id.startsWith(root)) id.stripPrefix(root)
    else id
  }

  import kinowo.build.ReaderNames.{isBookkeeping, isResource, madeFrom, source, tokens, walkers}

  /** Every module source with its string literals and the source trees it declares (`ReaderNames.Source`). */
  private def scan(repo: File): Seq[kinowo.build.ReaderNames.Source] = (repo * "*").get.filter(_.isDirectory).flatMap { module =>
    ((module / "src") ** ("*.scala" || "*.java")).get
  }.map(f => source(IO.relativize(repo, f).get, IO.read(f)))

  /**
   * THE REVERSE OF [[select]]: every path a change to which [[select]] would let reach a class of `roots` (repository paths
   * of sources) -- what `roots` can read, by the same rules: the sources its classes reach through every dependency Zinc
   * keeps, transitively, across every unit; the data files their string literals name (by [[tokens]]); every data file no
   * source's literal names (its reader is unknown); the build; and, of a stamp a reached literal names, every path it
   * digests. A path [[select]] would not let reach `roots` is none of these, so a commit whose `reach` holds the same
   * content as another's runs `roots`' classes on the same code and the same data. `exclude`d paths are left out (what the
   * caller keys apart); `tracked` is every file the repository tracks (`git ls-files`). Sorted repository paths.
   */
  def reach(repo: File, units: Seq[Unit], classFiles: ClassFiles, stamps: Seq[File], tracked: Seq[String], roots: Set[String],
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
    val sameFile: Map[String, Set[String]] =
      sameFileEdges(all.map(_._2), classFiles).groupBy(_._2).map { case (k, v) => k -> v.map(_._1).toSet }
    def depends(cls: String): Set[String] = (all.flatMap { case (_, r) =>
      r.internalClassDep.forward(cls) ++ r.externalClassDep.forward(cls).map(c => sourceNameOf.getOrElse(c, c))
    }.toSet ++ sameFile.getOrElse(cls, Set.empty)).filter(sourceOf.contains)
    val classes = kinowo.build.SameFileEdges.closure(roots.flatMap(classesOf), depends)
    val reached = classes.flatMap(sourceOf) ++ roots
    val sources = scan(repo)
    val reachedSources  = sources.filter(s => reached(s.path))
    val reachedLiterals = reachedSources.flatMap(_.literals).toSet
    val anyLiterals     = sources.flatMap(_.literals).toSet
    // every tracked file but a source: one a reached literal names, and data no literal names; and any file, a source
    // included, under a tree a reached source declares (a source tree) or a reached literal names (any other)
    val data = tracked.filterNot(p => p.endsWith(".scala") || p.endsWith(".java")).filter { path =>
      val names = tokens(path)
      names.exists(reachedLiterals) || (isResource(path) && !names.exists(anyLiterals))
    }
    val walked = tracked.filter(path => walkers(path, reachedSources).nonEmpty)
    val build = (repo * "*.sbt").get ++ (repo / "project" * ("*.scala" || "*.sbt" || "*.properties")).get
    val stamped = stamps.filter(_.exists).filter(list => tokens(list.getName.stripSuffix("-sources.txt") + "-version.txt").exists(reachedLiterals))
      .flatMap(IO.readLines(_)).filter(_.nonEmpty)
    (reached.toSeq ++ data ++ walked ++ build.map(f => IO.relativize(repo, f).get) ++ stamped).distinct.filterNot(exclude).sorted
  }

  /** The verdict: `ALL <why>` lines (every test), else the affected test classes as `<module> <class>` lines, then, as
   *  `READS <module> <class>` lines, those of them a test-side reader of a changed file reaches (its readers by name or
   *  tree, and the tests built on them): the combined-tree verification runs these in every module, not only the
   *  touched ones' (pre-push --verify-only). */
  def select(repo: File, units: Seq[Unit], classFiles: ClassFiles, tests: Map[String, Seq[String]], stamps: Seq[File],
             changed: Seq[String]): Seq[String] = {
    val all = units.map(u => u -> u.analysis.asInstanceOf[sbt.internal.inc.Analysis].relations)
    val classesOf: Map[String, Set[String]] = all.flatMap { case (u, r) =>
      r.classes.forwardMap.toSeq.map { case (src, cls) => repoPath(repo, src) -> cls.toSet }
    }.groupBy(_._1).map { case (k, v) => k -> v.flatMap(_._2).toSet }
    val sourceNameOf: Map[String, String] = all.flatMap { case (_, r) =>
      r.productClassName.all.map { case (sourceName, binaryName) => binaryName -> sourceName } }.toMap
    val known = classesOf.values.flatten.toSet
    // dependent <- dependency, every module's analysis at once: a class Zinc records another module's binary name for
    // is that module's source name; and the dependencies between the classes of one file, which Zinc does not record
    lazy val dependents: Map[String, Set[String]] = (all.flatMap { case (_, r) =>
      r.internalClassDep.all.toSeq ++ r.externalClassDep.all.toSeq.map { case (from, to) => from -> sourceNameOf.getOrElse(to, to) }
    }.collect { case (from, to) if known(to) => to -> from } ++ sameFileEdges(all.map(_._2), classFiles))
      .groupBy(_._1).map { case (k, v) => k -> v.map(_._2).toSet }

    val why = Seq.newBuilder[String]
    val seeds = Set.newBuilder[String]
    lazy val sources = scan(repo)
    def readers(names: Set[String]): Seq[String] = sources.collect { case s if s.literals.exists(names) => s.path }
    // the classes of a TEST-side source reading a changed file by name or declaring a tree holding it (a lint, its scan
    // helper): what `READS` lines name, wherever their module -- a scrapers change runs the worker's lints over it
    val readSeeds = Set.newBuilder[String]
    def seedReaders(srcs: Seq[String]): scala.Unit = srcs.foreach { src =>
      val cls = classesOf.getOrElse(src, Set.empty)
      seeds ++= cls
      if (src.matches("[^/]+/src/test/.*")) readSeeds ++= cls
    }
    // a changed path a stamp digests changes the stamp
    val stamped = stamps.filter(_.exists).flatMap { list =>
      val digested = IO.readLines(list).toSet
      if (changed.exists(digested)) {
        val name = list.getName.stripSuffix("-sources.txt")
        Seq(s"$name-version.txt", s"$name-sources.txt")
      } else Nil
    }
    (changed.flatMap(madeFrom) ++ stamped).distinct.foreach { path =>
      if (isBookkeeping(path)) () // the ratchets' and the build's own tables: no spec of the selection reads one
      else if ((path.endsWith(".sbt") && !path.contains("/")) || path.startsWith("project/")) why += s"ALL the build changed: $path"
      else if (path.endsWith(".scala") || path.endsWith(".java")) {
        seedReaders(walkers(path, sources))
        classesOf.get(path) match {
          case Some(cls) => seeds ++= cls
          case None if new File(repo, path).exists && !path.matches("[^/]+/src/.*") => () // not compiled by a module
          // an integration or page spec's source: on no unit test's class path (those configurations extend Test)
          case None if path.matches("[^/]+/src/(it|page)/.*") => ()
          case None if new File(repo, path).exists => why += s"ALL a source no analysis compiled: $path"
          case None => why += s"ALL a source deleted or renamed (its old dependents are not on record): $path"
        }
      } else {
        val by = readers(tokens(path))
        if (by.isEmpty && isResource(path)) why += s"ALL a resource no literal names (its reader is unknown): $path"
        seedReaders(by ++ walkers(path, sources))
      }
    }
    val reasons = why.result()
    if (reasons.nonEmpty) reasons.distinct
    else {
      def closure(from: Set[String]): Set[String] = kinowo.build.SameFileEdges.closure(from, dependents.getOrElse(_, Set.empty[String]))
      def lines(affected: Set[String], prefix: String): Seq[String] =
        tests.toSeq.sortBy(_._1).flatMap { case (module, names) => names.filter(affected).sorted.map(n => s"$prefix$module $n") }
      lines(closure(seeds.result()), "") ++ lines(closure(readSeeds.result()), "READS ")
    }
  }
}
