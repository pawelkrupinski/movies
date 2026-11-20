package kinowo.build

/**
 * The names a source may read a file by, for the test-impact selection (`project/TestImpact.scala`): a file is read by
 * a source one of whose STRING LITERALS is one of the file's names.
 *
 *  - [[tokens]]: the names a reader opens ONE file by -- its file name, its stem (a hyphenated one also by each leading
 *    run of two or more of its words), or a directory between its resource root and it.
 *  - [[declaredTrees]]: the trees a source DECLARES it walks, the literals of its `testsupport.SourceTrees(...)` calls:
 *    a path (`worker/src/main`) or a glob ([[globWalks]]: every module's `src/<config>/scala`). A lint that walks `worker/src/main` for a
 *    call (ListingConstraintsRoutingSpec) reads every source under it, which no class dependency records, so a changed
 *    `MovieService.scala` must select it. Under a module's source tree ([[isSourceTree]]) a declaration is the ONLY
 *    walk the selection follows: SourceTreeWalksLintSpec fails test code reaching one any other way, so no path
 *    literal there need be read as a walk (four review rounds each found a shape literal-spotting missed).
 *  - [[treeNames]]: the names a reader walking any OTHER tree reaches a file by -- its repository path and every
 *    directory above it (`test/resources/fixtures/flicks`). A tree's walker never says WHICH file it reads, so these
 *    names (and declarations) only add readers: they never make a file "named" (one no [[tokens]] literal names still
 *    has an unknown reader).
 *
 * Compiled twice: into the build (Scala 2.12) and into identity's tests (Scala 3), which check it -- so it is written in
 * the syntax both accept.
 */
object ReaderNames {

  /** The string literals of a source, their contents only: what a reader names a file by, never its comments. */
  def literals(text: String): Seq[String] = literalSpans(text).map(_._3)

  /** Every string literal of a source: the offset of its opening quote, the offset after its closing one, and its
   *  contents (a triple-quoted literal is ONE literal, the quotes inside it none). Comments hold none. */
  def literalSpans(text: String): IndexedSeq[(Int, Int, String)] = {
    val out = Vector.newBuilder[(Int, Int, String)] // indexed: code() walks it by position
    var i = 0
    val n = text.length
    while (i < n) {
      if (text.startsWith("//", i)) { val e = text.indexOf('\n', i); i = if (e < 0) n else e }
      else if (text.startsWith("/*", i)) { val e = text.indexOf("*/", i + 2); i = if (e < 0) n else e + 2 }
      else if (text.startsWith("\"\"\"", i)) {
        val e = text.indexOf("\"\"\"", i + 3); val end = if (e < 0) n else e
        out += ((i, math.min(end + 3, n), text.substring(i + 3, end))); i = end + 3
      } else if (text.charAt(i) == '"') {
        var j = i + 1
        while (j < n && text.charAt(j) != '"' && text.charAt(j) != '\n') j += (if (text.charAt(j) == '\\') 2 else 1)
        out += ((i, math.min(j + 1, n), text.substring(i + 1, math.min(j, n)))); i = j + 1
      } else if (text.charAt(i) == '\'' && i + 2 < n && (text.charAt(i + 2) == '\'' || text.charAt(i + 1) == '\\')) {
        val e = text.indexOf('\'', i + 2); i = if (e < 0) n else e + 1 // a character literal: '"' is no string
      } else i += 1
    }
    out.result()
  }

  /** A source's CODE: its comments and its literals' contents blanked to spaces, every offset kept -- where a call's
   *  name and parentheses are found, never inside a string or a comment. */
  def code(text: String): String = {
    val out = new StringBuilder(text)
    var i = 0
    val n = text.length
    def blank(from: Int, until: Int): scala.Unit = (from until math.min(until, n)).foreach(k => if (out.charAt(k) != '\n') out.setCharAt(k, ' '))
    val spans = literalSpans(text)
    spans.foreach { case (start, end, _) => blank(start + 1, end - 1) }
    // comments: every `//` or `/*` outside a literal
    var s = 0
    while (i < n) {
      while (s < spans.size && spans(s)._2 <= i) s += 1
      if (s < spans.size && spans(s)._1 <= i) i = spans(s)._2
      else if (text.startsWith("//", i)) { val e = text.indexOf('\n', i); val end = if (e < 0) n else e; blank(i, end); i = end }
      else if (text.startsWith("/*", i)) { val e = text.indexOf("*/", i + 2); val end = if (e < 0) n else e + 2; blank(i, end); i = end }
      else if (text.charAt(i) == '\'' && i + 2 < n && (text.charAt(i + 2) == '\'' || text.charAt(i + 1) == '\\')) {
        val e = text.indexOf('\'', i + 2); val end = if (e < 0) n else e + 1; blank(i + 1, end - 1); i = end // '(' is no call
      } else i += 1
    }
    out.toString
  }

  /** The argument span of every call `call` matches in `code` (see [[code]]): from its `(` to the matching `)`. */
  def argumentSpans(code: String, call: scala.util.matching.Regex): Seq[(Int, Int)] =
    call.findAllMatchIn(code).map { m =>
      val open = code.indexOf('(', m.end - 1)
      var depth = 0; var i = open; var close = code.length
      while (i < code.length && close == code.length) {
        code.charAt(i) match {
          case '(' => depth += 1
          case ')' => depth -= 1; if (depth == 0) close = i
          case _ =>
        }
        i += 1
      }
      (open, close)
    }.toSeq

  /** A source-tree DECLARATION: a call of testsupport.SourceTrees (its `apply` included). */
  val Declaration: scala.util.matching.Regex = """(?<!\bnew\s{1,4})\bSourceTrees\s*(?:\.\s*apply\s*)?\(""".r

  /** The trees a source DECLARES it reads: the literals of its `SourceTrees(...)` calls (testsupport.SourceTrees). */
  def declaredTrees(text: String): Seq[String] = if (!text.contains("SourceTrees")) Nil else {
    val spans = argumentSpans(code(text), Declaration)
    literalSpans(text).collect { case (start, _, tree) if spans.exists { case (open, close) => open < start && start < close } => tree.stripSuffix("/") }
  }

  /** Whether a path lies under a module's source tree, `<module>/src/...`: what only a declaration walks. */
  def isSourceTree(path: String): Boolean = path.matches("[^/]+/src/.+")

  /** Whether the declared tree `tree` (a path or a glob, [[globWalks]]) holds `path`, or is it. */
  def covers(tree: String, path: String): Boolean =
    path == tree || path.startsWith(tree + "/") || globWalks(tree, path)

  /** A module source as the selection reads it: its repository path, its string literals, the trees it declares. */
  final case class Source(path: String, literals: Set[String], declared: Seq[String])

  def source(path: String, text: String): Source = Source(path, literals(text).toSet, declaredTrees(text))

  /** The `sources` reading `path` through a tree walk: under a module's source tree, every source declaring a tree
   *  that holds it, or naming its very path; elsewhere, every source naming it or a directory above it ([[treeNames]]). */
  def walkers(path: String, sources: Seq[Source]): Seq[String] =
    if (isSourceTree(path)) sources.collect { case s if s.literals(path) || s.declared.exists(covers(_, path)) => s.path }
    else {
      val names = treeNames(path)
      sources.collect { case s if s.literals.exists(names) => s.path }
    }

  private val ResourceRoots = Seq("src/main/resources/", "src/test/resources/", "src/it/resources/", "src/page/resources/",
    "src/fixtures/resources/", "test/resources/fixtures/", "test/resources/")

  /** BOOKKEEPING (2026-11-19): the ratchets' and the build's own tables -- the coupling and quality baselines
   *  (`scripts/coupling-report --update`, `scripts/quality-report --update`), the coupling layer and exemption tables
   *  (the build's couplingReport, CouplingSpec), the identity boot-cost baseline (scripts/identity-boot-cost) -- and the
   *  worker's AOT class list (the image's class archive,
   *  AotClassListSpec). Read only by tools and specs outside what the selection is for, they seed nothing: a change to
   *  one moves no take, and is never a resource of unknown reader (46 of 100 common/ commits ran every identity spec
   *  for a re-recorded coupling-baseline.tsv or aot-classes.txt). */
  val Bookkeeping: Set[String] = Set("test/resources/coupling-baseline.tsv", "test/resources/coupling-fanout-refs.tsv",
    "test/resources/coupling-exemptions.tsv", "test/resources/coupling-layers.tsv", "test/resources/complexity-baseline.tsv",
    "test/resources/length-baseline.tsv", "test/resources/duplication-baseline.tsv", "worker/src/main/resources/aot-classes.txt",
    "test/resources/identity-boot-cost-baseline.tsv")

  def isBookkeeping(path: String): Boolean = Bookkeeping(path)

  /** Whether a path is data some spec may read off the class path or the fixtures tree. */
  def isResource(path: String): Boolean = ResourceRoots.exists(r => path.startsWith(r) || path.contains("/" + r))

  /** The names a reader may open `path` by (the scaladoc above). */
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

  /** The names a tree walker reaches `path` by: the path, and each directory above it of two or more segments, with
   *  or without its trailing `/` (a top-level `worker` or `scripts` names too much to be read as a walk). */
  def treeNames(path: String): Set[String] = {
    val segments = path.split('/').toSeq.filter(_.nonEmpty)
    val dirs     = (2 until segments.size).map(k => segments.take(k).mkString("/"))
    (dirs ++ dirs.map(_ + "/") :+ path).toSet
  }

  /** Whether a declared tree is a GLOB: two or more `/`-separated segments, a `*` in one and one without any, no leading
   *  `/` and no whitespace -- ModuleSources' one, never `*` or `services.identity.*`. */
  def isTreeGlob(literal: String): Boolean = {
    val segments = literal.split('/').toSeq.filter(_.nonEmpty)
    literal.contains('*') && !literal.startsWith("/") && !literal.exists(_.isWhitespace) && segments.size >= 2 &&
      segments.exists(!_.contains('*'))
  }

  /** Whether the tree glob `literal` (a declared tree) walks `path`: its segments match the path's leading ones (`*` any run of
   *  characters but `/`, a `**` segment any number of segments), so it names a directory above the path, or the path. */
  def globWalks(literal: String, path: String): Boolean =
    isTreeGlob(literal) && leads(literal.split('/').toList.filter(_.nonEmpty), path.split('/').toList.filter(_.nonEmpty))

  private def leads(glob: List[String], path: List[String]): Boolean = (glob, path) match {
    case (Nil, _)               => true
    case ("**" :: rest, _)      => leads(rest, path) || (path.nonEmpty && leads(glob, path.tail))
    case (g :: rest, p :: tail) => p.matches(g.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString("[^/]*")) && leads(rest, tail)
    case (_, Nil)               => false
  }
}
