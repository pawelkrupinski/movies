package kinowo.build

/**
 * The names a source may read a file by, for the test-impact selection (`project/TestImpact.scala`): a file is read by
 * a source one of whose STRING LITERALS is one of the file's names.
 *
 *  - [[tokens]]: the names a reader opens ONE file by -- its file name, its stem (a hyphenated one also by each leading
 *    run of two or more of its words), or a directory between its resource root and it.
 *  - [[treeNames]]: the names a reader WALKING A TREE reaches it by -- the file's repository path and every directory
 *    above it (`worker/src/main`, `test/resources/fixtures/flicks`). A lint that walks `worker/src/main` for a call
 *    (ListingConstraintsRoutingSpec) reads every source under it, which no class dependency records, so a changed
 *    `MovieService.scala` must select it. A tree's walker never says WHICH file it reads, so these names only add
 *    readers: they never make a file "named" (one no [[tokens]] literal names still has an unknown reader).
 *  - [[globWalks]]: a walker naming its trees by a GLOB (`testsupport.ModuleSources.SourceTrees`: every
 *    module's `src/<config>/scala`, the seven lints built on it naming no module) reaches every file under a tree the glob matches.
 *
 * Compiled twice: into the build (Scala 2.12) and into identity's tests (Scala 3), which check it -- so it is written in
 * the syntax both accept.
 */
object ReaderNames {

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

  /** Whether a literal is a tree GLOB: two or more `/`-separated segments, a `*` in one and one without any, no leading
   *  `/` (a URL route) and no whitespace -- ModuleSources.SourceTrees, never `*` or `services.identity.*`. */
  def isTreeGlob(literal: String): Boolean = {
    val segments = literal.split('/').toSeq.filter(_.nonEmpty)
    literal.contains('*') && !literal.startsWith("/") && !literal.exists(_.isWhitespace) && segments.size >= 2 &&
      segments.exists(!_.contains('*'))
  }

  /** Whether the tree glob `literal` walks `path`: its segments match the path's leading ones (`*` any run of
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
