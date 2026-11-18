package quality

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.Executors
import scala.jdk.CollectionConverters.*

/**
 * THE CODE-QUALITY MEASURES (scripts/quality-report; the pre-push hook's `sbt` check): every deployed module's
 * production source (every `.scala` under `<module>/src/main`) read as text, and the rows scripts/ci/quality_ratchet.py holds to
 * their baselines written out -- `kind<TAB>key<TAB>value`, a method's key `<path>:<Owner.method>` (a second method of
 * one name in a file `#2`, and so on, in source order):
 *
 *   limit       <kind>, the limit a row-less key may not pass
 *   cognitive   a method's cognitive complexity, past its limit (MethodMeasures)
 *   cyclomatic  a method's cyclomatic complexity, past its limit
 *   inner-defs  the defs nested in a method, past its limit
 *   method-lines a method's length in lines, past its limit
 *   file-lines  a source's length in lines (key: its path), past its limit
 *
 *   quality.QualityReport --root <repository> --rows <out> [--files <out>] [--report <markdown out>]
 *
 * `--files` writes one line a source -- `path<TAB>lines<TAB>cognitive` (its methods' sum) -- for scripts/hotspots;
 * `--report` the counts over each limit and the worst methods.
 */
object QualityReport {

  /** The deployed modules: what ships (testkit, itkit, repochecks, e2e and this module are test or tool code). */
  val ProductionModules: Seq[String] = Seq("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")

  /** Each measured kind of a method, and its limit (CLAUDE.md, "Code quality ratchets"). */
  val MethodLimits: Seq[(String, Int)] = Seq("cognitive" -> 15, "cyclomatic" -> 10, "inner-defs" -> 2, "method-lines" -> 50)

  /** Each measured kind of a source, and its limit. */
  val FileLimits: Seq[(String, Int)] = Seq("file-lines" -> 800)

  /** A production source: its path from the repository root, and its text. */
  final case class SourceFile(path: String, text: String)

  /** One measured method of a source, keyed. */
  final case class Measured(key: String, method: MethodMeasures.Method)

  def main(args: Array[String]): Unit = {
    val options = args.grouped(2).collect { case Array(k, v) => k -> v }.toMap
    require(args.length % 2 == 0 && options.contains("--root") && options.contains("--rows"),
      "quality.QualityReport --root <repository> --rows <out> [--files <out>] [--report <markdown out>]")
    val started = System.nanoTime
    val sources = productionSources(Paths.get(options("--root")))
    val measured = measure(sources)
    write(options("--rows"), rows(sources, measured))
    options.get("--files").foreach(write(_, fileLines(sources, measured)))
    options.get("--report").foreach(write(_, report(sources, measured)))
    println(s"quality: ${sources.size} sources, ${measured.values.map(_.size).sum} methods, " +
      s"${(System.nanoTime - started) / 1000000} ms")
  }

  /** Every production Scala source under `root`, sorted by path. */
  def productionSources(root: Path): Seq[SourceFile] =
    ProductionModules.flatMap { module =>
      val main = root.resolve(module).resolve("src").resolve("main")
      if (!Files.isDirectory(main)) Nil
      else {
        val walk = Files.walk(main)
        try walk.iterator.asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala")).toList
        finally walk.close()
      }.map(p => SourceFile(root.relativize(p).toString, new String(Files.readAllBytes(p), StandardCharsets.UTF_8)))
    }.sortBy(_.path)

  /** Each source's methods, keyed; a source scalameta cannot parse fails the measure, naming it (a source left out
   *  would be a hole in the ratchet). */
  def measure(sources: Seq[SourceFile]): Map[String, Seq[Measured]] = {
    val parsed = sources.par(s => s.path -> MethodMeasures.of(s.path, s.text))
    val errors = parsed.collect { case (_, Left(error)) => error }
    if (errors.nonEmpty) throw new IllegalStateException(s"quality: cannot parse ${errors.size} source(s):\n${errors.mkString("\n")}")
    parsed.collect { case (path, Right(methods)) => path -> keyed(path, methods) }.toMap
  }

  /** `<path>:<Owner.method>`, a repeated name `#2`, `#3`, ... in source order. */
  def keyed(path: String, methods: Seq[MethodMeasures.Method]): Seq[Measured] = {
    val seen = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    methods.map { m =>
      seen(m.qualifiedName) += 1
      val n = seen(m.qualifiedName)
      Measured(s"$path:${m.qualifiedName}${if (n == 1) "" else s"#$n"}", m)
    }
  }

  private def valueOf(kind: String, m: MethodMeasures.Method): Int = kind match {
    case "cognitive"  => m.cognitive
    case "cyclomatic" => m.cyclomatic
    case "inner-defs" => m.innerDefs
    case "method-lines" => m.lines
  }

  /** The limit rows, then every method past a limit, then every source past one, by kind and key. */
  def rows(sources: Seq[SourceFile], measured: Map[String, Seq[Measured]]): Seq[String] = {
    val all = measured.values.flatten.toSeq
    (MethodLimits ++ FileLimits).map { case (kind, limit) => s"limit\t$kind\t$limit" } ++
      MethodLimits.flatMap { case (kind, limit) => past(kind, limit, all.map(m => m.key -> valueOf(kind, m.method))) } ++
      FileLimits.flatMap { case (kind, limit) => past(kind, limit, sources.map(s => s.path -> lineCount(s))) }
  }

  private def past(kind: String, limit: Int, values: Seq[(String, Int)]): Seq[String] =
    values.filter(_._2 > limit).sortBy(_._1).map { case (key, v) => s"$kind\t$key\t$v" }

  private def lineCount(source: SourceFile): Int = source.text.linesIterator.size

  /** One line a source: `path<TAB>lines<TAB>cognitive`. */
  def fileLines(sources: Seq[SourceFile], measured: Map[String, Seq[Measured]]): Seq[String] =
    sources.map(s => s"${s.path}\t${lineCount(s)}\t${measured.getOrElse(s.path, Nil).map(_.method.cognitive).sum}")

  /** The counts over each limit, the twenty worst methods by cognitive complexity, and the ten longest sources. */
  def report(sources: Seq[SourceFile], measured: Map[String, Seq[Measured]]): Seq[String] = {
    val all = measured.values.flatten.toSeq
    Seq("## Method complexity and length", "", s"${all.size} methods in ${measured.size} production sources.", "",
      "| measure | limit | over it |", "|---|---|---|") ++
      MethodLimits.map { case (kind, limit) => s"| $kind | $limit | ${all.count(m => valueOf(kind, m.method) > limit)} methods |" } ++
      FileLimits.map { case (kind, limit) => s"| $kind | $limit | ${sources.count(lineCount(_) > limit)} sources |" } ++
      Seq("", "| method | cognitive | cyclomatic | inner defs | lines |", "|---|---|---|---|---|") ++
      all.sortBy(m => (-m.method.cognitive, m.key)).take(20).map { m =>
        s"| `${m.key}` | ${m.method.cognitive} | ${m.method.cyclomatic} | ${m.method.innerDefs} | ${m.method.lines} |"
      } ++
      Seq("", "| source | lines |", "|---|---|") ++
      sources.sortBy(s => (-lineCount(s), s.path)).take(10).map(s => s"| `${s.path}` | ${lineCount(s)} |")
  }

  private def write(path: String, lines: Seq[String]): Unit =
    Files.write(Paths.get(path), lines.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))

  extension [A](sources: Seq[A]) {
    /** `f` over every element, on a pool of the measure's own (one thread a core): parsing is its whole cost. */
    private def par[B](f: A => B): Seq[B] = {
      val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
      try sources.map(a => pool.submit(() => f(a))).map(_.get)
      finally pool.shutdown()
    }
  }
}
