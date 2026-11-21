package quality

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * WHERE EACH METHOD OF A SOURCE IS, for the refactoring tools that work on the methods a change touches
 * (scripts/mutation-check, scripts/coverage-touched: scripts/touched_methods.py intersects these spans with a diff's
 * hunks). The methods are [[MethodMeasures]]' own -- a `def` (or a member val, var or given alias) no other `def`
 * encloses, its nested defs and lambdas folded in -- keyed as the quality ratchets key them (`<path>:<Owner.method>`, a second method of one name in a file `#2`).
 *
 *   quality.MethodSpans --root <repository> --out <tsv> <path>...
 *
 * writes `path<TAB>key<TAB>first line<TAB>last line` for every method of each given source (a path from the root);
 * a source scalameta cannot parse fails the run, naming it.
 */
object MethodSpans {

  /** One method's span: its quality key and its first and last lines (1-based, inclusive). */
  final case class Span(path: String, key: String, startLine: Int, endLine: Int) {
    def row: String = s"$path\t$key\t$startLine\t$endLine"
  }

  /** Every method span of one source's text, keyed as [[QualityReport]] keys a method. */
  def of(path: String, text: String): Either[String, Seq[Span]] =
    MethodMeasures.of(path, text).map(QualityReport.keyed(path, _).map(m => Span(path, m.key, m.method.startLine, m.method.endLine)))

  def main(args: Array[String]): Unit = {
    val (options, paths) = parse(args.toList)
    val root  = Paths.get(options.getOrElse("--root", usage()))
    val out   = Paths.get(options.getOrElse("--out", usage()))
    val spans = paths.map { path =>
      of(path, new String(Files.readAllBytes(root.resolve(path)), StandardCharsets.UTF_8)).fold(why => sys.error(why), identity)
    }
    write(out, spans.flatten.map(_.row))
    println(s"method spans: ${spans.map(_.size).sum} methods of ${paths.size} sources to $out")
  }

  private def usage(): Nothing = sys.error("quality.MethodSpans --root <repository> --out <tsv> <path>...")

  private def parse(args: List[String]): (Map[String, String], Seq[String]) = args match {
    case flag :: value :: rest if flag.startsWith("--") => val (o, p) = parse(rest); (o + (flag -> value), p)
    case path :: rest                                   => val (o, p) = parse(rest); (o, path +: p)
    case Nil                                            => (Map.empty, Nil)
  }

  private def write(out: Path, lines: Seq[String]): Unit =
    Files.write(out, lines.mkString("", "\n", if (lines.isEmpty) "" else "\n").getBytes(StandardCharsets.UTF_8))
}
