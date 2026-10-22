package services.movies

import play.api.libs.json.{JsObject, Json}

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.zip.GZIPInputStream

import scala.collection.mutable
import scala.util.Try

/**
 * A [[NormalizerTrace]] read again under THIS checkout's title rules: every reading the traced resolve made, made again
 * by this code's normaliser of the same country, and each one that reads otherwise named. None reading otherwise is the
 * footprint measure's proof that a title-rule change moves no take (`scripts/identity-measure --footprint`): the
 * resolve's every other input is the same, so it reads the same, decides the same, and makes the same readings again.
 *
 * {{{
 *   java -cp <worker test classpath> services.movies.NormalizerTraceCheck --out differ.tsv trace-pl.jsonl.gz [...]
 * }}}
 *
 * Methods are found by name, so this file compiles into a ref from before the trace (a commit measured is laid out with
 * it dropped in: `scripts/identity-tools.sh`'s identity_tree). `<out>` holds each reading that differs, or that this
 * code cannot make again (its normaliser has no such method, or no venue of that name), as TSV: country, method,
 * arguments (` | `-joined), what the trace read, what this code reads.
 *
 * Exit 0: every reading alike. 1: some read otherwise. 3: none otherwise, but some could not be made again (or the trace
 * marks one unstable) — no proof either way. 2: usage, or a trace that cannot be read.
 */
object NormalizerTraceCheck {

  final case class Outcome(readings: Long, differ: Long, unreplayable: Long, traceOnly: Long = 0)

  /** The readings no take is decided by: `firedRules` names the rules that fired, by id, for the decision trace alone
   *  (`IdentityTraceStore`, the resolve dump's `rules` field) -- a merge of two rows into one renames them and moves no
   *  take (8728b27fb, 2026-10-22: 11 PL readings, the full measure 0 moved). Counted and listed apart, never a proof's
   *  failure. Keep it so: a decision reading `firedRules` must come off this list. */
  val TraceOnly: Set[String] = Set("firedRules")

  def main(args: Array[String]): Unit = {
    val (out, traces) = args.toList match {
      case "--out" :: o :: rest if rest.nonEmpty => (Paths.get(o), rest.map(Paths.get(_)))
      case _ => System.err.println("usage: NormalizerTraceCheck --out <differ.tsv> <trace.jsonl.gz>..."); sys.exit(2)
    }
    val outcome = Try(check(traces, out)).fold({ e => System.err.println(s"[normalizer-trace] could not read the trace: $e"); sys.exit(2) }, identity)
    println(s"[normalizer-trace] ${outcome.readings} reading(s) made again: ${outcome.differ} read otherwise, " +
      s"${outcome.unreplayable} could not be made again, ${outcome.traceOnly} decision-trace rule name(s) otherwise (no take reads them)")
    sys.exit(if (outcome.differ > 0) 1 else if (outcome.unreplayable > 0) 3 else 0)
  }

  def check(traces: Seq[Path], out: Path): Outcome = {
    val normalizers = mutable.Map.empty[String, Option[TitleNormalizer]]
    def normalizerOf(code: String) = normalizers.getOrElseUpdate(code,
      models.Country.all.find(_.code == code).filter(_ => code.nonEmpty).map(TitleNormalizer.forCountry))
    var readings, differ, unreplayable, traceOnly = 0L
    Option(out.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    val report = new PrintWriter(Files.newBufferedWriter(out, UTF_8))
    try traces.foreach { trace =>
      val in = new BufferedReader(new InputStreamReader(new GZIPInputStream(Files.newInputStream(trace)), UTF_8))
      try Iterator.continually(in.readLine()).takeWhile(_ != null).filter(_.nonEmpty).foreach { line =>
          val js      = Json.parse(line).as[JsObject]
          val country = (js \ "c").as[String]
          val method  = (js \ "m").as[String]
          val as      = (js \ "a").as[Seq[String]]
          val was     = (js \ "o").as[String]
          readings += 1
          val now = if ((js \ "unstable").asOpt[Boolean].contains(true)) None
                    else normalizerOf(country).flatMap(n => Try(reread(n, method, as)).toOption.flatten)
          now match {
            case Some(reads) if reads == was => ()
            case Some(_) if TraceOnly(method) => traceOnly += 1
            case Some(reads) => differ += 1; report.println(Seq(country, method, as.mkString(" | "), was, reads).map(tsv).mkString("\t"))
            case None        => unreplayable += 1; report.println(Seq(country, method, as.mkString(" | "), was, "(cannot read again)").map(tsv).mkString("\t"))
          }
        }
      finally in.close()
    }
    finally report.close()
    Outcome(readings, differ, unreplayable, traceOnly)
  }

  private def tsv(s: String) = s.replace('\t', ' ').replace('\n', ' ')

  /** The reading `method` of `args` by `n`, rendered as the trace renders it; `None` when `n` cannot make it. */
  def reread(n: TitleNormalizer, method: String, args: Seq[String]): Option[String] = method match {
    // the readings over a group of titles, by the trace's own encoding of their arguments
    case "mergeKey"         => args.headOption.map(title => n.mergeKey(title, args.tail))
    case "preferredDisplay" => Some(String.valueOf(n.preferredDisplay(args)))
    case "chooseDisplay"    => args match {
        case fallback +: tmdb +: hasTmdb +: year +: perCinema =>
          Some(n.chooseDisplay(perCinema, fallback, Option.when(hasTmdb == "+")(tmdb), year.toIntOption))
        case _ => None
      }
    case _ =>
      // one or two titles, a venue by its display name where the method takes one
      n.getClass.getMethods.find(m => m.getName == method && m.getParameterCount == args.size &&
        m.getParameterTypes.forall(t => t == classOf[String] || t == classOf[models.Cinema])).flatMap { m =>
        val values = m.getParameterTypes.toSeq.zip(args).map {
          case (t, a) if t == classOf[models.Cinema] => models.Cinema.byDisplayName.get(a)
          case (_, a)                                 => Some(a)
        }
        Option.when(values.forall(_.isDefined))(String.valueOf(m.invoke(n, values.map(_.get.asInstanceOf[AnyRef])*)))
      }
  }
}
