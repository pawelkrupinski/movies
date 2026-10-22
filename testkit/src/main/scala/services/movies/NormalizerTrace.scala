package services.movies

import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.GZIPOutputStream

import scala.jdk.CollectionConverters.*

/**
 * Every distinct title reading a process made, and what it read: the trace `scripts/identity-measure --footprint` replays
 * under another commit's rules ([[NormalizerTraceCheck]]). Set around a whole resolve ([[TitleNormalizer.reading]]), it
 * hears every normaliser's every public reading — the listings' cleaning, every search query, every candidate title's
 * merge key, nested readings too — so that a title-rule change reading ALL of them alike leaves the resolve that made them
 * to make exactly the same readings again, in the same order, and to decide exactly as before: no take can move.
 *
 * One line a reading, gzipped JSON: `{"c": country, "m": method, "a": [arguments], "o": what it read}`. A reading heard
 * twice with two answers (a normaliser that is not a pure function of its arguments) is kept as `"unstable"`, which
 * the check never passes.
 */
final class NormalizerTrace extends TitleNormalizer.Reader {
  import NormalizerTrace.Reading

  private val read     = new ConcurrentHashMap[Reading, String]()
  private val unstable = ConcurrentHashMap.newKeySet[Reading]()

  def read(country: String, method: String, args: Array[String], out: String): Unit = {
    val reading = Reading(country, method, args.toVector)
    val before  = read.putIfAbsent(reading, out)
    if ((before ne null) && before != out) unstable.add(reading)
  }

  def size: Int = read.size

  /** The readings, sorted, to `path` (gzipped JSON lines). */
  def write(path: Path): Unit = {
    Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
    val part = path.resolveSibling(s"${path.getFileName}.part")
    val out  = new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(Files.newOutputStream(part)), UTF_8))
    try read.asScala.toSeq.sortBy { case (r, _) => (r.country, r.method, r.args.mkString("\u0000")) }.foreach { case (r, o) =>
        out.write(Json.stringify(JsObject(Seq("c" -> JsString(r.country), "m" -> JsString(r.method), "a" -> JsArray(r.args.map(JsString(_))),
          "o" -> JsString(o)) ++ Option.when(unstable.contains(r))("unstable" -> play.api.libs.json.JsTrue))))
        out.write('\n')
      }
    finally out.close()
    Files.move(part, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
  }
}

object NormalizerTrace {
  final case class Reading(country: String, method: String, args: Vector[String])

  /** `body`, with every title reading it makes traced to `path` when one is named; `body` alone when none is. The trace is
   *  written whatever `body` ends in — a resolve that fails still says what it read. */
  def recording[A](path: Option[Path])(body: => A): A = path match {
    case None    => body
    case Some(p) =>
      val trace = new NormalizerTrace
      try TitleNormalizer.reading(trace)(body)
      finally {
        trace.write(p)
        System.err.println(s"[normalizer-trace] ${trace.size} distinct title reading(s) written to $p")
      }
  }
}
