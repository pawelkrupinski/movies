package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * Every Mongo find or aggregate read to completion asks for a bounded batch ([[MongoReplies]]).
 *
 * `toFuture()` on the reactive driver asks for batchSize = Int.MaxValue, so each reply fills to the
 * 16 MB message cap and the driver pools a read buffer that size: 32 MB of idle pooled buffers sat
 * in worker-uk's live heap (2026-09-29). A chain — `coll.find(…)` or `coll.aggregate(…)` and every
 * `.method(…)` chained after it, across lines — that reaches `.toFuture` must carry `.batchSize(…)`,
 * unless it reads one document (`.first()`, `.headOption()`, `.head()`, `.limit(1)`).
 */
class NoUnboundedMongoReadSpec extends AnyFlatSpec with Matchers {

  private val MainRoots = Seq("common/src/main", "worker/src/main", "web/src/main")
  private val Start     = """\.(find|aggregate)\b\s*(\[[^\]\n]*\])?\s*\(""".r
  private val SingleDocument = Set("first", "headOption", "head")

  private def scalaFiles(root: String): Seq[Path] = {
    val dir = Paths.get(root)
    if (!Files.isDirectory(dir)) Nil
    else Files.walk(dir).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq
  }

  /** The index just past the bracket closing the one opened at `open`. */
  private def closing(s: String, open: Int, left: Char, right: Char): Int = {
    var depth = 0; var i = open
    while (i < s.length) {
      if (s(i) == left) depth += 1
      else if (s(i) == right) { depth -= 1; if (depth == 0) return i + 1 }
      i += 1
    }
    s.length
  }

  /** The methods chained after the call ending at `from`, each with its argument text. */
  private def chain(s: String, from: Int): Seq[(String, String)] = {
    val links = Seq.newBuilder[(String, String)]
    var i = from; var going = true
    while (going) {
      var j = i
      while (j < s.length && s(j).isWhitespace) j += 1
      if (j < s.length && s(j) == '.' && j + 1 < s.length && s(j + 1).isLetter) {
        var k = j + 1
        while (k < s.length && (s(k).isLetterOrDigit || s(k) == '_')) k += 1
        val name = s.substring(j + 1, k)
        if (k < s.length && s(k) == '[') k = closing(s, k, '[', ']')
        val args = if (k < s.length && s(k) == '(') { val end = closing(s, k, '(', ')'); val a = s.substring(k + 1, end - 1); k = end; a } else ""
        links += name -> args
        i = k
      } else going = false
    }
    links.result()
  }

  "a Mongo find or aggregate read to completion" should "ask for a bounded batch" in {
    MainRoots.map(Paths.get(_)).foreach(root => withClue(s"$root must exist (run from the repo root)")(Files.isDirectory(root) shouldBe true))
    val offenders = for {
      file  <- MainRoots.flatMap(scalaFiles)
      text   = new String(Files.readAllBytes(file), "UTF-8")
      start <- Start.findAllMatchIn(text).toSeq
      links  = chain(text, closing(text, start.end - 1, '(', ')'))
      names  = links.map(_._1)
      if names.contains("toFuture") && !names.contains("batchSize") &&
         !names.exists(SingleDocument) && !links.contains("limit" -> "1")
    } yield s"$file:${text.substring(0, start.start).count(_ == '\n') + 1}: ${text.linesIterator.drop(text.substring(0, start.start).count(_ == '\n')).next().trim}"
    withClue("add .batchSize(tools.MongoReplies.<kind>) before .toFuture:\n" + offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }
}
