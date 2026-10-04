package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ScreeningTokens

/**
 * Every token a source's own table maps its codes onto is a badge `ScreeningTokens` keeps.
 *
 * The class of failure: a parser translates its site's room and version codes into tokens
 * (Ocine's "Sala 4D" → 4D, Webedia's `Format.Projection.HFR` → HFR), and the ingest choke point
 * then passes every token through `ScreeningTokens`, which DROPS a label it does not know. Each
 * table was right on its own and its client spec green, yet Girona's 22 4D-room screenings,
 * Ocine's ICE and Basque ones and the Gatsby brands' HFR ones were all published unmarked, because
 * the vocabulary had never heard of the token. Rule, naming file and token: in a file holding a
 * token table (a `"code" -> "TOKEN"` / `-> List("A", "B")` / `-> Some("A")` mapping onto at least
 * two badges), every mapped value is a badge (`ScreeningTokens.isBadge`). Add the token to
 * `ScreeningTokens.Canonical`, or stop emitting it in the parser, or allowlist it with why.
 */
class SourceTokenTablesSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, scalaFiles}

  private val Roots = Seq("worker/src/main/scala/services/cinemas", "common/src/main/scala")
  private val Vocabulary = "common/src/main/scala/services/movies/ScreeningTokens.scala"

  /** `file: TOKEN` → why that table may map onto a token the vocabulary drops. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/main/scala/services/cinemas/es/OcineParser.scala: URBAN" -> (
      "TODO(decide): Ocine's 'Urban' room — a badge (add \"urban\" to ScreeningTokens.Canonical) or a baseline the " +
        "parser should drop? Parked in review-tools/PARKED.md; until then the vocabulary drops it, logged once"),
  )

  private val Pair = """"[^"\n]*"[ \t]*->[ \t]*(?:"([^"\n]+)"|(?:List|Some)\(([^)\n]*)\))""".r
  private val Literal = """"([^"\n]+)"""".r

  /** Every value `src`'s mappings map onto, when they form a token table (two badges or more); else nothing. */
  private[tools] def tableTokens(src: String, isBadge: String => Boolean): Seq[String] = {
    val values = Pair.findAllMatchIn(src).flatMap { m =>
      Option(m.group(1)).toSeq ++ Option(m.group(2)).toSeq.flatMap(Literal.findAllMatchIn(_).map(_.group(1)))
    }.toSeq.distinct
    if (values.count(isBadge) >= 2) values else Nil
  }

  private[tools] def dropped(src: String, isBadge: String => Boolean): Seq[String] =
    tableTokens(src, isBadge).filterNot(isBadge)

  private lazy val found: Seq[String] =
    scalaFiles(Roots).filterNot(_.toString == Vocabulary).flatMap { path =>
      dropped(codeOf(path), ScreeningTokens.isBadge).map(token => s"$path: $token")
    }

  "the token-table matcher" should "find a table's tokens in each shape a parser writes them" in {
    val known = Set("2D", "3D", "IMAX", "HFR")
    tableTokens("""Map("sala 4d" -> "4D", "2d" -> "2D", "3d" -> "3D")""", known) shouldBe Seq("4D", "2D", "3D")
    tableTokens(""""format.projection.hfr"                -> "HFR",""" + "\n" + """"x" -> List("IMAX", "LASER")""", known) shouldBe
      Seq("HFR", "IMAX", "LASER")
    tableTokens(""""a" -> Some("IMAX"), "b" -> Some("3D")""", known) shouldBe Seq("IMAX", "3D")
    dropped("""Map("sala 4d" -> "4D", "2d" -> "2D", "3d" -> "3D")""", known) shouldBe Seq("4D")
    // A map onto anything else (genres, countries, a single version word) is no token table.
    dropped("""Map("action" -> "Akcja", "UK" -> "Wielka Brytania", "catalan" -> "CAT")""", known) shouldBe empty
  }

  "every source token table" should "map only onto badges ScreeningTokens keeps" in {
    scalaFiles(Roots).size should be > 100
    val unexplained = found.filterNot(Allowlist.contains)
    withClue("These tables emit tokens ScreeningTokens drops, so their screenings are published unmarked. Add the " +
      "token to ScreeningTokens.Canonical (if it names a format, version or accessibility feature), stop the parser " +
      "emitting it (if it names an audience, a seat or a venue property), or allowlist it with why:\n" +
      unexplained.mkString("\n") + "\n")(unexplained shouldBe empty)
  }

  it should "keep every allowlist entry still dropped (the backlog only shrinks)" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot(found.contains)
    withClue("Allowlisted but now a badge, or no longer emitted — drop the entry:\n" + stale.mkString("\n") + "\n")(
      stale shouldBe empty)
  }
}
