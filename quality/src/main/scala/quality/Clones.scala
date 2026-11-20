package quality

import java.nio.charset.StandardCharsets

import net.sourceforge.pmd.cpd.{CPDConfiguration, CPDReport, CpdAnalysis}
import net.sourceforge.pmd.lang.document.FileId

import scala.jdk.CollectionConverters.*

/**
 * DUPLICATED BLOCKS across the production sources: PMD's copy-paste detector (CPD) over their Scala tokens, every run
 * of at least `MinimumTokens` tokens found twice or more -- a CLONE GROUP, its copies the places it occurs.
 *
 * A group is held by the FILES it spans (`copies<TAB><file>|<file>...<TAB><copies>`, the copies of every group over
 * exactly those files summed), not by its text or lines: an edit around a clone moves its lines and can stretch or cut
 * its token run, but leaves its files. So a new group adds a row or raises one, a group gaining a copy (in another file:
 * another file set; in one it spans: more copies) does too, and a group shrinking or gone is a fall.
 *
 * CPD reads each source with its `package` and `import` statements blanked (`withoutImports`, the lines kept so every
 * copy's lines stay the source's): two sources importing alike -- a new scraper starting from another's imports -- are
 * no clone; only their code is.
 */
object Clones {

  /** The shortest token run counted as a clone: the `limit<TAB>clone-tokens` row (CLAUDE.md, "Code quality ratchets"). */
  val MinimumTokens = 50

  /** One place a clone occurs. */
  final case class Copy(path: String, startLine: Int, endLine: Int)

  /** A clone group: its token and line length, and its copies. */
  final case class Group(tokens: Int, lines: Int, copies: Seq[Copy]) {
    def files: Seq[String] = copies.map(_.path).distinct.sorted
  }

  /** The clone groups among `sources` (path -> text), largest first; a source CPD cannot read fails, naming it. */
  def of(sources: Seq[(String, String)], minimumTokens: Int = MinimumTokens): Seq[Group] = {
    val config = new CPDConfiguration()
    config.setMinimumTileSize(minimumTokens)
    config.setSourceEncoding(StandardCharsets.UTF_8)
    config.setOnlyRecognizeLanguage(config.getLanguageRegistry.getLanguageById("scala"))
    val analysis = CpdAnalysis.create(config)
    try {
      sources.foreach { case (path, text) => analysis.files().addSourceFile(FileId.fromPathLikeString(path), withoutImports(text)) }
      var groups = Seq.empty[Group]
      analysis.performAnalysis((report: CPDReport) => {
        val errors = report.getProcessingErrors.asScala
        if (errors.nonEmpty)
          throw new IllegalStateException(s"cpd: cannot read ${errors.size} source(s):\n${errors.map(e => s"${e.getFileId.getOriginalPath}: ${e.getMsg}").mkString("\n")}")
        groups = report.getMatches.asScala.toSeq.map { m =>
          Group(m.getTokenCount, m.getLineCount, m.getMarkSet.asScala.toSeq.map { mark =>
            val at = mark.getLocation
            Copy(at.getFileId.getOriginalPath, at.getStartLine, at.getEndLine)
          }.sortBy(c => (c.path, c.startLine)))
        }
      })
      groups.sortBy(g => (-g.tokens, g.copies.head.path, g.copies.head.startLine))
    } finally analysis.close()
  }

  /** `text` with every `package <name>` and `import ...` statement's lines (a multi-line selector `{...}` to its close)
   *  made blank, every other line as it was. */
  def withoutImports(text: String): String = {
    var open = 0 // the braces of an import's selector still open
    text.split("\n", -1).map { line =>
      val trimmed = line.trim
      val statement = open > 0 || trimmed.startsWith("import ") || (trimmed.startsWith("package ") && !trimmed.contains("{"))
      if (!statement) line
      else {
        open += line.count(_ == '{') - line.count(_ == '}')
        ""
      }
    }.mkString("\n")
  }

  /** One row a file set: the copies of every group spanning exactly those files. */
  def rows(groups: Seq[Group]): Seq[(String, Int)] =
    groups.groupBy(_.files.mkString("|")).toSeq.map { case (files, gs) => files -> gs.map(_.copies.size).sum }.sortBy(_._1)
}
