package deploy

/**
 * Reads one rule out of the Grafana provisioning file, by uid.
 *
 * Six specs already parse `alert-rules.yaml`, and the ones that guard a SINGLE
 * rule all needed the same two moves: cut the file at `- uid:` to isolate that
 * rule's block, then pull the PromQL out of it. Both are worth doing once. The
 * cut matters because a rule's `description:` annotation names its own metric in
 * prose, and a metric name in a sentence carries no selector — a spec that
 * regexes the whole file for a gauge finds the sentence too and asserts against
 * English. The `expr:` extraction matters because the rest of the block is
 * Grafana's model scaffolding, which no guard has an opinion about.
 */
object AlertRule {

  val File = "infra/nix/files/monitoring/grafana/alerting/alert-rules.yaml"

  /** The `- uid: <uid>` list item, up to the next rule. */
  def withUid(uid: String): Option[String] =
    RepoFile
      .read(File)
      .split("(?m)^\\s*- uid:")
      .find(_.trim.startsWith(uid))

  /** Every `expr:` PromQL in a rule block, unquoted and in file order. */
  def expressionsIn(ruleBlock: String): Seq[String] =
    """(?m)^\s*expr:\s*'(.*)'\s*$""".r
      .findAllMatchIn(ruleBlock)
      .map(_.group(1))
      .toSeq

  /** The `expr:` PromQL for one `data:` entry within a rule block, by its
   *  `refId` (the `- refId: <id>` list item), unquoted. `None` if that refId
   *  doesn't exist in the block, or names an expression with no `expr:` of
   *  its own (a `type: threshold`/`reduce` expression references another
   *  refId instead). A rule with more than one query — e.g. a detection
   *  query plus a purely informational companion for the alert text — needs
   *  this rather than `expressionsIn` so a guard on the DETECTION query
   *  doesn't also constrain a companion that is deliberately shaped
   *  differently. */
  def expressionFor(ruleBlock: String, refId: String): Option[String] =
    ruleBlock
      .split("(?m)^\\s*- refId:\\s*")
      .toSeq
      .find(_.takeWhile(c => !c.isWhitespace) == refId)
      .flatMap("""(?m)^\s*expr:\s*'(.*)'\s*$""".r.findFirstMatchIn(_).map(_.group(1)))

  /** Every `expr:` in a rule file of either kind — Prometheus's `*.rules` (folded `>-` / `|`
   *  blocks, PromQL `#` comment lines dropped) or this Grafana file (single-quoted inline) —
   *  as PromQL text, without the annotations and comments around it that name metrics in
   *  prose. */
  def everyExpression(ruleFile: String): Seq[String] = {
    val lines = ruleFile.linesIterator.toVector
    val Key   = """^(\s*)(?:- )?expr:\s*(.*)$""".r
    lines.indices.flatMap { at =>
      lines(at) match {
        case Key(indent, value) if value.isEmpty || Set(">-", ">", "|", "|-").contains(value.trim) =>
          Some(lines.drop(at + 1)
            .takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent.length)
            .filterNot(_.trim.startsWith("#"))
            .mkString("\n"))
        case Key(_, value) => Some(value.trim.stripPrefix("'").stripSuffix("'").stripPrefix("\"").stripSuffix("\""))
        case _             => None
      }
    }
  }

  val RecordingRulesDir = "infra/nix/files/monitoring/rules"

  /** {record name -> its PromQL} across every Prometheus rule file. */
  lazy val recordings: Map[String, String] = {
    val Record = """^\s*- record:\s*(\S+)\s*$""".r
    RepoFile.listed(RecordingRulesDir)(_.getName.endsWith(".rules")).flatMap { file =>
      val lines = RepoFile.read(file.getPath).linesIterator.toVector
      lines.indices.flatMap { at =>
        lines(at) match {
          case Record(name) => everyExpression(lines.drop(at + 1).mkString("\n")).headOption.map(name -> _)
          case _            => None
        }
      }
    }.toMap
  }

  /** `expr` with every recorded series it reads replaced by the recording's own PromQL, however
   *  deeply, so a guard on what a rule COMPUTES keeps holding when the computation moves into a
   *  recording rule (kinowo-movies-served-swing's query A is one since 2026-11-18). Names only:
   *  the text is for `include` checks, not for evaluation. */
  def withRecordingsExpanded(expr: String): String =
    Iterator.iterate(expr) { current =>
      recordings.foldLeft(current) { case (text, (name, body)) =>
        text.replaceAll("(?<![\\w:])" + java.util.regex.Pattern.quote(name) + "(?![\\w:])",
          java.util.regex.Matcher.quoteReplacement("(" + body + ")"))
      }
    }.sliding(2).collectFirst { case Seq(a, b) if a == b => a }.get
}
