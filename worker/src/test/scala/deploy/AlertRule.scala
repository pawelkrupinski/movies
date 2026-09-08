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
}
