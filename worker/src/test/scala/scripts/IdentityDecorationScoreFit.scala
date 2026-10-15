package scripts

import tools.CommandLine
import play.api.libs.json.Json
import services.identity.DecorationScore

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * Re-fits the candidate-decoration score ([[DecorationScore]]) from the outcomes the detector measured
 * (`integration.IdentityDecorationCandidates` writes `training.tsv`; the checked-in copy is [[Training]]) and writes it to
 * `worker/src/test/resources/identity-decoration-score.json`. A function of the rows: `IdentityDecorationScoreFitSpec`
 * pins that the shipped weights are what this fits from the checked-in rows.
 *
 *   worker/Test/runMain scripts.IdentityDecorationScoreFit [--training <tsv>] [--out <json>] [--version <v>]
 */
object IdentityDecorationScoreFit {

  val Training: Path = Paths.get("test/resources/fixtures/identity-decorations/training.tsv")
  val Artefact: Path = Paths.get("worker/src/test/resources", DecorationScore.ResourcePath)
  val Header: String = "key\taligned\trecurring\tvenues\tprefix\ttokens\tformatShare\tinnerResolved\tmatchedShare\tgood\tbad"

  val commandLine: CommandLine = CommandLine("IdentityDecorationScoreFit", "Re-fits the candidate-decoration score from the detector's outcomes.",
    CommandLine.Value("training", "tsv", "the detector's outcomes", default = Some(Training.toString)),
    CommandLine.Value("out", "json", "where the score is written", default = Some(Artefact.toString)),
    CommandLine.Value("version", "v", "the version the fit is filed under (default: the rows' digest)"))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val training = arguments.requiredPath("training")
    val out      = arguments.requiredPath("out")
    val score    = DecorationScore.fit(read(training), arguments.get("version").getOrElse(versionOf(training)))
    Files.writeString(out, Json.prettyPrint(Json.toJson(score)) + "\n")
    println(s"wrote $out: ${score.trainingRows} rows, cut ${score.cut}, held out ${score.heldOut}")
    score.features.zip(score.weights).foreach { case (name, weight) => println(s"  $name: $weight") }
  }

  /** The version a fit is filed under: the training rows' own digest, so the same rows always fit the same artefact. */
  def versionOf(training: Path): String =
    f"decoration-score-${scala.util.hashing.MurmurHash3.bytesHash(Files.readAllBytes(training)) & 0xffffffffL}%08x"

  def read(path: Path): Seq[DecorationScore.Row] =
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.drop(1).filter(_.nonEmpty).map(_.split("\t", -1)).map {
      case Array(key, aligned, recurring, venues, prefix, tokens, format, inner, matched, good, bad) =>
        DecorationScore.Row(key, DecorationScore.Features(aligned.toInt, recurring.toInt, venues.toInt, prefix.toBoolean, tokens.toInt,
          format.toDouble, inner.toDouble, matched.toDouble), good.toBoolean, bad.toBoolean)
      case other => throw new IllegalArgumentException(s"malformed training row: ${other.mkString("\t")}")
    }

  def line(row: DecorationScore.Row): String = {
    val f = row.features
    def fixed(x: Double) = String.format(java.util.Locale.ROOT, "%.4f", Double.box(x))
    Seq(row.key, f.aligned, f.recurring, f.venues, f.prefix, f.tokens, fixed(f.formatShare), fixed(f.innerResolved), fixed(f.matchedShare),
      row.good, row.bad).mkString("\t")
  }
}
