package services.review

import services.MongoConnection

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * The review answers from a terminal, against the store the dev pages write (`review_local.review_answers`
 * on the LOCAL mirror named by `MONGODB_MOVIES_MIRROR_URI`):
 *
 * {{{
 *   sbt "web/runMain services.review.ReviewLabelsCli import <all-answers.json>"
 *   sbt "web/runMain services.review.ReviewLabelsCli export [labels.tsv]"
 * }}}
 *
 * `import` adds the answers given on the hand-built review pages (skipping any already imported);
 * `export` writes every current answer into `labels.tsv` (the checkout's, by default) — the page's
 * "Export to labels.tsv" button. Both print what they did, contradiction warnings included.
 */
object ReviewLabelsCli {
  def main(args: Array[String]): Unit = {
    val configuration = settings.ProcessConfiguration.resolve()
    val mirror = configuration.mirrorMongoUri.getOrElse(sys.error("MONGODB_MOVIES_MIRROR_URI is not set: the answers live on the local mirror"))
    val client = MongoConnection.sharedClientFor(mirror.asMongoUri, Some(MongoConnection.ServerSelectionTimeout(MongoConnection.LocalMirrorTimeout)))
    try {
      val answers = new ReviewAnswers(new MongoReviewAnswerStore(client.getDatabase(MongoReviewAnswerStore.Database)))
      println(run(answers, args.toList))
    } finally client.close()
  }

  /** One command against `answers`, its report as text. */
  def run(answers: ReviewAnswers, args: List[String]): String = args match {
    case "import" :: path :: Nil =>
      val parsed = ReviewImport.parse(new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8))
      val added  = answers.importAll(parsed)
      (s"imported $added of ${parsed.size} answers (${parsed.size - added} already there)" +:
        parsed.flatMap(a => a.warnings.map(w => s"WARNING: ${a.country} ${a.title}: $w"))).mkString("\n")
    case "export" :: rest =>
      val path = rest.headOption.map(Paths.get(_)).getOrElse(LabelsTsv.locate())
      s"$path\n" + LabelsExport.exportTo(path, answers.current()).render
    case _ =>
      "usage: ReviewLabelsCli import <answers.json> | export [labels.tsv]"
  }
}
