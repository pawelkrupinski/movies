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
 *   sbt "web/runMain services.review.ReviewLabelsCli export [identity-judgements.tsv]"
 *   sbt "web/runMain services.review.ReviewLabelsCli queue [50] [170]"
 * }}}
 *
 * `import` adds the answers given on the hand-built review pages (skipping any already imported);
 * `export` writes every current answer into `identity-judgements.tsv` (the checkout's, by default) — the export the
 * pages run on each answer as it is given — for answers given while no page was writing the file. Both print what they did,
 * contradiction warnings included. `queue` measures the queue page's order ([[ReviewQueueReport]]): how many listings
 * the first answers settle, per country and merged.
 */
object ReviewLabelsCli {
  def main(args: Array[String]): Unit = {
    val configuration = settings.ProcessConfiguration.resolve()
    val mirror = configuration.mirrorMongoUri.getOrElse(sys.error("MONGODB_MOVIES_MIRROR_URI is not set: the answers live on the local mirror"))
    val client = MongoConnection.sharedClientFor(mirror.asMongoUri, Some(MongoConnection.ServerSelectionTimeout(MongoConnection.LocalMirrorTimeout)))
    try {
      val answers = new ReviewAnswers(new MongoReviewAnswerStore(client.getDatabase(MongoReviewAnswerStore.Database)))
      val sources: Map[models.Country, ReviewSource] = models.Country.all.map(c =>
        c -> (new MongoReviewSource(client.getDatabase(MongoConnection.mirrorDbFor(c.mongoDb))): ReviewSource)).toMap
      println(run(answers, args.toList, sources, configuration.reviewLabelsFile.map(_.value)))
    } finally client.close()
  }

  private def noLabelsFile: Nothing = sys.error("no judgements file: set REVIEW_LABELS_FILE (scripts/review-server does) or pass the path")

  /** One command against `answers`, its report as text. */
  def run(answers: ReviewAnswers, args: List[String], sources: Map[models.Country, ReviewSource] = Map.empty,
          labelsFile: Option[java.nio.file.Path] = None): String = args match {
    case "import" :: path :: Nil =>
      val parsed = ReviewImport.parse(new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8))
      val added  = answers.importAll(parsed)
      (s"imported $added of ${parsed.size} answers (${parsed.size - added} already there)" +:
        parsed.flatMap(a => a.warnings.map(w => s"WARNING: ${a.country} ${a.title}: $w"))).mkString("\n")
    case "export" :: rest =>
      val path = rest.headOption.map(Paths.get(_)).orElse(labelsFile).getOrElse(noLabelsFile)
      val history = answers.history()
      s"$path\n" + LabelsExport.exportTo(path, history, FilmIdentity.linking(history, sources)).render
    case "queue" :: rest =>
      val top     = rest.headOption.flatMap(_.toIntOption).getOrElse(50)
      val first   = rest.drop(1).headOption.flatMap(_.toIntOption).getOrElse(170)
      val judged  = new JudgedListings(LabelsTsv.read(labelsFile.getOrElse(noLabelsFile)))
      val index   = new ReviewAnswers.Index(answers.current())
      ReviewQueueReport.render(sources.toSeq.sortBy(_._1.code).map { case (c, s) => ReviewQueueReport.queued(c, s, judged, index) }, top, first)
    case _ =>
      "usage: ReviewLabelsCli import <answers.json> | export [identity-judgements.tsv] | queue [first per country] [first merged]"
  }
}
