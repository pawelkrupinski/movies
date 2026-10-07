package modules.webwiring

import controllers.ReviewController
import modules.Wiring
import play.api.Mode
import services.MongoConnection
import services.review.{CorpusCachingReviewSource, InMemoryReviewAnswerStore, LabelsTsv, MongoReviewAnswerStore, MongoReviewSource, ReviewAnswers, ReviewSource}

/** ── /debug/review ─────────────────────────────────────────────────────────
 *  The dev-only identity review pages. They read the LOCAL read-mirror and nothing else: with
 *  `MONGODB_MOVIES_MIRROR_URI` unset (or in prod) there are no sources, never a fall-back to the
 *  prod connection the rest of the app holds. Answers live on the same local instance, in
 *  `review_local.review_answers`. */
trait ReviewWiring { self: Wiring =>

  /** One client on the mirror for every country's review reads and the answer store; Dev only. */
  protected lazy val reviewMirrorClient: Option[org.mongodb.scala.MongoClient] =
    if (environmentMode == Mode.Prod) None
    else processConfiguration.mirrorMongoUri.map(mirror => managedResources.register("review mirror client",
      MongoConnection.sharedClientFor(mirror.asMongoUri, Some(MongoConnection.ServerSelectionTimeout(MongoConnection.LocalMirrorTimeout))))(_.close()))

  lazy val reviewSources: Map[models.Country, ReviewSource] =
    reviewMirrorClient.fold(Map.empty[models.Country, ReviewSource]) { client =>
      models.Country.all.map(c => c -> (new CorpusCachingReviewSource(
        new MongoReviewSource(client.getDatabase(MongoConnection.mirrorDbFor(c.mongoDb))), clock): ReviewSource)).toMap
    }

  lazy val reviewAnswers: ReviewAnswers = new ReviewAnswers(reviewMirrorClient.fold(new InMemoryReviewAnswerStore: services.review.ReviewAnswerStore)(
    client => new MongoReviewAnswerStore(client.getDatabase(MongoReviewAnswerStore.Database))))

  lazy val reviewController: ReviewController = new ReviewController(controllerComponents, environmentMode, reviewSources, reviewAnswers,
    LabelsTsv.locate(), clock,
    notices = if (reviewMirrorClient.isEmpty)
      Seq("MONGODB_MOVIES_MIRROR_URI is not set: these pages read only the local mirror (scripts/local-mirror/README.md), so they list nothing, and answers are kept in memory only.")
    else Nil)
}
