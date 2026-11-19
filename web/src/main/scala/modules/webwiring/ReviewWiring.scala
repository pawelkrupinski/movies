package modules.webwiring

import controllers.{AgentJudgedCards, AgentJudgedController, ReviewAnswerController, ReviewCardsPage, ReviewLabels, ReviewListingController, ReviewQueueController, ReviewReads, ReviewWhyController, TmdbPosterController}
import play.api.Mode
import services.MongoConnection
import java.nio.file.Paths
import services.review.{CachingReviewSource, InMemoryReviewAnswerStore, MongoReviewAnswerStore, MongoReviewSource, ReviewAnswers, ReviewSource, TmdbPosterLookup}

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
      models.Country.all.map(c => c -> (new CachingReviewSource(
        new MongoReviewSource(client.getDatabase(MongoConnection.mirrorDbFor(c.mongoDb))), clock): ReviewSource)).toMap
    }

  lazy val reviewAnswers: ReviewAnswers = new ReviewAnswers(reviewMirrorClient.fold(new InMemoryReviewAnswerStore: services.review.ReviewAnswerStore)(
    client => new MongoReviewAnswerStore(client.getDatabase(MongoReviewAnswerStore.Database))))

  private lazy val reviewLabelsPath = processConfiguration.reviewLabelsFile.map(_.value).getOrElse(Paths.get("REVIEW_LABELS_FILE-not-set"))

  // A candidate's TMDB poster (and an unnamed film's title) asked live when no stored read gives one; none without a TMDB key.
  private lazy val reviewTmdbPosters: Option[TmdbPosterLookup] =
    Option.when(environmentMode != Mode.Prod)(processConfiguration.tmdbApiKey.map(new TmdbPosterLookup(httpFetch, _))).flatten
  // Why the pages may be empty or answers not kept, for their banner (no mirror, answers in memory, no labels file).
  private lazy val reviewNotices: Seq[String] = (if (reviewMirrorClient.isEmpty)
      Seq("MONGODB_MOVIES_MIRROR_URI is not set: these pages read only the local mirror (scripts/local-mirror/README.md), so they list nothing, and answers are kept in memory only.")
    else Nil) ++ (if (processConfiguration.reviewLabelsFile.isEmpty) Seq(ReviewAnswerController.LabelsNotConfigured.capitalize) else Nil)
  private lazy val reviewReads  = new ReviewReads(reviewSources)
  private lazy val reviewLabels = new ReviewLabels(reviewLabelsPath)
  private lazy val reviewCardsPage = new ReviewCardsPage(reviewAnswers, reviewReads, reviewNotices)

  lazy val reviewQueueController: ReviewQueueController =
    new ReviewQueueController(controllerComponents, environmentMode, reviewReads, reviewLabels, reviewCardsPage, clock)
  lazy val agentJudgedController: AgentJudgedController = new AgentJudgedController(controllerComponents, environmentMode,
    reviewAnswers, reviewLabels, reviewReads, new AgentJudgedCards(reviewTmdbPosters), reviewCardsPage)
  lazy val reviewWhyController: ReviewWhyController = new ReviewWhyController(controllerComponents, environmentMode, reviewReads)
  lazy val reviewListingController: ReviewListingController =
    new ReviewListingController(controllerComponents, environmentMode, reviewReads, reviewLabels, reviewNotices)
  lazy val tmdbPosterController: TmdbPosterController = new TmdbPosterController(controllerComponents, environmentMode, reviewTmdbPosters)

  lazy val reviewAnswerController: ReviewAnswerController = new ReviewAnswerController(controllerComponents, environmentMode,
    reviewSources, reviewAnswers, reviewLabelsPath, clock, labelsWritable = processConfiguration.reviewLabelsFile.isDefined)
}
