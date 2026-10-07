package controllers

import models.Country
import play.api.Mode
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.identity.ResolverDecision
import services.review._

import java.nio.file.Path
import java.time.{Clock, Instant}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.{Failure, Success, Try}

/**
 * The dev-only identity review pages (`/debug/review*`): the clusters the resolver left unmatched, those
 * it rated matchable and still left, and those it matched lately — each a card with the venues' own
 * facts, the films it weighed, its explanation, and the answer buttons. Answers go to the local
 * `review_answers` store and from there into `labels.tsv`.
 *
 * Every read is of the LOCAL read-mirror (`sources`, built by the wiring from
 * `MONGODB_MOVIES_MIRROR_URI` alone); with no mirror configured the pages say so and list nothing.
 * Production 404s every route here, like the rest of `/debug`.
 */
class ReviewController(cc: ControllerComponents,
                       environment: Mode,
                       sources: Map[Country, ReviewSource],
                       answers: ReviewAnswers,
                       labelsPath: Path,
                       clock: Clock,
                       // Why the pages may be empty or answers not kept, for the banner (no mirror, answers in memory).
                       notices: Seq[String] = Nil) extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  private def countriesOf(code: Option[String]): Seq[Country] =
    code.filterNot(_ == "all").flatMap(Country.byCode).fold(Country.all.filter(sources.contains))(c => Seq(c).filter(sources.contains))

  /** `read` of each country, side by side: a page over every country waits on its slowest, not on their sum. Each
   *  read bounds its own Mongo calls. */
  private def perCountry[A](countries: Seq[Country])(read: Country => A): Seq[(Country, Try[A])] = {
    val reads = countries.map(c => c -> Future(blocking(Try(read(c))))(using ExecutionContext.global))
    reads.map { case (c, f) => c -> Await.result(f, Duration.Inf) }
  }

  // The clusters of each decisions read, kept as long as the read is: a source that keeps its reads answers the same
  // one until it reads again, so its clusters, their ids and member keys are worked out once. Weak keys compare by
  // identity; an empty read (`Nil`, shared) is no clusters whichever country it came from.
  private val clustersKept = tools.BoundedCache.ofSize(16).weakKeys().build[Seq[ResolverDecision], Seq[ReviewCluster]]()

  /** Each selected country's clusters, a failed read reported rather than shown as no clusters. */
  private def clustersOf(countries: Seq[Country], unmatchedOnly: Boolean): (Seq[ReviewCluster], Seq[String]) = {
    val read = perCountry(countries)(c => clustersKept.get(sources(c).decisions(unmatchedOnly), _.map(ReviewCluster.of(c, _))))
    (read.collect { case (_, Success(cs)) => cs }.flatten,
      read.collect { case (c, Failure(e)) => s"${c.code}: could not read identity_model_families (${e.getMessage})" })
  }

  private def render(page: ReviewPage, country: Option[String], selected: Seq[(ReviewCluster, Option[Instant])], limit: Int,
                     showAnswered: Boolean, errors: Seq[String], controls: ReviewView.Controls): Result = {
    val index    = new ReviewAnswers.Index(answers.current())
    val open     = if (showAnswered) selected else selected.filter { case (c, _) => index.answerFor(c.id, c.reviewMembers).isEmpty }
    val shown    = open.take(limit)
    // A labels file that cannot be read is said so on the page, never shown as "no labels".
    val (labels, labelsError) = Try(LabelsTsv.read(labelsPath)) match {
      case Success(rows) => (rows, None)
      case Failure(e)    => (Nil, Some(s"could not read $labelsPath: ${e.getMessage}"))
    }
    val byCountry = shown.groupBy(_._1.country)
    val cards    = perCountry(byCountry.keys.toSeq)(c => ReviewCards.build(sources(c), byCountry(c), labels, index)).flatMap(_._2.get)
    val order    = shown.map(_._1.id).zipWithIndex.toMap
    val answered = selected.count { case (c, _) => index.answerFor(c.id, c.reviewMembers).isDefined }
    val view     = ReviewView(page, country.getOrElse("all"), cards.sortBy(card => order(card.cluster.id)), total = open.size,
      answeredHidden = selected.size - open.size, answered, showAnswered, limit, controls, errors ++ labelsError ++ notices)
    Ok(views.html.review(view)).withHeaders("Content-Security-Policy" -> modules.CspFilter.WithGoogleFonts)
  }

  def queue(country: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val (clusters, errors) = clustersOf(countriesOf(country), unmatchedOnly = true)
      render(ReviewPage.Queue, country, ReviewSelection.queue(clusters).map(_ -> None), limit, answered, errors, ReviewView.Controls())
    }
  }

  def matchable(country: Option[String], min: Double, basis: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val (clusters, errors) = clustersOf(countriesOf(country), unmatchedOnly = true)
      val chosen = basis.flatMap(b => ResolverDecision.Basis.values.find(_.toString == b))
      render(ReviewPage.Matchable, country, ReviewSelection.matchable(clusters, min, chosen).map(_ -> None), limit, answered, errors,
        ReviewView.Controls(min = Some(min), basis = chosen, byBasis = ReviewSelection.matchableByBasis(clusters, min)))
    }
  }

  def recent(country: Option[String], hours: Int, limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val countries = countriesOf(country)
      val since     = clock.instant().minusSeconds(hours.toLong * 3600)
      // the slot times read while the decisions are: neither waits on the other
      val updating  = Future(blocking(perCountry(countries)(sources(_).updatedSince(since))))(using ExecutionContext.global)
      val (clusters, errors) = clustersOf(countries, unmatchedOnly = false)
      val updated   = Await.result(updating, Duration.Inf).flatMap(_._2.toOption).flatten.toMap
      render(ReviewPage.Recent, country, ReviewSelection.recent(clusters, updated, since).map { case (c, at) => c -> Some(at) },
        limit, answered, errors, ReviewView.Controls(hours = Some(hours)))
    }
  }

  /** One card's "Why" fold-out, read when it is opened: each member listing's trace — the evidence for and against
   *  the film it weighed, every rule's refusal, the candidates it scored and what it searched. */
  def why(country: String, cluster: String): Action[AnyContent] = Action {
    devOnly {
      Country.byCode(country).filter(sources.contains).flatMap { c =>
        Try(sources(c).decisions(unmatchedOnly = false)).toOption.flatMap(_.iterator.map(ReviewCluster.of(c, _)).find(_.id == cluster))
          .map(found => c -> found)
      } match {
        case None             => NotFound(s"no cluster $cluster in $country")
        case Some((c, found)) =>
          Try(sources(c).traces(found.members.map(services.movies.ListingKey.serialised))) match {
            case Success(traces) => Ok(views.html.reviewWhy(found.members.map(k => k -> traces.get(services.movies.ListingKey.serialised(k)))))
            case Failure(e)      => InternalServerError(s"could not read identity_traces: ${e.getMessage}")
          }
      }
    }
  }

  /** Records one answer; answers with the warnings the venues' own facts raise against it, and the film it named. */
  def answer(): Action[JsValue] = Action(parse.tolerantJson) { request =>
    devOnly {
      AnswerRequest.parse(request.body, request.session.get("userId").getOrElse("dev"), clock.instant()) match {
        case Left(why)     => BadRequest(Json.obj("error" -> why))
        case Right(answer) =>
          answers.record(answer)
          Ok(Json.obj("ok" -> true, "verdict" -> answer.verdict.code, "warnings" -> answer.warnings,
            "ref" -> answer.ref.map(_.render), "url" -> answer.ref.flatMap(_.url)))
      }
    }
  }

  /** Writes every current answer into the checkout's `labels.tsv`. */
  def exportLabels(): Action[AnyContent] = Action {
    devOnly {
      val current = answers.current()
      val summary = LabelsExport.exportTo(labelsPath, current, FilmIdentity.linking(current, sources))
      Ok(Json.obj("summary" -> summary.render, "added" -> summary.added, "flipped" -> summary.flipped,
        "unchanged" -> summary.unchanged, "warnings" -> summary.warnings, "path" -> labelsPath.toString))
    }
  }

  /** Every answer ever given, oldest first — the store as data. */
  def history(): Action[AnyContent] = Action {
    devOnly {
      Ok(Json.toJson(answers.history().map(a => Json.obj("clusterId" -> a.clusterId, "country" -> a.country,
        "page" -> a.page.code, "verdict" -> a.verdict.code, "ref" -> a.ref.map(_.render), "shown" -> a.shown.map(_.ref.render),
        "title" -> a.title, "who" -> a.who, "at" -> a.at.toString, "warnings" -> a.warnings, "legacyId" -> a.legacyId))))
    }
  }
}

object ReviewController {
  val DefaultLimit = 60
}

/** Everything `review.scala.html` renders. */
final case class ReviewView(page: ReviewPage, country: String, cards: Seq[ReviewCard], total: Int, answeredHidden: Int,
                            // every listed cluster with an answer, hidden or not — the progress bar's numerator
                            answered: Int, showAnswered: Boolean, limit: Int, controls: ReviewView.Controls, notices: Seq[String]) {
  /** This page's URL with one query parameter changed. */
  def link(changes: (String, Option[String])*): String = {
    val base = Seq("country" -> Some(country), "limit" -> Option.when(limit != ReviewController.DefaultLimit)(limit.toString),
      "answered" -> Option.when(showAnswered)("true"), "min" -> controls.min.map(_.toString),
      "basis" -> controls.basis.map(_.toString), "hours" -> controls.hours.map(_.toString))
    val merged = changes.foldLeft(base.toMap)((m, c) => m + c)
    val query  = base.map(_._1).flatMap(k => merged.get(k).flatten.map(v => s"$k=${java.net.URLEncoder.encode(v, "UTF-8")}"))
    ReviewView.pathOf(page) + (if (query.isEmpty) "" else query.mkString("?", "&", ""))
  }
}

object ReviewView {
  final case class Controls(min: Option[Double] = None, basis: Option[ResolverDecision.Basis] = None,
                            byBasis: Seq[(ResolverDecision.Basis, Int)] = Nil, hours: Option[Int] = None)

  def pathOf(page: ReviewPage): String = page match {
    case ReviewPage.Queue     => "/debug/review"
    case ReviewPage.Matchable => "/debug/review/matchable"
    case ReviewPage.Recent    => "/debug/review/recent"
  }
}
