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
 * `review_answers` store and from there into `identity-judgements.tsv`.
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
                       notices: Seq[String] = Nil,
                       // A candidate's TMDB poster asked live when no stored read gives one; none without a TMDB key.
                       tmdbPosters: Option[TmdbPosterLookup] = None,
                       // False when no labels file was configured (`REVIEW_LABELS_FILE`): answers are kept, never written.
                       labelsWritable: Boolean = true) extends AbstractController(cc) {

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
  private def clustersOf(countries: Seq[Country], unmatchedOnly: Boolean): (Seq[ReviewCluster], Seq[String]) =
    chosenOf(countries, unmatchedOnly)(_ => identity)

  /** What `choose` picks of each selected country's clusters, each country's on its own thread; a failed read reported
   *  rather than shown as no clusters. */
  private def chosenOf[A](countries: Seq[Country], unmatchedOnly: Boolean)(choose: Country => Seq[ReviewCluster] => Seq[A]): (Seq[A], Seq[String]) = {
    val read = perCountry(countries)(c => choose(c)(clustersKept.get(sources(c).decisions(unmatchedOnly), _.map(ReviewCluster.of(c, _)))))
    (read.collect { case (_, Success(cs)) => cs }.flatten,
      read.collect { case (c, Failure(e)) => s"${c.code}: could not read identity_model_families (${e.getMessage})" })
  }

  /** The labels file's rows; one that cannot be read is said so on the page, never shown as "no labels". */
  private def readLabels(): (Seq[LabelRow], Option[String]) = Try(LabelsTsv.read(labelsPath)) match {
    case Success(rows) => (rows, None)
    case Failure(e)    => (Nil, Some(s"could not read $labelsPath: ${e.getMessage}"))
  }

  private def render(page: ReviewPage, country: Option[String], selected: Seq[(ReviewCluster, Option[Instant])], limit: Int,
                     showAnswered: Boolean, errors: Seq[String], controls: ReviewView.Controls,
                     labelsRead: (Seq[LabelRow], Option[String]) = readLabels(),
                     priorities: Map[String, ReviewPriority] = Map.empty): Result = {
    val index    = new ReviewAnswers.Index(answers.current())
    val (done, notDone) = selected.partition { case (c, _) => index.answerFor(c.id, c.reviewMembers).isDefined }
    val open     = if (showAnswered) selected else notDone
    val shown    = open.take(limit)
    val (labels, labelsError) = labelsRead
    val byCountry = shown.groupBy(_._1.country)
    val cards    = perCountry(byCountry.keys.toSeq)(c => ReviewCards.build(sources(c), byCountry(c), labels, index)).flatMap(_._2.get)
      .map(card => card.copy(priority = priorities.get(card.cluster.id)))
    val order    = shown.map(_._1.id).zipWithIndex.toMap
    val view     = ReviewView(page, country.getOrElse("all"), cards.sortBy(card => order(card.cluster.id)), total = open.size,
      answeredHidden = selected.size - open.size, done.size, showAnswered, limit, controls, errors ++ labelsError ++ notices)
    Ok(views.html.review(view)).withHeaders("Content-Security-Policy" -> modules.CspFilter.WithGoogleFonts)
  }

  def queue(country: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val labelsRead = readLabels()
      val judged     = new JudgedListings(labelsRead._1)
      // each country's clusters ranked against what it serves, the countries side by side, then merged by the same order
      val (chosen, errors) = chosenOf(countriesOf(country), unmatchedOnly = false)(c => ReviewSelection.queue(_, sources(c).served(), judged))
      val ranked = chosen.sortBy { case (c, p) => (-p.score, -p.listings, c.title) }
      render(ReviewPage.Queue, country, ranked.map(_._1 -> None), limit, answered, errors, ReviewView.Controls(), labelsRead,
        ranked.map { case (c, p) => c.id -> p }.toMap)
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
      val updating  = countries.map(c => c -> Future(blocking(Try(sources(c).updatedSince(since))))(using ExecutionContext.global)).toMap
      // each country's clusters chosen by its own slot times, the countries side by side: no merged corpus is built
      val (chosen, errors) = chosenOf(countries, unmatchedOnly = false) { c => clusters =>
        ReviewSelection.recent(clusters, Await.result(updating(c), Duration.Inf).getOrElse(Map.empty), since)
      }
      render(ReviewPage.Recent, country, ReviewSelection.byConfidence(chosen).map { case (c, at) => c -> Some(at) },
        limit, answered, errors, ReviewView.Controls(hours = Some(hours)))
    }
  }

  /** The review digest: every take awaiting the user's word ([[ReviewDigest.pending]]) — the agents' judgements and
   *  the candidates they flagged — by country, each with its film named (TMDB's title, asked live, when the row files
   *  none) and the answers the other pages post. Reads no mirror: the judgements file, the candidates beside it and
   *  the answers. */
  def digest(country: Option[String]): Action[AnyContent] = Action {
    devOnly {
      val (labels, labelsError) = readLabels()
      val candidatesPath        = ReviewDigest.candidatesPath(labelsPath)
      val (candidates, candidatesError) = Try(ReviewDigest.readCandidates(candidatesPath)) match {
        case Success(read) => (read, None)
        case Failure(e)    => (Nil, Some(s"could not read $candidatesPath: ${e.getMessage}"))
      }
      val wanted  = country.filterNot(_ == "all").flatMap(Country.byCode).map(_.code)
      val pending = ReviewDigest.pending(labels, candidates, ReviewDigest.answered(answers.current()))
        .filter(i => wanted.forall(_ == i.country))
      // a film the row files no title for, named as TMDB names it in the country's language, the films asked side by side
      val unnamed = pending.filter(_.filmTitle.isEmpty).flatMap(i => i.film.tmdb.map(_ -> i.country)).distinct
      val looked  = tmdbPosters.fold(Map.empty[(Int, String), String]) { lookup =>
        unnamed.map { case key @ (id, cc) =>
          key -> Future(blocking(lookup.film(id, Country.byCode(cc).fold(java.util.Locale.UK)(_.language))))(using ExecutionContext.global)
        }.flatMap { case (key, film) => Await.result(film, Duration.Inf).flatMap(_.describe).map(key -> _) }.toMap
      }
      val items   = pending.map(i => if (i.filmTitle.isDefined) i else i.copy(filmTitle = i.film.tmdb.flatMap(id => looked.get(id -> i.country))))
      Ok(views.html.reviewDigest(ReviewDigestView(country.getOrElse("all"), items, labelsPath,
        labelsError.toSeq ++ candidatesError ++ notices)))
        .withHeaders("Content-Security-Policy" -> modules.CspFilter.WithGoogleFonts)
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
            case Success(traces) => Ok(views.html.reviewWhy(found.members.map(k => k -> traces.get(services.movies.ListingKey.serialised(k))), Some(c)))
            case Failure(e)      => InternalServerError(s"could not read identity_traces: ${e.getMessage}")
          }
      }
    }
  }

  /** One production listing explained ([[ListingExplain]]): its take, where its take history is, the latest decision
   *  trace, its judgements and its cluster on these pages — the listings `venue` (every venue when left out) bills as
   *  `title` exactly, else those whose title contains it to pick from. */
  def listing(country: String, venue: Option[String], title: String): Action[AnyContent] = Action {
    devOnly {
      val wantedVenue = venue.map(_.trim).filter(_.nonEmpty)
      Country.byCode(country).filter(sources.contains) match {
        case None    => NotFound(s"no mirror holds country $country")
        case Some(c) =>
          Try(sources(c).decisions(unmatchedOnly = false)) match {
            case Failure(e)         => InternalServerError(s"could not read identity_model_families: ${e.getMessage}")
            case Success(decisions) =>
              val keys     = if (title.isEmpty) Nil else ListingExplain.find(decisions, wantedVenue, title)
              val labels   = readLabels()
              val explains = keys.map(ListingExplain.build(c, _, sources(c), labels._1))
              val near     = if (keys.isEmpty && title.nonEmpty) ListingExplain.near(decisions, wantedVenue, title) else Nil
              val said     = Option.when(title.nonEmpty && keys.isEmpty && near.isEmpty)(s"no listing in ${c.code} is billed as “$title”")
              Ok(views.html.reviewListing(ListingExplainView(c, wantedVenue, title, explains, near, said.toSeq ++ labels._2 ++ notices)))
                .withHeaders("Content-Security-Policy" -> modules.CspFilter.WithGoogleFonts)
          }
      }
    }
  }

  /** A candidate's TMDB poster, looked up live ([[TmdbPosterLookup]]) in its country's language: a redirect to it, or not
   *  found — the card's "no TMDB poster" placeholder then takes the image's place. */
  def tmdbPoster(tmdbId: Int, country: Option[String]): Action[AnyContent] = Action {
    devOnly {
      val language = country.flatMap(Country.byCode).fold(java.util.Locale.UK)(_.language)
      tmdbPosters.flatMap(_.poster(tmdbId, language)).fold(NotFound("no TMDB poster"))(Redirect(_))
    }
  }

  /** Records one answer, and writes it into `identity-judgements.tsv` at once ([[exported]]); answers with the warnings
   *  the venues' own facts raise against it, the film it named, and what the export did — or why it could not: the
   *  answer is kept either way, and the next answer's export (or the export route) catches the file up. */
  def answer(): Action[JsValue] = Action(parse.tolerantJson) { request =>
    devOnly {
      AnswerRequest.parse(request.body, request.session.get("userId").getOrElse("dev"), clock.instant()) match {
        case Left(why)     => BadRequest(Json.obj("error" -> why))
        case Right(answer) =>
          answers.record(answer)
          val written = exported() match {
            case Success(summary) => Json.obj("exported" -> summary.render, "path" -> labelsPath.toString)
            case Failure(e)       => Json.obj("exportError" -> s"not written to $labelsPath: ${e.getMessage}")
          }
          Ok(Json.obj("ok" -> true, "verdict" -> answer.verdict.code, "warnings" -> answer.warnings,
            "ref" -> answer.ref.map(_.render), "url" -> answer.ref.flatMap(_.url)) ++ written)
      }
    }
  }

  /** Brings the checkout's `identity-judgements.tsv` up to the answers given since the last export ([[LabelsExport.exportTo]]). */
  def exportLabels(): Action[AnyContent] = Action {
    devOnly {
      exported() match {
        case Success(summary) => Ok(Json.obj("summary" -> summary.render, "added" -> summary.added, "flipped" -> summary.flipped,
          "unchanged" -> summary.unchanged, "alreadyUsed" -> summary.alreadyUsed, "withdrawn" -> summary.withdrawn, "warnings" -> summary.warnings, "path" -> labelsPath.toString))
        case Failure(e)       => InternalServerError(Json.obj("error" -> s"not written to $labelsPath: ${e.getMessage}"))
      }
    }
  }

  // One export at a time: each reads the file, merges, and writes it back whole.
  private val exporting = new Object

  /** The labels file brought up to every answer given: what the export did. */
  private def exported(): Try[LabelsExport.Summary] = exporting.synchronized {
    Try {
      if (!labelsWritable) throw new IllegalStateException(ReviewController.LabelsNotConfigured)
      val history = answers.history()
      LabelsExport.exportTo(labelsPath, history, FilmIdentity.linking(history, sources))
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
  val DefaultLimit = 20
  val LabelsNotConfigured = "REVIEW_LABELS_FILE is not set, so no judgements file is written (start the server with scripts/review-server, " +
    "from the review worktree); the answer is kept and the next export catches the file up"
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

/** Everything `reviewDigest.scala.html` renders: the takes awaiting the user, and where their answers are written. */
final case class ReviewDigestView(country: String, items: Seq[DigestItem], labelsPath: Path, notices: Seq[String]) {
  /** The items by country, in `Country.all`'s order. */
  def byCountry: Seq[(String, Seq[DigestItem])] = {
    val order = Country.all.map(_.code).zipWithIndex.toMap
    items.groupBy(_.country).toSeq.sortBy { case (cc, _) => (order.getOrElse(cc, Int.MaxValue), cc) }
  }

  /** A listing's explain page ([[ReviewController.listing]]): at its venue, or at every venue for a `*` row. */
  def explainUrl(item: DigestItem): String =
    routes.ReviewController.listing(item.country, Option.when(item.venue != "*")(item.venue), item.rawTitle).url
}

/** Everything `reviewListing.scala.html` renders: the query, each listing it names explained, or the listings near it. */
final case class ListingExplainView(countryOf: Country, venue: Option[String], title: String, explains: Seq[ListingExplain],
                                    near: Seq[services.movies.ListingKey], notices: Seq[String]) {
  def country: String = countryOf.code
}

object ReviewView {
  final case class Controls(min: Option[Double] = None, basis: Option[ResolverDecision.Basis] = None,
                            byBasis: Seq[(ResolverDecision.Basis, Int)] = Nil, hours: Option[Int] = None)

  def pathOf(page: ReviewPage): String = page match {
    case ReviewPage.Queue     => "/debug/review"
    case ReviewPage.Matchable => "/debug/review/matchable"
    case ReviewPage.Recent    => "/debug/review/recent"
    case ReviewPage.Digest    => "/debug/review/digest"
  }
}
