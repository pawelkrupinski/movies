package controllers

import java.nio.file.Path
import java.time.Clock

import scala.util.{Failure, Success, Try}

import models.Country
import play.api.Mode
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.*
import services.review.*

/**
 * The dev-only review pages' writes (`/debug/review/answer`, `/export`) and the answers' history: an answer goes to
 * the local `review_answers` store and from there into `identity-judgements.tsv`. The pages themselves are
 * [[ReviewQueueController]]'s and [[AgentJudgedController]]'s. Production 404s every route here, like the rest of `/debug`.
 */
class ReviewAnswerController(cc: ControllerComponents,
                             environment: Mode,
                             // What links a film across databases at export ([[FilmIdentity.linking]]).
                             sources: Map[Country, ReviewSource],
                             answers: ReviewAnswers,
                             labelsPath: Path,
                             clock: Clock,
                             // False when no labels file was configured (`REVIEW_LABELS_FILE`): answers are kept, never written.
                             labelsWritable: Boolean = true) extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

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
      if (!labelsWritable) throw new IllegalStateException(ReviewAnswerController.LabelsNotConfigured)
      answers.exportTo(labelsPath, sources)
    }
  }

  /** Every answer ever given, oldest first — the store as data. */
  def history(): Action[AnyContent] = Action {
    devOnly {
      Ok(Json.toJson(answers.history().map(ReviewAnswer.historyJson)))
    }
  }
}

object ReviewAnswerController {
  val LabelsNotConfigured = "REVIEW_LABELS_FILE is not set, so no judgements file is written (start the server with scripts/review-server, " +
    "from the review worktree); the answer is kept and the next export catches the file up"
}
