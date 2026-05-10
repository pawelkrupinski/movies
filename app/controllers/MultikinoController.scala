package controllers

import clients.{MultikinoShowtime, MultikinoScraper, Session}
import play.api.mvc._

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future, blocking}

/** Groups all showings for a single film across all dates. */
case class FilmSchedule(
  title: String,
  posterUrl: Option[String],
  filmUrl: Option[String],
  showings: Seq[(String, Seq[Session])]  // (date, sessions)
)

@Singleton
class MultikinoController @Inject() (cc: ControllerComponents)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  @volatile private var cache: Option[(Long, Seq[FilmSchedule])] = None
  private val CacheTtlMs = 60L * 60 * 1000  // 1 hour

  def index(): Action[AnyContent] = Action.async { implicit request =>
    Future {
      blocking { schedules() }
    }.map { films =>
      Ok(views.html.repertoire(films))
    }
  }

  def refresh(): Action[AnyContent] = Action.async { implicit request =>
    Future {
      blocking {
        cache = None
        schedules()
      }
    }.map { films =>
      Ok(views.html.repertoire(films))
    }
  }

  private def schedules(): Seq[FilmSchedule] = {
    val now = System.currentTimeMillis()
    cache match {
      case Some((ts, data)) if now - ts < CacheTtlMs => data
      case _ =>
        val data = toSchedules(MultikinoScraper.scrape())
        cache = Some((now, data))
        data
    }
  }

  private def toSchedules(showtimes: Seq[MultikinoShowtime]): Seq[FilmSchedule] =
    showtimes
      .groupBy(_.movieTitle)
      .toSeq
      .sortBy { case (_, entries) =>
        entries.flatMap(e => e.times.map(s => e.date + "T" + s.time)).minOption.getOrElse("9999")
      }
      .map { case (title, entries) =>
        FilmSchedule(
          title     = title,
          posterUrl = entries.flatMap(_.posterUrl).headOption,
          filmUrl   = entries.flatMap(_.filmUrl).headOption,
          showings  = entries.sortBy(_.date).map(e => e.date -> e.times)
        )
      }
}
