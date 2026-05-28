package controllers

import models._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.Mode

import java.time.format.DateTimeFormatter

// One showing in the JSON blob the /plan template embeds for its JS
// scheduler. Flat by design — the client builds movie / cinema / day
// indexes on the fly because the filter axes interact and a server-side
// pre-grouping would just have to be re-flattened on every toggle.
case class PlanShowing(
  movie:  String,
  cinema: String,
  room:   Option[String],
  date:   String,
  time:   String
)

object PlanShowing {
  implicit val writes: Writes[PlanShowing] = Json.writes[PlanShowing]
}

// Rooms picker source: one entry per cinema that has any room-tagged
// showing in the current snapshot. Cinemas with zero room data drop out
// (the per-cinema room section is hidden for them — no checkboxes to
// show). `pillName` mirrors the navbar pill label so the section header
// reads "Apollo", not "Kino Apollo".
case class PlanCinemaRooms(cinema: String, pillName: String, rooms: Seq[String])

case class PlanViewData(
  // Carries the poster + enrichment so the picker can render full movie
  // cards (poster, title link, runtime/year pills, rating badges) —
  // visually identical to the / and /kina cards minus the showings
  // block. Sorted by title for stable picker order.
  films:       Seq[FilmSchedule],
  showings:    Seq[PlanShowing],
  cinemaRooms: Seq[PlanCinemaRooms]
)

class PlanController(
  cc:                     ControllerComponents,
  movieControllerService: MovieControllerService,
  userRepo:               services.users.UserRepo,
  oauthProviders:         Set[String],
  environment:            Mode
) extends AbstractController(cc) with Logging {

  private def currentUser(request: RequestHeader): Option[models.User] =
    request.session.get("userId").flatMap(userRepo.findById)

  def plan(): Action[AnyContent] = Action { request =>
    val user      = currentUser(request)
    val schedules = movieControllerService.toSchedules()
    val data      = PlanController.viewData(schedules)
    Ok(views.html.plan(
      data,
      Cinema.all.map(_.displayName),
      Cinema.pillMap,
      devMode, user, oauthProviders
    ))
  }

  private def devMode: Boolean = environment != Mode.Prod
}

object PlanController {
  private val DateFmt = DateTimeFormatter.ISO_LOCAL_DATE
  private val TimeFmt = DateTimeFormatter.ofPattern("HH:mm")

  // Pure view-model builder so the snapshot spec can call it with a
  // fixture-pinned `toSchedules(now)` and render the template directly,
  // bypassing the controller's wall-clock dependency.
  def viewData(schedules: Seq[FilmSchedule]): PlanViewData = {
    val films: Seq[FilmSchedule] = schedules.sortBy(_.movie.title.toLowerCase)

    val showings: Seq[PlanShowing] = schedules.flatMap { fs =>
      fs.showings.flatMap { case (date, cinemas) =>
        cinemas.flatMap { cs =>
          cs.showtimes.map { st =>
            PlanShowing(
              movie  = fs.movie.title,
              cinema = cs.cinema.displayName,
              room   = st.room.map(_.trim).filter(_.nonEmpty),
              date   = date.format(DateFmt),
              time   = st.dateTime.format(TimeFmt)
            )
          }
        }
      }
    }

    val cinemaRooms: Seq[PlanCinemaRooms] = Cinema.all.flatMap { c =>
      val rooms = showings
        .filter(_.cinema == c.displayName)
        .flatMap(_.room).distinct.sorted
      if (rooms.isEmpty) None
      else Some(PlanCinemaRooms(c.displayName, c.pillName, rooms))
    }

    PlanViewData(films, showings, cinemaRooms)
  }
}
