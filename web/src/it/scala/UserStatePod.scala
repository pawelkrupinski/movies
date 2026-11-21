package integration

import controllers.UserStateController
import models.User
import org.mongodb.scala.MongoDatabase
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.{FakeRequest, Helpers}
import services.users._

import java.time.{Clock, Instant}
import scala.concurrent.Future

/** One web pod's user-state stack as `UsersWiring` builds it — the Mongo store over the pod's own
 *  view of the shared database and the controller in front of it — so a
 *  spec can run two of them against one database the way a rolling deploy does.
 *
 *  `users` is shared between pods: sign-in is not what these specs race, and `SignedInUser` only
 *  needs the row to exist. */
final class UserStatePod(
  db:            MongoDatabase,
  users:         UserRepository,
  clock:         Clock,
  writeOutcomes: UserStateWriteOutcomes = UserStateWriteOutcomes.none,
  indexHealth:   UserStateIndexHealth   = UserStateIndexHealth.none
) extends AutoCloseable {
  val states     = new services.users.MongoUserStateRepository(Some(db), writeOutcomes = writeOutcomes, indexHealth = indexHealth)
  val controller: UserStateController = UserStatePod.controller(states, users, clock)

  import UserStatePod.session
  def hide(userId: String, title: String, country: String = "pl"): Future[Result] =
    controller.hideFilm(country, title)(session(FakeRequest("PUT", s"/api/me/$country/hidden-films/x"), userId))
  def unhide(userId: String, title: String, country: String = "pl"): Future[Result] =
    controller.unhideFilm(country, title)(session(FakeRequest("DELETE", s"/api/me/$country/hidden-films/x"), userId))
  def putLanguage(userId: String, language: String): Future[Result] =
    controller.put()(session(FakeRequest("PUT", "/api/me/state"), userId).withBody(Json.obj("language" -> language)))
  def hiddenFilms(userId: String, ifModifiedSince: Option[String] = None, country: String = "pl"): Future[Result] = {
    val request = session(FakeRequest("GET", s"/api/me/$country/hidden-films"), userId)
    controller.hiddenFilms(country)(ifModifiedSince.fold(request)(ims => request.withHeaders("If-Modified-Since" -> ims)))
  }

  def close(): Unit = states.close()
}

object UserStatePod {
  /** The controller over `states`, with the metrics and clock a spec pins. Shared with
   *  `HiddenFilmsConcurrentWritesIntegrationSpec`, which drives one pod's controller. */
  def controller(states: UserStateRepository, users: UserRepository, clock: Clock): UserStateController =
    new UserStateController(Helpers.stubControllerComponents(), states, users, clock)

  /** A user row for `id`, so `SignedInUser` accepts a session naming it. Returns `id`. */
  def signIn(users: UserRepository, id: String): String = {
    users.upsert(User(id = id, provider = "google", providerSub = s"G-$id", email = None, displayName = None,
      avatarUrl = None, createdAt = Instant.EPOCH, lastSeenAt = Instant.EPOCH))
    id
  }

  private def session[A](request: FakeRequest[A], userId: String): FakeRequest[A] = request.withSession("userId" -> userId)
}
