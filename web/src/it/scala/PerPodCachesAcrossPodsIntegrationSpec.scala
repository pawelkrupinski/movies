package integration

import tools.SpecTimeouts
import models.{CityScreening, ResolvedMovie, ResolvedRatings, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import services.readmodel.{MongoReadModelRepository, TestReadModel}
import services.users.InMemoryUserRepository
import tools.ConcurrentInstances
import tools.Eventually.eventually

import java.time.{Instant, LocalDateTime}

/**
 * Every web pod keeps its own caches in front of Mongo, and every one of them answers "not
 * modified" from what it holds. A cache that never hears about ANOTHER pod's write tells that
 * pod's clients nothing changed when it did — the failure df4146340 removed two whole caches for.
 * The one that remains is fed by a change stream precisely so the other pod's write reaches it;
 * these specs hold it to that, and hold the user state — cached nowhere now — to answering from
 * the shared row:
 *
 *  - `WebReadModel.lastModifiedFor(city)` — the per-city validator behind the ETag and the 304,
 *    moved on every pod by a write the worker made;
 *  - the hidden-films `If-Modified-Since` — another pod's write is a 200 at once.
 */
class PerPodCachesAcrossPodsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  // Well inside anything a stream needs.
  private val StreamBoundMs = SpecTimeouts.Settle.toMillis

  private def hiddenAfter(pod: UserStatePod, userId: String, ifModifiedSince: String): (Int, String) = {
    val result = pod.hiddenFilms(userId, Some(ifModifiedSince))
    (status(result), if (status(result) == OK) contentAsString(result) else "")
  }

  // No pod caches the user state any more: an `If-Modified-Since` is answered from the shared row, so
  // another pod's write is a 200 at once, with no stream to wait on.
  "a pod answering If-Modified-Since" should "304 its own still-current write, and 200 at once after ANOTHER pod's" in
    ConcurrentInstances.withInstances(mongoTarget, "user-state-two-pods") { instances =>
      val users = new InMemoryUserRepository
      val writes    = new tools.MutableClock(Instant.parse("2026-06-01T10:00:00Z"))
      val Seq(a, b) = instances.map(instance => new UserStatePod(instance.database, users, writes))
      try {
        val userId = UserStatePod.signIn(users, "two-pods")
        val write  = a.hide(userId, "Seen on A")
        status(write) shouldBe OK
        val lastModified = header("Last-Modified", write).get
        hiddenAfter(a, userId, lastModified)._1 shouldBe NOT_MODIFIED

        // A later second: inside A's one, an If-Modified-Since cannot tell the two writes apart.
        writes.advanceSeconds(1)
        status(b.hide(userId, "Hidden on B")) shouldBe OK
        val (code, body) = hiddenAfter(a, userId, lastModified)
        code shouldBe OK
        body should include ("Hidden on B")
      } finally Seq(a, b).foreach(_.close())
    }

  private val ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private val film    = ResolvedMovie("f-two-pods", "Diuna", None, None, Nil, None, Some(2021), Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def screening(hour: Int) =
    CityScreening(s"${film._id}|poznan|Kino Muza", film._id, "poznan", "Kino Muza", None,
      Seq(Showtime(LocalDateTime.of(2026, 6, 2, hour, 0), None)))

  "every web pod's per-city validator" should "move, and serve the change, when the worker writes that city" in
    ConcurrentInstances.withInstances(mongoTarget, "per-city-validator-two-pods", count = 3) { instances =>
      val Seq(podA, podB, worker) = instances
      val writer = new MongoReadModelRepository(Some(worker.database), stopwatch = _root_.tools.Stopwatch.System)
      writer.upsertMovie(film)
      writer.upsertScreening(screening(18))
      val pods = Seq(podA, podB).map(pod => TestReadModel.kept(new MongoReadModelRepository(Some(pod.database), stopwatch = _root_.tools.Stopwatch.System), clock = _root_.tools.SpecClock.Pinned))
      try {
        pods.foreach(_.start())
        pods.foreach(pod => pod.screeningsForCity("poznan").flatMap(_.showtimes).map(_.dateTime.getHour) shouldBe Seq(18))
        val before = pods.map(_.lastModifiedFor("poznan"))

        writer.upsertScreening(screening(21))

        pods.zip(before).zipWithIndex.foreach { case ((pod, validator), i) =>
          withClue(s"web pod ${i + 1}: ") {
            eventually({
              pod.screeningsForCity("poznan").flatMap(_.showtimes).map(_.dateTime.getHour) shouldBe Seq(21)
              pod.lastModifiedFor("poznan").isAfter(validator) shouldBe true
            }, timeoutMs = StreamBoundMs, pollMs = 50)
          }
        }
      } finally pods.foreach(_.stop())
    }
}
