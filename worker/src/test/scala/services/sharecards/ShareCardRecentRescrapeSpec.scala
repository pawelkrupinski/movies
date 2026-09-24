package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.TaskType
import ShareCardTestKit.*

import java.time.Instant
import scala.concurrent.duration.*

/** A film in its first week is the one people are sharing, so when its card changes (a ratings
 *  update, a new poster) Facebook is asked to fetch its pages again — on the same task and metric
 *  as the repair of a film published before its card existed. */
class ShareCardRecentRescrapeSpec extends AnyFlatSpec with Matchers {

  private def rescrapes(rig: Rig): Seq[(String, Instant)] = {
    val tasks = Iterator.continually(rig.queue.claim("spec", 1.minute, T0.plusSeconds(365L * 86400))).takeWhile(_.isDefined).flatten.toSeq
    tasks.filter(_.taskType == TaskType.RescrapeShareCard).map(t => t.payload("filmId") -> Instant.EPOCH)
  }

  /** A rig whose clock reads `now`, over a store where `film` was first published at `published`. */
  private def rigWith(now: Instant, published: Option[Instant]): (Rig, models.ResolvedMovie) = {
    val movie = film()
    // The first card: the gate's (recording the publication) when there was one, else the backfill's.
    val first = new Rig(clock = clockAt(published.getOrElse(now)))
    first.service.render(first.service.inputs(movie), Seq(ShareCardReason.NewFilm), first = published.isDefined)
    val rig = new Rig(store = first.store, clock = clockAt(now))
    (rig, movie)
  }

  private def rerate(rig: Rig, movie: models.ResolvedMovie): Unit =
    rig.service.render(rig.service.inputs(movie.copy(ratings = movie.ratings.copy(imdb = Some(8.4)))), Seq(ShareCardReason.Ratings))

  "A film in its first week" should "have Facebook re-scrape its pages when its card changes" in {
    val (rig, movie) = rigWith(now = T0, published = Some(T0.minusSeconds(3 * 86400)))
    rerate(rig, movie)
    rescrapes(rig).map(_._1) shouldBe Seq(movie._id)
  }

  it should "be asked only after the new card can be on web_movies, and spaced from the others" in {
    val (rig, movie) = rigWith(now = T0, published = Some(T0.minusSeconds(86400)))
    rerate(rig, movie)
    rig.queue.claim("spec", 1.minute, T0.plusSeconds(10)) shouldBe None
    rig.queue.claim("spec", 1.minute, T0.plusSeconds(120)).map(_.taskType) shouldBe Some(TaskType.RescrapeShareCard)
  }

  "A film past its first week" should "not be re-scraped" in {
    val (rig, movie) = rigWith(now = T0, published = Some(T0.minusSeconds(8 * 86400)))
    rerate(rig, movie)
    rescrapes(rig) shouldBe empty
  }

  "A film published before first-publish times were kept" should "not be re-scraped" in {
    val (rig, movie) = rigWith(now = T0, published = None)
    rerate(rig, movie)
    rescrapes(rig) shouldBe empty
  }

  "A film's first card" should "not be a change: nothing was cached before it" in {
    val rig = new Rig
    rig.service.render(rig.service.inputs(film()), Seq(ShareCardReason.NewFilm), first = true)
    rescrapes(rig) shouldBe empty
    rig.store.published(rig.store.cardPath(film()._id)) shouldBe Some(T0)
    // …and every later card of the film carries the date forward.
    rig.service.render(rig.service.inputs(film().copy(title = "Diuna 2")), Seq(ShareCardReason.Title))
    rig.store.published(rig.store.cardPath(film()._id)) shouldBe Some(T0)
  }
}
