package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.time.Instant

/** A film in its first week is the one people are sharing, so when its card changes (a ratings
 *  update, a new poster) Facebook is asked to fetch its pages again — through the fleet's re-scrape
 *  queue, as is the repair of a film published before its card existed. Never onto the task
 *  queue: a re-scrape there held a task-worker slot while it waited on Facebook, and a burst of
 *  them queued ahead of the card renders. */
class ShareCardRecentRescrapeSpec extends AnyFlatSpec with Matchers {

  /** The films waiting in the fleet's queue, and when each is due. */
  private def rescrapes(rig: Rig): Seq[(String, Instant)] =
    rig.rescrapeStore.waiting.collect { case RescrapeEntry(_, RescrapeTarget.FilmPages(_, filmId), notBefore, _) => filmId -> notBefore }

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

  it should "be asked only after the new card can be on web_movies" in {
    val (rig, movie) = rigWith(now = T0, published = Some(T0.minusSeconds(86400)))
    rerate(rig, movie)
    rescrapes(rig) shouldBe Seq(movie._id -> T0.plusMillis(ShareCardService.RescrapeDelay.toMillis))
  }

  it should "leave nothing on the task queue: the card renders never wait behind re-scrapes" in {
    val (rig, movie) = rigWith(now = T0, published = Some(T0.minusSeconds(86400)))
    rerate(rig, movie)
    drain(rig.queue) shouldBe empty
  }

  "The landing of a pending film's card" should "queue each film once in the fleet's queue" in {
    val rig = new Rig
    rig.service.onPendingCardLanded("fa"); rig.service.onPendingCardLanded("fb"); rig.service.onPendingCardLanded("fa")
    rescrapes(rig).map(_._1) shouldBe Seq("fa", "fb")
    drain(rig.queue) shouldBe empty
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
