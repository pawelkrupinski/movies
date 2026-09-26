package services.identity

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ListingIntake.{Offer, Outcome}
import services.movies.{ScrapeGuardState, ScrapeHealth, SingleCountryNormalizer}

import java.time.LocalDateTime

/** The cut-over venue's listing after a scrape: the old landing's scrape-health rules, deciding
 *  the evidence set instead of which slots to write and prune. */
class ListingIntakeSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val now        = LocalDateTime.of(2026, 9, 26, 12, 0)

  private def film(title: String, showtimes: Int = 3): CinemaMovie =
    CinemaMovie(Movie(title), KinoApollo, None, None, None, Nil, Nil,
      (1 to showtimes).map(d => Showtime(now.plusDays(d.toLong), None)))

  private val board: Seq[CinemaMovie] = (1 to 10).map(i => film(s"Film $i"))

  private def decide(known: Seq[CinemaMovie], fresh: Seq[CinemaMovie], guard: ScrapeGuardState = ScrapeGuardState.Fresh,
                     complete: Boolean = true, source: Option[String] = None, fallback: Boolean = false) =
    ListingIntake.decide(KinoApollo, known, Offer(fresh, complete, source, fallback), guard, now,
      ScrapeHealth.MaxConsecutiveDepthRejections, normalizer)

  "A healthy scrape" should "replace the venue's listing, withdrawing the film it stopped listing" in {
    val next = board.tail :+ film("Film 11")
    val v    = decide(board, next)
    v.outcome shouldBe Outcome.Replaced
    v.accepted shouldBe next
  }

  "An empty scrape" should "keep the listing: a silent failure is not an empty programme" in {
    val v = decide(board, Nil)
    v.outcome shouldBe Outcome.Kept
    v.accepted shouldBe board
  }

  "A scrape whose upcoming showtimes collapsed" should "be discarded until the depth guard's grace runs out" in {
    val thin = board.map(_.copy(showtimes = Seq(Showtime(now.plusDays(1), None))))
    val first = decide(board, thin)
    first.outcome shouldBe Outcome.Kept
    first.accepted shouldBe board
    first.guard.depthRejections shouldBe 1
    val exhausted = decide(board, thin, ScrapeGuardState(depthRejections = ScrapeHealth.MaxConsecutiveDepthRejections))
    exhausted.outcome shouldBe Outcome.Replaced
    exhausted.accepted shouldBe thin
    exhausted.guard.depthRejections shouldBe 0
  }

  "A short scrape" should "be ADDED — what it failed to mention keeps its showtimes — until the breadth guard gives up" in {
    val short = board.take(3)
    val v = decide(board, short.map(_.copy(showtimes = (1 to 30).map(d => Showtime(now.plusHours(d.toLong), None)))))
    v.outcome shouldBe Outcome.Added
    v.accepted.map(_.movie.title).toSet shouldBe board.map(_.movie.title).toSet
    v.guard.breadthRejections shouldBe 1
  }

  "A scrape the scraper knows is incomplete" should "never replace the listing" in {
    val v = decide(board, board.take(9), guard = ScrapeGuardState(breadthRejections = 50), complete = false)
    v.outcome shouldBe Outcome.Added
    v.accepted.map(_.movie.title).toSet shouldBe board.map(_.movie.title).toSet
  }

  "A fallback-served scrape" should "add to the listing and leave the guards' state alone" in {
    val guard = ScrapeGuardState(Some("primary"), 1, 2)
    val fresher = film("Film 1", showtimes = 5)
    val v = decide(board, Seq(fresher, film("Film 12")), guard, fallback = true)
    v.outcome shouldBe Outcome.Added
    v.guard shouldBe guard
    v.accepted.count(_.movie.title == "Film 1") shouldBe 1
    v.accepted.find(_.movie.title == "Film 1").get.showtimes.size shouldBe 5
    v.accepted.map(_.movie.title) should contain("Film 12")
  }

  "A scrape from another upstream" should "replace the listing however few it lists, and record the new source" in {
    val v = decide(board, Seq(film("Other")), ScrapeGuardState(sourceKey = Some("old")), source = Some("new"))
    v.outcome shouldBe Outcome.Replaced
    v.accepted.map(_.movie.title) shouldBe Seq("Other")
    v.guard shouldBe ScrapeGuardState(sourceKey = Some("new"))
  }
}
