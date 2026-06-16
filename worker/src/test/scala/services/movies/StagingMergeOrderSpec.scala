package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.time.LocalDateTime

/**
 * Fast synthetic reproduction of the inline-staging cinema-merge order
 * dependence that `StagingOrderDeterminismSpec` surfaces on edge-case films
 * (Iron Maiden, Maraton horrorów): two cinemas reporting the SAME film with
 * different spelling/case land in separate records in some arrival orders. This
 * feeds a handful of same-`sanitize` scrapes through the REAL staging path —
 * `recordCinemaScrape` → a reaper tick between arrivals (`advanceStagingOnce`,
 * prod's inline cadence) → final fold — in EVERY arrival order, and asserts the
 * persisted records are byte-identical. Seconds, not the 14-min full corpus run.
 */
class StagingMergeOrderSpec extends AnyFlatSpec with Matchers {

  private val when = LocalDateTime.of(2026, 6, 9, 20, 0)

  private def scrape(title: String, cinema: Cinema, url: String, year: Option[Int] = None): (Cinema, Seq[CinemaMovie]) =
    cinema -> Seq(CinemaMovie(
      movie = Movie(title = title, releaseYear = year), cinema = cinema, posterUrl = None,
      filmUrl = Some(url), synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(when, Some(url)))))

  /** Drive the inline staging path for one arrival order: record each cinema,
   *  advance the reaper one step (as prod does between arrivals), then fold. */
  private def runOrder(order: Seq[(Cinema, Seq[CinemaMovie])]): Seq[StoredMovieRecord] = {
    val w = new FixtureTestWiring("08-06-2026")
    order.foreach { case (cinema, movies) =>
      w.movieCache.recordCinemaScrape(cinema, movies)
      w.advanceStagingOnce()
    }
    w.drainStaging()
    w.converge() // settle to the steady state, as the whole-corpus spec does
    w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
  }

  "the inline staging path" should
    "land two same-sanitize cinema spellings in one identical record regardless of arrival order" in {
    val scrapes = Seq(
      scrape("Iron Maiden: Burning Ambition", Helios, "http://a/iron"),
      scrape("Iron maiden: burning ambition", HeliosMagnolia, "http://b/iron")
    )
    val results = scrapes.permutations.map(runOrder).toSeq
    val ref = results.head
    withClue(s"records across ${results.size} arrival orders:\n" +
      results.map(rs => rs.map(r => (r.title, r.year, r.record.cinemaData.keySet)).mkString(" | ")).mkString("\n") + "\n") {
      results.foreach(_ shouldBe ref)
    }
    // The same film — they must collapse to ONE record carrying both cinemas.
    assert(ref.size == 1, s"expected one merged record, got ${ref.map(_.title)}")
    assert(ref.head.record.cinemaData.keySet == Set(Helios, HeliosMagnolia))
  }

  it should "resolve a Δ-year unresolved sibling into the same film regardless of arrival order" in {
    // Mirrors 'Głos Hind Rajab' (TMDB 1480382, 2025): most cinemas report 2025,
    // one (Kino Amondo) reports the production year 2022 — Δ3, outside the ±2
    // fold window, so the 2022 row only joins the film if it RESOLVES itself.
    // Whether it does flipped with arrival order (it resolves on its own title
    // search regardless of year, but only when it hadn't already concluded
    // no-match against a partial corpus first).
    val scrapes = Seq(
      scrape("Głos Hind Rajab", Helios,            "http://a/hind", year = Some(2025)),
      scrape("Głos Hind Rajab", HeliosMagnolia,    "http://b/hind", year = Some(2025)),
      scrape("Głos Hind Rajab", HeliosAlejaBielany, "http://c/hind", year = Some(2022))
    )
    val results = scrapes.permutations.map(runOrder).toSeq
    val ref = results.head
    withClue(s"records across ${results.size} arrival orders:\n" +
      results.map(rs => rs.map(r => (r.title, r.year, r.record.tmdbId, r.record.cinemaData.keySet)).mkString(" | ")).mkString("\n") + "\n") {
      results.foreach(_ shouldBe ref)
    }
  }
}
