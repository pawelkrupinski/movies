package services.identity

import integration.Reachable
import models.{CinemaMovie, Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** What a resolve hands the trace store keeps alive until the store's one writer thread gets to it — and a
 *  seed or restore hands over the WHOLE corpus at once. The refusal reasons of 3f4bf656a were a thunk closing
 *  over each unmatched listing's scored candidates, so the pending hand-over held every candidate list of the
 *  corpus: worker-uk (~7k unmatched listings, a 537 MB heap) ran out of heap restoring its model on every boot,
 *  2026-10-02 22:21Z onwards, about every 7 minutes. */
class IdentityTraceRetentionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Listings   = 300
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  // Every title search finds three films no listing's facts fit, so every listing is scored and refused.
  private object Namesakes extends IdentityLookups {
    private val ids = Seq(9001, 9002, 9003)
    def hasDetail(l: Listing) = false
    def detail(l: Listing)    = Answer.Known(None)
    def candidates(q: CandidateQuery) = Answer.Known(q match {
      case CandidateQuery.Title(text) => ids.map(id => Hit(id, s"$text $id", None, Some(1950), 1.0))
      case _                          => Nil
    })
    def film(id: Int) = Answer.Known(Some(IdentityMeasures.Film(s"Namesake $id", None, Nil, Some(1950), Some(80),
      Some(Seq("Someone Else")), None, Some(1.0))))
  }

  "a resolve's trace hand-over" should "keep no scored candidate alive until it is written" in {
    var pending: Option[Seq[FamilyTraces]] = None
    val held = new IdentityTraceStore { def replace(removed: Set[String], added: Seq[FamilyTraces]): Unit = pending = Some(added) }
    val listings = (0 until Listings).map(n => Listing.of(Helios, CinemaMovie(Movie(s"Film $n", releaseYear = Some(2099)), Helios, None,
      Some(s"https://helios.pl/film/$n"), None, Nil, Nil, Seq(show)), normalizer))
    new IncrementalResolver(Namesakes, normalizer, IdentityCalibration.resolver, traces = held).seed(listings)
    val handOver = pending.getOrElse(fail("no trace hand-over"))
    handOver.iterator.flatMap(_.build()).flatMap(_.rules).count(_.startsWith("refused:")) should be > 0
    val kept = Reachable.count(handOver, outside = Seq(Namesakes, normalizer, IdentityCalibration.resolver))
    withClue("scored candidates the pending hand-over keeps: ")(kept.getOrElse("services.identity.Scored", 0L) shouldBe 0L)
  }
}
