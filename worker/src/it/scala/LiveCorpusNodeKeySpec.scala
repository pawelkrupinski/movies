package services.identity

import integration.Reachable
import models.{CinemaMovie, Helios, KinoMuza, Kinoteka, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** A node is the listings sharing one evidence; the corpus keyed every listing by its OWN copy of
 *  the node's key — a tuple per listing where the nodes are far fewer (~3 MB of Tuple2 on worker-us). */
class LiveCorpusNodeKeySpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Titles     = 1000
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  "the live corpus" should "keep one node key per node, not one per listing" in {
    val listings = for {
      n      <- 0 until Titles
      cinema <- Seq(Helios, KinoMuza, Kinoteka)
    } yield Listing.of(cinema, CinemaMovie(Movie(s"Film $n"), cinema, None, None, None, Nil, Nil, Seq(show)), normalizer)
    val live = new LiveCorpus(NoFilmLookups, normalizer, PinConstraints(Nil), TitleDecorations.None)
    live.seen(listings)
    live.nodeCount shouldBe Titles
    val tuples = Reachable.count(live, outside = Seq(NoFilmLookups, normalizer)).getOrElse("scala.Tuple2", 0L)
    // Per node its key and its head; per listing its (venue, listing) group entry. 7,000 before: a
    // node key per LISTING.
    withClue(s"tuples the corpus reaches for ${listings.size} listings over $Titles nodes: ")(tuples should be <= 2L * Titles + listings.size + 10)
  }
}
