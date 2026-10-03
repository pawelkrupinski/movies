package services.identity

import integration.Reachable
import models.{CinemaMovie, Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** What the model keeps per listing beside the corpus: which listing it holds and which family it is
 *  in. Two maps over the same keys kept a hash node each per listing, and the family id boxed —
 *  ~12 MB on worker-us's ~104k listings (live dump, 2026-09-29). */
class IncrementalResolverFootprintSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Listings   = 2000
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  "the model" should "keep one entry per listing for what it holds and which family it is in" in {
    val listings = (0 until Listings).map(n => Listing.of(Helios, CinemaMovie(Movie(s"Film $n"), Helios, None,
      Some(s"https://helios.pl/film/$n"), None, Nil, Nil, Seq(show)), normalizer))
    val model = new IncrementalResolver(NoFilmLookups, normalizer, IdentityCalibration.resolver)
    model.seed(listings)
    model.familyOf.size shouldBe Listings
    val kept  = Reachable.count(model, outside = Seq(NoFilmLookups, normalizer))
    val nodes = kept.getOrElse("scala.collection.mutable.HashMap$Node", 0L)
    val boxed = kept.getOrElse("java.lang.Integer", 0L)
    info(s"per listing: ${nodes.toDouble / Listings} hash nodes, ${boxed.toDouble / Listings} boxed Integers")
    // 22 nodes and 7.6 boxed Integers per listing across the whole model while `held` and
    // `familyOfKey` were two maps; one entry per key takes a node and the family id's box off each.
    withClue("hash nodes per listing: ")(nodes.toDouble / Listings should be < 21.5)
    withClue("boxed Integers per listing: ")(boxed.toDouble / Listings should be < 7.0)
  }
}
