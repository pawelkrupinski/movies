package services.identity

import models.{CinemaMovie, Helios, KinoMuza, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** Family ids number the live families, reused as families go: a counter that only grew put every
 *  id past the JVM's small-integer cache within a day, and each one was boxed in five maps and sets
 *  (~6 boxed Integers per listing, ~10 MB on worker-us). Kept below the live count, they fit the
 *  worker's raised cache (`-XX:AutoBoxCacheMax`, infra/jvm/worker.options). */
class IncrementalResolverFamilyIdSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  private object NoLookups extends IdentityLookups {
    def hasDetail(l: Listing) = false
    def detail(l: Listing)    = Answer.Known(None)
    def candidates(q: CandidateQuery) = Answer.Known(Nil)
    def film(id: Int)                 = Answer.Known(None)
  }
  private def listing(cinema: models.Cinema, n: Int) = Listing.of(cinema, CinemaMovie(Movie(s"Film $n"), cinema, None,
    Some(s"https://example.org/${cinema.displayName}/$n"), None, Nil, Nil, Seq(show)), normalizer)

  "the model" should "keep its family ids below the number of families it holds, however often they are re-resolved" in {
    val model  = new IncrementalResolver(NoLookups, normalizer, IdentityCalibration.resolver)
    val helios = (0 until 50).map(listing(Helios, _))
    val muza   = (0 until 50).map(listing(KinoMuza, _))
    model.seed(helios ++ muza)
    (1 to 20).foreach { _ =>
      model.listingsGone(muza.map(_.key))
      model.listingsSeen(muza)
    }
    val ids = model.familyOf.values.toSet
    withClue(s"${ids.size} families, ids up to ${ids.max}: ")(ids.max should be < ids.size)
  }
}
