package tools

import models.{CinemaMovie, Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap

/** A resolve announces every detail page it will read (`IdentityLookups.prefetch`); the memo reads them ahead on its
 *  pool. Read one after another inside the resolve, decompressing and parsing the US's recorded pages was over a third
 *  of its 285 s on a four-core runner (JFR, 2026-10-15). */
class MemoLookupsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val show       = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  private def listing(n: Int) = Listing.of(Helios, CinemaMovie(Movie(s"Film $n"), Helios, None,
    Some(s"https://example.org/helios/$n"), None, Nil, Nil, Seq(show)), normalizer)

  private final class CountingLookups extends IdentityLookups {
    val read = new ConcurrentHashMap[String, Integer]()
    def hasDetail(l: Listing) = true
    def detail(l: Listing)    = { read.merge(l.page.get, 1, Integer.sum); Answer.Known(None) }
    def candidates(q: CandidateQuery) = Answer.Known(Nil)
    def film(id: Int)                 = Answer.Known(None)
  }

  "A memo's prefetch" should "read every announced detail page ahead, once, and serve the resolve's asks from it" in {
    val inner    = new CountingLookups
    val memo     = new MemoLookups(inner)
    val listings = (0 until 40).map(listing)
    memo.prefetch(Nil, Nil, listings)
    inner.read.size shouldBe 40
    listings.foreach(memo.detail)
    inner.read.values.toArray.map(_.asInstanceOf[Integer].intValue).toSet shouldBe Set(1)
  }
}
