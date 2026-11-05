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

  private class CountingLookups extends IdentityLookups {
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

  "A memo" should "ask its source every lookup the resolver can ask, not answer one with the trait's default" in {
    // a cast and a release day the source knows were read through the memo as unknown (`IdentityLookups`' defaults),
    // so no replay through it -- hermetic or live -- ever reached them
    val day   = java.time.LocalDate.of(1966, 3, 1)
    val inner = new CountingLookups {
      override def cast(id: Int)       = Answer.Known(Some(Seq("Ulrich Schamoni")))
      override def releaseDay(id: Int) = Answer.Known(Some(day))
      override def numbersFilmsByTmdbId = false
    }
    val memo = new MemoLookups(inner)
    memo.cast(133035) shouldBe Answer.Known(Some(Seq("Ulrich Schamoni")))
    memo.releaseDay(133035) shouldBe Answer.Known(Some(day))
    // a lookup newer than the oldest ref a slice runs under, passed through by name where the slice wires the memo
    OlderRefs.passing(classOf[IdentityLookups], memo, inner, OlderRefs.NewerLookups).numbersFilmsByTmdbId shouldBe false
  }

  /** A source whose prefetch must never run beside another of its own (StoredTmdbLookups': each prefetch lets go of the
   *  last one's documents, so two at once NPE'd in its `load`), counting what each prefetch was asked. */
  private class OnePrefetchAtATime extends CountingLookups {
    val inFlight   = new java.util.concurrent.atomic.AtomicInteger()
    val overlapped = new java.util.concurrent.atomic.AtomicBoolean()
    val asked      = new java.util.concurrent.ConcurrentLinkedQueue[CandidateQuery]()
    override def prefetch(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[Listing]): Unit = {
      if (inFlight.incrementAndGet() > 1) overlapped.set(true)
      qs.foreach(asked.add)
      Thread.sleep(5)
      inFlight.decrementAndGet(); ()
    }
  }

  "A memo shared by concurrent family resolves" should "never run two of its source's prefetches at once" in {
    val inner = new OnePrefetchAtATime
    val memo  = new MemoLookups(inner)
    val pool  = java.util.concurrent.Executors.newFixedThreadPool(8)
    try (0 until 32).map(n => pool.submit(new java.util.concurrent.Callable[Unit] {
      def call(): Unit = memo.prefetch(Seq(CandidateQuery.Title(s"Film $n")), Nil, Nil)
    })).foreach(_.get()) finally pool.shutdown()
    inner.overlapped.get shouldBe false
  }

  it should "not prefetch again what it already holds: a family's prefetch of the corpus's answers reaches no source" in {
    val inner = new OnePrefetchAtATime
    val memo  = new MemoLookups(inner)
    memo.prefetch(Seq(CandidateQuery.Title("Film 1"), CandidateQuery.Title("Film 2")), Nil, Nil)
    memo.prefetch(Seq(CandidateQuery.Title("Film 2"), CandidateQuery.Title("Film 3")), Nil, Nil)
    memo.prefetch(Seq(CandidateQuery.Title("Film 1")), Nil, Nil)
    import scala.jdk.CollectionConverters._
    inner.asked.asScala.toSeq shouldBe Seq(CandidateQuery.Title("Film 1"), CandidateQuery.Title("Film 2"), CandidateQuery.Title("Film 3"))
  }
}
