package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.lang.ref.WeakReference
import scala.collection.mutable

/** `IdentityResolver.askAll`, the shadow fill's question walk: every question a resolve of the same
 *  listings asks, each once, while holding no chunk's answers past that chunk — a whole-corpus
 *  resolve holds every answer and film record at once, which is what OOMed worker-uk on
 *  2026-09-28 (two of them alive together: 2 × 28,324 listings in the dump). */
class IdentityResolverAskAllSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val Seeds      = 1L to 20L

  /** Every call, and every answer handed out as a fresh instance a weak reference can watch. */
  private class Watched(inner: IdentityLookups, watchFirst: Int = 0) extends IdentityLookups {
    val calls   = mutable.ArrayBuffer.empty[String]
    val watched = mutable.ArrayBuffer.empty[WeakReference[AnyRef]]
    var onCall: Int => Unit = _ => ()
    private def handOut[A <: AnyRef](a: A): A = { if (watched.size < watchFirst) watched += new WeakReference[AnyRef](a); a }
    private def called(what: String): Unit = { calls += what; onCall(calls.size) }
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = { called(s"detail ${l.venue} ${l.page}"); inner.detail(l) }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = {
      called(q.sortKey)
      inner.candidates(q) match {
        case Answer.Known(hits) => Answer.Known(hits.map(h => handOut(h.copy())))
        case unknown            => unknown
      }
    }
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = { called(s"film $id"); inner.film(id) }
  }

  "askAll" should "ask exactly the questions a resolve of the same listings asks, each query and film once, however small its chunks" in {
    Seeds.foreach { seed =>
      val corpus  = GeneratedIdentityCorpus.generate(seed, normalizer)
      val resolve = new Watched(corpus.lookups)
      IdentityResolver.resolve(corpus.listings, resolve, normalizer)
      Seq(1, 7, corpus.listings.size).foreach { chunk =>
        val asked = new Watched(corpus.lookups)
        IdentityResolver.askAll(corpus.listings, asked, normalizer, chunk)
        withClue(s"seed $seed, chunk $chunk: ") {
          asked.calls.toSet shouldBe resolve.calls.toSet
          val searches = asked.calls.filterNot(_.startsWith("detail "))
          searches.size shouldBe searches.distinct.size
        }
      }
    }
  }

  it should "hold none of a chunk's answers once it has moved past that chunk" in {
    val corpus = GeneratedIdentityCorpus.generate(1L, normalizer, films = 30, listings = 120)
    // A dry walk to learn how many calls the walk makes: the check runs on its very last one.
    val dry = new Watched(corpus.lookups)
    IdentityResolver.askAll(corpus.listings, dry, normalizer, chunk = 10)
    val last = dry.calls.size

    val lookups = new Watched(corpus.lookups, watchFirst = 3)
    var alive   = -1
    lookups.onCall = n => if (n == last) {
      (1 to 10).takeWhile { _ => System.gc(); lookups.watched.exists(_.get != null) }
      alive = lookups.watched.count(_.get != null)
    }
    IdentityResolver.askAll(corpus.listings, lookups, normalizer, chunk = 10)
    lookups.watched.size shouldBe 3
    alive shouldBe 0
  }
}
