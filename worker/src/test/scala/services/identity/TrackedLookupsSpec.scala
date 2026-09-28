package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ConcurrentLinkedQueue, Executors}
import scala.jdk.CollectionConverters._

/** The model's lookups: a prefetch asks its questions in parallel, each read still filed under its
 *  own question; the asks that follow are served once each from what it fetched; and the next
 *  prefetch drops what nobody asked, so an answer that changed since is never served stale. */
class TrackedLookupsSpec extends AnyFlatSpec with Matchers {

  /** A store: every question reads the key `q:<sortKey>`, on whatever thread asks, and answers
   *  what `answers` says now. */
  private final class Store(reads: ObservationReads) extends IdentityLookups {
    @volatile var answers: Map[CandidateQuery, Seq[Hit]] = Map.empty
    val askedOn = new ConcurrentLinkedQueue[(CandidateQuery, String)]()
    def hasDetail(listing: Listing): Boolean = false
    def detail(listing: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = {
      askedOn.add(query -> Thread.currentThread().getName)
      reads.read("q:" + query.sortKey)
      Answer.Known(answers.getOrElse(query, Nil))
    }
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
  }

  private val queries = (1 to 20).map(i => CandidateQuery.Title(s"Film $i"))
  private def pool() = Executors.newFixedThreadPool(4, { (task: Runnable) => new Thread(task, "prefetch") })

  "a prefetch" should "ask its questions on the pool, and serve each ask that follows from what it fetched" in {
    val reads  = new ObservationReads
    val store  = new Store(reads)
    store.answers = queries.zipWithIndex.map { case (q, i) => q -> Seq(Hit(i, s"Film $i", None, None, 1)) }.toMap
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    queries.map(lookups.candidates) shouldBe queries.map(q => Answer.Known(store.answers(q)))
    store.askedOn.asScala.map(_._1).toSeq.sortBy(_.sortKey) shouldBe queries.sortBy(_.sortKey)   // each once
    store.askedOn.asScala.map(_._2).toSet shouldBe Set("prefetch")
  }

  it should "file every read under its own question, whatever thread made it" in {
    val reads   = new ObservationReads
    val lookups = new TrackedLookups(new Store(reads), reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    queries.foreach(q => reads.changedBy(Seq("q:" + q.sortKey)).queries shouldBe Set(q))
  }

  "a released question" should "leave nothing behind in the reads index" in {
    val reads   = new ObservationReads
    val lookups = new TrackedLookups(new Store(reads), reads, Some(pool()))
    lookups.prefetch(queries, Nil, Nil)
    reads.keys shouldBe queries.size
    lookups.released(queries, Nil, Nil)
    reads.keys shouldBe 0
  }

  "a prefetch" should "never serve a later ask what an earlier prefetch fetched and nobody asked" in {
    val reads   = new ObservationReads
    val store   = new Store(reads)
    val lookups = new TrackedLookups(store, reads, Some(pool()))
    val query   = queries.head
    lookups.prefetch(Seq(query), Nil, Nil)
    store.answers = Map(query -> Seq(Hit(9, "Film 1", None, Some(2026), 5)))   // the fill filed a new answer
    lookups.prefetch(Nil, Nil, Nil)                                              // the next read phase
    lookups.candidates(query) shouldBe Answer.Known(Seq(Hit(9, "Film 1", None, Some(2026), 5)))
  }
}
