package services.identity

import org.bson.{BsonDocument, BsonInt32}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/** The cache keeps the documents' storage contract… */
class CachedTmdbDocumentsContractSpec extends TmdbDocumentRetentionBehaviour {
  protected def newDocuments(): TmdbDocuments & TmdbDocumentRetention = new CachedTmdbDocuments(new InMemoryTmdbDocuments)
}

/** …and answers a document it read before, unchanged since, without asking the backend again: every
 *  projection tick re-read the same TMDB answers (worker-us: 237 `tmdb_films` finds in 90 s). */
class CachedTmdbDocumentsSpec extends AnyFlatSpec with Matchers {

  /** An in-memory backend that records every id an answer read asked it for. */
  private final class CountingDocuments extends TmdbDocuments with TmdbDocumentRetention {
    val held  = new InMemoryTmdbDocuments
    val asked = new ConcurrentLinkedQueue[String]()
    @volatile var beforeAnswer: () => Unit = () => ()
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = held.get(kind, ids)
    override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
      ids.foreach(asked.add); val got = held.answers(kind, ids); beforeAnswer(); got
    }
    @volatile var beforePut: () => Unit = () => ()
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = { beforePut(); held.put(kind, docs) }
    def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = held.fetchedBefore(kind, cutoff)
    def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int = held.deleteIfStill(kind, stamped)
    def drain(): Seq[String] = { val all = asked.asScala.toSeq; asked.clear(); all }
  }

  private val language = "en-US"
  private def hit(id: Int, title: String) = Hit(id, title, None, Some(2020), 5.0)

  /** The worker's own wiring: the store over the coalescing over the cache over the backend. */
  private final class World {
    val backend = new CountingDocuments
    val cached  = new CachedTmdbDocuments(backend)
    val store   = new TmdbStore(new CoalescedTmdbDocuments(cached), Clock.fixed(Instant.parse("2026-10-04T10:00:00Z"), ZoneOffset.UTC))
    val lookups = new StoredTmdbLookups(store, language, UnansweredTmdbLookups, new ObservationReads)
    def tick(queries: Seq[CandidateQuery]): Seq[Option[Seq[Int]]] = {
      lookups.prefetch(queries, Nil, Nil)
      val answered = queries.map(q => lookups.candidates(q).toOption.map(_.map(_.tmdbId)))
      lookups.prefetchAnswered()
      answered
    }
  }

  "a projection tick" should "read no document the last tick read and nothing has rewritten since" in {
    val w = new World
    (1 to 20).foreach(i => w.store.question(TmdbStore.titleSearchId(language, s"Film $i"), Seq(hit(100 + i, s"Film $i"))))
    val queries = (1 to 20).map(i => CandidateQuery.Title(s"Film $i")) :+ CandidateQuery.Title("Never asked TMDB")
    w.tick(queries) shouldBe (1 to 20).map(i => Some(Seq(100 + i))) :+ None
    w.backend.drain() should not be empty
    w.tick(queries) shouldBe (1 to 20).map(i => Some(Seq(100 + i))) :+ None
    w.backend.drain() shouldBe empty
  }

  it should "read again exactly the documents a refresh rewrote, and answer from their new value" in {
    val w = new World
    val search = TmdbStore.titleSearchId(language, "Lalka")
    w.store.question(search, Seq(hit(1018, "Lalka")))
    val queries = Seq(CandidateQuery.Title("Lalka"), CandidateQuery.Title("Nowa"))
    w.tick(queries) shouldBe Seq(Some(Seq(1018)), None)
    w.backend.drain()
    // The refresh: the search now names a second film, and a question never answered gets its answer.
    w.store.question(search, Seq(hit(1018, "Lalka"), hit(2001, "Lalka 2")))
    w.store.question(TmdbStore.titleSearchId(language, "Nowa"), Seq(hit(3003, "Nowa")))
    w.tick(queries) shouldBe Seq(Some(Seq(1018, 2001)), Some(Seq(3003)))
    w.backend.drain().toSet shouldBe Set(search, TmdbStore.titleSearchId(language, "Nowa"), "2001", "3003")
  }

  private def recordDoc(film: IdentityMeasures.Film) = new BsonDocument("record", IdentityAnswerBson.film(Some(film)))
  private val lalka = IdentityMeasures.Film("Lalka", Some("Lalka"), year = Some(2025), popularity = Some(7.3))

  // A resolve asks every film its questions name for its hit, and the hit of a recorded film is read off its record:
  // keeping the record for it held the whole film beside the record the corpus keeps decoded — 31k of worker-uk's 33k
  // cached film answers, 9.2 MiB of their 12.3 (heap dump 2026-10-07).
  "a resolve's candidates" should "keep each named film's hit, not its record, answering it as the record reads it" in {
    val w = new World
    w.backend.put(TmdbKind.Film, Seq("1018" -> recordDoc(lalka)))
    w.store.question(TmdbStore.titleSearchId(language, "Lalka"), Seq(hit(1018, "Lalka")))
    val q = CandidateQuery.Title("Lalka")
    w.lookups.prefetch(Seq(q), Nil, Nil)
    w.lookups.candidates(q) shouldBe Answer.Known(TmdbStore.filmHit(1018, recordDoc(lalka)).toSeq)
    w.lookups.prefetchAnswered()
    w.cached.holdsAnswer(TmdbKind.Film, "1018") shouldBe false
    w.backend.drain()
    w.tick(Seq(q)) shouldBe Seq(Some(Seq(1018)))
    w.backend.drain() shouldBe empty                                  // the hit is held, so nothing is read again
  }

  it should "answer a rewritten film's new hit" in {
    val w = new World
    w.backend.put(TmdbKind.Film, Seq("1018" -> recordDoc(lalka)))
    w.store.question(TmdbStore.titleSearchId(language, "Lalka"), Seq(hit(1018, "Lalka")))
    val q = CandidateQuery.Title("Lalka")
    w.tick(Seq(q))
    w.cached.put(TmdbKind.Film, Seq("1018" -> recordDoc(lalka.copy(title = "Lalka (2025)", popularity = Some(9.9)))))
    w.lookups.prefetch(Seq(q), Nil, Nil)
    w.lookups.candidates(q).toOption.toSeq.flatten.map(h => h.title -> h.popularity) shouldBe Seq("Lalka (2025)" -> 9.9)
  }

  // The corpus decodes each record it loads and keeps that ([[LiveCorpus]]'s `records`): the cache keeping its bytes too
  // was the same film twice.
  "a record the corpus loads" should "be read through, not kept" in {
    val w = new World
    w.backend.put(TmdbKind.Film, Seq("1018" -> recordDoc(lalka)))
    w.lookups.prefetch(Nil, Seq(1018), Nil)
    w.lookups.film(1018).toOption.flatten.map(_.title) shouldBe Some("Lalka")
    w.lookups.prefetchAnswered()
    w.cached.holdsAnswer(TmdbKind.Film, "1018") shouldBe false
  }

  "a record asked alone" should "still be answered from the cache when asked again" in {
    val w = new World
    w.backend.put(TmdbKind.Film, Seq("1018" -> recordDoc(lalka)))
    w.lookups.film(1018).toOption.flatten.map(_.title) shouldBe Some("Lalka")
    w.backend.drain()
    w.lookups.film(1018).toOption.flatten.map(_.title) shouldBe Some("Lalka")
    w.backend.drain() shouldBe empty
  }

  "a document the sweep deleted" should "be read again, not answered from the cache" in {
    val w = new World
    w.backend.put(TmdbKind.Query, Seq("q" -> new BsonDocument("ids", new org.bson.BsonArray()).append(TmdbStore.FetchedAt, org.bson.BsonInt64(1))))
    w.cached.answers(TmdbKind.Query, Seq("q")).keySet shouldBe Set("q")
    w.cached.deleteIfStill(TmdbKind.Query, w.cached.fetchedBefore(TmdbKind.Query, 10)) shouldBe 1
    w.cached.answers(TmdbKind.Query, Seq("q")) shouldBe empty
  }

  "a read that raced a write" should "not keep the value the write replaced" in {
    val w = new World
    w.cached.put(TmdbKind.Film, Seq("7" -> new BsonDocument("hit", new BsonDocument("n", BsonInt32(1)))))
    // The read has its old value in hand when the write lands: the write must win the next read.
    w.backend.beforeAnswer = () => {
      w.backend.beforeAnswer = () => ()
      w.cached.put(TmdbKind.Film, Seq("7" -> new BsonDocument("hit", new BsonDocument("n", BsonInt32(2)))))
    }
    w.cached.answers(TmdbKind.Film, Seq("7"))("7").getDocument("hit").getInt32("n").getValue shouldBe 1
    w.cached.answers(TmdbKind.Film, Seq("7"))("7").getDocument("hit").getInt32("n").getValue shouldBe 2
  }

  it should "not keep the value it read while a write was in flight, though the write landed first" in {
    val w = new World
    def n(i: Int) = new BsonDocument("hit", new BsonDocument("n", BsonInt32(i)))
    w.cached.put(TmdbKind.Film, Seq("7" -> n(1)))
    val inFlight = new java.util.concurrent.CountDownLatch(1)
    val readOld  = new java.util.concurrent.CountDownLatch(1)
    val landed   = new java.util.concurrent.CountDownLatch(1)
    w.backend.beforePut = () => { inFlight.countDown(); readOld.await() }
    val writer = new Thread(() => { w.cached.put(TmdbKind.Film, Seq("7" -> n(2))); landed.countDown() })
    writer.start()
    inFlight.await()
    w.backend.beforeAnswer = () => { w.backend.beforeAnswer = () => (); readOld.countDown(); landed.await() }
    w.cached.answers(TmdbKind.Film, Seq("7"))("7") shouldBe n(1)          // read before the write landed…
    writer.join()
    w.cached.answers(TmdbKind.Film, Seq("7"))("7") shouldBe n(2)          // …and not kept past it
  }

  "the cache" should "give each reader its own copy, and hold no more than its bound" in {
    val backend = new CountingDocuments
    val cached  = new CachedTmdbDocuments(backend, maxBytes = 64 * 1024)
    backend.put(TmdbKind.Film, (1 to 2000).map(i => i.toString -> new BsonDocument("hit", new BsonDocument("n", BsonInt32(i)))))
    cached.answers(TmdbKind.Film, (1 to 2000).map(_.toString))
    cached.heldBytes should (be > 0L and be <= 64L * 1024)
    cached.answers(TmdbKind.Film, Seq("1"))("1").put("hit", BsonInt32(0))
    cached.answers(TmdbKind.Film, Seq("1"))("1").getDocument("hit").getInt32("n").getValue shouldBe 1
  }
}
