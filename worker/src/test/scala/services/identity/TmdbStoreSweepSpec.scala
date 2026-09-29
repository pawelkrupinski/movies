package services.identity

import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{MutableClock, TestWiring}

import scala.concurrent.duration._

/** The store keeps what the live model reads: a document no current question reaches, and not
 *  fetched for a week, goes. Nothing grew smaller before this — every film ever answered stayed, and
 *  TMDB's daily change list re-fetched each one it edited, reached or not. */
class TmdbStoreSweepSpec extends AnyFlatSpec with Matchers {

  private val now = TestWiring.FixedInstant

  private final class World {
    val clock = new MutableClock(now)
    val docs  = new InMemoryTmdbDocuments
    def stored(kind: TmdbKind, id: String, fetchedDaysAgo: Int): Unit =
      docs.put(kind, Seq(id -> new BsonDocument("x", BsonString(id)).append(TmdbStore.FetchedAt, BsonInt64(now.toEpochMilli - fetchedDaysAgo.days.toMillis))))
    def sweep(reachable: Option[Set[String]], maxShare: Double = 1.0) =
      new TmdbStoreSweep(docs, () => reachable, clock, maxShare = maxShare)
    def ids(kind: TmdbKind, of: String*): Set[String] = docs.get(kind, of).keySet
  }

  "the store sweep" should "delete what no live question reaches and a week has not re-fetched, and keep the rest" in {
    val w = new World
    w.stored(TmdbKind.Film, "1", fetchedDaysAgo = 30)      // reached
    w.stored(TmdbKind.Film, "2", fetchedDaysAgo = 30)      // unreached, old: garbage
    w.stored(TmdbKind.Film, "3", fetchedDaysAgo = 2)       // unreached, fresh: a listing may come back
    w.stored(TmdbKind.Person, "9", fetchedDaysAgo = 30)    // unreached, old: garbage
    w.stored(TmdbKind.Query, "title|pl-PL|lalka", fetchedDaysAgo = 30)
    w.stored(TmdbKind.Query, TmdbChangesSweep.Watermark, fetchedDaysAgo = 30)       // markers are never questions'
    w.stored(TmdbKind.Query, TmdbGapMemory.Prefix + "title|pl-PL|x", fetchedDaysAgo = 30)
    val result = w.sweep(Some(Set(TmdbStore.keyOf(TmdbKind.Film, "1"), TmdbStore.keyOf(TmdbKind.Query, "title|pl-PL|lalka")))).sweep()
    w.ids(TmdbKind.Film, "1", "2", "3") shouldBe Set("1", "3")
    w.ids(TmdbKind.Person, "9") shouldBe empty
    w.ids(TmdbKind.Query, "title|pl-PL|lalka", TmdbChangesSweep.Watermark, TmdbGapMemory.Prefix + "title|pl-PL|x") shouldBe
      Set("title|pl-PL|lalka", TmdbChangesSweep.Watermark, TmdbGapMemory.Prefix + "title|pl-PL|x")
    result.map(_.deleted) shouldBe Some(2)
  }

  it should "delete nothing while the model is not taken up: an empty index reaches nothing" in {
    val w = new World
    w.stored(TmdbKind.Film, "2", fetchedDaysAgo = 30)
    w.sweep(None).sweep() shouldBe None
    w.sweep(Some(Set.empty)).sweep() shouldBe None                 // an index naming no store document is no index
    w.ids(TmdbKind.Film, "2") shouldBe Set("2")
  }

  it should "refuse to delete more than its share of the old documents at once" in {
    val w = new World
    (1 to 10).foreach(i => w.stored(TmdbKind.Film, i.toString, fetchedDaysAgo = 30))
    val reached = Set(TmdbStore.keyOf(TmdbKind.Film, "1"))
    w.sweep(Some(reached), maxShare = 0.25).sweep().map(_.refused) shouldBe Some(true)
    w.docs.size(TmdbKind.Film) shouldBe 10
  }

  it should "run at most once a day" in {
    val w = new World
    val sweep = w.sweep(Some(Set(TmdbStore.keyOf(TmdbKind.Film, "1"))))
    sweep.behind shouldBe true
    sweep.sweep()
    sweep.behind shouldBe false
    w.clock.advance(java.time.Duration.ofDays(1))
    sweep.behind shouldBe true
  }
}
