package services.identity
package adapters

import org.bson.{BsonArray, BsonDocument, BsonInt64}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.TmdbAgeBackstop.Aged

import tools.MutableClock

import java.time.Instant
import scala.concurrent.duration._

/** Records and people renew only when TMDB's changes name them; the backstop names those the model reads that TMDB last
 *  gave over a month ago, oldest first, a round's share at a time, and nothing the model no longer reads. */
class TmdbAgeBackstopSpec extends AnyFlatSpec with Matchers {

  private val start = Instant.parse("2026-10-24T00:00:00Z")
  private def daysAgo(days: Int) = start.toEpochMilli - days.days.toMillis
  private def doc(fetched: Long) = new BsonDocument("ids", new BsonArray()).append(TmdbStore.FetchedAt, BsonInt64(fetched))

  private def store() = {
    val d = new InMemoryTmdbDocuments
    d.put(TmdbKind.Film, Seq("1" -> doc(daysAgo(40)), "2" -> doc(daysAgo(90)), "3" -> doc(daysAgo(3)), "4" -> doc(daysAgo(90))))
    d.put(TmdbKind.Person, Seq("7" -> doc(daysAgo(60))))
    d
  }
  private val read = Set("1", "2", "3").map(TmdbStore.keyOf(TmdbKind.Film, _)) + TmdbStore.keyOf(TmdbKind.Person, "7")

  "the age backstop" should "name what the model reads past a month, oldest first, a round's share at a time" in {
    val backstop = new TmdbAgeBackstop(store(), () => Some(read), new MutableClock(start), perRound = 2)
    backstop.due() shouldBe Seq(Aged.Film(2), Aged.Person(7))
    backstop.due() shouldBe Seq(Aged.Film(1))
    backstop.due() shouldBe empty
  }

  it should "scan again only past its scan interval" in {
    val d        = store()
    val clock    = new MutableClock(start)
    val backstop = new TmdbAgeBackstop(d, () => Some(read), clock)
    backstop.due() should have size 3
    backstop.due() shouldBe empty
    clock.advanceMillis(TmdbAgeBackstop.ScanEvery.toMillis)
    // still aged, still named: a fetch that failed is asked again
    backstop.due() should have size 3
  }

  it should "name nothing while no model is taken up" in {
    new TmdbAgeBackstop(store(), () => None, new MutableClock(start)).due() shouldBe empty
  }
}
