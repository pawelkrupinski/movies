package services.identity

import org.bson.{BsonArray, BsonDocument, BsonInt64}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._

/** The normalized TMDB store kept every answer and gap marker it was ever given; the sweep deletes what
 *  the model no longer reads and markers past their grace, and nothing on a scan it could not finish. */
class TmdbStoreSweepSpec extends AnyFlatSpec with Matchers {

  private val now   = Instant.parse("2026-10-04T00:00:00Z")
  private val clock = Clock.fixed(now, ZoneOffset.UTC)
  private def daysAgo(days: Int) = now.toEpochMilli - days.days.toMillis
  private def doc(fetched: Long) = new BsonDocument("ids", new BsonArray()).append(TmdbStore.FetchedAt, BsonInt64(fetched))

  private def store() = {
    val d = new InMemoryTmdbDocuments
    d.put(TmdbKind.Query, Seq(
      "movie|pl-PL|Read"       -> doc(daysAgo(90)),   // old, but a question reads it
      "movie|pl-PL|Unread"     -> doc(daysAgo(90)),   // old and unread
      "movie|pl-PL|Recent"     -> doc(daysAgo(3)),    // unread, but fetched lately
      "unanswered|film|1"      -> doc(daysAgo(10)),   // marker past its grace
      "unanswered|film|2"      -> doc(daysAgo(2)),    // marker within it
      "changes-watermark"      -> new BsonDocument("day", org.bson.BsonString("2026-10-03"))))  // no stamp
    d.put(TmdbKind.Film, Seq("1" -> doc(daysAgo(90)), "2" -> doc(daysAgo(90))))
    d
  }
  private val read = Set(TmdbStore.keyOf(TmdbKind.Query, "movie|pl-PL|Read"), TmdbStore.keyOf(TmdbKind.Film, "1"))

  "a sweep" should "delete answers the model no longer reads and markers past their grace, keeping the rest" in {
    val d = store()
    new TmdbStoreSweep(d, () => Some(read), clock).sweep() shouldBe TmdbStoreSweep.Swept(markers = 1, deleted = 3, modelUp = true)
    d.size(TmdbKind.Query) shouldBe 4
    d.get(TmdbKind.Query, Seq("movie|pl-PL|Unread", "unanswered|film|1")) shouldBe empty
    d.get(TmdbKind.Film, Seq("1", "2")).keySet shouldBe Set("1")
  }

  // Another film database family's answers are no question of the model's: kept a year and more since last fetched,
  // unread by the model or not — the agreement reads them only for clusters TMDB matched to nothing.
  it should "keep another family's answers until long after they were fetched, read by the model or not" in {
    val d = new InMemoryTmdbDocuments
    d.put(TmdbKind.Family, Seq("imdb|record|tt1" -> doc(daysAgo(200)), "imdb|record|tt2" -> doc(daysAgo(401))))
    new TmdbStoreSweep(d, () => Some(Set.empty), clock).sweep().deleted shouldBe 1
    d.get(TmdbKind.Family, Seq("imdb|record|tt1", "imdb|record|tt2")).keySet shouldBe Set("imdb|record|tt1")
  }

  it should "delete no answer while no model is taken up, but still age markers out" in {
    val d = store()
    new TmdbStoreSweep(d, () => None, clock).sweep() shouldBe TmdbStoreSweep.Swept(markers = 1, deleted = 1, modelUp = false)
    d.size(TmdbKind.Query) shouldBe 5
    d.size(TmdbKind.Film) shouldBe 2
  }

  it should "delete nothing when a scan fails part way" in {
    val d = store()
    val failing = new TmdbDocumentRetention {
      def fetchedBefore(kind: TmdbKind, cutoff: Long) =
        if (kind == TmdbKind.Query) d.fetchedBefore(kind, cutoff) else throw new IllegalStateException("cursor died")
      def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]) = d.deleteIfStill(kind, stamped)
    }
    an[IllegalStateException] should be thrownBy new TmdbStoreSweep(failing, () => Some(read), clock).sweep()
    d.size(TmdbKind.Query) shouldBe 6
    d.size(TmdbKind.Film) shouldBe 2
  }
}
