package scripts

import clients.TmdbClient
import clients.tools.{FakeHttpFetch, FixtureFile}
import org.bson.BsonDocument
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import services.identity.adapters.{InMemoryTmdbDocuments, NormalizingHttpFetch, TmdbKind, TmdbNormalizer, TmdbStore}
import tools.{HttpStatusException, MutableClock, TestWiring}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class TmdbRecordBackfillSpec extends AnyFlatSpec with Matchers {
  import TmdbRecordBackfill._

  private def clock = new MutableClock(TestWiring.FixedInstant)
  /** The film every fixture tree holds TMDB's localized record of: Mulholland Drive, cast and language in it. */
  private val film = 1018
  private def recorded(name: String): String = {
    val text = FixtureFile.resource(s"fixtures/08-06-2026/api.themoviedb.org/3/movie/$name")
    text.substring(text.indexOf('{'))
  }

  /** Its localized partial as a record filed before 2026-10-06 holds it: no cast, no original language, no genres. */
  private def filedBeforeCast(docs: InMemoryTmdbDocuments): Unit = {
    val minimal = TmdbNormalizer.minimal(Json.parse(recorded(s"$film.cdb52e10"))).as[JsObject]
    val local   = (minimal - "original_language" - "genres") + ("credits" -> Json.obj("crew" -> (minimal \ "credits" \ "crew").get))
    val doc     = new BsonDocument(TmdbStore.Partial.Local.field, TmdbStore.bsonOf(local))
      .append(TmdbStore.Partial.English.field, TmdbStore.bsonOf(TmdbNormalizer.minimal(Json.parse(recorded(s"$film.85ac50d9")))))
    docs.put(TmdbKind.Film, Seq(film.toString -> doc))
  }

  private def states(docs: InMemoryTmdbDocuments, ids: Int*): Seq[RecordState] =
    docs.get(TmdbKind.Film, ids.map(_.toString)).toSeq.flatMap { case (id, d) => RecordState.of(id, d) }

  "a record filed before the store kept cast, language and genres" should "be read again, refiled with all three, and not chosen again" in {
    val docs   = new InMemoryTmdbDocuments
    val time   = clock
    filedBeforeCast(docs)
    docs.put(TmdbKind.Film, Seq("2" -> new BsonDocument("hit", new BsonDocument())))   // a film a search named alone: no record
    states(docs, film, 2) shouldBe Seq(RecordState(film, hasCast = false, hasLanguage = false, hasGenres = false))

    val chosen = select(states(docs, film, 2), scope = None)
    chosen.toRead shouldBe Seq(film)
    val fetch  = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), new TmdbNormalizer(new TmdbStore(docs, time)))
    val client = new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")))
    readAll(chosen.toRead, client.readRecordAgain, time, time.advanceMillis).read shouldBe 1

    states(docs, film) shouldBe Seq(RecordState(film, hasCast = true, hasLanguage = true, hasGenres = true))
    select(states(docs, film), scope = None).toRead shouldBe empty
  }

  "the selection" should "read only records lacking a language or genres, within the current films unless all are asked" in {
    def state(id: Int, cast: Boolean, language: Boolean, genres: Boolean) = RecordState(id, cast, language, genres)
    val records = Seq(state(1, cast = false, language = false, genres = false), state(2, cast = true, language = false, genres = false),
      state(3, cast = true, language = true, genres = true),
      // read by code keeping all three: TMDB credits no one on it, so it is not asked again
      state(4, cast = false, language = true, genres = true),
      // filed between the languages and the genres being kept
      state(5, cast = true, language = true, genres = false))
    select(records, scope = None) shouldBe
      Selection(records = 5, lackingCast = 2, lackingLanguage = 2, lackingGenres = 3, inScope = 5, toRead = Seq(1, 2, 5))
    select(records, scope = Some(Set(2, 3, 5, 99))) shouldBe
      Selection(records = 5, lackingCast = 2, lackingLanguage = 2, lackingGenres = 3, inScope = 3, toRead = Seq(2, 5))
  }

  "the concurrency" should "halve on a refusal, once per back-off window, never below one" in {
    val time = clock
    val concurrency = new Concurrency(10, time, 5.seconds)
    concurrency.refused(); concurrency.limit shouldBe 5
    concurrency.refused(); concurrency.limit shouldBe 5    // the same burst
    time.advanceSeconds(5)
    concurrency.refused(); concurrency.limit shouldBe 2
    time.advanceSeconds(5); concurrency.refused()
    time.advanceSeconds(5); concurrency.refused(); concurrency.limit shouldBe 1
  }

  "the reads" should "ask a refused record again later, give one up after its attempts, and not retry any other failure" in {
    val time     = clock
    val asked    = new ConcurrentHashMap[Int, AtomicInteger]()
    def refuse(id: Int, code: Int) = throw new HttpStatusException(code, "GET", s"https://api.themoviedb.org/3/movie/$id", None)
    val read: Int => Unit = id => asked.computeIfAbsent(id, _ => new AtomicInteger).incrementAndGet() match {
      case n if id == 1 && n <= 2 => refuse(id, 429)
      case _ if id == 2           => refuse(id, 503)
      case _ if id == 3           => refuse(id, 404)
      case _                      => ()
    }
    val outcome = readAll(1 to 20, read, time, time.advanceMillis, workers = 4)
    outcome.read shouldBe 18
    outcome.refusals shouldBe 2 + MaxAttempts
    outcome.failed.map(_._1) shouldBe Seq(2, 3)
    asked.get(1).get shouldBe 3
    asked.get(3).get shouldBe 1
    outcome.finalConcurrency should be < 4
  }

  "the pace" should "space the reads to the rate asked" in {
    val time = clock
    val pace = new Pace(30, time, time.advanceMillis)
    (1 to 31).foreach(_ => pace.await())
    time.millis() - TestWiring.FixedInstant.toEpochMilli should (be >= 990L and be <= 1010L)
  }
}
