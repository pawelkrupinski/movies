package services.identity

import org.bson.BsonDocument
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.LogCapture

import scala.util.Success

class TmdbNormalizerSpec extends AnyFlatSpec with Matchers with LoneElement {

  /** A store whose every round-trip fails: Mongo timing out under a take-up. */
  private object Unreachable extends TmdbDocuments {
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = throw new IllegalStateException("store unreachable")
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit      = throw new IllegalStateException("store unreachable")
    def scan(kind: TmdbKind)(page: Seq[(String, Option[Long])] => Unit): Boolean = false
    def delete(kind: TmdbKind, ids: Seq[String]): Unit = ()
  }

  // TMDB's v3 key rides the query string; the worker's log is kept for 14 days.
  "a response the store could not file" should "be logged without the request's API key" in {
    val normalizer = new TmdbNormalizer(new TmdbStore(Unreachable, java.time.Clock.fixed(java.time.Instant.parse("2026-09-26T10:00:00Z"), java.time.ZoneOffset.UTC)))
    val url = "https://api.themoviedb.org/3/search/movie?api_key=0123456789abcdef&language=pl-PL&query=lalka"
    val logged = LogCapture.thisThread(classOf[TmdbNormalizer].getName)(normalizer.filed("GET", url, Success("""{"results":[]}""")))
      .map(_.getFormattedMessage).loneElement
    logged should include("not normalized")
    logged should not include "0123456789abcdef"
  }

  // The normalizer hands a film response's parse to the client on the same thread (`JsonBodies`). A
  // pipeline read (`details`, `fullDetails`) that parsed the body itself never took it, and the parse —
  // a whole cast and crew — stayed on the thread until its next normalized response: on each of the
  // prefetch's 64 threads and every enrichment thread.
  "a film response the pipeline reads" should "leave no parse behind on its thread" in {
    val bodies  = new tools.JsonBodies
    val fetch   = new tools.HttpFetch {
      override def get(url: String): String = """{"id":7,"title":"Lalka","credits":{"crew":[],"cast":[]},"alternative_titles":{"titles":[]}}"""
      override def post(url: String, body: String, contentType: String): String = get(url)
    }
    val store  = new TmdbStore(new InMemoryTmdbDocuments, java.time.Clock.systemUTC())
    val client = new clients.TmdbClient(new NormalizingHttpFetch(fetch, new TmdbNormalizer(store, bodies)),
      apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => (), bodies = bodies)
    client.details(7) shouldBe defined
    bodies.retained shouldBe false
    client.fullDetails(7) shouldBe defined
    bodies.retained shouldBe false
  }
}
