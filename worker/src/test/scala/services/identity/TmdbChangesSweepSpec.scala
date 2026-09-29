package services.identity

import clients.TmdbClient
import org.bson.BsonDocument
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpFetch, MutableClock}

import java.nio.file.{Files, Paths}
import java.time.Instant
import scala.collection.mutable

/** TMDB's change lists keep the normalized store current: a held film whose edits touch its record,
 *  and a held person credited anew on any film, are fetched again — nothing else — and a sweep that
 *  fails leaves its window to be swept again. Over TMDB's own recorded change answers. */
class TmdbChangesSweepSpec extends AnyFlatSpec with Matchers {

  private def fixture(name: String) = new String(Files.readAllBytes(Paths.get(s"test/resources/fixtures/tmdb/$name")), "UTF-8")
  private val changeList = fixture("movie_changes_2026-09-28_page1.json")
  private val director   = 3214936   // credited Director on film 1782278 in its recorded edits
  private val producer   = 6509957   // credited Executive Producer there: not a credit the walk reads

  /** TMDB as recorded: the change list's first page (its other pages empty), each film's edits, and
   *  a record or credits for anything fetched again — every request kept. */
  private final class Tmdb(failChangeList: Boolean = false, unreadable: Set[Int] = Set.empty) extends HttpFetch {
    val asked = mutable.ArrayBuffer.empty[String]
    def get(url: String): String = {
      asked += url
      val path = new java.net.URI(url).getPath
      if (path == "/3/movie/changes") {
        if (failChangeList) throw new java.io.IOException("reset")
        if (url.contains("page=1&") || url.endsWith("page=1")) changeList else """{"results":[],"total_pages":57}"""
      } else if (path.endsWith("/changes")) path.split('/')(3).toInt match {
        case id if unreadable(id) => throw new java.io.IOException("reset")
        case 1782278              => fixture("movie_1782278_changes.json")
        case 1744659              => fixture("movie_1744659_changes_status_only.json")
        case _                    => """{"changes":[]}"""
      }
      else if (path.startsWith("/3/movie/")) """{"id":1,"title":"Nowy tytuł","credits":{"crew":[]},"alternative_titles":{"titles":[]}}"""
      else if (path.startsWith("/3/person/")) """{"crew":[{"id":1782278,"title":"A","department":"Directing","release_date":"2026-01-01"}]}"""
      else "{}"
    }
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  private final class World(tmdb: Tmdb) {
    val clock = new MutableClock(Instant.parse("2026-09-28T20:00:00Z"))
    val docs  = new InMemoryTmdbDocuments
    val store = new TmdbStore(docs, clock)
    // What the store holds before the sweep: two films and two people.
    docs.put(TmdbKind.Film, Seq("1782278" -> new BsonDocument(), "1744659" -> new BsonDocument()))
    docs.put(TmdbKind.Person, Seq(director.toString -> new BsonDocument(), producer.toString -> new BsonDocument()))
    val client = new TmdbClient(new NormalizingHttpFetch(tmdb, new TmdbNormalizer(store)), apiKey = Some(settings.TmdbApiKey("k")),
      retrySleep = (_: Long) => ())
    val sweep = new TmdbChangesSweep(store, docs, client, "pl-PL", clock)
  }

  private def refetched(tmdb: Tmdb) = tmdb.asked.map(u => new java.net.URI(u).getPath)
    .filter(p => !p.endsWith("/changes")).toSet

  "a sweep" should "fetch again the held film whose edits touch its record, and the held person credited anew — nothing else" in {
    val tmdb = new Tmdb
    val w    = new World(tmdb)
    w.sweep.behind shouldBe true
    val result = w.sweep.sweep()
    refetched(tmdb) shouldBe Set("/3/movie/1782278", s"/3/person/$director/movie_credits")   // 1744659's edit was only its status
    (result.films, result.people) shouldBe ((1, 1))
    w.sweep.behind shouldBe false
    w.docs.get(TmdbKind.Person, Seq(director.toString))(director.toString).getArray("directed").size shouldBe 1
  }

  it should "fetch again a held film whose edits cannot be read, rather than miss one" in {
    val tmdb = new Tmdb(unreadable = Set(1744659))
    new World(tmdb).sweep.sweep()
    refetched(tmdb) should contain ("/3/movie/1744659")
  }

  it should "leave its window to be swept again when it cannot read it to the end" in {
    val tmdb = new Tmdb(failChangeList = true)
    val w    = new World(tmdb)
    an[Exception] should be thrownBy w.sweep.sweep()
    w.sweep.behind shouldBe true
  }

  it should "sweep every day since the last complete sweep, in TMDB's windows of at most 14 days" in {
    val tmdb = new Tmdb
    val w    = new World(tmdb)
    w.sweep.sweep()
    w.clock.advance(java.time.Duration.ofDays(20))
    tmdb.asked.clear()
    w.sweep.sweep()
    val windows = tmdb.asked.filter(_.contains("/3/movie/changes?")).filter(_.contains("page=1"))
      .map(u => "start_date=([0-9-]+)&end_date=([0-9-]+)".r.findFirstMatchIn(u).map(m => (m.group(1), m.group(2))).get).toSet
    windows shouldBe Set(("2026-09-27", "2026-10-10"), ("2026-10-11", "2026-10-18"))
  }
}
