package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbClient
import tools.GetOnlyHttpFetch

class ImdbClientSpec extends AnyFlatSpec with Matchers {

  private val client = new ImdbClient()

  "parseRating" should "extract the aggregateRating from the GraphQL response" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.0,"voteCount":16966}}}}"""
    client.parseRating(body) shouldBe Some(7.0)
  }

  it should "treat a missing ratingsSummary as None" in {
    val body = """{"data":{"title":null}}"""
    client.parseRating(body) shouldBe None
  }

  it should "treat a zero rating as None (IMDb returns 0 when unrated)" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":0,"voteCount":0}}}}"""
    client.parseRating(body) shouldBe None
  }

  // Mirror TMDB's "suppress single-enthusiast" guard: <5 votes is noise.
  it should "suppress the rating when there are fewer than MinVotes" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":10.0,"voteCount":3}}}}"""
    client.parseRating(body) shouldBe None
  }

  it should "accept integer aggregateRating values (GraphQL drops the .0)" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":7,"voteCount":1000}}}}"""
    client.parseRating(body) shouldBe Some(7.0)
  }

  it should "return None for an empty / malformed response" in {
    client.parseRating("""{}""") shouldBe None
  }

  // ── IMDb suggestion (id-by-title fallback) ─────────────────────────────────
  //
  // Fixture: test/resources/fixtures/imdb/suggestion_mortal_kombat_ii.json —
  // captured live from https://v3.sg.media-imdb.com/suggestion/x/mortal%20kombat%20ii.json
  // on 2026-05-14, then trimmed to the fields we parse (id, l, q, qid, y, rank).
  //
  // Used when TMDB resolves a film but has no IMDb cross-reference — we hit
  // IMDb's own suggestion endpoint to find the id. Picks the canonical movie
  // entry over video-game / TV / "imdbpicks" article entries with the same
  // title.

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  private val MortalKombatFixture = "/fixtures/imdb/suggestion_mortal_kombat_ii.json"

  "parseSuggestions" should "return the tt-id of the movie that exact-title matches the query" in {
    val body = loadFixture(MortalKombatFixture)
    // Mortal Kombat II (2026, movie) is the obvious hit; two video-game
    // entries share the title but must be filtered by qid.
    client.parseSuggestions(body, "Mortal Kombat II", Some(2026)) shouldBe Some("tt17490712")
  }

  it should "filter out non-movie entries (video games, TV series) even when the title matches" in {
    val body =
      """{"d":[
         {"id":"tt0203703","l":"Mortal Kombat II","q":"video game","qid":"videoGame","rank":21499,"y":1993},
         {"id":"tt17490712","l":"Mortal Kombat II","q":"feature","qid":"movie","rank":3,"y":2026}
       ]}"""
    client.parseSuggestions(body, "Mortal Kombat II", None) shouldBe Some("tt17490712")
  }

  it should "filter out non-tt ids (imdbpicks / lists / persons) even when the title matches" in {
    val body =
      """{"d":[
         {"id":"/imdbpicks/foo/","l":"Mortal Kombat II"},
         {"id":"nm12345","l":"Mortal Kombat II","q":"actress","qid":"name"}
       ]}"""
    client.parseSuggestions(body, "Mortal Kombat II", None) shouldBe None
  }

  it should "use year-closest as the tie-break when several movies share the title" in {
    // Two movie entries with the same title, different years. Year=2026 must
    // pick the 2026 entry; year=1995 picks the 1995 reboot.
    val body =
      """{"d":[
         {"id":"tt2026","l":"Same Title","q":"feature","qid":"movie","rank":50,"y":2026},
         {"id":"tt1995","l":"Same Title","q":"feature","qid":"movie","rank":100,"y":1995}
       ]}"""
    client.parseSuggestions(body, "Same Title", Some(2026)) shouldBe Some("tt2026")
    client.parseSuggestions(body, "Same Title", Some(1995)) shouldBe Some("tt1995")
  }

  it should "return None when nothing exact-matches the query (no wild guessing)" in {
    val body = loadFixture(MortalKombatFixture)
    // Nothing in the fixture is exactly "Inconnu de la Grande Arche" — return
    // None rather than picking the most popular Mortal Kombat film.
    client.parseSuggestions(body, "Inconnu de la Grande Arche", Some(2025)) shouldBe None
  }

  it should "tolerate entries missing optional fields (rank, year)" in {
    val body =
      """{"d":[{"id":"tt2026","l":"Same Title","q":"feature","qid":"movie"}]}"""
    client.parseSuggestions(body, "Same Title", None) shouldBe Some("tt2026")
  }

  it should "return None for empty / malformed bodies" in {
    client.parseSuggestions("""{}""",            "anything", None) shouldBe None
    client.parseSuggestions("""{"d":[]}""",     "anything", None) shouldBe None
    client.parseSuggestions("not even json",    "anything", None) shouldBe None
  }

  "findId" should "hit the suggestion endpoint and return the parsed tt-id" in {
    val fixture = loadFixture(MortalKombatFixture)
    val c = new ImdbClient(http = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("v3.sg.media-imdb.com/suggestion/")) fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.findId("Mortal Kombat II", Some(2026)) shouldBe Some("tt17490712")
  }

  it should "swallow network / HTTP failures and return None" in {
    val c = new ImdbClient(http = new GetOnlyHttpFetch {
      def get(url: String): String = throw new RuntimeException("HTTP 503")
    })
    c.findId("anything", None) shouldBe None
  }
}
