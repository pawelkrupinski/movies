package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbClient
import tools.{GetOnlyHttpFetch, HttpFetch, HttpStatusException, RealHttpFetch}

class ImdbClientSpec extends AnyFlatSpec with Matchers {

  private val client = new ImdbClient(new RealHttpFetch)

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
  // Captured live from https://v3.sg.media-imdb.com/suggestion/k/Kumotry.json
  // on 2026-06-13. The Polish comedy "Kumotry" is listed on IMDb under its
  // international title "Double Trouble" — searching the Polish title returns
  // it as the #1 movie hit (an AKA match), followed by popularity padding
  // (The Godfather films) that does NOT match the query year.
  private val KumotryFixture = "/fixtures/imdb/suggestion_kumotry.json"

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
    // Nothing in the fixture is exactly "Inconnu de la Grande Arche", and the
    // #1 movie (Mortal Kombat II, 2026) doesn't match the query year — return
    // None rather than picking the most popular Mortal Kombat film.
    client.parseSuggestions(body, "Inconnu de la Grande Arche", Some(2025)) shouldBe None
  }

  it should "resolve a foreign film listed under its international title via year-corroborated #1 hit" in {
    val body = loadFixture(KumotryFixture)
    // "Kumotry" (PL, 2025) has no exact-title entry — IMDb lists it as
    // "Double Trouble". It is the top movie suggestion and its year matches,
    // so we bind to it rather than dropping the IMDb rating entirely.
    client.parseSuggestions(body, "Kumotry", Some(2025)) shouldBe Some("tt36396038")
  }

  it should "NOT take the #1 hit when its year contradicts the query year" in {
    val body = loadFixture(KumotryFixture)
    // Same fixture, wrong year: the #1 hit is a 2025 film, so a 1999 query
    // gets no corroboration and no lower entry exact-matches → None.
    client.parseSuggestions(body, "Kumotry", Some(1999)) shouldBe None
  }

  it should "NOT take the #1 hit when the caller supplied no year" in {
    val body = loadFixture(KumotryFixture)
    // Without a year there's no second signal to corroborate an AKA match —
    // the foreign-title fallback stays off rather than guessing.
    client.parseSuggestions(body, "Kumotry", None) shouldBe None
  }

  it should "prefer an exact title match over a year-matching #1 hit" in {
    // The #1 movie is a year-matching foreign title, but a later entry
    // exactly matches the query — the exact match must win.
    val body =
      """{"d":[
         {"id":"tt0001","l":"Some Other Title","q":"feature","qid":"movie","rank":5,"y":2025},
         {"id":"tt0002","l":"Exact","q":"feature","qid":"movie","rank":900,"y":2025}
       ]}"""
    client.parseSuggestions(body, "Exact", Some(2025)) shouldBe Some("tt0002")
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

  // Regression: cinema title carries an en-dash, IMDb stores a plain hyphen.
  // Without dash folding the exact-title contains-match fails and the id is
  // lost. Year is None so the year-corroborated foreign-title fallback can't
  // rescue it — the match must come from the dash-folded exact-title path.
  it should "fold dash variants so an en-dash query matches a hyphen IMDb title" in {
    val body =
      """{"d":[{"id":"tt0145487","l":"Spider-Man","q":"feature","qid":"movie","rank":3,"y":2002}]}"""
    client.parseSuggestions(body, "Spider–Man", None) shouldBe Some("tt0145487")
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

  // Was "swallow network / HTTP failures and return None" — the behaviour the
  // 2026-07-30 IMDb outage proved wrong. A failed read is not an answer of "no
  // id"; it now propagates so the caller can retry rather than book a healthy
  // refresh over a dead source. See tools.EnrichmentRead.
  it should "propagate a network / HTTP failure rather than reporting 'no id'" in {
    val c = new ImdbClient(http = new GetOnlyHttpFetch {
      def get(url: String): String = throw new RuntimeException("HTTP 503")
    })
    a[RuntimeException] should be thrownBy c.findId("anything", None)
  }

  // ── Diacritic normalisation ────────────────────────────────────────────────
  //
  // IMDb stores titles in ASCII (ł→l, ą→a, ś→s, etc.) while our query titles
  // retain Polish diacritics.  `parseSuggestions` deburrs both sides so
  // "Chłopiec na krańcach świata" matches IMDb's "Chlopiec na krancach swiata".

  it should "match a Polish title whose diacritics are stripped in IMDb's response" in {
    // IMDb returns the ASCII form; our query title keeps the Polish diacritics.
    val body =
      """{"d":[{"id":"tt39637392","l":"Chlopiec na krancach swiata","q":"feature","qid":"movie","rank":1,"y":2025}]}"""
    // year=2026 but IMDb says 2025 — distance 1, still the best candidate
    client.parseSuggestions(body, "Chłopiec na krańcach świata", Some(2026)) shouldBe Some("tt39637392")
  }

  it should "deburr suggestion titles when applying the exact-match filter" in {
    val body =
      """{"d":[
         {"id":"tt111","l":"Basia. Radze sobie!","q":"feature","qid":"movie","rank":1,"y":2025},
         {"id":"tt222","l":"Unrelated Film","q":"feature","qid":"movie","rank":2,"y":2025}
       ]}"""
    client.parseSuggestions(body, "Basia. Radzę sobie!", Some(2025)) shouldBe Some("tt111")
  }

  // ── Director disambiguation ────────────────────────────────────────────────
  //
  // When `parseSuggestions` returns None (no title match — film may be listed
  // under a different/international title on IMDb), `findId` considers ALL
  // suggestion-API movie candidates and picks the one whose director overlaps.
  // This covers AKA/foreign-title cases where year isn't yet set on IMDb.
  //
  // In all tests below the suggestion has a DIFFERENT title from the query so
  // that `parseSuggestions` returns None, allowing director disambiguation to run.

  private def detailsBody(directorName: String): String =
    s"""{"data":{"title":{
       |  "titleText":{"text":"IMDb Title"},
       |  "originalTitleText":{"text":"IMDb Title"},
       |  "releaseYear":null,"runtime":null,
       |  "ratingsSummary":{"aggregateRating":0,"voteCount":0},
       |  "countriesOfOrigin":{"countries":[]},
       |  "primaryImage":null,
       |  "principalCredits":[
       |    {"category":{"id":"director"},"credits":[
       |      {"name":{"nameText":{"text":"$directorName"}}}
       |    ]}
       |  ]
       |}}}""".stripMargin

  // The suggestion returns a film under a DIFFERENT title so parseSuggestions gets None,
  // then director confirms the match. This is the AKA-film pattern ("Nasz Film" on the
  // cinema side, "IMDb Title" on IMDb).
  private val AkaSuggestionBody =
    """{"d":[{"id":"tt9999999","l":"IMDb Title","q":"feature","qid":"movie","rank":1}]}"""

  it should "use director to identify a film listed under a different title on IMDb" in {
    val c = new ImdbClient(http = new HttpFetch {
      def get(url: String): String = AkaSuggestionBody
      override def post(url: String, body: String, contentType: String): String = detailsBody("Jakub Pączek")
    })
    val found = c.findId("Nasz Film", Some(2026), Set("Jakub Pączek"))
    found shouldBe Some("tt9999999")
  }

  it should "deburr director names on both sides when disambiguating" in {
    val c = new ImdbClient(http = new HttpFetch {
      def get(url: String): String = AkaSuggestionBody
      // IMDb stores director name without diacritics; our record has Polish diacritics
      override def post(url: String, body: String, contentType: String): String = detailsBody("Jakub Paczek")
    })
    val found = c.findId("Nasz Film", Some(2026), Set("Jakub Pączek"))
    found shouldBe Some("tt9999999")
  }

  it should "return None when no candidate's director matches" in {
    val c = new ImdbClient(http = new HttpFetch {
      def get(url: String): String = AkaSuggestionBody
      override def post(url: String, body: String, contentType: String): String = detailsBody("Anna Kowalska")
    })
    val found = c.findId("Nasz Film", Some(2026), Set("Jan Nowak"))
    found shouldBe None
  }

  it should "skip director disambiguation when directors set is empty (no details POSTs)" in {
    // parseSuggestions returns None (no title match, no year-corroborated #1 hit),
    // directors empty → return None without calling details
    val c = new ImdbClient(http = new HttpFetch {
      def get(url: String): String = AkaSuggestionBody
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("details should not be called without directors")
    })
    c.findId("Nasz Film", Some(2026)) shouldBe None
  }

  // ── A blocked read must not masquerade as "this film has no rating" ────────
  // IMDb's CDN began 403-ing us on 2026-07-30 and `lookup` swallowed it into
  // None, so every worker logged the ordinary-looking "→ rating none" and
  // RatingHandler booked a healthy "checked, unchanged" refresh. ~47h invisible.
  // A 403/429/5xx/timeout must now reach the caller; only a real "not found" is
  // still an answer. See tools.EnrichmentRead.

  private def failingWith(exception: Throwable) = new ImdbClient(http = new HttpFetch {
    def get(url: String): String = throw exception
    override def post(url: String, body: String, contentType: String): String = throw exception
  })

  private def statusError(code: Int) = new HttpStatusException(code, "POST", "https://caching.graphql.imdb.com/", None)

  "lookup" should "rethrow the CDN block instead of reporting 'no rating'" in {
    a[HttpStatusException] should be thrownBy failingWith(statusError(403)).lookup("tt0816692")
  }

  it should "rethrow a throttle, a server error and a timeout too" in {
    a[HttpStatusException] should be thrownBy failingWith(statusError(429)).lookup("tt0816692")
    a[HttpStatusException] should be thrownBy failingWith(statusError(503)).lookup("tt0816692")
    a[java.net.http.HttpTimeoutException] should be thrownBy
      failingWith(new java.net.http.HttpTimeoutException("slow")).lookup("tt0816692")
  }

  it should "still report None when IMDb genuinely has no such title" in {
    failingWith(statusError(404)).lookup("tt0000000") shouldBe None
  }

  it should "still report None when the title exists but carries no rating" in {
    val c = new ImdbClient(http = new HttpFetch {
      def get(url: String): String = ""
      override def post(url: String, body: String, contentType: String): String =
        """{"data":{"title":{"ratingsSummary":null}}}"""
    })
    c.lookup("tt0816692") shouldBe None
  }

  "details" should "rethrow a block rather than emptying the SourceData(Imdb) slot" in {
    a[HttpStatusException] should be thrownBy failingWith(statusError(403)).details("tt0816692")
  }

  it should "still report None on a genuine not-found" in {
    failingWith(statusError(404)).details("tt0000000") shouldBe None
  }

  // `findId` probes the suggestion endpoint, a DIFFERENT host that stayed healthy
  // throughout. It keeps its own leniency for the not-found case, but a block on
  // that host must surface too rather than silently yielding "no imdbId".
  "findId" should "rethrow a block on the suggestion endpoint" in {
    a[HttpStatusException] should be thrownBy failingWith(statusError(403)).findId("Interstellar", Some(2014))
  }

  it should "still report None when the suggestion endpoint has no such title" in {
    failingWith(statusError(404)).findId("Nonexistent Film", Some(2014)) shouldBe None
  }
}
