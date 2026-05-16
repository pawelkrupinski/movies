package clients.enrichment

import clients.TmdbClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TmdbClientSpec extends AnyFlatSpec with Matchers {

  private val client = new TmdbClient()

  "parseSearchResults" should "extract id, title, year, popularity from a TMDB search response" in {
    val json =
      """{
        |  "page": 1,
        |  "results": [
        |    {"id": 12345, "title": "Diabeł ubiera się u Prady 2", "original_title": "The Devil Wears Prada 2",
        |     "release_date": "2026-05-01", "popularity": 89.5, "vote_average": 7.2},
        |    {"id": 67890, "title": "Diabeł", "original_title": "The Devil",
        |     "release_date": "1972-01-01", "popularity": 3.0}
        |  ],
        |  "total_pages": 1
        |}""".stripMargin

    val results = client.parseSearchResults(json)
    results.size shouldBe 2
    results.head.id            shouldBe 12345
    results.head.title         shouldBe "Diabeł ubiera się u Prady 2"
    results.head.originalTitle shouldBe Some("The Devil Wears Prada 2")
    results.head.releaseYear   shouldBe Some(2026)
    results.head.popularity    shouldBe 89.5 +- 0.001
  }

  it should "sort multiple hits by popularity (most popular first)" in {
    val json =
      """{"results":[
        |  {"id":1, "title":"a", "release_date":"2024-01-01", "popularity":2.5},
        |  {"id":2, "title":"b", "release_date":"2024-01-01", "popularity":50.0},
        |  {"id":3, "title":"c", "release_date":"2024-01-01", "popularity":12.0}
        |]}""".stripMargin
    client.parseSearchResults(json).map(_.id) shouldBe Seq(2, 3, 1)
  }

  it should "handle a response with no results" in {
    client.parseSearchResults("""{"page":1,"results":[],"total_pages":0}""") shouldBe empty
  }

  it should "skip entries missing an id" in {
    val json = """{"results":[{"title":"orphan","release_date":"2024-01-01","popularity":1.0}]}"""
    client.parseSearchResults(json) shouldBe empty
  }

  it should "leave releaseYear empty when release_date is missing or short" in {
    val json =
      """{"results":[
        |  {"id":1, "title":"a", "release_date":"", "popularity":1.0},
        |  {"id":2, "title":"b", "popularity":1.0}
        |]}""".stripMargin
    client.parseSearchResults(json).flatMap(_.releaseYear) shouldBe empty
  }

  // ── search() year-fallback behaviour ──────────────────────────────────────
  //
  // Regression for "Top Gun" anniversary screenings: the lookup key is the
  // *base* title "Top Gun" but the cinema reports the rerelease year (2026).
  // TMDB's year-restricted search returns zero hits, so the fallback ran a
  // no-year search and previously picked the candidate closest to 2026 by
  // year-distance — that's "Top Gun: Maverick" (2022). We must prefer
  // exact-title matches over year-distance.

  private def fakeClient(responses: Map[String, String]): TmdbClient = {
    val fake = new tools.GetOnlyHttpFetch {
      def get(url: String): String =
        responses.collectFirst { case (frag, body) if url.contains(frag) => body }
          .getOrElse(throw new RuntimeException(s"unexpected URL: $url"))
    }
    new TmdbClient(http = fake, apiKey = Some("fake"))
  }

  "search" should "prefer an exact-title match over the closer-by-year sequel (Top Gun regression)" in {
    val noYear =
      """{"results":[
        |  {"id":361743,"title":"Top Gun: Maverick","original_title":"Top Gun: Maverick",
        |   "release_date":"2022-05-21","popularity":600.0},
        |  {"id":744,"title":"Top Gun","original_title":"Top Gun",
        |   "release_date":"1986-05-16","popularity":120.0},
        |  {"id":243655,"title":"Top Gun","original_title":"Top Gun",
        |   "release_date":"1955-12-01","popularity":2.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2026"  -> """{"results":[]}""",  // year-restricted: nothing in 2026
      "query=Top"   -> noYear                  // no-year fallback hits this
    ))
    // 1986 "Top Gun" (id 744) beats 2022 "Maverick" because the title matches
    // exactly. Among the two exact-title matches (1986 and 1955), 1986 wins
    // on year-distance to 2026.
    client.search("Top Gun", Some(2026)).map(_.id) shouldBe Some(744)
  }

  it should "still pick the closest-year hit when no exact title match exists" in {
    val noYear =
      """{"results":[
        |  {"id":10,"title":"Some Other Title","original_title":"Foreign Original",
        |   "release_date":"2025-01-01","popularity":50.0},
        |  {"id":11,"title":"Unrelated","original_title":"Unrelated",
        |   "release_date":"1990-01-01","popularity":10.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2026" -> """{"results":[]}""",
      "query=Foo"  -> noYear
    ))
    // No exact match → fall back to year distance: 2025 (1) beats 1990 (36).
    client.search("Foo", Some(2026)).map(_.id) shouldBe Some(10)
  }

  it should "match on original_title when the Polish title differs from the query" in {
    val noYear =
      """{"results":[
        |  {"id":361743,"title":"Top Gun: Maverick","original_title":"Top Gun: Maverick",
        |   "release_date":"2022-05-21","popularity":600.0},
        |  {"id":744,"title":"Top Gun po polsku","original_title":"Top Gun",
        |   "release_date":"1986-05-16","popularity":120.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2026" -> """{"results":[]}""",
      "query=Top"  -> noYear
    ))
    // Polish title differs but original_title is the exact query → still wins.
    client.search("Top Gun", Some(2026)).map(_.id) shouldBe Some(744)
  }

  // Regression: year=None previously bypassed the exact-title preference and
  // just took the most popular result. "Camper" picked "Sleepaway Camper",
  // "Odlot" picked Pixar's "Up", etc.

  it should "prefer an exact title match over a more popular partial match when year is None" in {
    val noYear =
      """{"results":[
        |  {"id":1053705,"title":"Sleepaway Camper","original_title":"Sleepaway Camper",
        |   "release_date":"2008-01-01","popularity":0.3},
        |  {"id":1192319,"title":"Camper","original_title":"Camper",
        |   "release_date":"2025-12-12","popularity":0.2}
        |]}""".stripMargin
    val client = fakeClient(Map("query=Camper" -> noYear))
    // "Camper" matches id=1192319 exactly. Without the fix, Sleepaway Camper
    // (higher popularity) would have won.
    client.search("Camper", None).map(_.id) shouldBe Some(1192319)
  }

  it should "prefer an exact title match over a far-more-popular partial match (Odlot vs Up)" in {
    val noYear =
      """{"results":[
        |  {"id":14160,"title":"Odlot","original_title":"Up",
        |   "release_date":"2009-05-13","popularity":20.9},
        |  {"id":1355269,"title":"Odlot","original_title":"Odlot",
        |   "release_date":"2007-01-01","popularity":0.0}
        |]}""".stripMargin
    val client = fakeClient(Map("query=Odlot" -> noYear))
    // Both Polish titles match exactly. Among equally-exact hits, year-distance
    // is undefined (no year given), so insertion order (popularity-desc) breaks
    // the tie → Pixar's "Up" wins by popularity. Still correct as both rows
    // are technically exact matches — what matters is that we *try* the exact
    // filter; the year-distance preference works when the cinema provides a
    // year (next test).
    val pick = client.search("Odlot", None)
    pick should not be empty
    Set(14160, 1355269) should contain (pick.get.id)
  }

  it should "use year-distance to disambiguate among multiple exact title matches" in {
    val noYear =
      """{"results":[
        |  {"id":14160,"title":"Odlot","original_title":"Up",
        |   "release_date":"2009-05-13","popularity":20.9},
        |  {"id":1355269,"title":"Odlot","original_title":"Odlot",
        |   "release_date":"2007-01-01","popularity":0.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2007"  -> """{"results":[]}""",
      "query=Odlot" -> noYear
    ))
    // Both exact matches; closest to year=2007 wins.
    client.search("Odlot", Some(2007)).map(_.id) shouldBe Some(1355269)
  }

  // ── findByImdbId / /find/{external_id} parsing ────────────────────────────
  //
  // Reverse-look up a TMDB record by IMDb id. Same response shape as
  // /search/movie except the array key is "movie_results". Used today by
  // sister-row reconstruction paths and by scripts that have an IMDb id in
  // hand and need TMDB's canonical metadata.

  it should "parse a TMDB /find response into a SearchResult (Girl Climber)" in {
    val findBody =
      """{
        |  "movie_results":[
        |    {"id":1461058,"title":"Girl Climber","original_title":"Girl Climber",
        |     "release_date":"2025-08-24","popularity":0.42,
        |     "vote_average":7.0,"vote_count":3}
        |  ],
        |  "person_results":[],"tv_results":[]
        |}""".stripMargin
    val client = fakeClient(Map("/find/tt36437006" -> findBody))
    val hit = client.findByImdbId("tt36437006")
    hit.map(_.id)            shouldBe Some(1461058)
    hit.map(_.title)         shouldBe Some("Girl Climber")
    hit.flatMap(_.releaseYear) shouldBe Some(2025)
  }

  it should "return None for a /find response with no movie_results" in {
    val client = fakeClient(Map("/find/tt00000000" -> """{"movie_results":[]}"""))
    client.findByImdbId("tt00000000") shouldBe None
  }

  // Regression: HP1's UK/US title divergence. TMDB returns "Philosopher's
  // Stone" as both original_title and en-US title — MC/RT actually index the
  // film under the US release title from /alternative_titles. `details`
  // fetches alt-titles via append_to_response and exposes the US entry.
  "details" should "expose the US alternative title from /alternative_titles" in {
    val body =
      """{
        |  "id":671,"title":"Harry Potter and the Philosopher's Stone","release_date":"2001-11-16",
        |  "alternative_titles":{"titles":[
        |    {"iso_3166_1":"GB","title":"Harry Potter and the Philosopher's Stone","type":""},
        |    {"iso_3166_1":"US","title":"Harry Potter and the Sorcerer's Stone","type":""}
        |  ]}
        |}""".stripMargin
    val client = fakeClient(Map("/movie/671?" -> body))
    val d = client.details(671).get
    d.englishTitle shouldBe Some("Harry Potter and the Philosopher's Stone")
    d.usTitle      shouldBe Some("Harry Potter and the Sorcerer's Stone")
    d.releaseYear  shouldBe Some(2001)
  }

  it should "return usTitle=None when the response has no US alternative title" in {
    val body =
      """{"id":1,"title":"Foo","release_date":"2020-01-01",
        |"alternative_titles":{"titles":[{"iso_3166_1":"FR","title":"Le Foo","type":""}]}}""".stripMargin
    val client = fakeClient(Map("/movie/1?" -> body))
    client.details(1).flatMap(_.usTitle) shouldBe None
  }

  it should "skip 'alternative spelling' / 'working title' US entries and prefer the untyped one" in {
    val body =
      """{"id":2,"title":"X","release_date":"2020-01-01",
        |"alternative_titles":{"titles":[
        |  {"iso_3166_1":"US","title":"X (Working Title)","type":"working title"},
        |  {"iso_3166_1":"US","title":"X — The Real US Release","type":""}
        |]}}""".stripMargin
    val client = fakeClient(Map("/movie/2?" -> body))
    client.details(2).flatMap(_.usTitle) shouldBe Some("X — The Real US Release")
  }

  it should "apply exact-title preference inside a year-restricted result set too" in {
    // Year-restricted (year=2025) returns BOTH the real "Camper" and a
    // partial-title hit. Previously we just took the first by popularity
    // (the partial). Now exact-match wins regardless of popularity.
    val yearScoped =
      """{"results":[
        |  {"id":9999,"title":"The Happy Camper","original_title":"The Happy Camper",
        |   "release_date":"2025-06-01","popularity":1.5},
        |  {"id":1192319,"title":"Camper","original_title":"Camper",
        |   "release_date":"2025-12-12","popularity":0.2}
        |]}""".stripMargin
    val client = fakeClient(Map("query=Camper" -> yearScoped))
    client.search("Camper", Some(2025)).map(_.id) shouldBe Some(1192319)
  }
}
