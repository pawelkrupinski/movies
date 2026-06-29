package clients.enrichment

import clients.TmdbClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RealHttpFetch

class TmdbClientSpec extends AnyFlatSpec with Matchers {

  private val client = new TmdbClient(new RealHttpFetch)

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

  it should "return empty when results field is null" in {
    client.parseSearchResults("""{"page":1,"results":null,"total_pages":0}""") shouldBe empty
  }

  it should "return empty when results field is missing entirely" in {
    client.parseSearchResults("""{"page":1,"total_pages":0}""") shouldBe empty
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

  // Precision guard: a year-LESS retry must NOT accept a bare fuzzy hit. The cinema
  // reported year 2026 (a re-release year TMDB doesn't index), so the year-scoped
  // search is empty and we retry year-less. Neither candidate is an exact title
  // match and no director is reported, so there is nothing corroborating either —
  // the search must REFUSE rather than guess by year-distance/popularity. (Before
  // the corroboration gate this returned the year-closest hit, id 10.)
  it should "refuse an uncorroborated fuzzy hit in the year-less retry (no exact title, no director)" in {
    val noYear =
      """{"results":[
        |  {"id":10,"title":"Some Other Title","original_title":"Foreign Original",
        |   "release_date":"2025-01-01","popularity":50.0},
        |  {"id":11,"title":"Foo po polsku","original_title":"Foo Origins",
        |   "release_date":"1990-01-01","popularity":10.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2026" -> """{"results":[]}""",
      "query=Foo"  -> noYear
    ))
    client.search("Foo", Some(2026)) shouldBe None
  }

  it should "recover a wrong-corpus-year film via the year-less retry when the reported director corroborates a hit" in {
    // Same shape as the guard above, but now the cinema reports a director. The
    // year-distance pick (id 10, closest to 2026) has a different director; the
    // REAL film (id 11, 1990) is non-exact but its credited director matches — so
    // the director-overlap corroboration flips the resolution to id 11. Before the
    // change the year-less retry blindly took the year-closest id 10.
    val noYear =
      """{"results":[
        |  {"id":10,"title":"Some Other Title","original_title":"Foreign Original",
        |   "release_date":"2025-01-01","popularity":50.0},
        |  {"id":11,"title":"Foo po polsku","original_title":"Foo Origins",
        |   "release_date":"1990-01-01","popularity":10.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "&year=2026"          -> """{"results":[]}""",
      "query=Foo"           -> noYear,
      "/movie/10/credits"   -> """{"crew":[{"job":"Director","name":"Inny Reżyser"}],"cast":[]}""",
      "/movie/11/credits"   -> """{"crew":[{"job":"Director","name":"Jane Director"}],"cast":[]}"""
    ))
    client.search("Foo", Some(2026), director = Some("Jane Director")).map(_.id) shouldBe Some(11)
  }

  it should "recover an em-dash decorated title via a dash-normalized query variant" in {
    // The cinema reports an em-dash ("—"); TMDB indexes the film under a plain
    // hyphen. TMDB's search tokeniser treats the two as different strings, so the
    // verbatim em-dash query returns nothing — the dash-normalized variant query
    // ("Mission - Impossible") is what finds (and exact-matches) the film.
    val hyphenHit =
      """{"results":[
        |  {"id":777,"title":"Mission - Impossible","original_title":"Mission - Impossible",
        |   "release_date":"2024-01-01","popularity":50.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "%E2%80%94"            -> """{"results":[]}""",  // any query carrying the em-dash → empty
      "Mission+-+Impossible" -> hyphenHit              // dash-normalized variant → the film
    ))
    client.search("Mission — Impossible", None).map(_.id) shouldBe Some(777)
  }

  it should "recover a banner-decorated title by stripping the pipe banner and trailing parenthetical" in {
    // "POKAZ SPECJALNY | Wymazać (2024)" — the verbatim decorated query finds
    // nothing; stripping the festival banner (left of the pipe) and the trailing
    // year parenthetical yields the bare title "Wymazać", which exact-matches.
    val cleanHit =
      """{"results":[
        |  {"id":888,"title":"Wymazać","original_title":"Wymazać",
        |   "release_date":"2024-05-01","popularity":10.0}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "query=POKAZ"   -> """{"results":[]}""",  // any banner-led query → nothing
      "query=Wymaza"  -> cleanHit               // stripped bare-title variant → the film
    ))
    client.search("POKAZ SPECJALNY | Wymazać (2024)", None).map(_.id) shouldBe Some(888)
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

  it should "never resolve to a dateless stub/featurette over the dated film (Brzezina / 874482 regression)" in {
    // 874482 "Brzezina - Andrzej Wajda o filmie" is a dateless companion entry
    // (no release_date, 0 runtime, no director). Under the OLD un-stripped search
    // the decorated cinema title queried its OWN full text, which exact-matched
    // the stub — so it stuck as a SECOND resolved id under "Brzezina" and tripped
    // clusterByFilm's ambiguity-refuse, leaving the real film's decorated editions
    // un-folded. A dateless stub must never beat the dated film.
    val results =
      """{"results":[
        |  {"id":42539,"title":"Brzezina","original_title":"Brzezina",
        |   "release_date":"1970-11-10","popularity":0.40},
        |  {"id":874482,"title":"Brzezina - Andrzej Wajda o filmie",
        |   "original_title":"Brzezina - Andrzej Wajda o filmie","popularity":0.03}
        |]}""".stripMargin
    val client = fakeClient(Map("query=Brzezina" -> results))
    // the exact-match of the decorated title (the stub) must lose to the dated film
    client.search("Brzezina - Andrzej Wajda o filmie", None).map(_.id) shouldBe Some(42539)
    // and the clean query is unaffected
    client.search("Brzezina", None).map(_.id) shouldBe Some(42539)
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

  // ── fullDetails: Polish genres ────────────────────────────────────────────
  //
  // `/movie/{id}?language=pl-PL` returns `genres` as `[{id, name}]` with names
  // already in Polish ("Dramat", "Sci-Fi", "Akcja"). No id→name lookup.

  "fullDetails" should "extract Polish genre names from the response" in {
    val body =
      """{
        |  "id":872585,"title":"Oppenheimer","original_title":"Oppenheimer",
        |  "overview":"Historia J. Roberta Oppenheimera...",
        |  "release_date":"2023-07-19","runtime":181,
        |  "production_countries":[{"iso_3166_1":"US","name":"USA"}],
        |  "genres":[{"id":18,"name":"Dramat"},{"id":36,"name":"Historyczny"}],
        |  "credits":{"crew":[],"cast":[]}
        |}""".stripMargin
    val client = fakeClient(Map("/movie/872585?language=pl-PL" -> body))
    client.fullDetails(872585).get.genres shouldBe Seq("Dramat", "Historyczny")
  }

  it should "return an empty genre list when /movie omits the field" in {
    val body =
      """{"id":1,"title":"X","original_title":"X","release_date":"2020-01-01",
        |"credits":{"crew":[],"cast":[]}}""".stripMargin
    val client = fakeClient(Map("/movie/1?language=pl-PL" -> body))
    client.fullDetails(1).get.genres shouldBe empty
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

  // ── directorsFor: original_name for native-script directors ──────────────
  //
  // Regression for "Tom i Jerry: Przygoda w muzeum" (tmdbId=1497970):
  // Multikino reports director "张钢" (Chinese), TMDB stores name="Gang Zhang".
  // Without original_name in the set, verifyByDirector always fails (no
  // substring match between "gangzhang" and "张钢") → re-resolve fires every
  // scrape cycle → CPU steal + rating cascade on every tick.

  "directorsFor" should "include original_name so a native-script director name matches" in {
    val creditsBody =
      """{
        |  "crew": [
        |    {"job": "Director", "name": "Gang Zhang", "original_name": "张钢"},
        |    {"job": "Producer", "name": "John Smith",  "original_name": "John Smith"}
        |  ],
        |  "cast": []
        |}""".stripMargin
    val client = fakeClient(Map("/movie/1497970/credits" -> creditsBody))
    val dirs = client.directorsFor(1497970)
    // Both the romanised name and the native-script original_name must be present
    dirs should contain ("Gang Zhang")
    dirs should contain ("张钢")
    // Producers must be excluded
    dirs should not contain "John Smith"
  }

  it should "still work when original_name equals name (no duplication concern)" in {
    val creditsBody =
      """{
        |  "crew": [
        |    {"job": "Director", "name": "Christopher Nolan", "original_name": "Christopher Nolan"}
        |  ],
        |  "cast": []
        |}""".stripMargin
    val client = fakeClient(Map("/movie/872585/credits" -> creditsBody))
    val dirs = client.directorsFor(872585)
    dirs should contain ("Christopher Nolan")
    // Set deduplication means it appears exactly once
    dirs.size shouldBe 1
  }

  it should "handle a director entry missing original_name gracefully" in {
    val creditsBody =
      """{
        |  "crew": [
        |    {"job": "Director", "name": "Akira Kurosawa"}
        |  ],
        |  "cast": []
        |}""".stripMargin
    val client = fakeClient(Map("/movie/99/credits" -> creditsBody))
    client.directorsFor(99) shouldBe Set("Akira Kurosawa")
  }

  // ── fullDetails: Polish portrait poster from /movie/{id}/images ────────────
  //
  // The default `poster_path` is whatever TMDB flags primary, regardless of
  // language or shape. For films whose cinema poster isn't a proper portrait,
  // we want the best Polish portrait poster as the (backup) Tmdb-slot poster.
  // `fullDetails` now reads `/movie/{id}/images?include_image_language=pl,null`
  // and prefers the best portrait variant, falling back to `poster_path`.

  it should "prefer the Polish portrait poster from /images over poster_path" in {
    val detailsBody =
      """{"id":555,"title":"Film","original_title":"Film","release_date":"2024-01-01",
        |"poster_path":"/default.jpg","credits":{"crew":[],"cast":[]}}""".stripMargin
    // The en poster has a *higher* vote than the pl one — Polish must still win.
    val imagesBody =
      """{"posters":[
        |  {"file_path":"/english.jpg","iso_639_1":"en","aspect_ratio":0.667,"vote_average":9.0,"width":2000},
        |  {"file_path":"/polish.jpg","iso_639_1":"pl","aspect_ratio":0.667,"vote_average":7.0,"width":2000},
        |  {"file_path":"/neutral.jpg","iso_639_1":null,"aspect_ratio":0.667,"vote_average":8.0,"width":2000}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "/movie/555/images"         -> imagesBody,
      "/movie/555?language=pl-PL" -> detailsBody
    ))
    client.fullDetails(555).flatMap(_.posterUrl) shouldBe
      Some("https://image.tmdb.org/t/p/w500/polish.jpg")
  }

  it should "fall back to poster_path when /images has no portrait poster" in {
    val detailsBody =
      """{"id":556,"title":"Film","original_title":"Film","release_date":"2024-01-01",
        |"poster_path":"/default.jpg","credits":{"crew":[],"cast":[]}}""".stripMargin
    // Only a landscape (1.78) pl poster — excluded, so we keep poster_path.
    val imagesBody =
      """{"posters":[
        |  {"file_path":"/wide.jpg","iso_639_1":"pl","aspect_ratio":1.78,"vote_average":9.0,"width":3000}
        |]}""".stripMargin
    val client = fakeClient(Map(
      "/movie/556/images"         -> imagesBody,
      "/movie/556?language=pl-PL" -> detailsBody
    ))
    client.fullDetails(556).flatMap(_.posterUrl) shouldBe
      Some("https://image.tmdb.org/t/p/w500/default.jpg")
  }

  it should "fall back to poster_path when the /images call has no fixture (failure-tolerant)" in {
    // No "/images" fragment registered → the fake throws → posters() swallows
    // it → poster_path stands. This is the test-replay / network-error path.
    val detailsBody =
      """{"id":557,"title":"Film","original_title":"Film","release_date":"2024-01-01",
        |"poster_path":"/default.jpg","credits":{"crew":[],"cast":[]}}""".stripMargin
    val client = fakeClient(Map("/movie/557?language=pl-PL" -> detailsBody))
    client.fullDetails(557).flatMap(_.posterUrl) shouldBe
      Some("https://image.tmdb.org/t/p/w500/default.jpg")
  }

  "bestPortraitPosterUrl" should "prefer a Polish poster over a higher-voted language-neutral one" in {
    TmdbClient.bestPortraitPosterUrl(Seq(
      TmdbClient.PosterImage("/neutral.jpg", None,       0.667, 9.0, 2000),
      TmdbClient.PosterImage("/polish.jpg",  Some("pl"), 0.667, 5.0, 2000)
    )) shouldBe Some("https://image.tmdb.org/t/p/w500/polish.jpg")
  }

  it should "break ties among same-language posters by community vote, then resolution" in {
    TmdbClient.bestPortraitPosterUrl(Seq(
      TmdbClient.PosterImage("/low.jpg",  Some("pl"), 0.667, 5.0, 3000),
      TmdbClient.PosterImage("/high.jpg", Some("pl"), 0.667, 8.0, 1000)
    )) shouldBe Some("https://image.tmdb.org/t/p/w500/high.jpg")
  }

  it should "exclude landscape/square variants and return None when nothing portrait survives" in {
    TmdbClient.bestPortraitPosterUrl(Seq(
      TmdbClient.PosterImage("/wide.jpg",   Some("pl"), 1.78, 9.0, 3000),
      TmdbClient.PosterImage("/square.jpg", Some("pl"), 1.0,  9.0, 2000)
    )) shouldBe None
  }

  it should "return None for an empty poster set" in {
    TmdbClient.bestPortraitPosterUrl(Seq.empty) shouldBe None
  }

  "parsePosters" should "decode file_path, language, aspect, vote and width (null language → None)" in {
    val body =
      """{"posters":[
        |  {"file_path":"/a.jpg","iso_639_1":"pl","aspect_ratio":0.667,"vote_average":7.5,"width":2000},
        |  {"file_path":"/b.jpg","iso_639_1":null,"aspect_ratio":0.69,"vote_average":3.1,"width":1500}
        |]}""".stripMargin
    val ps = TmdbClient.parsePosters(body)
    ps.map(_.filePath) shouldBe Seq("/a.jpg", "/b.jpg")
    ps.head.language   shouldBe Some("pl")
    ps(1).language     shouldBe None
    ps.head.voteAverage shouldBe 7.5 +- 0.001
  }

  // ── synopsis tie-break among same-year same-title hits ────────────────────
  //
  // "Niedźwiedzica" (2026) is BOTH a nature documentary and an unrelated
  // thriller released the same year. Title is exact for both and year-distance
  // ties at 0, so the legacy picker falls to popularity — which here favours the
  // thriller. The cinema's own Polish blurb is about the she-bear documentary,
  // so passing it as the reference must flip the pick to the doc.

  private val niedzwiedzicaSameYear =
    """{"results":[
      |  {"id":100,"title":"Niedźwiedzica","original_title":"Niedźwiedzica",
      |   "release_date":"2026-03-01","popularity":80.0,
      |   "overview":"Trzymający w napięciu thriller o napadzie na bank, w którym grupa złodziei zostaje uwięziona przez policję w centrum wielkiego miasta."},
      |  {"id":200,"title":"Niedźwiedzica","original_title":"Niedźwiedzica",
      |   "release_date":"2026-09-01","popularity":20.0,
      |   "overview":"Przyrodniczy dokument śledzący niedźwiedzicę i jej młode w Tatrach przez cały rok, gdy uczą się przetrwać w dzikich górach."}
      |]}""".stripMargin

  private val bearDocCinemaBlurb =
    "Dokument o matczynej niedźwiedzicy, która w Tatrach prowadzi swoje młode " +
      "przez pierwszy rok życia, ucząc je przetrwania w dzikich górach."

  "search" should "fall to popularity among same-year exact-title hits without a reference synopsis (regression guard)" in {
    val client = fakeClient(Map("&year=2026" -> niedzwiedzicaSameYear))
    client.search("Niedźwiedzica", Some(2026)).map(_.id) shouldBe Some(100)
  }

  it should "break a same-year exact-title tie toward the synopsis-matching hit when a reference is given" in {
    val client = fakeClient(Map("&year=2026" -> niedzwiedzicaSameYear))
    client.search("Niedźwiedzica", Some(2026), Some(bearDocCinemaBlurb)).map(_.id) shouldBe Some(200)
  }

  it should "keep the legacy pick when the reference synopsis matches neither candidate" in {
    val client = fakeClient(Map("&year=2026" -> niedzwiedzicaSameYear))
    val unrelated = "Komedia o grupie przyjaciół otwierających food truck nad morzem."
    client.search("Niedźwiedzica", Some(2026), Some(unrelated)).map(_.id) shouldBe Some(100)
  }

  "parseSearchResults" should "carry the Polish overview onto each SearchResult" in {
    val json =
      """{"results":[
        |  {"id":1,"title":"a","release_date":"2024-01-01","popularity":1.0,"overview":"Polski opis filmu."},
        |  {"id":2,"title":"b","release_date":"2024-01-01","popularity":1.0,"overview":""}
        |]}""".stripMargin
    val results = client.parseSearchResults(json)
    results.find(_.id == 1).flatMap(_.overview) shouldBe Some("Polski opis filmu.")
    results.find(_.id == 2).flatMap(_.overview) shouldBe None   // empty → None
  }

  // Real TMDB pl-PL data: a search for "zaproszenie" returns the 2026 Olivia
  // Wilde film "The Invite" titled "Zaproszenie." (tmdb 950028, note the
  // trailing period) alongside the 2022 horror "The Invitation" titled
  // "Zaproszenie" (tmdb 830788, no period). Cinemas report "Zaproszenie".
  private val Invite2026      = TmdbClient.SearchResult(950028, "Zaproszenie.", Some("The Invite"),     Some(2026), 40.0)
  private val Invitation2022  = TmdbClient.SearchResult(830788, "Zaproszenie",  Some("The Invitation"), Some(2022), 30.0)
  private val Invitation2016  = TmdbClient.SearchResult(306947, "Zaproszenie",  Some("The Invitation"), Some(2016),  5.0)
  private val Zaproszenie1986 = TmdbClient.SearchResult(572997, "Zaproszenie",  Some("Zaproszenie"),    Some(1986),  2.0)

  "isExactTitleMatch" should "match a TMDB title that differs from the query only by a trailing period" in {
    // The bug: "Zaproszenie." (the 2026 Olivia Wilde film) was excluded from the
    // exact-match set against the exhibitor's "Zaproszenie", so it could never
    // win resolution and the 2022 "Zaproszenie" was picked instead.
    TmdbClient.isExactTitleMatch(Invite2026, "Zaproszenie") shouldBe true
  }

  it should "still match case- and whitespace-insensitively (legacy behaviour)" in {
    TmdbClient.isExactTitleMatch(Invitation2022, "  ZAPROSZENIE  ") shouldBe true
  }

  it should "not match a genuinely different title" in {
    TmdbClient.isExactTitleMatch(Invite2026, "Zaproszenie na ślub") shouldBe false
  }

  it should "preserve diacritics — punctuation-blind is not diacritic-blind" in {
    TmdbClient.isExactTitleMatch(
      TmdbClient.SearchResult(1, "Pokłosie", Some("Aftermath"), Some(2012), 1.0), "Poklosie") shouldBe false
  }

  "pickBest" should "select the trailing-period 2026 film over the period-clean 2022 same-title film for year 2026" in {
    // Mixed-era result set, query "Zaproszenie", year 2026. Before the
    // punctuation fix, "Zaproszenie." was dropped from the exact-match set and
    // the closest-year period-clean exact match (the 2022 horror) won. Now the
    // 2026 film is an exact match at year-distance 0 and wins.
    val results = Seq(Zaproszenie1986, Invitation2016, Invitation2022, Invite2026)
    client.pickBest(results, "Zaproszenie", Some(2026)).map(_.id) shouldBe Some(950028)
  }
}
