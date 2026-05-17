package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.FilmwebClient
import services.enrichment.FilmwebClient.{Candidate, SearchHit}
import tools.{GetOnlyHttpFetch, RealHttpFetch}

class FilmwebClientSpec extends AnyFlatSpec with Matchers {

  /** Test stub: routes URLs by substring to canned JSON. */
  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"HTTP 404 for $url"))
  }

  private val client = new FilmwebClient(new RealHttpFetch)

  // ── parseSearch ─────────────────────────────────────────────────────────────

  "parseSearch" should "extract film- and serial-type ids in the order Filmweb returned them, skipping persons / users" in {
    val json =
      """{
        |  "total": 4,
        |  "searchHits": [
        |    {"id": 10058855, "type": "film",   "matchedTitle": "Sentimental Value"},
        |    {"id": 2559,     "type": "person", "matchedTitle": "Stellan Skarsgård"},
        |    {"id": 838929,   "type": "film",   "matchedTitle": "Wartość sentymentalna"},
        |    {"id": 10113677, "type": "serial", "matchedTitle": "Kicia Kocia w podróży"}
        |  ]
        |}""".stripMargin
    client.parseSearch(json) shouldBe Seq(
      SearchHit(10058855, "film"),
      SearchHit(838929,   "film"),
      SearchHit(10113677, "serial")
    )
  }

  it should "return empty when search has no hits" in {
    client.parseSearch("""{"total":0,"searchHits":[]}""") shouldBe empty
  }

  it should "skip entries missing an id" in {
    val json = """{"searchHits":[{"type":"film","matchedTitle":"orphan"}]}"""
    client.parseSearch(json) shouldBe empty
  }

  // ── parseInfo ───────────────────────────────────────────────────────────────

  "parseInfo" should "pull title + originalTitle + year from /film/{id}/info" in {
    val json =
      """{
        |  "title": "Obcy: Przebudzenie",
        |  "originalTitle": "Alien: Resurrection",
        |  "year": 1997,
        |  "type": "film",
        |  "subType": "film_cinema",
        |  "posterPath": "/01/34/134/7517908_1.$.jpg"
        |}""".stripMargin
    val info = client.parseInfo(json).get
    info.title         shouldBe "Obcy: Przebudzenie"
    info.originalTitle shouldBe Some("Alien: Resurrection")
    info.year          shouldBe Some(1997)
  }

  it should "handle responses without an originalTitle field" in {
    val json = """{"title":"Wartość sentymentalna","year":2025,"type":"film"}"""
    val info = client.parseInfo(json).get
    info.title         shouldBe "Wartość sentymentalna"
    info.originalTitle shouldBe None
    info.year          shouldBe Some(2025)
  }

  it should "return None when title is missing" in {
    client.parseInfo("""{"year":2025}""") shouldBe None
  }

  // ── parsePreview ────────────────────────────────────────────────────────────

  "parsePreview" should "extract director names from /film/{id}/preview" in {
    // Real-shape /preview payload — directors as [{id, name}, ...].
    val json =
      """{
        |  "mainCast":[{"id":1,"name":"Stellan Skarsgård"}],
        |  "directors":[{"id":2,"name":"Joachim Trier"}],
        |  "year":2025,
        |  "originalTitle":{"title":"Sentimental Value","country":"NO","lang":"no","original":true}
        |}""".stripMargin
    client.parsePreview(json).directors shouldBe Set("Joachim Trier")
  }

  it should "handle multi-director films" in {
    val json = """{"directors":[{"id":1,"name":"Bill Ross IV"},{"id":2,"name":"Turner Ross"}]}"""
    client.parsePreview(json).directors shouldBe Set("Bill Ross IV", "Turner Ross")
  }

  it should "return an empty director set when the field is absent" in {
    client.parsePreview("""{"year":2024}""").directors shouldBe empty
  }

  it should "drop director entries without a name" in {
    val json = """{"directors":[{"id":1},{"id":2,"name":"X"}]}"""
    client.parsePreview(json).directors shouldBe Set("X")
  }

  // ── parseRating ─────────────────────────────────────────────────────────────

  "parseRating" should "extract the 1-10 rate value" in {
    client.parseRating("""{"count": 26186, "rate": 7.5034}""") shouldBe Some(7.5034)
  }

  it should "return None when rate is missing" in {
    client.parseRating("""{"count": 0}""") shouldBe None
  }

  // ── pickBest: title acceptance bar ──────────────────────────────────────────

  private def candidate(
    id:            Int    = 1,
    title:         String = "",
    originalTitle: Option[String] = None,
    year:          Option[Int]    = Some(2024),
    directors:     Set[String]    = Set.empty,
    kind:          String = "film"
  ): Candidate = Candidate(id, kind, title, originalTitle, year, directors)

  "pickBest" should "accept an exact title match (case-insensitive)" in {
    val hits = Seq(candidate(id = 1, title = "Wartość sentymentalna", year = Some(2025)))
    client.pickBest(hits, "wartość sentymentalna", Some(2025), Set.empty).map(_.id) shouldBe Some(1)
  }

  it should "accept an exact match on originalTitle when title differs" in {
    val hits = Seq(candidate(id = 2, title = "Pulp Fiction", originalTitle = Some("Pulp Fiction"), year = Some(1994)))
    client.pickBest(hits, "Pulp Fiction", Some(1994), Set.empty).map(_.id) shouldBe Some(2)
  }

  it should "accept a Polish title match when originalTitle differs" in {
    // Diuna 2 — cinema reports the Polish title; Filmweb's /info has both
    // Polish title and English originalTitle. Polish-title match wins.
    val hits = Seq(candidate(
      id = 779836, title = "Diuna: Część druga",
      originalTitle = Some("Dune: Part Two"), year = Some(2024)
    ))
    client.pickBest(hits, "Diuna: Część druga", Some(2024), Set.empty).map(_.id) shouldBe Some(779836)
  }

  it should "REJECT a candidate whose title doesn't match the query at all" in {
    // The bug: Filmweb's search returns "matchedTitle" hits where the
    // canonical title is completely different (e.g. id=838929 search-matches
    // "Wartość sentymentalna" but its /info title is "It's About Time").
    val hits = Seq(candidate(id = 838929, title = "It's About Time", year = Some(2015)))
    client.pickBest(hits, "Wartość sentymentalna", Some(2025), Set.empty) shouldBe None
  }

  it should "REJECT a partial-word match (no modifier-suffix)" in {
    // Query "Belle" must not accept "Belleville Cop" or "Beauty and the Belle".
    val hits = Seq(
      candidate(id = 1, title = "Belleville Cop"),
      candidate(id = 2, title = "Beauty and the Belle")
    )
    client.pickBest(hits, "Belle", Some(2021), Set.empty) shouldBe None
  }

  it should "accept a modifier-suffix match (re-release / restoration / colon-subtitled)" in {
    // RT/MC pattern: "Title - Re-Release", "Title: Restored", "Title (Anniversary Edition)".
    val hits = Seq(candidate(id = 1, title = "La Dolce Vita - Re-Release", year = Some(2020)))
    client.pickBest(hits, "La Dolce Vita", Some(1960), Set.empty).map(_.id) shouldBe Some(1)
  }

  // ── pickBest: year disambiguation ───────────────────────────────────────────

  it should "prefer the year-closest candidate among accepted ones" in {
    val hits = Seq(
      candidate(id = 1, title = "Belle", year = Some(1973)),
      candidate(id = 2, title = "Belle", year = Some(2021)),
      candidate(id = 3, title = "Belle", year = Some(2013))
    )
    client.pickBest(hits, "Belle", Some(2022), Set.empty).map(_.id) shouldBe Some(2)
  }

  it should "fall back to the first accepted candidate when no year is supplied" in {
    val hits = Seq(
      candidate(id = 1, title = "Belle", year = Some(1973)),
      candidate(id = 2, title = "Belle", year = Some(2021))
    )
    client.pickBest(hits, "Belle", None, Set.empty).map(_.id) shouldBe Some(1)
  }

  // ── pickBest: director verification ─────────────────────────────────────────

  it should "require director overlap when caller AND candidate both have directors" in {
    val hits = Seq(
      candidate(id = 1, title = "Belle", year = Some(2013), directors = Set("Amma Asante")),
      candidate(id = 2, title = "Belle", year = Some(2021), directors = Set("Mamoru Hosoda"))
    )
    client.pickBest(hits, "Belle", None, Set("Mamoru Hosoda")).map(_.id) shouldBe Some(2)
  }

  it should "reject every candidate when the caller's director matches none of them" in {
    val hits = Seq(
      candidate(id = 1, title = "Belle", year = Some(2013), directors = Set("Amma Asante")),
      candidate(id = 2, title = "Belle", year = Some(2021), directors = Set("Mamoru Hosoda"))
    )
    client.pickBest(hits, "Belle", None, Set("Christopher Nolan")) shouldBe None
  }

  it should "match directors case-insensitively and ignore diacritics" in {
    // Cinema reports "Malgorzata Szumowska" without diacritics; Filmweb has
    // "Małgorzata Szumowska". They should match.
    val hits = Seq(candidate(id = 1, title = "Bodysnatchers", directors = Set("Małgorzata Szumowska")))
    client.pickBest(hits, "Bodysnatchers", None, Set("malgorzata szumowska")).map(_.id) shouldBe Some(1)
  }

  it should "skip director verification when the candidate has no directors" in {
    // FW row with empty directors → can't verify; don't reject, fall back to
    // title+year. This protects against missing /preview data.
    val hits = Seq(candidate(id = 1, title = "Foo", year = Some(2024), directors = Set.empty))
    client.pickBest(hits, "Foo", Some(2024), Set("Anyone")).map(_.id) shouldBe Some(1)
  }

  it should "skip director verification when the caller passes no directors" in {
    val hits = Seq(candidate(id = 1, title = "Foo", year = Some(2024), directors = Set("Someone")))
    client.pickBest(hits, "Foo", Some(2024), Set.empty).map(_.id) shouldBe Some(1)
  }

  // ── lookup: end-to-end flow with stubbed HTTP ───────────────────────────────

  "lookup" should "return None when no candidate's canonical title matches the query" in {
    // Filmweb's search returns id 838929 for "Wartość sentymentalna" but
    // /info reveals the canonical title is "It's About Time" (2015). The
    // tightened lookup must NOT pick this candidate.
    val routes = Map(
      "/live/search"      -> """{"searchHits":[{"id":838929,"type":"film","matchedTitle":"Wartość sentymentalna"}]}""",
      "/film/838929/info" -> """{"title":"It's About Time","year":2015,"type":"film"}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    fw.lookup("Wartość sentymentalna", Some(2025)) shouldBe None
  }

  // Production regression: "Cirque du Soleil: Kooza" was stored against
  // `https://www.filmweb.pl/film/Asystent+wampira-2009-212254` — id 212254
  // is "Asystent wampira" / "Cirque du Freak: The Vampire's Assistant"
  // (2009), a completely different film. The row pre-dated the tightened
  // matcher; the cheap rating-only refresh path left it untouched, so the
  // wrong URL persisted. Filmweb's `live/search` ranks "Cirque du Freak"
  // first for the query "Cirque du Soleil: Kooza" — the matchedTitle
  // field is fuzzy across the shared "Cirque du" prefix. Today's matcher
  // must reject every candidate (no canonical title or originalTitle
  // equals or modifier-suffixes the query) and store None. MC and RT
  // already return None for this film because their slug-probe / search-
  // scrape paths require an exact-or-modifier title match from day one;
  // this guards Filmweb against regressing back to the loose match.
  it should "return None for 'Cirque du Soleil: Kooza' — Filmweb has no entry and the top fuzzy hit is unrelated" in {
    val routes = Map(
      "/live/search"      -> """{"searchHits":[
        |  {"id":212254,"type":"film","matchedTitle":"Cirque du Freak: The Vampire's Assistant"},
        |  {"id":743459,"type":"film","matchedTitle":"Juste la fin du monde"},
        |  {"id":61,"type":"film","matchedTitle":"Les Couloirs du temps: Les visiteurs 2"},
        |  {"id":650592,"type":"film","matchedTitle":"Cirque du Soleil: Dalekie światy"},
        |  {"id":7895,"type":"film","matchedTitle":"Cet obscur objet du désir"}
        |]}""".stripMargin,
      "/film/212254/info" -> """{"title":"Asystent wampira","originalTitle":"Cirque du Freak: The Vampire's Assistant","year":2009}""",
      "/film/743459/info" -> """{"title":"To tylko koniec świata","originalTitle":"Juste la fin du monde","year":2016}""",
      "/film/61/info"     -> """{"title":"Goście, goście II - korytarz czasu","originalTitle":"Les Couloirs du temps: Les visiteurs 2","year":1998}""",
      "/film/650592/info" -> """{"title":"Cirque du Soleil: Dalekie światy","originalTitle":"Cirque du Soleil: Worlds Away","year":2012}""",
      "/film/7895/info"   -> """{"title":"Mroczny przedmiot pożądania","originalTitle":"Cet obscur objet du désir","year":1977}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    fw.lookup("Cirque du Soleil: Kooza", Some(2008)) shouldBe None
  }

  it should "pick the candidate whose canonical title matches and skip the search-noise ones" in {
    val routes = Map(
      "/live/search"          -> """{"searchHits":[
        |{"id":838929,"type":"film","matchedTitle":"Wartość sentymentalna"},
        |{"id":10058855,"type":"film","matchedTitle":"Sentimental Value"}
        |]}""".stripMargin,
      "/film/838929/info"     -> """{"title":"It's About Time","year":2015}""",
      "/film/10058855/info"   -> """{"title":"Wartość sentymentalna","originalTitle":"Sentimental Value","year":2025}""",
      "/film/10058855/rating" -> """{"rate":7.8,"count":1000}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Wartość sentymentalna", Some(2025))
    r should not be empty
    r.get.url    should include ("-10058855")
    r.get.rating shouldBe Some(7.8)
  }

  it should "fall back to the fallback title when the primary query has no acceptable match" in {
    // Cinema's Polish title doesn't resolve, but TMDB's originalTitle does.
    val routes = Map(
      "/live/search?query=Diuna"          -> """{"searchHits":[]}""",
      "/live/search?query=Dune"           -> """{"searchHits":[{"id":779836,"type":"film","matchedTitle":"Dune: Part Two"}]}""",
      "/film/779836/info"                 -> """{"title":"Diuna: Część druga","originalTitle":"Dune: Part Two","year":2024}""",
      "/film/779836/rating"               -> """{"rate":8.2,"count":1}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Diuna", Some(2024), fallback = Some("Dune: Part Two"))
    r should not be empty
    r.get.url should include ("-779836")
  }

  it should "consult /preview only when the caller passes directors" in {
    // When directors=Set.empty the lookup must NOT hit /preview — saves a
    // round-trip per candidate. We prove it by leaving /preview unstubbed:
    // any call would throw "HTTP 404" from the stub.
    val routes = Map(
      "/live/search"       -> """{"searchHits":[{"id":42,"type":"film","matchedTitle":"X"}]}""",
      "/film/42/info"      -> """{"title":"X","year":2024}""",
      "/film/42/rating"    -> """{"rate":7.0,"count":1}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    noException should be thrownBy fw.lookup("X", Some(2024))
  }

  it should "use /preview to verify director overlap when caller passes directors" in {
    val routes = Map(
      "/live/search"           -> """{"searchHits":[
        |{"id":1,"type":"film","matchedTitle":"Belle"},
        |{"id":2,"type":"film","matchedTitle":"Belle"}
        |]}""".stripMargin,
      "/film/1/info"           -> """{"title":"Belle","year":2013}""",
      "/film/2/info"           -> """{"title":"Belle","year":2021}""",
      "/film/1/preview"        -> """{"directors":[{"id":10,"name":"Amma Asante"}]}""",
      "/film/2/preview"        -> """{"directors":[{"id":11,"name":"Mamoru Hosoda"}]}""",
      "/film/2/rating"         -> """{"rate":7.5,"count":1}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Belle", None, directors = Set("Mamoru Hosoda"))
    r.map(_.url) should not be empty
    r.get.url should include ("-2")
  }

  it should "return None when the only candidate's director disagrees with the caller's" in {
    val routes = Map(
      "/live/search"        -> """{"searchHits":[{"id":1,"type":"film","matchedTitle":"Belle"}]}""",
      "/film/1/info"        -> """{"title":"Belle","year":2013}""",
      "/film/1/preview"     -> """{"directors":[{"id":10,"name":"Amma Asante"}]}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    fw.lookup("Belle", Some(2013), directors = Set("Christopher Nolan")) shouldBe None
  }

  // Some Polish children's "films" in cinemas are actually episodic content
  // that Filmweb files under /serial/ (TV-series taxonomy) rather than
  // /film/. The same `/info` / `/rating` endpoints serve both — the only
  // difference is the canonical page URL prefix. Kicia Kocia w podróży was
  // the trigger: Filmweb's serial entry is the only exact-title match, and
  // dropping it (the prior film-only filter) left the matcher to settle
  // for unrelated fuzzy film hits ("Nie ma duchów w mieszkaniu na Dobrej").
  it should "match a serial-typed result and build a /serial/ URL" in {
    val routes = Map(
      "/live/search"          -> """{"searchHits":[
        |{"id":10113677,"type":"serial","matchedTitle":"Kicia Kocia w podróży"},
        |{"id":752725,"type":"film","matchedTitle":"Dobrze się kłamie w miłym towarzystwie"}
        |]}""".stripMargin,
      "/film/10113677/info"   -> """{"title":"Kicia Kocia w podróży","originalTitle":"Kicia Kocia w podróży","year":2026,"type":"serial"}""",
      "/film/10113677/rating" -> """{"rate":6.7,"count":87}""",
      "/film/752725/info"     -> """{"title":"Dobrze się kłamie w miłym towarzystwie","year":2016}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Kicia Kocia w podróży", Some(2026))
    r should not be empty
    r.get.url    shouldBe "https://www.filmweb.pl/serial/Kicia+Kocia+w+podr%C3%B3%C5%BCy-2026-10113677"
    r.get.rating shouldBe Some(6.7)
  }
}
