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

  // ── search query normalisation ───────────────────────────────────────────────

  "search" should "query Filmweb with a case- and diacritic-folded title, so a row's exact spelling can't shift the recorded fixture" in {
    def queryUrlFor(title: String): String = {
      val urls = scala.collection.mutable.ListBuffer.empty[String]
      new FilmwebClient(new GetOnlyHttpFetch {
        override def get(url: String): String = { urls += url; """{"searchHits":[]}""" }
      }).search(title)
      urls.head
    }
    // "Słodkie Życie" → deburr + lowercase → "slodkie zycie" → URL-encoded.
    queryUrlFor("Słodkie Życie") should include ("query=slodkie+zycie")
    // Any casing/diacritic re-spelling of the SAME title (the settle's
    // minSpelling↔displayTitle flip) must hit the IDENTICAL query URL — hence the
    // same fingerprinted fixture — so canonicalisation never churns rating fixtures.
    queryUrlFor("SŁODKIE ŻYCIE") shouldBe queryUrlFor("Słodkie Życie")
  }

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

  it should "extract Polish genres from /preview" in {
    // Real-shape /preview payload — genres as [{id, name:{text}, nameKey}],
    // Polish labels in `name.text`.
    val json =
      """{
        |  "directors":[{"id":2,"name":"Christopher Nolan"}],
        |  "genres":[
        |    {"id":6,"name":{"text":"Dramat"},"nameKey":"6"},
        |    {"id":3,"name":{"text":"Biograficzny"},"nameKey":"3"}
        |  ]
        |}""".stripMargin
    client.parsePreview(json).genres shouldBe Seq("Dramat", "Biograficzny")
  }

  it should "return an empty genre list when /preview omits the field" in {
    client.parsePreview("""{"directors":[]}""").genres shouldBe empty
  }

  it should "extract the Polish plot from plot.synopsis" in {
    // Real-shape /preview payload — plot nested under `plot.synopsis`
    // (alongside sourceType + author), confirmed against /film/469476/preview.
    val json =
      """{
        |  "directors":[{"id":57013,"name":"Denis Villeneuve"}],
        |  "plot":{"synopsis":"Szlachetny ród Atrydów przybywa na Diunę.","sourceType":7,"author":"Yankes06"}
        |}""".stripMargin
    client.parsePreview(json).plot shouldBe Some("Szlachetny ród Atrydów przybywa na Diunę.")
  }

  it should "leave plot empty when /preview omits it or it's blank" in {
    client.parsePreview("""{"directors":[]}""").plot shouldBe None
    client.parsePreview("""{"plot":{"synopsis":""}}""").plot shouldBe None
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

  it should "match titles that differ only by a dash variant (en-dash vs hyphen)" in {
    // "Chainsaw Man – The Movie: Reze Arc" — the cinema title uses an en-dash,
    // Filmweb's /info uses a plain hyphen. Without dash folding the exact-title
    // bar fails (and isModifierSuffix needs a prefix match, which the dash also
    // breaks), so the film is missed despite being identical.
    val hits = Seq(candidate(id = 1, title = "Chainsaw Man - The Movie: Reze Arc", year = Some(2025)))
    client.pickBest(hits, "Chainsaw Man – The Movie: Reze Arc", Some(2025), Set.empty).map(_.id) shouldBe Some(1)
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

  // ── pickBest: no-director exact-title year gate (false-positive guard) ───────
  //
  // A yearless retrospective screening ("Konwicki: Lawa (1989)") that TMDB never
  // resolved carries no director. The `(1989)` read off the title is the only
  // disambiguator — without gating on it, Filmweb's fuzzy search took the sole
  // exact-"Lawa" hit, which is the UNRELATED "Lawa"/orig "Lava" (2014), id
  // 719437. When we have no director and a year IS known, an EXACT same-title
  // candidate must sit within ±1 of that year, or it's the same-title-different-
  // film collision and must be rejected.

  it should "reject an exact-title candidate whose year is far off when no director is available" in {
    // Konwicki's "Lawa" (1989) must NOT take the 2014 "Lava".
    val hits = Seq(candidate(id = 719437, title = "Lawa", originalTitle = Some("Lava"), year = Some(2014)))
    client.pickBest(hits, "Lawa", Some(1989), Set.empty) shouldBe None
  }

  it should "accept an exact-title candidate within ±1 of the supplied year with no director" in {
    val hits = Seq(candidate(id = 1, title = "Lawa", year = Some(1989)))
    client.pickBest(hits, "Lawa", Some(1989), Set.empty).map(_.id) shouldBe Some(1)
  }

  it should "still accept a modifier-suffix candidate whose year is far off (re-release exemption)" in {
    // The year gate is for EXACT same-title collisions only — a legitimate
    // "Title - Re-Release" re-dates by design and must survive.
    val hits = Seq(candidate(id = 1, title = "La Dolce Vita - Re-Release", year = Some(2020)))
    client.pickBest(hits, "La Dolce Vita", Some(1960), Set.empty).map(_.id) shouldBe Some(1)
  }

  it should "not gate the year when a matching director confirms the far-year candidate" in {
    // A director-verified match is already disambiguated; the year gate is a
    // no-director fallback and must not fire here.
    val hits = Seq(candidate(id = 1, title = "Lawa", year = Some(2014), directors = Set("Tadeusz Konwicki")))
    client.pickBest(hits, "Lawa", Some(1989), Set("Tadeusz Konwicki")).map(_.id) shouldBe Some(1)
  }

  // ── pickBest: prefer film over serial ────────────────────────────────────────

  it should "prefer a film over a same-title serial even when the serial's year is closer" in {
    // The "Ziemia obiecana" shape: Filmweb has Wajda's FILM (1974) and a TV
    // SERIES (1975). Querying at 1975 used to pick the year-closest serial; the
    // cinema is screening the film, so the film must win.
    val hits = Seq(
      candidate(id = 1073,   title = "Ziemia obiecana", year = Some(1974), kind = "film"),
      candidate(id = 105396, title = "Ziemia obiecana", year = Some(1975), kind = "serial")
    )
    client.pickBest(hits, "Ziemia obiecana", Some(1975), Set.empty).map(_.id) shouldBe Some(1073)
  }

  it should "still use a serial when it is the only match (legit children's serial)" in {
    // Kicia Kocia / Basia / Pucio exist on Filmweb ONLY as a /serial/ — no film
    // entry — so dropping serials would lose their rating; we keep them.
    val hits = Seq(candidate(id = 10113677, title = "Kicia Kocia w podróży", year = Some(2026), kind = "serial"))
    client.pickBest(hits, "Kicia Kocia w podróży", Some(2025), Set.empty).map(_.id) shouldBe Some(10113677)
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

  // ── pickBest: director + year override for transliterated titles ────────────
  //
  // Mavka — cinemas report the Polish "Mawka. Prawdziwy mit" but Filmweb files
  // the film under its transliterated original "Mavka. Spravzhnij mif". The
  // title bar alone drops it; a confident director+year pair (Katya Tsarik,
  // 2026) must override the title mismatch and accept the candidate.

  it should "accept a title-mismatched candidate when director overlaps AND year matches within ±1" in {
    val hits = Seq(candidate(
      id = 1, title = "Mavka. Spravzhnij mif", year = Some(2026),
      directors = Set("Katya Tsarik")
    ))
    client.pickBest(hits, "Mawka. Prawdziwy mit", Some(2026), Set("Katya Tsarik")).map(_.id) shouldBe Some(1)
  }

  it should "NOT override on director alone when the year disagrees (precision)" in {
    val hits = Seq(candidate(
      id = 1, title = "Mavka. Spravzhnij mif", year = Some(2010),
      directors = Set("Katya Tsarik")
    ))
    client.pickBest(hits, "Mawka. Prawdziwy mit", Some(2026), Set("Katya Tsarik")) shouldBe None
  }

  it should "NOT override a title-mismatched candidate whose director differs (precision)" in {
    val hits = Seq(candidate(
      id = 1, title = "Mavka. Spravzhnij mif", year = Some(2026),
      directors = Set("Someone Else")
    ))
    client.pickBest(hits, "Mawka. Prawdziwy mit", Some(2026), Set("Katya Tsarik")) shouldBe None
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

  it should "return the winner's full content slot — originalTitle, year, directors, genres, plot" in {
    // Filmweb is a first-class enrichment source now: the winner's /info +
    // /preview content is carried on FilmwebInfo (not just url + rating + genres)
    // so callers can persist a full slot. Caller passes no directors, so the
    // winner-only /preview fetch supplies directors/genres/plot.
    val routes = Map(
      "/live/search"     -> """{"searchHits":[{"id":42,"type":"film","matchedTitle":"Diuna"}]}""",
      "/film/42/info"    -> """{"title":"Diuna: Część druga","originalTitle":"Dune: Part Two","year":2024}""",
      "/film/42/preview" -> """{"directors":[{"id":1,"name":"Denis Villeneuve"}],"genres":[{"id":2,"name":{"text":"Sci-Fi"}}],"plot":{"synopsis":"Paul Atryda łączy siły z Fremenami."}}""",
      "/film/42/rating"  -> """{"rate":8.2,"count":100}"""
    )
    val r = new FilmwebClient(new StubFetch(routes)).lookup("Diuna: Część druga", Some(2024)).get
    r.originalTitle shouldBe Some("Dune: Part Two")
    r.year          shouldBe Some(2024)
    r.directors     shouldBe Seq("Denis Villeneuve")
    r.genres        shouldBe Seq("Sci-Fi")
    r.plot          shouldBe Some("Paul Atryda łączy siły z Fremenami.")
    r.rating        shouldBe Some(8.2)
  }

  "detailsFor" should "rebuild the full content slot from a stored canonical URL" in {
    // The resolution-cache HIT path: only the URL survived, so /info + /preview +
    // /rating are re-fetched off the id to repopulate the whole slot.
    val routes = Map(
      "/film/42/info"    -> """{"title":"X","originalTitle":"Y","year":2001}""",
      "/film/42/preview" -> """{"directors":[{"id":1,"name":"Dir Name"}],"genres":[{"id":2,"name":{"text":"Dramat"}}],"plot":{"synopsis":"Blurb."}}""",
      "/film/42/rating"  -> """{"rate":7.0,"count":1}"""
    )
    val r = new FilmwebClient(new StubFetch(routes)).detailsFor("https://www.filmweb.pl/film/X-2001-42").get
    r.originalTitle shouldBe Some("Y")
    r.year          shouldBe Some(2001)
    r.directors     shouldBe Seq("Dir Name")
    r.genres        shouldBe Seq("Dramat")
    r.plot          shouldBe Some("Blurb.")
    r.rating        shouldBe Some(7.0)
  }

  it should "fall back to the fallback title when the primary query has no acceptable match" in {
    // Cinema's Polish title doesn't resolve, but TMDB's originalTitle does.
    val routes = Map(
      "/live/search?query=diuna"          -> """{"searchHits":[]}""",        // query is deburr+lowercased
      "/live/search?query=dune"           -> """{"searchHits":[{"id":779836,"type":"film","matchedTitle":"Dune: Part Two"}]}""",
      "/film/779836/info"                 -> """{"title":"Diuna: Część druga","originalTitle":"Dune: Part Two","year":2024}""",
      "/film/779836/rating"               -> """{"rate":8.2,"count":1}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Diuna", Some(2024), fallback = Some("Dune: Part Two"))
    r should not be empty
    r.get.url should include ("-779836")
  }

  it should "find the film when a screening-cycle banner hides the bare title behind a pipe" in {
    // "Reze Arc | 26. Festiwal Filmowy" — the raw query (with the festival
    // banner) returns nothing; the pipe-stripped "Reze Arc" variant surfaces the
    // film. Stub returns empty for any search whose query still carries the
    // banner, the hit otherwise.
    val fw = new FilmwebClient(new GetOnlyHttpFetch {
      override def get(url: String): String =
        if (url.contains("/live/search"))
          if (url.contains("festiwal")) """{"searchHits":[]}"""
          else """{"searchHits":[{"id":7,"type":"film","matchedTitle":"Reze Arc"}]}"""
        else if (url.contains("/film/7/info"))   """{"title":"Reze Arc","year":2025}"""
        else if (url.contains("/film/7/rating")) """{"rate":7.5,"count":10}"""
        else throw new RuntimeException(s"HTTP 404 for $url")
    })
    val r = fw.lookup("Reze Arc | 26. Festiwal Filmowy", Some(2025))
    r should not be empty
    r.get.url should include ("-7")
  }

  // ── searchQueryVariants: banner / decoration stripping ───────────────────────

  "searchQueryVariants" should "expose the bare film title behind a pipe-separated cycle banner" in {
    FilmwebClient.searchQueryVariants("""Lawa - opowieść o "Dziadach" | Poniedziałki z Konwickim: pisarz""") should
      contain ("""Lawa - opowieść o "Dziadach"""")
  }

  it should "strip a trailing parenthetical" in {
    FilmwebClient.searchQueryVariants("Mama Mu wraca do domu (2021)") should contain ("Mama Mu wraca do domu")
  }

  it should "yield no variants for an undecorated title (no extra requests on the common path)" in {
    FilmwebClient.searchQueryVariants("Pulp Fiction") shouldBe empty
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

  it should "resolve a transliterated Filmweb entry via the director+year override (end-to-end)" in {
    // "Mawka. Prawdziwy mit" (cinema) vs Filmweb's /info title "Mavka.
    // Spravzhnij mif" — the title bar drops it, but /preview confirms director
    // Katya Tsarik and the year matches, so the override accepts it. Before the
    // change /preview was fetched only for title-accepted candidates, so this
    // never resolved.
    val routes = Map(
      "/live/search"    -> """{"searchHits":[{"id":1,"type":"film","matchedTitle":"Mawka"}]}""",
      "/film/1/info"    -> """{"title":"Mavka. Spravzhnij mif","year":2026}""",
      "/film/1/preview" -> """{"directors":[{"id":9,"name":"Katya Tsarik"}]}""",
      "/film/1/rating"  -> """{"rate":6.9,"count":50}"""
    )
    val fw = new FilmwebClient(new StubFetch(routes))
    val r = fw.lookup("Mawka. Prawdziwy mit", Some(2026), directors = Set("Katya Tsarik"))
    r.map(_.url) should not be empty
    r.get.url should include ("-1")
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

  // ── pickBest: synopsis tie-break among same-year same-title films ────────────
  //
  // Two "Niedźwiedzica" films, same year, both passing the title bar with no
  // director to separate them — sortKey ties, so the legacy picker keeps the
  // first. The TMDB reference blurb is about the she-bear documentary, so it
  // must flip the pick to that candidate.

  private val bearDoc = candidate(id = 200, title = "Niedźwiedzica", year = Some(2026))
    .copy(plot = Some("Przyrodniczy dokument śledzący niedźwiedzicę i jej młode w Tatrach przez cały rok, gdy uczą się przetrwać w dzikich górach."))
  private val bankThriller = candidate(id = 100, title = "Niedźwiedzica", year = Some(2026))
    .copy(plot = Some("Trzymający w napięciu thriller o napadzie na bank, w którym grupa złodziei zostaje uwięziona przez policję w centrum miasta."))
  private val bearReference =
    "Dokument o matczynej niedźwiedzicy, która w Tatrach prowadzi swoje młode przez pierwszy rok życia, ucząc je przetrwania w dzikich górach."

  it should "keep the first same-year same-title candidate without a reference synopsis (regression guard)" in {
    client.pickBest(Seq(bankThriller, bearDoc), "Niedźwiedzica", Some(2026), Set.empty).map(_.id) shouldBe Some(100)
  }

  it should "break a same-year same-title tie toward the plot-matching film when a reference is given" in {
    client.pickBest(Seq(bankThriller, bearDoc), "Niedźwiedzica", Some(2026), Set.empty, Some(bearReference)).map(_.id) shouldBe Some(200)
  }

  it should "keep the legacy pick when the reference matches neither plot" in {
    val unrelated = "Komedia o grupie przyjaciół otwierających food truck nad morzem."
    client.pickBest(Seq(bankThriller, bearDoc), "Niedźwiedzica", Some(2026), Set.empty, Some(unrelated)).map(_.id) shouldBe Some(100)
  }
}
