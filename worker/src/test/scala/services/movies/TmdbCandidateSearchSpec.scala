package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{ResolutionCache, TmdbBasis}
import tools.GetOnlyHttpFetch

/** The search half of TMDB resolution, asked directly — no cache, no bus, no
 *  write. What it returns is a candidate and the basis it was concluded on; what
 *  `MovieService` does with that is the other class's business. */
class TmdbCandidateSearchSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private val Dreams = 1134463

  private def search(tmdb: TmdbClient) =
    new TmdbCandidateSearch(tmdb, titleNormalizer, ResolutionCache.passthrough, letterboxdIdResolver = None, wikidata = None)

  "TmdbCandidateSearch" should "walk a reported director's filmography and say so in the basis" in {
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"              -> """{"results":[]}""",
      "query=Michel+Franco"        -> """{"results":[{"id":5000,"name":"Michel Franco","known_for_department":"Directing"}]}""",
      "/person/5000/movie_credits" -> s"""{"crew":[{"id":$Dreams,"title":"Dreams","original_title":"Dreams: Sueños","release_date":"2025-07-10","department":"Directing","popularity":6.2}]}""",
      s"/movie/$Dreams?"           -> s"""{"id":$Dreams,"title":"Dreams","original_title":"Dreams: Sueños","release_date":"2025-07-10","runtime":98,"credits":{"crew":[{"job":"Director","name":"Michel Franco"}],"cast":[]}}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Dreams"), director = Seq("Michel Franco"), runtimeMinutes = Some(98))))

    val found = search(tmdb).resolve("Dreams", Some(2025), row, originalTitle = None, director = None)
    found.map(_._1) shouldBe Some(Dreams)
    found.flatMap(_._3) shouldBe Some(TmdbBasis.DirectorWalk)
  }

  it should "collapse a cached id onto its adjacent-year TMDB duplicate even when the reported person is credited only as WRITER" in {
    // The walk finds a film through a person's WRITING credits when they have no
    // directing ones (a cinema printed the writer), so the duplicate collapse has
    // to look the same person up the same way — otherwise a cached higher-id
    // duplicate of one film ("Gourou" held as 1315702/2025 and 1259983/2026)
    // survives just because the credit sits under Writing.
    val (low, high) = (1259983, 1315702)
    def details(id: Int, date: String) =
      s"""{"id":$id,"title":"Gourou","original_title":"Gourou","release_date":"$date","runtime":120,"credits":{"crew":[],"cast":[]}}"""
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=Yann+Gozlan"          -> """{"results":[{"id":5000,"name":"Yann Gozlan","known_for_department":"Writing"}]}""",
      "/person/5000/movie_credits" -> s"""{"crew":[
        |{"id":$low,"title":"Gourou","original_title":"Gourou","release_date":"2026-01-28","department":"Writing"},
        |{"id":$high,"title":"Gourou","original_title":"Gourou","release_date":"2025-10-01","department":"Writing"}
        |]}""".stripMargin,
      s"/movie/$low?"              -> details(low, "2026-01-28"),
      s"/movie/$high?"             -> details(high, "2025-10-01")
    )), apiKey = Some("stub"))
    val cachedHigh = new ResolutionCache {
      def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = Some(high.toString)
    }
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Gourou"), director = Seq("Yann Gozlan"))))

    new TmdbCandidateSearch(tmdb, titleNormalizer, cachedHigh, letterboxdIdResolver = None, wikidata = None)
      .resolve("Gourou", Some(2026), row).map(_._1) shouldBe Some(low)
  }

  it should "not collapse onto a NAMESAKE's same-title film from the adjacent year" in {
    // TMDB answers a director's name with every person of that name. The collapse pools
    // credits only to find DUPLICATES of the film the row resolved to, so they must come
    // from the person whose filmography holds that film — pooling a namesake's writing
    // credits let an unrelated lower-id "Home" (2024) take over the resolved one (2025).
    val (resolved, namesakes) = (900001, 100001)
    def details(id: Int, date: String) =
      s"""{"id":$id,"title":"Home","original_title":"Home","release_date":"$date","runtime":95,"credits":{"crew":[],"cast":[]}}"""
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=Anna+Nowak"             -> """{"results":[
        |{"id":7001,"name":"Anna Nowak","known_for_department":"Directing"},
        |{"id":7002,"name":"Anna Nowak","known_for_department":"Writing"}]}""".stripMargin,
      "/person/7001/movie_credits"   -> s"""{"crew":[
        |{"id":$resolved,"title":"Home","original_title":"Home","release_date":"2025-03-01","department":"Directing","job":"Director"}
        |]}""".stripMargin,
      "/person/7002/movie_credits"   -> s"""{"crew":[
        |{"id":$namesakes,"title":"Home","original_title":"Home","release_date":"2024-11-20","department":"Writing","job":"Screenplay"}
        |]}""".stripMargin,
      s"/movie/$resolved?"           -> details(resolved, "2025-03-01"),
      s"/movie/$namesakes?"          -> details(namesakes, "2024-11-20")
    )), apiKey = Some("stub"))
    val cachedResolved = new ResolutionCache {
      def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = Some(resolved.toString)
    }
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Home"), director = Seq("Anna Nowak"))))

    new TmdbCandidateSearch(tmdb, titleNormalizer, cachedResolved, letterboxdIdResolver = None, wikidata = None)
      .resolve("Home", Some(2025), row).map(_._1) shouldBe Some(resolved)
  }

  it should "refuse to guess for a bare title the search cannot narrow to one film" in {
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie" -> """{"results":[
        |{"id":1,"title":"Guru","original_title":"Guru","release_date":"2026-01-01","popularity":5.0},
        |{"id":2,"title":"Guru","original_title":"Gourou","release_date":"2025-01-01","popularity":4.0}
        |]}""".stripMargin
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some("Guru"))))

    search(tmdb).resolve("Guru", None, row, originalTitle = None, director = None) shouldBe None
  }

  // ── `searchUnique` must not trust an irrelevant decoration-split fragment ──

  /** UK convergence, 2026-09-15→17: `SearchTitles.candidates`' banner-split adds
   *  "Catching Fire" as an extra candidate for "The Hunger Games: Catching
   *  Fire" (everything after the first ": "). The undivided title's own TMDB
   *  search correctly sees two results and refuses — but a live TMDB
   *  search-ranking anomaly briefly made the SHORT split fragment return a
   *  single, spurious hit (a same-franchise, unrelated entry — sharing
   *  "Hunger"/"Games" with the undivided title, which is exactly why the fix
   *  compares a hit against the SPECIFIC candidate that found it, "Catching
   *  Fire", rather than the whole candidate set), and the OLD unconditional
   *  "exactly one row" trust let `searchUnique` accept it. Verifying the hit
   *  shares a distinctive word with "Catching Fire" itself — which it does
   *  not — is what now refuses it. */
  it should "refuse to resolve via a decoration-split fragment whose sole hit shares no distinctive word with it" in {
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=The+Hunger+Games%3A+Catching+Fire" ->
        """{"results":[
          |{"id":101299,"title":"The Hunger Games: Catching Fire","release_date":"2013-11-15"},
          |{"id":871533,"title":"Surviving the Game: Making The Hunger Games: Catching Fire","release_date":"2014-03-07"}
          |]}""".stripMargin,
      "query=Catching+Fire" ->
        """{"results":[{"id":999,"title":"The Hunger Games: Sunrise on the Reaping","release_date":"2026-11-18"}]}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("The Hunger Games: Catching Fire"))))

    search(tmdb).resolve(
      "The Hunger Games: Catching Fire", None, row, originalTitle = None, director = None) shouldBe None
  }

  /** The mirror of the case above, and the reason the fix could not simply drop
   *  decoration-split candidates from `searchUnique` altogether (tried and
   *  reverted — it broke real resolutions): a Polish venue's own listing
   *  routinely resolves ONLY via its colon-banner-split fragment, because the
   *  undivided "banner: film" string never matches anything on TMDB. The split
   *  fragment here ("drzewo magii") shares a distinctive word with its sole
   *  hit's own title, so it must still resolve. */
  it should "still resolve via a decoration-split fragment whose sole hit shares a distinctive word with it" in {
    val DrzewoMagii = 1140521
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=Kino+przyjazne+sensorycznie%3A+drzewo+magii" -> """{"results":[]}""",
      "query=drzewo+magii" ->
        s"""{"results":[{"id":$DrzewoMagii,"title":"Drzewo magii","original_title":"The Magic Faraway Tree","release_date":"2026-11-20"}]}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Kino przyjazne sensorycznie: drzewo magii"))))

    val found = search(tmdb).resolve(
      "Kino przyjazne sensorycznie: drzewo magii", None, row, originalTitle = None, director = None)
    found.map(_._1) shouldBe Some(DrzewoMagii)
  }

  /** UK convergence, 2026-09-15→17, second regression on the SAME fix: an earlier
   *  version required every `searchUnique` hit to share a distinctive word with its
   *  query, with no exception — which broke a genuine cross-language TRANSLATION
   *  with no other signal. A Ukrainian-dubbed listing's OWN reported title
   *  ("Посіпаки і Монстряки", not a decoration split of anything — it is the whole
   *  title as printed) shares no token at all with "Minionki i straszydła"/"Minions
   *  & Monsters", by definition: different words, different languages, same film.
   *  It must still resolve — only a de-decorated FRAGMENT needs the extra check. */
  it should "still resolve a row's own complete title via a translation sharing no distinctive word with the hit" in {
    val MinionkiIStraszydla = 1315772
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "query=%D0%9F%D0%BE%D1%81%D1%96%D0%BF%D0%B0%D0%BA%D0%B8+%D1%96+%D0%9C%D0%BE%D0%BD%D1%81%D1%82%D1%80%D1%8F%D0%BA%D0%B8" ->
        s"""{"results":[{"id":$MinionkiIStraszydla,"title":"Minionki i straszydła","original_title":"Minions & Monsters","release_date":"2026-08-01"}]}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Посіпаки і Монстряки"))))

    val found = search(tmdb).resolve(
      "Посіпаки і Монстряки", None, row, originalTitle = None, director = None)
    found.map(_._1) shouldBe Some(MinionkiIStraszydla)
  }

  // ── `directorWalk`'s year-pinned tier must not fall for a same-franchise
  //    entry sharing only the common prefix ──────────────────────────────────

  /** UK convergence, 2026-09-15→17, third regression, and the one that actually
   *  reproduces what CI saw: a rerelease listing stamps EVERY title with the
   *  season's current year (`437d1fa21`'s "Odeon" trap, `YearWindow`'s own doc),
   *  so a bare "The Hunger Games: Catching Fire" row carries `releaseYear =
   *  2026` though the real film is 2013. That wrong year does TWO things: it
   *  filters the real "Catching Fire" credit OUT of `eligible` (year 13 apart,
   *  past `titleClose`'s year tolerance), so `byTitle` finds nothing despite an
   *  exact title match existing — and it then uniquely pins `byYear` to
   *  whichever OTHER Francis Lawrence credit happens to BE 2026: "Sunrise on the
   *  Reaping", a not-yet-released franchise entry sharing nothing but "The
   *  Hunger Games" with the query. `corroboratedByTitle` saw that shared prefix
   *  as confirmation before `SequelMarker`'s curated list knew the two apart. */
  it should "refuse a year-pinned credit that is a not-yet-released same-franchise sibling, not the wrong year's real film" in {
    val SunriseOnTheReaping = 1300968
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"                -> """{"results":[]}""",
      "query=Francis+Lawrence"       -> """{"results":[{"id":10943,"name":"Francis Lawrence","known_for_department":"Directing"}]}""",
      "/person/10943/movie_credits"  -> s"""{"crew":[
        |{"id":101299,"title":"The Hunger Games: Catching Fire","original_title":"The Hunger Games: Catching Fire","release_date":"2013-11-15","department":"Directing","popularity":23.5},
        |{"id":$SunriseOnTheReaping,"title":"The Hunger Games: Sunrise on the Reaping","original_title":"The Hunger Games: Sunrise on the Reaping","release_date":"2026-11-18","department":"Directing","popularity":40.0}
        |]}""".stripMargin
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("The Hunger Games: Catching Fire"),
        director = Seq("Francis Lawrence"), releaseYear = Some(2026))))

    val found = search(tmdb).resolve(
      "The Hunger Games: Catching Fire", Some(2026), row, originalTitle = None, director = None)
    found.map(_._1) should not be Some(SunriseOnTheReaping)
    found shouldBe None
  }

  // ── An IMDb-style disambiguator suffix must not blind the person search ────

  /** IMDb tells two same-named people apart with a trailing "(I)"/"(II)"/…
   *  suffix — "Tom Holland (II)" is the "Child's Play" 1988 director, not the
   *  "Spider-Man" actor of the same name. That suffix is IMDb's own scheme; TMDB
   *  has never heard of it, and `/search/person` matches the raw string
   *  verbatim. Sent with the parenthetical intact, "Tom Holland (II)" finds
   *  nobody and the whole director-walk resolution abstains — confirmed on
   *  corpus row `chuckydiemorderpuppe|1988` (DE), `director: ["Tom Holland
   *  (II)"]`, `originalTitle: "Child's Play"`, `releaseYear: 1988`. Stubbing
   *  `/search/person` ONLY for the clean "Tom Holland" query (not the
   *  disambiguated form) is what makes this fail before the strip and pass
   *  after it. */
  it should "strip a trailing IMDb disambiguator before searching TMDB for the director" in {
    val ChildsPlay = 587219
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      "/search/movie"               -> """{"results":[]}""",
      "query=Tom+Holland&"          -> """{"results":[{"id":9001,"name":"Tom Holland","known_for_department":"Directing"}]}""",
      "/person/9001/movie_credits"  -> s"""{"crew":[{"id":$ChildsPlay,"title":"Chucky - die Mörderpuppe","original_title":"Child's Play","release_date":"1988-11-09","department":"Directing","popularity":10.0}]}""",
      s"/movie/$ChildsPlay?"        -> s"""{"id":$ChildsPlay,"title":"Chucky - die Mörderpuppe","original_title":"Child's Play","release_date":"1988-11-09","runtime":87,"credits":{"crew":[{"job":"Director","name":"Tom Holland"}],"cast":[]}}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Chucky - die Mörderpuppe"), originalTitle = Some("Child's Play"),
        director = Seq("Tom Holland (II)"), runtimeMinutes = Some(87))))

    val found = search(tmdb).resolve(
      "Chucky - die Mörderpuppe", Some(1988), row, originalTitle = Some("Child's Play"), director = None)
    found.map(_._1) shouldBe Some(ChildsPlay)
    found.flatMap(_._3) shouldBe Some(TmdbBasis.DirectorWalk)
  }

  /** IMDb's disambiguator is always an UPPERCASE Roman numeral, and a small one
   *  — nobody has a hundred namesakes. A lowercase parenthetical that happens to
   *  be spelled with Roman-numeral letters ("(mix)", "(dim)", "(vi)") is part of
   *  the credit, and so is an uppercase run that is no valid numeral ("(IIII)",
   *  "(VX)") or a large one that reads as a word ("(MIX)", "(DC)"). */
  "ImdbDisambiguatorSuffix" should "strip only an uppercase, valid Roman numeral" in {
    def strip(name: String) = TmdbCandidateSearch.ImdbDisambiguatorSuffix.replaceFirstIn(name, "")

    Seq("Tom Holland (II)", "Tom Holland (I)", "Tom Holland (IV)", "Tom Holland (XIV)", "Tom Holland (XLIX)")
      .foreach(name => withClue(name)(strip(name) shouldBe "Tom Holland"))
    Seq("DJ Food (mix)", "Lights (dim)", "Someone (vi)", "Someone (ii)", "Someone (IIII)", "Someone (VX)",
        "Someone (MIX)", "Someone (DC)", "Someone (IL)", "Tom Holland (II) Jr")
      .foreach(name => withClue(name)(strip(name) shouldBe name))
  }
}
