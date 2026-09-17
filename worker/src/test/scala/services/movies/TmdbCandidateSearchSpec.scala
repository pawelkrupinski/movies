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
}
