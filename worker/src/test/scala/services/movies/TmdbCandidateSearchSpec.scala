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

  // ── `searchUnique` must not trust a decoration-split fragment ──────────────

  /** UK convergence, 2026-09-15→17: `SearchTitles.candidates`' banner-split adds
   *  "Catching Fire" as an extra candidate for "The Hunger Games: Catching
   *  Fire" (everything after the first ": "). The undivided title's own TMDB
   *  search correctly sees two results and refuses — but a live TMDB
   *  search-ranking anomaly briefly made the SHORT split fragment return a
   *  single, spurious hit (a same-franchise, unrelated entry), and the OLD
   *  unrestricted candidate pool let `searchUnique` accept it unconditionally.
   *  `wholeCandidates` keeps that check from ever trying the split fragment,
   *  so the row correctly refuses instead of mis-resolving. */
  it should "refuse to resolve via a banner-split fragment even when that fragment alone looks unique" in {
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
