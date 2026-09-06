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
}
