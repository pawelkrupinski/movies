package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.{ResolutionCache, TmdbBasis}
import tools.GetOnlyHttpFetch

/**
 * Regression: "Queen: Hungarian Rhapsody – Live in Budapest '86" (DE, reported
 * director "János Zsombolyai", year 1987) — 134 cinema slots, 265 upcoming
 * showtimes, all invisible because it never got a tmdbId. Confirmed real and on
 * TMDB as id 142773.
 *
 * TMDB's own crew/person data for this specific 1986 Eastern-Bloc TV recording
 * doesn't credit that director in a way `directorWalk`'s filmography walk can
 * find — TMDB's `/search/person` doesn't even resolve the name to a person, so
 * the walk yields nothing — and until now a director-bearing row had NO other
 * path to try once the walk failed, however unambiguous a plain title+year
 * search would have been on its own.
 *
 * This is a DIFFERENT, weaker check than what `DirectorWalkResolvesSpec`'s
 * "refuse rather than accept a same-director title-search hit" case guards
 * against: that case is about verifying a search hit by asking whether its
 * crew names the reported director, which can't separate two films by the SAME
 * director. This fallback never looks at the director at all — it only takes a
 * title+year hit strict enough (a singleton, or an exact-title year-scoped top)
 * that the director-LESS branch already trusts it unguarded.
 */
class DirectorWalkNoCreditFallsBackToTitleSearchSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private val QueenBudapest86 = 142773
  private val Title           = "Queen: Hungarian Rhapsody – Live in Budapest '86"
  private val Director        = "János Zsombolyai"

  private def search(tmdb: TmdbClient) =
    new TmdbCandidateSearch(tmdb, titleNormalizer, ResolutionCache.passthrough, letterboxdIdResolver = None, wikidata = None)

  "a director-bearing row whose walk finds no matching credit" should
    "fall through to the same year-scoped exact-title search a director-less row trusts" in {
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      // TMDB doesn't resolve the reported director to a person at all — the
      // walk abstains immediately, exactly as it did for the real Zsombolyai row.
      "/search/person" -> """{"results":[]}""",
      // But the title (scoped to the cinema's year) is an exact, unambiguous hit.
      "/search/movie"  -> s"""{"results":[{"id":$QueenBudapest86,"title":"$Title","original_title":"$Title","release_date":"1987-01-01","popularity":3.0}]}""",
      s"/movie/$QueenBudapest86?" -> s"""{"id":$QueenBudapest86,"title":"$Title","original_title":"$Title","release_date":"1987-01-01","runtime":85,"credits":{"crew":[],"cast":[]}}"""
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(1987))))

    val found = search(tmdb).resolve(Title, Some(1987), row, originalTitle = None, director = None)

    found.map(_._1) shouldBe Some(QueenBudapest86)
    // Resolved from the fallback title search, not a walk that never ran.
    found.flatMap(_._3) shouldBe Some(TmdbBasis.YearScoped)
    found.flatMap(_._3) should not be Some(TmdbBasis.DirectorWalk)
  }

  it should "still refuse when neither the walk nor the fallback title search can narrow it" in {
    val tmdb = new TmdbClient(http = new StubFetch(Map(
      // The walk finds a person, but their filmography has no matching credit
      // (no title/year match) — directorWalk genuinely returns None.
      "/search/person" -> """{"results":[{"id":9001,"name":"Janos Zsombolyai","known_for_department":"Directing"}]}""",
      "/person/9001/movie_credits" -> """{"crew":[
        |{"id":999999,"title":"Some Other Film","original_title":"Some Other Film",
        | "release_date":"2001-01-01","department":"Directing","popularity":1.0}
        |]}""".stripMargin,
      // And the title search itself is ambiguous — two same-title hits at the
      // same year, so the fallback correctly abstains too.
      "/search/movie" -> s"""{"results":[
        |{"id":$QueenBudapest86,"title":"$Title","original_title":"$Title","release_date":"1987-01-01","popularity":3.0},
        |{"id":222222,"title":"$Title","original_title":"$Title","release_date":"1987-06-06","popularity":1.0}
        |]}""".stripMargin
    )), apiKey = Some("stub"))
    val row = MovieRecord(data = Map[Source, SourceData](
      Helios -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(1987))))

    search(tmdb).resolve(Title, Some(1987), row, originalTitle = None, director = None) shouldBe None
  }
}
