package services.enrichment

import clients.TmdbClient
import models.{KinoMuza, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.TmdbAttempt
import services.tasks.RatingSources
import tools.{GetOnlyHttpFetch, RealHttpFetch, UpstreamNotFound}

import java.time.Instant

/** Rating links for films TMDB has no record of (`TmdbLessRatingLinks`): asked only once TMDB found
 *  nothing, and a page taken only when it positively agrees with what the cinemas publish. */
class TmdbLessRatingLinksSpec extends AnyFlatSpec with Matchers {

  private val asked = Some(TmdbAttempt("searched", Instant.parse("2026-09-30T00:00:00Z")))

  /** A row TMDB was asked about and could not match, as one cinema published it. */
  private def unmatched(title: String, year: Option[Int], director: Seq[String]) = {
    val row = MovieRecord(tmdbAttempt = asked, data = Map(KinoMuza -> SourceData(title = Some(title), releaseYear = year, director = director)))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq((title, year, row)), normalizer = titleNormalizer), normalizer = titleNormalizer)
    (cache, cache.keyOf(title, year), row)
  }

  "a film TMDB could not match" should "be linked only when it publishes a year or a director" in {
    TmdbLessRatingLinks.eligible(unmatched("NADZY | Kino dyskomfortu", Some(2026), Seq("Mike Leigh"))._3) shouldBe true
    TmdbLessRatingLinks.eligible(unmatched("A Festival 2026 | Zmiana pierwszeństwa", None, Nil)._3) shouldBe false
    // Nor before TMDB was asked: a row still resolving is not a TMDB-less one.
    TmdbLessRatingLinks.eligible(unmatched("NADZY", Some(2026), Seq("Mike Leigh"))._3.copy(tmdbAttempt = None)) shouldBe false
    // The rating tasks queue it: RT and Metacritic everywhere, Filmweb where it is enabled.
    val row = unmatched("NADZY | Kino dyskomfortu", Some(2026), Seq("Mike Leigh"))._3
    RatingSources.all.filter(_.eligible(row)).map(_.taskType.toString) should contain allOf ("RtRating", "McRating", "FilmwebRating")
  }

  it should "be searched under its stripped titles" in {
    val (_, key, row) = unmatched("NADZY | Kino dyskomfortu", Some(2026), Seq("Mike Leigh"))
    TmdbLessRatingLinks.titlesOf(key, row, titleNormalizer) should contain ("NADZY")
  }

  "a page for it" should "be its film only when its director agrees, whatever year the screening dates it" in {
    // Leigh's 1993 "Naked", shown in 2026: the page's year differs, its director agrees.
    val (_, naked, nakedRow) = unmatched("NADZY | Kino dyskomfortu", Some(2026), Seq("Mike Leigh"))
    TmdbLessRatingLinks.corroborated(naked, nakedRow, Some(1993), Set("Mike Leigh")) shouldBe true
    TmdbLessRatingLinks.corroborated(naked, nakedRow, Some(2026), Set("Someone Else")) shouldBe false
    // A page crediting no one says nothing of the director; only an equal year can speak for it.
    TmdbLessRatingLinks.corroborated(naked, nakedRow, None, Set.empty) shouldBe false
    // A year-only row takes a page dated that very year, and nothing else.
    val (_, tabu, tabuRow) = unmatched("Akademia Kina Polskiego: Tabu (1987)", Some(1987), Nil)
    TmdbLessRatingLinks.corroborated(tabu, tabuRow, Some(1987), Set("Andrzej Barański")) shouldBe true
    TmdbLessRatingLinks.corroborated(tabu, tabuRow, Some(1988), Set.empty) shouldBe false
  }

  "Metacritic" should "link a TMDB-less film to a page crediting its director, and not to a namesake's" in {
    def page(director: String, year: Int) =
      s"""<html><head><script type="application/ld+json">{"@type":"Movie","name":"Naked","datePublished":"$year-01-01",
         |"director":[{"@type":"Person","name":"$director"}],
         |"aggregateRating":{"@type":"AggregateRating","ratingValue":86,"bestRating":100,"worstRating":0,"reviewCount":10}}
         |</script></head><body></body></html>""".stripMargin
    def resolving(pages: Map[String, String]) = {
      val (cache, key, _) = unmatched("NADZY | Kino dyskomfortu", Some(2026), Seq("Mike Leigh"))
      val mc = new MetacriticClient(new GetOnlyHttpFetch {
        def get(url: String): String = pages.get(url.stripSuffix("/")).orElse(pages.get(url)).getOrElse(UpstreamNotFound(url))
      })
      new MetascoreRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), mc).refreshOneSync(key)
      cache.get(key).flatMap(_.metacriticUrl)
    }
    resolving(Map("https://www.metacritic.com/movie/nadzy" -> page("Mike Leigh", 1993))) shouldBe Some("https://www.metacritic.com/movie/nadzy")
    resolving(Map("https://www.metacritic.com/movie/nadzy" -> page("Somebody Else", 2026))) shouldBe None
  }
}
