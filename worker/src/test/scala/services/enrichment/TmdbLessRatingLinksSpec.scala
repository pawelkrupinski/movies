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

  it should "not be linked when it is no single film: a pass, a fan event, a double bill, a live event" in {
    // Convergence 2026-09-30: "12. SPLAT! FilmFest | karnet" and "Avengers: Doomsday RealD 3D Fan Event"
    // were searched on Metacritic and RT — live requests for nothing, in production too.
    Seq("12. SPLAT! FilmFest | karnet", "Avengers: Doomsday RealD 3D Fan Event", "Blumfest Presents Other Mommy Fan Event Screening",
      "MOBILE SUIT GUNDAM HATHAWAY DOUBLE BILL", "Basia. Humor w paski mam + Kocia Szajka", "Koncert Jacka Wójcickiego")
      .foreach(title => withClue(title)(TmdbLessRatingLinks.eligible(unmatched(title, Some(2026), Seq("Someone"))._3) shouldBe false))
    // A film whose title merely contains such a word stays eligible, and so does one film plus a
    // discussion or a meeting: Warsaw's "Wśród nocnej ciszy + dyskusja" is Chmielewski's 1978 film.
    TmdbLessRatingLinks.eligible(unmatched("Passengers", Some(2016), Seq("Morten Tyldum"))._3) shouldBe true
    Seq("Wśród nocnej ciszy + dyskusja | Kino (nie)jawne: queerowe kody PRL-u", "Zygfryd + spotkanie z reżyserem", "Pan's Labyrinth + Q&A")
      .foreach(title => withClue(title)(TmdbLessRatingLinks.eligible(unmatched(title, Some(1978), Seq("Tadeusz Chmielewski"))._3) shouldBe true))
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

  "a stored Rotten Tomatoes page" should "stay when its director agrees, though a retrospective's screening dates the row decades later" in {
    // "Letnie przesilenie: Pokaz przedpremierowy Przekleństw niewinności" (2026) is Coppola's 1999
    // "The Virgin Suicides": linked on the director, then dropped on the year every refresh, then re-linked.
    val url  = "https://www.rottentomatoes.com/m/virgin_suicides"
    val page =
      """<html><head><script type="application/ld+json">{"@type":"Movie","name":"The Virgin Suicides","dateCreated":"1999-05-19",
        |"director":[{"@type":"Person","name":"Sofia Coppola"}]}</script></head>
        |<body><rt-text slot="criticsScore">77%</rt-text><script>{"releaseYear":"1999"}</script></body></html>""".stripMargin
    val (cache, key, row) = unmatched("Przekleństwa niewinności", Some(2026), Seq("Sofia Coppola"))
    cache.putIfPresent(key, _.copy(rottenTomatoesUrl = Some(url)))
    val rt = new RottenTomatoesClient(new GetOnlyHttpFetch { def get(u: String): String = if (u == url) page else UpstreamNotFound(u) })
    new RottenTomatoesRatings(cache, new TmdbClient(new RealHttpFetch, apiKey = None), rt).refreshOneSync(key)
    cache.get(key).flatMap(_.rottenTomatoesUrl) shouldBe Some(url)
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
