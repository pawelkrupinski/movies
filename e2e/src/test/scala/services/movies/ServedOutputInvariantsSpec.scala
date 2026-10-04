package services.movies

import controllers.{CinemaShowtimes, FilmSchedule}
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

import java.time.{LocalDate, LocalDateTime}

/**
 * Every card a user is served keeps the served-output invariants ([[ServedOutputInvariants]]):
 * the rules themselves over hand-built cards, then the Polish fixture boot's whole served corpus.
 *
 * Each country's RECORDED corpus is held to the same rules by its convergence leg
 * (`CountryConvergenceBehaviour`, "serve only cards that keep the served-output invariants"),
 * which boots that corpus through the whole pipeline against its recorded enrichment tree — the
 * only place a recorded corpus is enriched, so the only place its cards exist. The allowlist both
 * read is [[ServedOutputAllowlist]].
 */
class ServedOutputInvariantsSpec extends AnyFlatSpec with Matchers {

  import ServedOutputInvariantsSpec._

  private def findings(cards: ServedCard*): Seq[String] =
    ServedOutputInvariants.violations(Country.Poland, SingleCountryNormalizer.titleNormalizer, cards).map(_.finding)

  "the invariants" should "pass a card a user can trust" in {
    findings(card()) shouldBe empty
  }

  they should "find two records serving one tmdbId in a city, but not one record's display-title split" in {
    findings(card(title = "Diuna", recordId = "a"), card(title = "Diuna 2", recordId = "b", slug = Some("diuna-2"))) shouldBe
      Seq("tmdb-shared-across-records: 693134 (Diuna | Diuna 2)", "tmdb-shared-across-records: 693134 (Diuna | Diuna 2)")
    findings(card(title = "Diuna", variants = 2), card(title = "Diuna ukraiński dubbing", variants = 2, slug = Some("diuna-ua"))) shouldBe empty
    findings(card(title = "Diuna", recordId = "a"), card(title = "Diuna", recordId = "b", city = Warszawa)) shouldBe empty
  }

  they should "find a card no stored film projects" in {
    findings(card().copy(record = None, recordId = None)) shouldBe Seq("card-without-stored-film")
  }

  they should "find a slug two cards of a city share" in {
    findings(card(recordId = "a", tmdbId = Some(1)), card(title = "Diuna 2", recordId = "b", tmdbId = Some(2))) shouldBe
      Seq("slug-shared: diuna", "slug-shared: diuna")
  }

  they should "find a showtime listed twice, an empty run, and a card with nothing to see" in {
    findings(card(times = Seq(at(18), at(18)))) shouldBe Seq("showtime-repeated: Kino Rialto 2026-06-08T18:00 ×2")
    findings(card(times = Seq(at(18), at(18, url = Some("https://www.bilety24.pl#"))))) shouldBe
      Seq("showtime-repeated: Kino Rialto 2026-06-08T18:00 ×2")
    findings(card(times = Seq(at(18, room = Some("Sala 1")), at(18, room = Some("Sala 1"), url = None)))) shouldBe
      Seq("showtime-repeated: Kino Rialto 2026-06-08T18:00 Sala 1 ×2")
    withClue("parallel screens, each sold under its own link, are two screenings: ")(
      findings(card(times = Seq(at(18, url = Some("https://kinorialto.pl/kup/1")), at(18, url = Some("https://kinorialto.pl/kup/2"))))) shouldBe empty)
    findings(card(times = Seq(at(18, format = List("2D")), at(18, format = List("IMAX"))))) shouldBe empty
    findings(card(times = Nil)) shouldBe Seq("card-without-showtime", "showing-without-showtime: Kino Rialto")
  }

  they should "find a link a browser cannot follow" in {
    findings(card(poster = Some("/img/poster.jpg"))) shouldBe Seq("poster-url-unfollowable: /img/poster.jpg")
    findings(card(filmUrl = "https://kinorialto.pl/film /diuna")) shouldBe Seq("film-url-unfollowable: Kino Rialto https://kinorialto.pl/film /diuna")
    findings(card(times = Seq(at(18, url = Some("javascript:buy()"))))) shouldBe Seq("booking-url-unfollowable: Kino Rialto javascript:buy()")
    findings(card(times = Seq(at(18, url = Some("https:///kup"))))) shouldBe Seq("booking-url-unfollowable: Kino Rialto https:///kup")
  }

  they should "find a rating out of its range, or linked to another film or to a search page" in {
    findings(card(ratings = Ratings.copy(imdb = Some(0.0)))) shouldBe Seq("imdb-out-of-range: 0.0")
    findings(card(ratings = Ratings.copy(metascore = Some(101)))) shouldBe Seq("metascore-out-of-range: 101")
    findings(card(ratings = Ratings.copy(rottenTomatoes = Some(-1)))) shouldBe Seq("rotten-tomatoes-out-of-range: -1")
    findings(card(ratings = Ratings.copy(imdbUrl = Some("https://www.imdb.com/title/tt0000001/")))) shouldBe
      Seq("imdb-link-not-the-film's: https://www.imdb.com/title/tt0000001/ (imdbId tt15239678)")
    findings(card(ratings = Ratings.copy(imdbUrl = None))) shouldBe Seq("imdb-rating-without-link")
    findings(card(imdbId = Some("tt123"), ratings = Ratings.copy(imdb = None, imdbUrl = Some("https://www.imdb.com/title/tt123/")))) shouldBe
      Seq("imdb-id-malformed: tt123")
    findings(card(ratings = Ratings.copy(metacriticUrl = RatingSearchUrls.metacritic("Diuna")))) shouldBe
      Seq("metacritic-score-with-search-link: https://www.metacritic.com/search/Diuna/?category=2")
    findings(card(ratings = Ratings.copy(rottenTomatoesUrl = "https://www.imdb.com/m/dune"))) shouldBe
      Seq("rotten-tomatoes-link-off-site: https://www.imdb.com/m/dune")
    findings(card(ratings = Ratings.copy(metascore = None, metacriticUrl = RatingSearchUrls.metacritic("Diuna")))) shouldBe empty
    ServedOutputInvariants.violations(Country.UnitedKingdom, TitleNormalizer.forCountry(Country.UnitedKingdom), Seq(card(city = London, countries = Seq("United States"), genres = Seq("Science Fiction")))).map(_.finding) shouldBe
      Seq("filmweb-served-outside-poland")
  }

  they should "find a year far from TMDB's" in {
    findings(card(year = Some(2014))) shouldBe Seq("year-far-from-tmdb: 2014 (TMDB 2024)")
    findings(card(year = Some(2022))) shouldBe empty
  }

  they should "find an empty title, and a screening marker in a plain card's title but not in a variant's" in {
    findings(card(title = " ")) shouldBe Seq("title-empty")
    findings(card(title = "Diuna 2D napisy")) shouldBe Seq("title-carries-marker: 2d, napisy")
    findings(card(title = "Fonomo 26 - Joybubbles reż. Rachel J. Morrison")) shouldBe Seq("title-carries-marker: reż.")
    findings(card(title = "Diuna 2D dubbing", variants = 2)) shouldBe empty
    withClue("an event's or a programme's own words name its card by design: ")(findings(card(title = "Pokaz specjalny – Diuna")) shouldBe empty)
    ServedOutputInvariants.violations(Country.UnitedKingdom, TitleNormalizer.forCountry(Country.UnitedKingdom),
      Seq(card(title = "(4DX Rewind) Twisters", city = London, countries = Nil, genres = Nil, ratings = Ratings.copy(filmweb = None, filmwebUrl = "")))) shouldBe empty
  }

  they should "find a country or genre name the language does not spell so" in {
    findings(card(countries = Seq("Niderlandy"))) shouldBe Seq("country-not-canonical (→ Holandia): Niderlandy")
    findings(card(countries = Seq("Vereinigte Staaten"))) shouldBe Seq("country-in-another-language: Vereinigte Staaten")
    findings(card(countries = Seq("USA", "usa"))) shouldBe Seq("country-not-canonical (→ USA): usa", "country-repeated: USA")
    findings(card(genres = Seq("Comedy"))) shouldBe Seq("genre-in-another-language: Comedy")
    findings(card(genres = Seq("Dramat, Komedia"))) shouldBe Seq("genre-is-a-list: Dramat, Komedia")
    findings(card(genres = Seq("Dramat", "Sci-Fi", "Fantasy"))) shouldBe empty
    ServedOutputInvariants.violations(Country.Germany, TitleNormalizer.forCountry(Country.Germany), Seq(card(city = Berlin, countries = Seq("USA"), genres = Seq("Komödie"))))
      .map(_.finding) should contain only ("country-not-canonical (→ Vereinigte Staaten): USA", "filmweb-served-outside-poland")
  }

  "the fixture boot 08-06-2026" should "serve only cards that keep the served-output invariants" in {
    val wiring = new FixtureTestWiring("08-06-2026")
    wiring.bootStartup()
    val cards = ServedOutputInvariants.cardsOf(wiring, Country.Poland, SingleCountryNormalizer.titleNormalizer, FixtureNow)
    info(ServedOutputInvariants.coverage(cards))
    cards.size should be > 100
    withClue("no card joined a stored film with a tmdbId, so the rules reading it held over nothing: ")(
      cards.count(_.record.exists(_.tmdbId.isDefined)) should be > 100)
    val verdict = ServedOutputAllowlist.judge(Country.Poland, SingleCountryNormalizer.titleNormalizer, cards)
    withClue(s"Served cards break the served-output invariants — fix the stage that produced the value, or allowlist " +
      s"the card in ServedOutputAllowlist with why it is right:\n${verdict.unexplained.mkString("\n")}\n")(verdict.unexplained shouldBe empty)
    withClue("Allowlisted but no longer breaking a rule — drop the entry: ")(verdict.stale shouldBe empty)
  }
}

object ServedOutputInvariantsSpec {

  private val FixtureNow = LocalDateTime.of(2026, 6, 8, 0, 0)
  private val London = Country.UnitedKingdom.cities.head
  private val Berlin = Country.Germany.cities.head

  private val Ratings = ResolvedRatings(
    imdb = Some(8.1), imdbUrl = Some("https://www.imdb.com/title/tt15239678/"),
    metascore = Some(79), metacriticUrl = "https://www.metacritic.com/movie/dune-part-two/",
    rottenTomatoes = Some(92), rottenTomatoesUrl = "https://www.rottentomatoes.com/m/dune_part_two",
    filmweb = Some(8.0), filmwebUrl = "https://www.filmweb.pl/film/Diuna%3A+Cz%C4%99%C5%9B%C4%87+druga-2024-10000")

  private def at(hour: Int, url: Option[String] = Some("https://kinorialto.pl/kup/1"), room: Option[String] = None,
                 format: List[String] = Nil): Showtime =
    Showtime(LocalDateTime.of(2026, 6, 8, hour, 0), url, room, format)

  private def card(title: String = "Diuna", city: City = Poznan, recordId: String = "diuna", variants: Int = 1,
                   tmdbId: Option[Int] = Some(693134), imdbId: Option[String] = Some("tt15239678"),
                   year: Option[Int] = Some(2024), poster: Option[String] = Some("https://image.tmdb.org/t/p/w500/dune.jpg"),
                   filmUrl: String = "https://kinorialto.pl/film/diuna", times: Seq[Showtime] = Seq(at(18)),
                   ratings: ResolvedRatings = Ratings, countries: Seq[String] = Seq("USA"), genres: Seq[String] = Seq("Sci-Fi"),
                   slug: Option[String] = Some("diuna")): ServedCard = {
    val resolved = ResolvedMovie(_id = s"$recordId|$title", title = title, originalTitle = None, posterUrl = poster,
      fallbackPosterUrls = Nil, runtimeMinutes = Some(166), releaseYear = year, genres = genres, countries = countries,
      directors = Seq("Denis Villeneuve"), cast = Nil, synopsis = None, trailerUrls = Nil, ratings = ratings, weightedRating = 8.0)
    val schedule = FilmSchedule(Movie(title, Some(166), year, countries, genres), poster, None, Nil, Nil, Seq(Rialto -> filmUrl),
      Seq(LocalDate.of(2026, 6, 8) -> Seq(CinemaShowtimes(Rialto, times))), resolved, slug, LocalDate.of(2026, 6, 8))
    val record = MovieRecord(tmdbId = tmdbId, imdbId = imdbId, data = Map(Tmdb -> SourceData(releaseYear = Some(2024))))
    ServedCard(city, schedule, Some(record), Some(recordId), variants)
  }
}
