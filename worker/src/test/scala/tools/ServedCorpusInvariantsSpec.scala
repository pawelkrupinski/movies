package tools

import models._
import models.Kinoteka
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, StoredMovieRecord}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/** Each check of [[ServedCorpusInvariants]] fires on the one corruption it names, and the
 *  clean corpus it is built from passes — so a check that stopped firing would fail here,
 *  not silently pass a convergence leg. */
class ServedCorpusInvariantsSpec extends AnyFlatSpec with Matchers {

  private val Seven = LocalDateTime.of(2026, 9, 25, 19, 0)
  private val Nine  = LocalDateTime.of(2026, 9, 25, 21, 0)

  private def listing(cinema: Cinema, title: String, at: LocalDateTime*): (Cinema, CinemaMovie) =
    cinema -> CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, at.map(Showtime(_, bookingUrl = None)))

  private def film(id: String, title: String, tmdbId: Option[Int], slots: (Cinema, String)*): StoredMovieRecord =
    StoredMovieRecord(title, Some(2026), MovieRecord(tmdbId = tmdbId, data = slots.map { case (c, t) =>
      (CinemaShowing.keyFor(c, t, titleNormalizer): Source) -> SourceData(title = Some(t), showtimes = Seq(Showtime(Seven, None)))
    }.toMap), FilmId(id))

  private def card(id: String): ResolvedMovie = ResolvedMovie(
    _id = id, title = id, originalTitle = None, posterUrl = None, fallbackPosterUrls = Nil, runtimeMinutes = None,
    releaseYear = Some(2026), genres = Nil, countries = Nil, directors = Nil, cast = Nil, synopsis = None,
    trailerUrls = Nil, ratings = ResolvedRatings(imdb = None, imdbUrl = None, metascore = None, metacriticUrl = "",
      rottenTomatoes = None, rottenTomatoesUrl = "", filmweb = None, filmwebUrl = ""), weightedRating = 0.0)

  private def screening(filmId: String, cinema: Cinema, at: LocalDateTime*): CityScreening =
    CityScreening(s"$filmId|poznan|${cinema.displayName}", filmId, "poznan", cinema.displayName, None,
      at.map(Showtime(_, bookingUrl = None)))

  // Two films at one multiplex, both at 19:00 — the shape a set of (cinema, start) pairs cannot tell apart.
  private val listings = Seq(listing(Helios, "Diuna", Seven, Nine), listing(Helios, "Lalka", Seven))
  private val records  = Seq(film("f1", "Diuna", Some(1), Helios -> "Diuna"), film("f2", "Lalka", Some(2), Helios -> "Lalka"))
  private val cards    = Seq(card("f1"), card("f2"))
  private val served   = Seq(screening("f1", Helios, Seven, Nine), screening("f2", Helios, Seven))

  private def check(listings: Seq[(Cinema, CinemaMovie)] = listings, records: Seq[StoredMovieRecord] = records,
                    cards: Seq[ResolvedMovie] = cards, served: Seq[CityScreening] = served,
                    from: LocalDateTime = LocalDateTime.MIN, country: Option[Country] = None): Seq[String] =
    ServedCorpusInvariants.violations(listings, records, cards, served, titleNormalizer, from, country)

  "a corpus served exactly as listed" should "pass" in {
    check() shouldBe empty
  }

  "a listing no stored film holds" should "be named as lost" in {
    check(records = records.take(1), cards = cards.take(1), served = served.take(1)).mkString should
      (include("held by NO stored film") and include("'Lalka' at Helios Posnania"))
  }

  "a listing held by two films" should "be named as duplicated" in {
    check(records = records :+ film("f3", "Lalka bis", Some(3), Helios -> "Lalka"), cards = cards :+ card("f3"),
      served = served :+ screening("f3", Helios, Seven)).mkString should include("held by SEVERAL stored films")
  }

  "a showtime served under the wrong film" should "be named on both sides, though every (cinema, start) pair is present" in {
    // Diuna's 19:00 moved onto Lalka's card: the SET of (Helios, 19:00) and (Helios, 21:00) is unchanged.
    val moved = Seq(screening("f1", Helios, Nine), screening("f2", Helios, Seven))
    check(served = moved).mkString should include("NOT served under the film that holds them")
    val doubled = Seq(screening("f1", Helios, Seven, Nine), screening("f2", Helios, Seven, Nine))
    check(served = doubled).mkString should include("no archived listing of that film gave it")
  }

  "a card with no screenings, or a screening with no card" should "be named" in {
    check(served = served.take(1)).mkString should include("NO screening")
    check(cards = cards.take(1)).mkString should include("whose film is not served")
  }

  "a film ready to project that is not served" should "be named" in {
    check(cards = cards.take(1), served = served.take(1)).mkString should include("ready to project that the read model does not serve")
  }

  "two films holding one tmdbId" should "be named" in {
    check(records = Seq(records.head, records(1).copy(record = records(1).record.copy(tmdbId = Some(1))))).mkString should
      include("tmdb 1:")
  }

  "a screening filed in another country, or a film holding a foreign venue" should "be named as a leak" in {
    val uk = Country.UnitedKingdom
    check(country = Some(Country.Poland)) shouldBe empty
    val leaked = check(country = Some(uk)).mkString
    leaked should include("screening row(s) outside")
    leaked should include("holding a venue of another country")
  }

  "a venue whose own year and director both deny its film" should "be named as a wrong merge — and nothing weaker should" in {
    // PL sample, 2026-09-25: Kinoteka's Wong Kar Wai "Happy Together" served as Kim Jeong-hwan's 2018 film.
    def resolved(tmdbYear: Int, tmdbDirector: String, slotYear: Int, slotDirector: String) = StoredMovieRecord("Happy Together",
      Some(tmdbYear), MovieRecord(tmdbId = Some(551655), data = Map[Source, SourceData](
        Tmdb     -> SourceData(title = Some("Happy Together"), releaseYear = Some(tmdbYear), director = Seq(tmdbDirector)),
        Kinoteka -> SourceData(title = Some("Happy Together"), releaseYear = Some(slotYear), director = Seq(slotDirector)))), FilmId("f9"))
    def keys(r: StoredMovieRecord) = ServedCorpusInvariants.wrongMerges(Seq(r), titleNormalizer).map(_._1)

    keys(resolved(2018, "Kim Jeong-hwan", 2026, "Wong Kar Wai")) shouldBe Seq("happytogether|2018")
    keys(resolved(1997, "Wong Kar-wai", 2026, "Wong Kar Wai")) shouldBe empty      // the screening year alone
    keys(resolved(2018, "Kim Jeong-hwan", 2019, "Wong Kar Wai")) shouldBe empty    // a director alone
    keys(resolved(1994, "王家衛", 2026, "Wong Kar Wai")) shouldBe empty             // two scripts, one man

    // A venue that writes its year only into the title (the US shape): "A Star Is Born (1954)"
    // served as Cooper's 2018 film, found by the per-listing check on the US corpus.
    val garland = StoredMovieRecord("A Star Is Born", Some(2018), MovieRecord(tmdbId = Some(332562), data = Map[Source, SourceData](
      Tmdb     -> SourceData(title = Some("A Star Is Born"), releaseYear = Some(2018), director = Seq("Bradley Cooper")),
      Kinoteka -> SourceData(title = Some("A Star Is Born (1954)"), director = Seq("George Cukor")))), FilmId("f8"))
    ServedCorpusInvariants.wrongMerges(Seq(garland), titleNormalizer).map(_._1) shouldBe Seq("astarisborn|2018")
  }

  "showtimes before `from`" should "count on neither side" in {
    // A failed venue keeps its old slot, yesterday's 19:00 included; that is not an invented showtime.
    check(listings = Seq(listing(Helios, "Diuna", Nine), listing(Helios, "Lalka", Nine)),
      served = Seq(screening("f1", Helios, Seven, Nine), screening("f2", Helios, Seven, Nine)), from = Nine) shouldBe empty
  }
}
