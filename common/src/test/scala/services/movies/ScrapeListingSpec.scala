package services.movies

import tools.SpecClock.given

import models.{CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

class ScrapeListingSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, showtimes: Seq[Showtime], year: Option[Int] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), cinema = Helios, posterUrl = None, filmUrl = None,
      synopsis = None, cast = Nil, director = Nil, showtimes = showtimes)

  private def at(hour: Int, format: List[String] = Nil): Showtime =
    Showtime(LocalDateTime.of(2026, 9, 7, hour, 0), bookingUrl = None, format = format)

  private def prepare(movies: CinemaMovie*) =
    ScrapeListing.prepare(Helios, movies, titleNormalizer, ScreeningTokens.forDefaultCountry())

  "ScrapeListing.prepare" should "fold a venue's several rows for one film onto one slot, keeping every showing" in {
    // Two event pages for the same film, each listing one session, one of them twice
    // under a different booking link.
    val p = prepare(
      listing("Ojczyzna", Seq(at(18))),
      listing("Ojczyzna", Seq(at(20), at(20)), year = Some(2024)))
    p.movies.map(p.cleaned) shouldBe Seq("Ojczyzna")
    p.movies.head.showtimes.map(_.dateTime.getHour) shouldBe Seq(18, 20)
  }

  it should "give the listing the facts its title bills, the title's director over the one the client carried" in {
    // DE ScharfrichterKino's "Alpha (dir. Ducournau)" (recording 38713274617) carried kinoheld's copied credit of Shiv
    // Rawail's 2026 "Alpha"; the venue's own words name Ducournau. The slot's title loses the credit.
    val germany = TitleNormalizer.forCountry(models.Country.Germany)
    val copied  = listing("Alpha (dir. Ducournau)", Seq(at(18))).copy(director = Seq("Shiv Rawail"))
    val p = ScrapeListing.prepare(Helios, Seq(copied), germany, ScreeningTokens.forDefaultCountry())
    p.movies.map(p.cleaned) shouldBe Seq("Alpha")
    p.movies.head.director shouldBe Seq("Ducournau")
    // the age rating the country's system bills, where the client set none
    val rated = ScrapeListing.prepare(Helios, Seq(listing("Jüdische Filmwoche: Das siebte Kreuz D, ab 12", Seq(at(18)))), germany,
      ScreeningTokens.forDefaultCountry())
    rated.movies.head.ageRating shouldBe Some("FSK 12")
  }

  it should "keep apart a house's two seasons' productions of one work at one venue" in {
    // UK recording 38351832400: The Ashford Cinema lists "RBO Cinema Season 2026-27: Swan Lake" beside "Royal Ballet &
    // Opera 2024-25: Swan Lake"; folded onto one slot, the 2026/27 relay took the 2024/25 record.
    val p = prepare(
      listing("RBO Cinema Season 2026-27: Swan Lake", Seq(at(18))),
      listing("RBO Cinema Season 2024-25: Swan Lake", Seq(at(20))),
      listing("RBO Cinema Season 2026/27: Swan Lake", Seq(at(21))))
    p.movies.map(_.showtimes.size).sorted shouldBe Seq(1, 2)
    // a programme's season names no production: a film club's one film across a season's turn is one slot
    prepare(listing("Kino Seniora 2025/2026: Lalka", Seq(at(18))), listing("Kino Seniora 2026/2027: Lalka", Seq(at(20))))
      .movies.map(_.showtimes.size) shouldBe Seq(2)
  }

  it should "strip a format tag off the title into the showings' badges, through the shared vocabulary" in {
    val p = prepare(listing("Zwierzogród 2 (Napisy PL)", Seq(at(18))))
    p.movies.map(p.cleaned) shouldBe Seq("Zwierzogród 2")
    p.movies.head.showtimes.head.format should contain ("NAP")
  }

  it should "be a function of the listing, not of the order the scraper emitted it in" in {
    val a = listing("Diuna", Seq(at(18))); val b = listing("Diuna", Seq(at(21))); val c = listing("Guru", Seq(at(19)))
    prepare(a, b, c).movies shouldBe prepare(c, b, a).movies
  }

  // A cleaning runs every title rule of the venue and peels its format tags; a scrape tick asks it of
  // every listing several times over, and the same titles at venue after venue.
  it should "clean each title once, however many venues list it and however often its rows are asked for it" in {
    val normalizer = new TitleNormalizer(titleNormalizer.rules)
    val venues     = Seq(Helios, KinoMuza, Multikino, Rialto, KinoApollo)
    val titles     = Seq("Ojczyzna (Napisy PL)", "Ojczyzna (Napisy PL)", "Diuna")
    val prepared   = venues.map(venue => ScrapeListing.prepare(venue, titles.zipWithIndex.map { case (t, i) =>
      listing(t, Seq(at(18 + i))).copy(cinema = venue) }, normalizer, ScreeningTokens.forDefaultCountry()))
    prepared.foreach(p => p.movies.map(p.cleaned).sorted shouldBe Seq("Diuna", "Ojczyzna"))
    venues.foreach(venue => titles.foreach { title =>
      ScrapeListing.cleanTitle(venue, title, normalizer) shouldBe
        FormatTags.extractFormatTags(normalizer.cinemaClean(services.titlerules.TitleRuleKey.of(venue), title))
    })
    normalizer.listingTitlesCached shouldBe 2
  }

  // A worker runs for days and the titles it lists turn over weekly: the memo held every title ever
  // listed. It now holds at most its bound, evicting the coldest.
  it should "hold at most its bound of cleaned titles, however many distinct ones it has seen" in {
    val normalizer = new TitleNormalizer(titleNormalizer.rules, memoEntries = 50)
    (1 to 1000).foreach(n => normalizer.listingTitle(Helios, s"Film number $n"))
    normalizer.listingTitlesCached should be <= 50L
  }
}
