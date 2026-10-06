package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.nio.file.{Files, Paths}

class TmdbFilmRecordSpec extends AnyFlatSpec with Matchers {

  private def recorded(name: String) =
    Json.parse(Files.readString(Paths.get(s"test/resources/fixtures/tmdb/$name")))

  "A TMDB film record" should "name its co-directors among its directors" in {
    // "The Last Whale Singer" (Vincent. Legenda oceanu): TMDB credits Reza Memari as Director and
    // Pavel Hrubos and Steven Majaury as Co-Director. Venues that name the co-directors scored
    // "director different" against the film, which vetoed 51 PL listings and split the film.
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-677558-credits-pl.json"))).get
    film.directors.get should contain theSameElementsAs Seq("Reza Memari", "Pavel Hrubos", "Steven Majaury")
  }

  it should "name a filmed stage production's stage director among its directors" in {
    // "Fallen Angels by Noël Coward" (US ×365, UK ×287): the venues credit Scott Ellis, TMDB's
    // Stage Director; its Director is Annette Jolles, who directed the filming.
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-1702350-credits-en.json"))).get
    film.directors.get should contain allOf ("Annette Jolles", "Scott Ellis")
  }

  it should "carry the day it was released, a broadcast's air date" in {
    val (film, _) = TmdbFilmRecord.parse(Seq(recorded("movie-1702350-credits-en.json"))).get
    film.released shouldBe Some(java.time.LocalDate.of(2026, 10, 22))
    film.year shouldBe Some(2026)
  }

  // "Once Upon a Time in America": TMDB keeps a runtime per translation — pl-PL (and de-DE) state the 229-minute cut
  // Polish cinemas screen, en-US the 139-minute US theatrical cut. Recorded 2026-10-06 from
  // /3/movie/311?language=pl-PL&append_to_response=credits,release_dates and ?language=en-US&append_to_response=alternative_titles.
  private lazy val onceUponATimeInAmerica =
    TmdbFilmRecord.parse(Seq(recorded("movie_311_pl.json"), recorded("movie_311_en.json"))).get._1

  it should "state the runtime of every translation it was fetched in, the deployment language's first" in {
    onceUponATimeInAmerica.runtime shouldBe Some(229)
    onceUponATimeInAmerica.runtimes shouldBe Seq(229, 139)
  }

  "A listing's runtime" should "match a film when it matches the runtime of ANY translation TMDB states, the closest counting" in {
    def measured(minutes: Int) = IdentityMeasures.listingFilm(
      IdentityMeasures.Listing("Dawno temu w Ameryce", runtime = Some(minutes)), onceUponATimeInAmerica, None, 0, 0)
    measured(229)("runtime.delta") shouldBe IdentityMeasures.Number(0)
    measured(139)("runtime.delta") shouldBe IdentityMeasures.Number(0)
    IdentityMeasures.runtimeContradicts(measured(229)) shouldBe false
    IdentityMeasures.runtimeContradicts(measured(139)) shouldBe false
    // 41 minutes off the closest (139): still a contradiction.
    measured(180)("runtime.delta") shouldBe IdentityMeasures.Number(41)
    IdentityMeasures.runtimeContradicts(measured(180)) shouldBe true
  }

  it should "vote for a record when it runs as any of the record's translations, and not when it runs as none" in {
    def votes(minutes: Int) = agreement.Agreement.listingVotes(
      Seq(FilmTable.listing(models.Multikino, "Dawno temu w Ameryce", Some(1984), Some("Sergio Leone")).copy(runtime = Some(minutes))),
      Seq(agreement.SourceRecord(onceUponATimeInAmerica)))
    votes(229)(agreement.Agreement.ListingRuntime) shouldBe true
    votes(139)(agreement.Agreement.ListingRuntime) shouldBe true
    votes(180)(agreement.Agreement.ListingRuntime) shouldBe false
  }

  // "Apocalypse Now": one record, 28 (1979, 147 minutes), whose release_dates date every edition — GB's 2019 "Final Cut",
  // 2001 "Redux", the 1995 and 2011 re-releases. Recorded 2026-10-06 from /3/movie/28?language=en-GB&append_to_response=credits,release_dates.
  private lazy val apocalypseNow = TmdbFilmRecord.parse(Seq(recorded("movie_28_gb.json"), recorded("movie_28_en.json"))).get._1

  "A film's record" should "keep the cinema releases TMDB dates, each with whether its note names an edition" in {
    val gb = TmdbFilmRecord.Release.all(apocalypseNow.releases.get).filter(_.country == "GB")
    gb shouldBe Seq(TmdbFilmRecord.Release("GB", 1979, edition = false), TmdbFilmRecord.Release("GB", 1995, edition = true),
      TmdbFilmRecord.Release("GB", 2001, edition = true), TmdbFilmRecord.Release("GB", 2011, edition = true), TmdbFilmRecord.Release("GB", 2019, edition = true))
  }

  "A listing's year" should "be read against the closest of the film's cinema releases in the venue's country" in {
    def measured(year: Int, minutes: Int, country: Option[String] = Some("GB")) = IdentityMeasures.listingFilm(
      IdentityMeasures.Listing("Apocalypse Now", year = Some(year), runtime = Some(minutes)),
      apocalypseNow, None, 0, 0, country = country)
    // Prince Charles's 2019 Final Cut, 183 minutes: the 2019 release, and an edition's runtime longer than 147 is neutral.
    measured(2019, 183)("year.distance") shouldBe IdentityMeasures.Number(0)
    measured(2019, 183)("runtime.delta") shouldBe IdentityMeasures.Missing("listing")
    // The original's year reads the original; a year no release of the venue's country dates reads the closest.
    measured(1979, 147)("year.distance") shouldBe IdentityMeasures.Number(0)
    measured(2017, 147)("year.distance") shouldBe IdentityMeasures.Number(2)
    // PL dates only a 2003 Redux and a 2024 Director's Cut: 2019 is five years off the closest there.
    measured(2019, 183, Some("PL"))("year.distance") shouldBe IdentityMeasures.Number(5)
    // A runtime SHORTER than the film's still counts, edition or not.
    measured(2019, 100)("runtime.delta") shouldBe IdentityMeasures.Number(47)
  }
}
