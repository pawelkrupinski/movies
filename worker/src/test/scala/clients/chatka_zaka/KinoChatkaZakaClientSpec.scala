package clients.chatka_zaka

import org.scalatest.OptionValues
import models.KinoChatkaZaka
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoChatkaZakaClient

import java.time.LocalDateTime

/** Replays the recorded UMCS "Chatka Żaka" event calendar
 *  (`umcs.pl/pl/kalendarz-wydarzen,9469,…`) — the list page plus its per-event
 *  detail pages — through the client.
 *
 *  The calendar mixes the venue's films (a French-cinema review at 18:00) with
 *  its concerts/theatre, so the fixture list also carries a real venue concert
 *  ("Koncert: Muzyka łatwa…") that [[services.cinemas.pl.OnlyMovieEventsFilter]]
 *  must drop. Previously scraped from Filmweb, which had silently gone empty. */
class KinoChatkaZakaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoChatkaZakaClient(new FakeHttpFetch("chatka-zaka")).fetch()

  "KinoChatkaZakaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoChatkaZaka)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "parse a French-review film with its date+time and metadata" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("mi amor")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 23, 18, 0))
    film.movie.originalTitle.value shouldBe "Mi amor"
    film.movie.releaseYear.value shouldBe 2025
    film.movie.countries should contain("Francja")
    film.director should contain("Guillaume Nicloux")
  }

  it should "carry every French-review film in the window" in {
    val titles = movies.map(_.movie.title.toLowerCase)
    titles should have size 5 // the 5 films; the concert is filtered out
    titles.exists(_.contains("nowa fala")) shouldBe true
    titles.exists(_.contains("windą na szafot")) shouldBe true
  }

  it should "drop the venue's non-film concert via the event filter" in {
    movies.map(_.movie.title).exists(_.toLowerCase.contains("koncert")) shouldBe false
  }
}

/** Replays a 2026-10-07 capture of the same calendar. By October the detail
 *  pages' meta description had dropped the pipes — `"13.10.26 wtorek 18:00\nKTOŚ
 *  CAŁKIEM OBCY\nreż. …"`, sometimes under a banner line ("WIECZÓR Z KLASYKĄ") —
 *  so the `| HH:MM` stamp the client looked for was gone, every entry lost its
 *  time and was dropped, and the venue read white while it listed five DKF
 *  screenings. */
class KinoChatkaZakaPipelessDetailSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoChatkaZakaClient(new FakeHttpFetch("chatka-zaka-2026-10")).fetch()

  "KinoChatkaZakaClient" should "time a screening off a detail stamp without pipes" in {
    movies.flatMap(_.showtimes) should have size 5
    val film = movies.find(_.movie.title.toLowerCase.contains("ktoś całkiem obcy")).value
    film.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 13, 18, 0))
    film.director should contain("Brandt Andersen")
    film.movie.runtimeMinutes.value shouldBe 103
  }

  it should "time a screening whose stamp sits under a banner line" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("czerwone latarnie")).value
    film.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 14, 18, 0))
  }
}
