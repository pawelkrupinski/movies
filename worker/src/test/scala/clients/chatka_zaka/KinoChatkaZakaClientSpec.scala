package clients.chatka_zaka

import clients.tools.FakeHttpFetch
import models.KinoChatkaZaka
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoChatkaZakaClient

import java.time.LocalDateTime

/** Replays the recorded UMCS "Chatka Żaka" event calendar
 *  (`umcs.pl/pl/kalendarz-wydarzen,9469,…`) — the list page plus its per-event
 *  detail pages — through the client.
 *
 *  The calendar mixes the venue's films (a French-cinema review at 18:00) with
 *  its concerts/theatre, so the fixture list also carries a real venue concert
 *  ("Koncert: Muzyka łatwa…") that [[services.cinemas.OnlyMovieEventsFilter]]
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
