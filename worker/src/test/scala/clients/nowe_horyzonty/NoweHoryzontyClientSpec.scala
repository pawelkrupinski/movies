package clients.nowe_horyzonty

import clients.tools.FakeHttpFetch
import models.{KinoNoweHoryzonty, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.NoweHoryzontyClient

import java.time.{LocalDate, LocalDateTime}

class NoweHoryzontyClientSpec extends AnyFlatSpec with Matchers {

  // The full schedule comes from the per-day `rep.json?dzien=DD-MM-YYYY` AJAX
  // endpoint, not the `program.s` teaser page. Pin `today` to the recording
  // date so the seven day-requests hit the recorded fixtures deterministically.
  private val today   = LocalDate.of(2026, 6, 6)
  private val client  = new NoweHoryzontyClient(new FakeHttpFetch("nowe-horyzonty"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // Regression guard for the under-scrape bug: scraping `program.s` returned
  // only ~1 slot per film (~14 live), whole films missing. The arthouse runs
  // ~40 screenings/day across nine screens; reading `rep.json` per day over a
  // week recovers the full repertoire.
  "NoweHoryzontyClient.fetch" should "scrape the full week's repertoire, not a teaser slice" in {
    results.size                      shouldBe 43
    results.flatMap(_.showtimes).size shouldBe 226
  }

  it should "assign Kino Nowe Horyzonty to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoNoweHoryzonty)
  }

  // These films were entirely missing from the old `program.s` scrape (the
  // diff-vs-Filmweb finding that triggered the fix) — and one we had only a
  // single screening of. The full schedule carries all their weekly slots.
  it should "capture films the program.s scrape missed, with their full screening counts" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Diabeł ubiera się u Prady 2")                   shouldBe 19
    counts("Mandalorian i Grogu")                           shouldBe 22
    counts("Obsesja")                                       shouldBe 18
    counts("Zawodowcy")                                     shouldBe 16
    counts("Niesamowite przygody skarpetek 3. Ale kosmos!") shouldBe 1
  }

  it should "enrich metadata from the op.s detail page" in {
    val m = byTitle("Obsesja")
    m.movie.runtimeMinutes shouldBe Some(108)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.originalTitle  shouldBe Some("Obsession")
    m.movie.countries      shouldBe Seq("USA")
    m.movie.genres         shouldBe Seq("Horror")
    m.director             shouldBe Seq("Curry Barker")
    m.filmUrl              shouldBe Some("https://www.kinonh.pl/op.s?id=22588")
    m.synopsis.getOrElse("").length should be > 30
  }

  it should "parse a multi-genre film and keep its original title" in {
    val m = byTitle("Diabeł ubiera się u Prady 2")
    m.movie.originalTitle shouldBe Some("The Devil Wears Prada 2")
    m.movie.genres        shouldBe Seq("Komedia", "Dramat")
    m.movie.countries     shouldBe Seq("USA")
  }

  it should "build booking URLs from the day's eventId, dated by the requested day" in {
    byTitle("Obsesja").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 6, 21, 30),
        Some("https://www.kinonh.pl/bilet.s?eventId=194204&forwardback=https://www.kinonh.pl/program.s"),
        None, Nil
      )
  }

  it should "carry posters from the listing's inline background-image" in {
    byTitle("Obsesja").posterUrl.getOrElse("") should startWith("https://www.kinonh.pl/")
  }
}
