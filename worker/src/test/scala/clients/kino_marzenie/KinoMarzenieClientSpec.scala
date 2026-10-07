package clients.kino_marzenie

import org.scalatest.OptionValues
import clients.tools.{FakeHttpFetch, FilmPages}
import models.KinoMarzenie
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoMarzenieClient

import java.time.{LocalDate, LocalDateTime}

/** Replays three recorded `kinomarzenie.pl/embed/events?start_date=…` day
 *  partials (2026-09-23 through 2026-09-25) through the client as ONE chunk —
 *  there is no whole-programme feed, so a chunk is read one request per day. */
class KinoMarzenieClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoMarzenieClient(
    new FakeHttpFetch("kino-marzenie"), KinoMarzenie, today = LocalDate.of(2026, 9, 23)
  ).fetchChunk("2026-09-23,2026-09-24,2026-09-25")

  "KinoMarzenieClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMarzenie)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a film's showtimes across the swept days, read off the request date (not page text)" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 23, 16, 0),
      LocalDateTime.of(2026, 9, 24, 16, 0),
      LocalDateTime.of(2026, 9, 25, 19, 0)
    )
  }

  it should "carry the venue's MSI booking link off each showtime" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 23, 16, 0)).value
    slot.bookingUrl.value should include ("marzenieonline.tck.pl/MSI/Default.aspx?event_id=37013")
  }

  it should "read genres off the 'GENRE, GENRE | AGE+ LAT' metadata line, dropping the age tag" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.movie.genres shouldBe Seq("BIOGRAFICZNY", "DRAMAT")
  }

  it should "tag a subtitled showtime NAP off its sibling format div" in {
    val film = movies.find(_.movie.title == "OBCY").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 23, 20, 15)).value
    slot.format shouldBe List("NAP")
  }

  it should "tag a dubbed showtime DUB off its sibling format div" in {
    val film = movies.find(_.movie.title == "MARSUPILAMI").value
    val slot = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 24, 14, 0)).value
    slot.format shouldBe List("DUB")
  }

  it should "drop a film listed with no showtime on a given day, keeping it on the day it screens" in {
    // MARSUPILAMI is listed (poster + title) on 2026-09-23 but its showtimes
    // column is empty that day — it only carries a real screening from 09-24.
    val film = movies.find(_.movie.title == "MARSUPILAMI").value
    film.showtimes.map(_.dateTime) should not contain LocalDateTime.of(2026, 9, 23, 0, 0)
    film.showtimes.map(_.dateTime.toLocalDate) should not contain LocalDate.of(2026, 9, 23)
  }

  // The identity resolver reads these: without the director this "DYRYGENT" stood beside every other "Dyrygent".
  // Recorded 2026-10-05 from https://www.kinomarzenie.pl/repertuar/1027,dyrygent-polaczony-z-koncertem-muzyki-na-zywo
  it should "read the film page's facts: Wajda's Dyrygent, premiere 24 March 1980, 97 min, Polska" in {
    val client = new KinoMarzenieClient(new FakeHttpFetch("kino-marzenie"), KinoMarzenie, today = LocalDate.of(2026, 9, 23))
    val detail = FilmPages.detailOf(client, "https://www.kinomarzenie.pl/repertuar/1027,dyrygent-polaczony-z-koncertem-muzyki-na-zywo")
    detail.director shouldBe Seq("Andrzej Wajda")
    detail.cast shouldBe Seq("John Gielgud", "Krystyna Janda", "Andrzej Seweryn", "Marek Dąbrowski")
    detail.releaseYear shouldBe Some(1980)
    detail.runtimeMinutes shouldBe Some(97)
    detail.countries shouldBe Seq("Polska")
  }

  it should "expose the film's own detail page as filmUrl" in {
    val film = movies.find(_.movie.title == "MISTYCZKA").value
    film.filmUrl.value shouldBe "https://www.kinomarzenie.pl/repertuar/1011,mistyczka"
  }

  it should "declare the venue's own host as its scrape host" in {
    new KinoMarzenieClient(new FakeHttpFetch("kino-marzenie"), KinoMarzenie, today = _root_.tools.SpecClock.PinnedDay).scrapeHosts shouldBe
      Set("www.kinomarzenie.pl")
  }
}

/** Replays a 2026-10-07 capture: `/repertuar` plus the first week's day partials.
 *  From 2026-09-30 each partial took 8–10 s to first byte from the worker, so the
 *  old fourteen sequential day fetches overran `AdaptiveTimeoutScraper`'s 45 s
 *  ceiling on every scrape and the venue sat red, uncovered, for a week. Chunked,
 *  each week is its own task; and the plan is the slider's own day list, not a
 *  fixed fourteen days. */
class KinoMarzenieChunkedSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val client = new KinoMarzenieClient(new FakeHttpFetch("kino-marzenie-2026-10"), KinoMarzenie, today = LocalDate.of(2026, 10, 7))

  "KinoMarzenieClient" should "be scraped as chunk tasks, outside the per-scrape budget" in {
    client shouldBe a[services.cinemas.common.ChunkedCinemaScraper]
  }

  it should "plan every day the repertoire slider offers, in week-long chunks" in {
    val days = client.planChunks().flatMap(services.cinemas.common.DayChunks.days)
    days.head shouldBe LocalDate.of(2026, 10, 7)
    days.last shouldBe LocalDate.of(2026, 12, 7)
    days should have size 62
    client.planChunks() should have size 9
  }

  it should "read a week's chunk one partial per day" in {
    val movies = client.fetchChunk(client.planChunks().head)
    movies.find(_.movie.title == "LALKA").value.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 10, 7, 16, 0),
      LocalDateTime.of(2026, 10, 13, 19, 15))
    movies.flatMap(_.showtimes) should have size 16
  }
}
