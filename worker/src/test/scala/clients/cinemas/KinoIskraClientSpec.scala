package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.{KinoIskra, Showtime}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoIskraClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays the 27-09-2026 capture of Kino Iskra Augustów's own site: the
 *  `/repertuar/` full-repertoire page (24 film/event blocks, yearless
 *  "27 września" + HH:MM screening buttons) and each film's
 *  `ajax/user/get_movie.php?movie=<id>` record (genres, countries, year,
 *  runtime, director, cast, age, poster, synopsis). The live events' records
 *  (stand-up, "Spektakl …") were recorded 08-10-2026: each answers 200, those of
 *  past events with a "Film o podanym ID nie istnieje" stub.
 *
 *  Fixture directory: test/resources/fixtures/kino-iskra/ (recorded with
 *  RecordingHttpFetch over RealHttpFetch). */
class KinoIskraClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 27)
  private val movies = new KinoIskraClient(new FakeHttpFetch("kino-iskra"), KinoIskra, today).fetch()
  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoIskraClient" should "read the films on the full repertoire and drop its stand-up and stage shows" in {
    movies.map(_.movie.title) should contain allOf (
      "Marsupilami", "100 dni: Misja Zeus", "Odyseja", "Lalka", "Lalka - Seans Seniora",
      "Avengers: Koniec gry - wersja rozszerzona", "Recepta na szczęście 2")
    movies.map(_.movie.title).filter(t => t.contains("Stand-up") || t.startsWith("Spektakl")) shouldBe empty
    movies should have size 16
    movies.map(_.cinema).toSet shouldBe Set(KinoIskra)
  }

  it should "place each yearless screening in the year `today` implies and link the ticketing portal's day" in {
    film("Marsupilami").showtimes.head shouldBe Showtime(
      LocalDateTime.of(2026, 9, 27, 10, 30),
      Some("https://bilety.kino-iskra.pl/MSI/mvc/pl?sort=Name&date=2026-09-27"),
      format = List("2D", "DUB"))
    film("Recepta na szczęście 2").showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 11, 17, 17, 30))
    film("Lalka").showtimes should have size 25
  }

  it should "merge a film's dubbed and subtitled versions into one film with per-showtime versions" in {
    val avengers = film("Avengers: Koniec gry - wersja rozszerzona")
    avengers.showtimes should have size 4
    avengers.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 29, 19, 45)).value.format shouldBe List("2D", "NAP")
    avengers.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 27, 19, 15)).value.format shouldBe List("2D", "DUB")
  }

  it should "emit the identity signals of each film's record" in {
    val marsupilami = film("Marsupilami")
    marsupilami.movie.releaseYear shouldBe Some(2026)
    marsupilami.movie.countries shouldBe Seq("Francja", "Belgia")
    marsupilami.movie.runtimeMinutes shouldBe Some(98)
    marsupilami.movie.genres shouldBe Seq("Komedia", "Przygodowy", "Familijny")
    marsupilami.director shouldBe Seq("Philippe Lacheau")
    marsupilami.ageRating shouldBe Some("7+")
    marsupilami.posterUrl shouldBe Some("https://kino-iskra.pl/uploads/img/movies/2338-1787745055.jpg")
    marsupilami.synopsis.value should startWith("Komedia dla całej rodziny")
    marsupilami.filmUrl shouldBe Some("https://kino-iskra.pl/zapowiedzi/#2338")

    val odyseja = film("Odyseja")
    odyseja.director shouldBe Seq("Christopher Nolan")
    odyseja.movie.countries shouldBe Seq("Wielka Brytania", "USA")
    odyseja.cast should contain("Zendaya")
  }

  it should "keep a studio named where the countries go out of the country list" in {
    val avengers = film("Avengers: Koniec gry - wersja rozszerzona")
    avengers.movie.releaseYear shouldBe Some(2019)
    avengers.movie.countries shouldBe empty
    avengers.director shouldBe Seq("Anthony Russo", "Joe Russo")
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy new KinoIskraClient(new FailingHttpFetch(503), KinoIskra, today).fetch()
  }

  // A film's record adds metadata only — its showtimes are the listing's. One record that failed
  // to load used to fail the whole scrape, turning the venue red over one film's genres.
  it should "list a film whose record failed to load, just without its metadata" in {
    val replay = new FakeHttpFetch("kino-iskra")
    val marsupilamiRecord = KinoIskraClient.movieUrl(film("Marsupilami").filmUrl.value.dropWhile(_ != '#').drop(1))
    val oneDown = new tools.HttpFetch {
      def get(url: String): String = if (url == marsupilamiRecord) throw new HttpStatusException(503, "GET", url, None) else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val partial = new KinoIskraClient(oneDown, KinoIskra, today).fetch()
    partial should have size 16
    val marsupilami = partial.find(_.movie.title == "Marsupilami").value
    marsupilami.showtimes shouldBe film("Marsupilami").showtimes
    marsupilami.movie.runtimeMinutes shouldBe None
  }

  // A film whose TITLE is event vocabulary keeps itself only by its record's director and year: when that
  // record fails to load the film is dropped as a live event, so the listing must not read as complete —
  // a complete listing lets the cache prune the film until the next good scrape.
  it should "leave the listing incomplete when a failed record drops a film whose title reads as an event" in {
    val replay  = new FakeHttpFetch("kino-iskra")
    val record  = KinoIskraClient.movieUrl(film("Odyseja").filmUrl.value.dropWhile(_ != '#').drop(1))
    // the same film, billed under a title the classifier reads as a stage show
    def billedAsEvent(down: Boolean) = new tools.HttpFetch {
      def get(url: String): String =
        if (url == KinoIskraClient.RepertoireUrl) replay.get(url).replace(">Odyseja<", ">Stand-up: Odyseja<")
        else if (down && url == record) throw new HttpStatusException(503, "GET", url, None)
        else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val title = "Stand-up: Odyseja"
    new KinoIskraClient(billedAsEvent(down = false), KinoIskra, today).fetch().map(_.movie.title) should contain(title)
    val (partial, reads) = services.cinemas.common.ListingReads.during(new KinoIskraClient(billedAsEvent(down = true), KinoIskra, today).fetch())
    partial.map(_.movie.title) should not contain title
    reads.complete shouldBe false
  }

  it should "not count a failed record against the listing when it only leaves a film bare" in {
    val replay  = new FakeHttpFetch("kino-iskra")
    val record  = KinoIskraClient.movieUrl(film("Marsupilami").filmUrl.value.dropWhile(_ != '#').drop(1))
    val oneDown = new tools.HttpFetch {
      def get(url: String): String = if (url == record) throw new HttpStatusException(503, "GET", url, None) else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    services.cinemas.common.ListingReads.during(new KinoIskraClient(oneDown, KinoIskra, today).fetch())._2.complete shouldBe true
  }

  // The stand-up and stage shows are dropped by title, so their records are read to give each a chance to
  // prove itself a film. Live, those records answer (even a past event's, with a stub): replay must too, or
  // the venue reads incomplete on every scrape and never prunes.
  it should "read the dropped live events' records, so a clean scrape is a complete listing" in {
    services.cinemas.common.ListingReads.during(
      new KinoIskraClient(new FakeHttpFetch("kino-iskra"), KinoIskra, today).fetch())._2.complete shouldBe true
  }

  // Two versions of one film are two records of it: when one fails but the other names the director and
  // year, the film is kept and the listing is complete — the failed record could not have changed anything.
  it should "not count a failed version's record when the film's other version proves it a film" in {
    val replay   = new FakeHttpFetch("kino-iskra")
    val failing  = KinoIskraClient.movieUrl("2326") // the capture's dubbed version; 2325 is the subtitled one
    def billedAsEvent = new tools.HttpFetch {
      def get(url: String): String =
        if (url == KinoIskraClient.RepertoireUrl)
          replay.get(url).replace("Avengers: Koniec gry - wersja rozszerzona", "Stand-up: Avengers")
        else if (url == failing) throw new HttpStatusException(503, "GET", url, None)
        else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val (listed, reads) = services.cinemas.common.ListingReads.during(new KinoIskraClient(billedAsEvent, KinoIskra, today).fetch())
    listed.map(_.movie.title) should contain("Stand-up: Avengers")
    reads.complete shouldBe true
  }
}
