package clients.kino_mikro

import clients.tools.FakeHttpFetch
import models.{KinoMikro, MikroBronowice}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsString
import services.cinemas.{KinoMikroClient, KinoMikroParser}

import java.time.LocalDate

/** Replays the recorded per-day `api.php/v1/repertoires?from=D&to=D+1` JSON
 *  payloads (one file per day, 13.06–19.06.2026) through the client, pinning
 *  `today` to the capture date so the day-window URLs hit the recorded
 *  fixtures.
 *
 *  The per-day fetch is load-bearing: a single broad request silently caps the
 *  feed at ~25 records (today's schedule plus one teaser per future day) and
 *  ignores `limit`, so the advance-date repeats are dropped — every film
 *  collapsed to one screening and Filmweb showed 3–4× our count. Querying one
 *  narrow `from`/`to` day at a time bypasses the cap; merging the days
 *  reconstructs the full window (52 Kino Mikro showings here, up from the 28
 *  the capped feed exposed; 10 Mikro Bronowice, up from 1). */
class KinoMikroClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http  = new FakeHttpFetch("kino-mikro")
  private val today = LocalDate.of(2026, 6, 13)

  private def showtimeCount(movies: Seq[models.CinemaMovie]): Int = movies.map(_.showtimes.size).sum

  "KinoMikroClient" should "merge the per-day feeds into the whole window for Kino Mikro" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro, today).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMikro)
    // The capped single-request feed exposed 28 Kino Mikro showings (one per
    // film per future day); per-day merging recovers the full 52.
    showtimeCount(movies) shouldBe 52

    // The fix's whole point: a film now carries its multi-day repeats, not a
    // single survivor. "Zniknięcie" runs once a day across the window.
    val zniknięcie = movies.find(_.movie.title == "Zniknięcie").value
    zniknięcie.showtimes.size shouldBe 6
    zniknięcie.showtimes.map(_.dateTime.toLocalDate).distinct.size should be > 1
  }

  it should "scope the same per-day feeds to Mikro Bronowice, tagging rows with that venue" in {
    val movies = new KinoMikroClient(http, "Mikro Bronowice", MikroBronowice, today).fetch()

    movies.map(_.cinema).toSet shouldBe Set(MikroBronowice)
    // The capped feed published a single Bronowice screening; per-day fetch
    // recovers all 10 across 3 films.
    showtimeCount(movies) shouldBe 10

    val tomIJerry = movies.find(_.movie.title == "Tom i Jerry: Przygoda w muzeum").value
    tomIJerry.showtimes.size shouldBe 4
    tomIJerry.showtimes.flatMap(_.bookingUrl).head should startWith("https://kinomikro.pl/repertoire/")
  }

  it should "parse the director out of the event_description HTML blob" in {
    val movies = new KinoMikroClient(http, "Kino Mikro", KinoMikro, today).fetch()

    // No-colon layout terminated by the next field label, taken from the real
    // recorded feed:
    //   `…<div>Reżyseria George Sluizer</div><div>Obsada …`
    movies.find(_.movie.title == "Zniknięcie").value.director shouldBe Seq("George Sluizer")

    // The terminator label must never be swallowed into the captured name.
    val forbidden = Seq("Obsada", "Scenariusz", "Gatunek", "Produkcja", "Czas", "Dystrybutor")
    val allDirectors = movies.flatMap(_.director)
    forbidden.foreach(label => all(allDirectors) should not include label)
  }

  // Director extraction is fixture-independent — exercise the colon layout
  // (`Reżyseria: …`) and co-director splitting through the public parser with a
  // hand-built record, so coverage doesn't hinge on which films happen to be
  // showing in the recorded week (the no-colon layout is covered above via the
  // real "Zniknięcie" feed).
  private def directorOf(eventDescription: String): Seq[String] = {
    val json =
      s"""{"data":[{"location_institution_name":"Kino Mikro","event_title":"Probe",
         |"event_date":"15.06.2026 18:00","event_description":${JsString(eventDescription)}}]}""".stripMargin
    KinoMikroParser.parse(Seq(json), "Kino Mikro", KinoMikro).head.director
  }

  "KinoMikroParser.parse" should "read a colon-layout director terminated by the next label" in {
    directorOf("<br>Reżyseria: Federico Fellini <br>Scenariusz: Federico Fellini, Tullio Pinelli") shouldBe
      Seq("Federico Fellini")
  }

  it should "split co-directors and return no director when the Reżyseria marker is absent" in {
    directorOf("<div>Reżyseria: Joel Coen, Ethan Coen</div><div>Gatunek dramat</div>") shouldBe
      Seq("Joel Coen", "Ethan Coen")
    directorOf("<div>Gatunek: dramat</div>") shouldBe empty
  }

  it should "strip a '- dubbing'/'- napisy' language suffix into the showtime format and merge the editions" in {
    def ev(title: String, hm: String) =
      s"""{"location_institution_name":"Kino Mikro","event_title":"$title","event_date":"02.07.2026 $hm","slug":"toy-story-5","event_id":"1"}"""
    val json   = s"""{"data":[${ev("Toy Story 5- dubbing", "18:00")},${ev("Toy Story 5- napisy", "20:00")}]}"""
    val movies = KinoMikroParser.parse(Seq(json), "Kino Mikro", KinoMikro)
    movies should have size 1
    movies.head.movie.title             shouldBe "Toy Story 5"
    movies.head.showtimes.map(_.format).toSet shouldBe Set(List("DUB"), List("NAP"))
  }
}
