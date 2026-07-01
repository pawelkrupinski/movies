package clients.kino_pod_baranami

import clients.tools.FakeHttpFetch
import models.KinoPodBaranami
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoPodBaranamiClient
import tools.GetOnlyHttpFetch

import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `/repertuar.php` page (07-06-2026 capture, ISO-8859-2)
 *  through the client. The fixture is fetched via `getBytes` so the encoding
 *  decode path is exercised correctly, just as in production. `today` is pinned
 *  to the capture date so year-inference is stable. */
class KinoPodBaranamiClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-pod-baranami")
  private val client = new KinoPodBaranamiClient(http, KinoPodBaranami, LocalDate.of(2026, 6, 7))

  "KinoPodBaranamiClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoPodBaranami" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoPodBaranami)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 10:30" in {
    val movies = client.fetch()
    val diabel = movies.find(_.movie.title.contains("Diabe")).value
    // Full Polish title with all diacritics intact — proves the ISO-8859-2
    // bytes survive the decode. The fixture used to be a lossy UTF-8 capture
    // (every Polish letter → U+FFFD), so this exact-title match failed.
    diabel.movie.title shouldBe "Diabeł ubiera się u Prady 2"
    diabel.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 10, 30))
    diabel.showtimes.flatMap(_.bookingUrl).head should include("/rezerwacja_start.php?event_id=")
  }

  it should "capture the anchor's original (international) title as a TMDB-search hint" in {
    // The cinema exposes the original title in the link's `title=` attribute
    // ("The Devil Wears Prada 2", "Fight Club"); we keep it so a same-Polish-title
    // film disambiguates to the right TMDB entry deterministically.
    val movies = client.fetch()
    movies.find(_.movie.title.contains("Diabe")).value.movie.originalTitle.value shouldBe "The Devil Wears Prada 2"
    movies.find(_.movie.title.contains("Podziemny")).value.movie.originalTitle.value shouldBe "Fight Club"
  }

  it should "strip the trailing (SMAK) discussion-club label so the film folds into the main record" in {
    // "(SMAK)" is Kino Pod Baranami's discussion-club programme tag ("Seans z
    // dyskusją"), not part of the film title. Left in, "Dzień objawienia (SMAK)"
    // sanitizes to a different merge key (`_id` = sanitize(title)|year) and forks
    // off as a separate one-cinema record. It must not survive into the title.
    val titles = client.fetch().map(_.movie.title)
    all(titles) should not include "SMAK"
    titles should contain("Dzień objawienia")
  }

  it should "leave originalTitle empty when the original equals the displayed title" in {
    // A film whose `title=` attribute just repeats the visible title carries no
    // distinct original — don't store a redundant one.
    client.fetch().find(_.movie.title.equalsIgnoreCase("Hamnet"))
      .foreach(_.movie.originalTitle shouldBe None)
  }

  it should "fold a film's (Napisy PL) / (Dubbing PL) editions into one movie, surfacing each screening's language as a Showtime format" in {
    // The cinema lists the subtitled and dubbed editions of the same film as two
    // separate `film.php?film_id=` entries whose only title difference is a
    // trailing "(Napisy PL)" / "(Dubbing PL)" language tag. Left in the title,
    // both sanitize to the SAME slot key and clobber each other every scrape
    // (only the last edition's showtimes survive). Stripping the tag at parse
    // time collapses them to one film with the UNION of screenings, and the
    // language is carried per-screening on `Showtime.format` (NAP / DUB).
    val html =
      """<html><body>
        |<p class="rep_date">Niedziela 7 czerwca // Sunday, June 7</p>
        |<ul class="program_list">
        |  <li><a href="film.php?film_id=12964" title="Toy Story 5">Toy Story 5 (Napisy PL)</a>
        |      <span><a href="/rezerwacja_start.php?event_id=1">18:00</a></span></li>
        |  <li><a href="film.php?film_id=12963" title="Toy Story 5">Toy Story 5 (Dubbing PL)</a>
        |      <span><a href="/rezerwacja_start.php?event_id=2">15:45</a></span></li>
        |</ul>
        |</body></html>""".stripMargin
    val fake = new GetOnlyHttpFetch {
      def get(url: String): String = html
      override def getBytes(url: String): Array[Byte] = html.getBytes(Charset.forName("ISO-8859-2"))
    }
    val movies = new KinoPodBaranamiClient(fake, KinoPodBaranami, LocalDate.of(2026, 6, 7)).fetch()

    movies should have size 1
    val toy = movies.head
    toy.movie.title shouldBe "Toy Story 5"
    toy.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 6, 7, 15, 45), LocalDateTime.of(2026, 6, 7, 18, 0))
    toy.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 18, 0)).value.format shouldBe List("NAP")
    toy.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 15, 45)).value.format shouldBe List("DUB")
  }

  it should "fetch per-film detail (synopsis, director, cast, runtime, original title, poster) from the film.php page" in {
    // Deferred detail: the EnrichDetails task calls fetchFilmDetail with the
    // slot's filmUrl. Replays the recorded ISO-8859-2 detail pages for the two
    // Toy Story 5 editions (film_id 12963 = Dubbing, 12964 = Napisy).
    val napisy = client.fetchFilmDetail("https://kinopodbaranami.pl/film.php?film_id=12964").value
    napisy.originalTitle          shouldBe Some("Toy Story 5")
    napisy.director               should contain("Andrew Stanton")
    // `aktorzy` is present on the subtitle page — prove the parser reads it.
    napisy.cast                   should contain("Tom Hanks")
    napisy.runtimeMinutes         shouldBe Some(102)
    napisy.releaseYear            shouldBe Some(2026)
    napisy.countries              should contain("USA")
    napisy.synopsis.value         should include("Toy Story 5")
    napisy.posterUrl.value        should (startWith("https://kinopodbaranami.pl/") and include("images_lib/"))

    // The dubbing page carries the same shared facts (director/runtime/synopsis).
    val dubbing = client.fetchFilmDetail("https://kinopodbaranami.pl/film.php?film_id=12963").value
    dubbing.director              should contain("Andrew Stanton")
    dubbing.runtimeMinutes        shouldBe Some(102)
    dubbing.synopsis              shouldBe defined
  }

  it should "expose itself as a deferred DetailEnricher that resolves TMDB from the listing" in {
    client shouldBe a[services.cinemas.DetailEnricher]
    client.detailGroup            shouldBe "kino-pod-baranami"
    client.defersTmdbResolution   shouldBe false
  }
}
