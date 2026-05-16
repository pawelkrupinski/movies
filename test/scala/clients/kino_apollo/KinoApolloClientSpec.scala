package clients.kino_apollo

import clients.tools.FakeHttpFetch
import models.{KinoApollo, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoApolloClient

import java.time.LocalDateTime

class KinoApolloClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoApolloClient(new FakeHttpFetch("kino-apollo"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "KinoApolloClient.fetch" should "return 13 films from the recorded repertoire page" in {
    // Two "Drzewo Magii - seans przedpremierowy" screenings merge into the plain
    // "Drzewo Magii" entry after the suffix is stripped (15 → 14), and the
    // "DZIEŃ DZIECKA W APOLLO - Drzewo Magii" children's-day event merges in
    // too after the prefix is stripped (14 → 13).
    results.size shouldBe 13
  }

  it should "return 30 unique screenings in total" in {
    // 60 time-links on the page (mobile + desktop renderings of each event);
    // deduplicated to 30 by booking URL.
    results.flatMap(_.showtimes).size shouldBe 30
  }

  it should "assign KinoApollo cinema to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoApollo)
  }

  it should "sort the result alphabetically by title" in {
    results.map(_.movie.title) shouldBe results.map(_.movie.title).sorted
  }

  // ── Complete title set ────────────────────────────────────────────────────

  it should "return exactly the expected set of titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Cykl \"Kultowa klasyka\" - Zawieście czerwone latarnie",
      "Cykl \"Kultowa klasyka\" - Żywot Briana / Life of Brian (1979)",
      "Cykl \"Zawsze Ósmego\" - Follemente. W tym szaleństwie jest metoda",
      "Cykl „Wajda: re-wizje\" - Bez znieczulenia / Rough Treatment (1978)",
      "Cykl „Wajda: re-wizje\" - Brzezina / The Birch Wood (1970)",
      "Cykl „Wajda: re-wizje\" - Człowiek z marmuru / Man of Marble (1977)",
      "Cykl „Wajda: re-wizje\" - Krajobraz po bitwie / Landscape After the Battle (1970)",
      "Cykl „Wajda: re-wizje\" - Niewinni czarodzieje / Innocent Sorcerers (1960)",
      "Drzewo Magii",
      "Milcząca przyjaciółka",
      "Miłość w czasach apokalipsy",
      "Znaki Pana Śliwki",
      "Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem",
    )
  }

  // ── Showtime counts ───────────────────────────────────────────────────────

  it should "return correct showtime count per title" in {
    val counts = results.map(cm => cm.movie.title -> cm.showtimes.size).toMap
    counts("Miłość w czasach apokalipsy")                                            shouldBe 4
    counts("Milcząca przyjaciółka")                                                  shouldBe 6
    // 3 plain "Drzewo Magii" + 2 pre-premiere screenings (suffix stripped)
    // + 2 children's-day screenings (prefix stripped) = 7
    counts("Drzewo Magii")                                                           shouldBe 7
    counts("Znaki Pana Śliwki")                                                      shouldBe 4
    counts("Znaki Pana Śliwki + prelekcja i spotkanie z Damianem Dudkiem")           shouldBe 1
    counts("Cykl „Wajda: re-wizje\" - Niewinni czarodzieje / Innocent Sorcerers (1960)") shouldBe 1
    counts("Cykl „Wajda: re-wizje\" - Człowiek z marmuru / Man of Marble (1977)")    shouldBe 1
    counts("Cykl „Wajda: re-wizje\" - Brzezina / The Birch Wood (1970)")             shouldBe 1
    counts("Cykl „Wajda: re-wizje\" - Krajobraz po bitwie / Landscape After the Battle (1970)") shouldBe 1
    counts("Cykl „Wajda: re-wizje\" - Bez znieczulenia / Rough Treatment (1978)")    shouldBe 1
    counts("Cykl \"Kultowa klasyka\" - Żywot Briana / Life of Brian (1979)")         shouldBe 1
    counts("Cykl \"Kultowa klasyka\" - Zawieście czerwone latarnie")                 shouldBe 1
    counts("Cykl \"Zawsze Ósmego\" - Follemente. W tym szaleństwie jest metoda")     shouldBe 1
  }

  // ── Full showtime detail for the busiest titles ───────────────────────────

  it should "return exact showtimes for Miłość w czasach apokalipsy" in {
    byTitle("Miłość w czasach apokalipsy").showtimes shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 15, 18,  0), Some("https://bilety.kinoapollo.pl/event/view/id/662072")),
      Showtime(LocalDateTime.of(2026, 5, 15, 20, 15), Some("https://bilety.kinoapollo.pl/event/view/id/662076")),
      Showtime(LocalDateTime.of(2026, 5, 16, 18,  0), Some("https://bilety.kinoapollo.pl/event/view/id/662077")),
      Showtime(LocalDateTime.of(2026, 5, 17, 19,  0), Some("https://bilety.kinoapollo.pl/event/view/id/662078")),
    )
  }

  it should "return exact showtimes for Milcząca przyjaciółka" in {
    byTitle("Milcząca przyjaciółka").showtimes shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 22, 20, 15), Some("https://bilety.kinoapollo.pl/event/view/id/662095")),
      Showtime(LocalDateTime.of(2026, 5, 23, 17, 15), Some("https://bilety.kinoapollo.pl/event/view/id/662097")),
      Showtime(LocalDateTime.of(2026, 5, 24, 17, 30), Some("https://bilety.kinoapollo.pl/event/view/id/662098")),
      Showtime(LocalDateTime.of(2026, 5, 29, 17, 30), Some("https://bilety.kinoapollo.pl/event/view/id/662099")),
      Showtime(LocalDateTime.of(2026, 5, 30, 19,  0), Some("https://bilety.kinoapollo.pl/event/view/id/662100")),
      Showtime(LocalDateTime.of(2026, 5, 31, 18,  0), Some("https://bilety.kinoapollo.pl/event/view/id/662101")),
    )
  }

  it should "return exact showtimes for Znaki Pana Śliwki" in {
    byTitle("Znaki Pana Śliwki").showtimes shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 5, 17, 17, 15), Some("https://bilety.kinoapollo.pl/event/view/id/662088")),
      Showtime(LocalDateTime.of(2026, 5, 22, 18, 30), Some("https://bilety.kinoapollo.pl/event/view/id/662089")),
      Showtime(LocalDateTime.of(2026, 5, 24, 20, 30), Some("https://bilety.kinoapollo.pl/event/view/id/662090")),
      Showtime(LocalDateTime.of(2026, 5, 29, 20, 30), Some("https://bilety.kinoapollo.pl/event/view/id/662091")),
    )
  }

  // Children's-day banner — "DZIEŃ DZIECKA W APOLLO - Drzewo Magii" is the
  // same film as the regular run. Strip the prefix so the 2 family showtimes
  // merge into the base "Drzewo Magii" entry rather than competing as a
  // separate row that no enrichment can resolve (TMDB's title search for the
  // banner-prefixed string returns nothing, and the row stays at tmdbId=None).
  it should "strip the 'DZIEŃ DZIECKA W APOLLO - ' prefix and merge with the base title" in {
    val titles = results.map(_.movie.title).toSet
    titles.foreach(t => withClue(s"title: $t") {
      t                            should not startWith "DZIEŃ DZIECKA W APOLLO - "
    })
    val drzewo = byTitle("Drzewo Magii").showtimes.flatMap(_.bookingUrl)
    drzewo                         should contain ("https://bilety.kinoapollo.pl/event/view/id/662084")
    drzewo                         should contain ("https://bilety.kinoapollo.pl/event/view/id/662085")
  }

  it should "return the single far-future showtime for the Wajda cycle's Krajobraz po bitwie" in {
    byTitle("Cykl „Wajda: re-wizje\" - Krajobraz po bitwie / Landscape After the Battle (1970)").showtimes shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 9, 13, 15, 0), Some("https://bilety.kinoapollo.pl/event/view/id/639183")),
    )
  }

  // ── Title cleanup: strip " - seans przedpremierowy" ─────────────────────

  it should "strip the ' - seans przedpremierowy' suffix and merge with the base title" in {
    val titles = results.map(_.movie.title).toSet
    titles.foreach(t => withClue(s"title: $t") {
      t                                                 should not endWith " - seans przedpremierowy"
    })
    // The two pre-premiere screenings of Drzewo Magii roll into the same entry
    // as the three regular ones — both URLs end up on the merged Movie.
    val drzewo = byTitle("Drzewo Magii").showtimes.flatMap(_.bookingUrl)
    drzewo                       should contain ("https://bilety.kinoapollo.pl/event/view/id/662079")
    drzewo                       should contain ("https://bilety.kinoapollo.pl/event/view/id/662080")
  }

  // ── Booking URLs ──────────────────────────────────────────────────────────

  it should "use bilety.kinoapollo.pl event URLs for every showtime" in {
    results.flatMap(_.showtimes).foreach { st =>
      withClue(s"booking URL: ${st.bookingUrl}") {
        st.bookingUrl.exists(_.startsWith("https://bilety.kinoapollo.pl/event/view/id/")) shouldBe true
      }
    }
  }

  it should "treat booking URLs as unique screening ids (no duplicates after dedup)" in {
    val urls = results.flatMap(_.showtimes).flatMap(_.bookingUrl)
    urls.distinct.size shouldBe urls.size
  }

  // ── Posters ───────────────────────────────────────────────────────────────

  it should "extract a poster URL for every film" in {
    results.foreach { cm =>
      withClue(s"${cm.movie.title}: ") {
        cm.posterUrl                       should not be empty
        cm.posterUrl.get          should startWith ("https://kinoapollo.pl/wp-content/uploads/")
      }
    }
  }

  it should "prefer the largest poster variant for Miłość w czasach apokalipsy" in {
    // The page also has the "-200x300" thumbnail variant; the parser must pick
    // the un-suffixed full-resolution variant instead.
    val poster = byTitle("Miłość w czasach apokalipsy").posterUrl
    poster                       should not be empty
    poster.get                   should not include "200x300"
    poster.get                   should include ("milosc-w-czasach-apo_plakat")
  }

  it should "pick scaled/full poster for Milcząca przyjaciółka over its size variants" in {
    val poster = byTitle("Milcząca przyjaciółka").posterUrl
    poster                       should not be empty
    poster.get                   should include ("Milczaca-przyjaciolka_plakat")
    poster.get                   should not include "208x300"
  }

  // ── Fields the listing page does not expose ───────────────────────────────

  it should "leave director, cast, synopsis and filmUrl empty (not surfaced on the listing page)" in {
    results.foreach { cm =>
      cm.director shouldBe None
      cm.cast     shouldBe None
      cm.synopsis shouldBe None
      cm.filmUrl  shouldBe None
    }
  }

  it should "produce showtimes with no room and no format" in {
    val all = results.flatMap(_.showtimes)
    all.exists(_.room.isDefined)  shouldBe false
    all.exists(_.format.nonEmpty) shouldBe false
  }
}
