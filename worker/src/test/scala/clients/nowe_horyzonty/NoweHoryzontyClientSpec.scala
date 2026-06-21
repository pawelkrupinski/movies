package clients.nowe_horyzonty

import clients.tools.FakeHttpFetch
import models.{KinoNoweHoryzonty, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, NoweHoryzontyClient}

import java.time.{LocalDate, LocalDateTime}

class NoweHoryzontyClientSpec extends AnyFlatSpec with Matchers {

  // The full schedule comes from the per-day `rep.json?dzien=DD-MM-YYYY` AJAX
  // endpoint, not the `program.s` teaser page. Pin `today` to the recording
  // date so the seven day-requests hit the recorded fixtures deterministically.
  private val today   = LocalDate.of(2026, 6, 6)
  private val client  = new NoweHoryzontyClient(new FakeHttpFetch("nowe-horyzonty"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title")))
      .getOrElse(fail(s"no detail for $title"))

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
    m.filmUrl shouldBe Some("https://www.kinonh.pl/op.s?id=22588")
    val d = detailFor("Obsesja")
    d.runtimeMinutes shouldBe Some(108)
    d.releaseYear    shouldBe Some(2025)
    d.originalTitle  shouldBe Some("Obsession")
    d.countries      shouldBe Seq("USA")
    d.genres         shouldBe Seq("Horror")
    d.director       shouldBe Seq("Curry Barker")
    d.synopsis.getOrElse("").length should be > 30
  }

  // Regression: `selectFirst("div.txt.wciecia.opisf p")` grabbed only the FIRST
  // `<p>` of a multi-paragraph synopsis, truncating ~half the film's plot (23 of
  // 43 op.s pages carry 2+ Polish paragraphs). The whole synopsis must survive,
  // with the paragraph breaks preserved as blank lines.
  it should "preserve every paragraph of a multi-paragraph synopsis" in {
    val synopsis = detailFor("Zawodowcy").synopsis.getOrElse(fail("no synopsis for Zawodowcy"))
    withClue(s"synopsis = ${synopsis.replace("\n", "\\n")}\n") {
      synopsis should include("\n\n")
      synopsis should include("rollercoaster")  // 2nd paragraph
      synopsis should include("Eve")            // 3rd paragraph
    }
  }

  // The op.s synopsis container also wraps a foreign-language version of the plot
  // behind a bare "FR:" / "EN:" `<h4>` label, plus a "gatunek:" genre `<h4>`.
  // Neither belongs in the displayed synopsis — only the Polish prose does.
  it should "drop the foreign-language translation and the genre label from the synopsis" in {
    val synopsis = detailFor("Przeżyj to sam").synopsis.getOrElse(fail("no synopsis for Przeżyj to sam"))
    withClue(s"synopsis = $synopsis\n") {
      synopsis should include("dorastaniu")     // Polish prose kept
      synopsis should not include "Nous sommes" // French translation dropped
      synopsis should not include "gatunek"     // genre label dropped
    }
  }

  it should "parse a multi-genre film and keep its original title" in {
    val d = detailFor("Diabeł ubiera się u Prady 2")
    d.originalTitle shouldBe Some("The Devil Wears Prada 2")
    d.genres        shouldBe Seq("Komedia", "Dramat")
    d.countries     shouldBe Seq("USA")
  }

  it should "build booking URLs from the day's eventId, dated by the requested day" in {
    byTitle("Obsesja").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 6, 21, 30),
        Some("https://www.kinonh.pl/bilet.s?eventId=194204&forwardback=https://www.kinonh.pl/program.s"),
        None, Nil
      )
  }

  // The listing `span.ilustr` background-image is a gallery still
  // (`glw_…_mini.jpg`), not the film poster — so we deliberately emit no
  // listing poster and let detail enrichment supply the real `div.plakat`
  // poster (`plak1at_…`), which the merge would otherwise never override.
  it should "not carry the listing still as a poster" in {
    byTitle("Obsesja").posterUrl shouldBe None
  }

  // Regression: Kumotry's listing still (`glw_1330805_1.13.jpg_mini.jpg`) was
  // displayed instead of its actual poster (`plak1at_8241740.8.jpg_x_standa`),
  // which lives only on the op.s detail page.
  it should "take the poster from the op.s detail page, not the listing still" in {
    detailFor("Kumotry").posterUrl shouldBe
      Some("https://www.kinonh.pl/pliki/wgrane/image/fotosy/2026/POKAZY_SPECJALNE/KUMOTRY__SPOTKANIE_Z_REZYSERKA/plak1at_8241740.8.jpg_x_standa.jpg")
  }
}
