package clients.nowe_horyzonty

import org.scalatest.matchers.should.Matchers
import models.{KinoNoweHoryzonty, Showtime}
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.FilmDetail
import services.cinemas.pl.NoweHoryzontyClient
import services.cinemas.common.ScrapeHorizon

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

  // ── The programme past the first week ────────────────────────────────────
  //
  // `rep.json` answers for ANY date, and the scrape only ever asked for seven
  // days — so Besson's "Joanna d'Arc" on 2026-08-27, a Mistrzowie Kina screening
  // at the country's largest arthouse, was never fetched and never reached the
  // database. Retrospectives and cycles are exactly what lives past a week here,
  // so the window has to follow the programme rather than assume its length.
  //
  // A stub rather than a recorded corpus: what is under test is which DAYS get
  // asked for, not how a day parses (the fixtures above cover that).

  /** One film with one slot, in the shape `rep.json` returns. */
  private def dayWithAFilm(id: String, title: String): String =
    play.api.libs.json.Json.obj("lista" ->
      s"""<div class="boks"><a class="tyt" href="op.s?id=$id">$title</a>
         |<div class="seanserep"><a class="xseans" href="bilet.s?eventId=$id-1">20:00</a></div></div>""".stripMargin
    ).toString

  private val emptyDay = play.api.libs.json.Json.obj("lista" -> "").toString

  private def stubServing(liveDates: Map[String, String]) = new tools.GetOnlyHttpFetch {
    def get(url: String): String = {
      val day = """dzien=(\d{2}-\d{2}-\d{4})""".r.findFirstMatchIn(url).map(_.group(1)).getOrElse("")
      liveDates.getOrElse(day, emptyDay)
    }
  }

  it should "reach a screening weeks out, past the old one-week window" in {
    val start = LocalDate.of(2026, 8, 5)
    // A running programme, as this venue actually has one — measured live on
    // 5 August, 15 August and 27 August — with Besson's screening on the 27th,
    // 22 days out and four times beyond the window the scrape used to ask for.
    val fmt   = java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val daily = (0 to 22).map(d => start.plusDays(d).format(fmt) -> dayWithAFilm("100", "Coś w repertuarze")).toMap
    val far   = new NoweHoryzontyClient(
      stubServing(daily + ("27-08-2026" -> dayWithAFilm("22672", "Joanna d'Arc"))), start)

    val found = far.fetch()
    found.map(_.movie.title).toSet should contain ("Joanna d'Arc")
    found.find(_.movie.title == "Joanna d'Arc").flatMap(_.filmUrl) shouldBe
      Some("https://www.kinonh.pl/op.s?id=22672")
  }

  it should "stop walking once the programme runs out, so a dormant venue stays cheap" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    val counting = new tools.GetOnlyHttpFetch {
      def get(url: String): String = { asked += url; emptyDay }
    }
    val dormant = new NoweHoryzontyClient(counting, LocalDate.of(2026, 8, 5))

    dormant.fetch() shouldBe empty
    // Exactly the stop rule's worth of probes — not two years of them.
    asked.size shouldBe ScrapeHorizon.MaxEmptyDays
  }
}
