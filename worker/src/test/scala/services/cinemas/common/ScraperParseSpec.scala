package services.cinemas.common

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.jsoup.Jsoup

import java.time.{LocalDate, LocalDateTime, LocalTime, MonthDay, Period}

/** Direct coverage for the parsing snippets shared across the cinema scrapers.
  * Each `*Client` spec exercises these via a real fixture, but the shared helper
  * earns its own edge-case tests (out-of-range time, the `&quot;` CSS quoting,
  * a custom `<dt>` selector) so a future tweak can't silently break one scraper. */
class ScraperParseSpec extends AnyFlatSpec with Matchers {

  "parseHHmm" should "pull the first HH:mm out of surrounding text" in {
    ScraperParse.parseHHmm("seans o 19:30 dziś") shouldBe Some(LocalTime.of(19, 30))
  }

  it should "accept a single-digit hour" in {
    ScraperParse.parseHHmm("9:05") shouldBe Some(LocalTime.of(9, 5))
  }

  it should "return None when there is no time" in {
    ScraperParse.parseHHmm("brak godziny") shouldBe None
  }

  it should "return None for an out-of-range time instead of throwing" in {
    ScraperParse.parseHHmm("25:99") shouldBe None
  }

  "DayMonthYearPat" should "capture a month name that carries a diacritic" in {
    // `\w` is ASCII-only in Java, so it stops at the "ś"/"ź" and these two —
    // the only genitive month names with a diacritic — silently stop matching.
    Seq("4 września 2026" -> "września", "4 października 2026" -> "października",
        "4 grudnia 2026" -> "grudnia").foreach { case (text, month) =>
      ScraperParse.DayMonthYearPat.findFirstMatchIn(text).map(_.group(2)) shouldBe Some(month)
    }
  }

  "DayMonthPat" should "capture a month name that carries a diacritic" in {
    Seq("4 września" -> "września", "4 października" -> "października",
        "7 czerwca" -> "czerwca").foreach { case (text, month) =>
      ScraperParse.DayMonthPat.findFirstMatchIn(text).map(_.group(2)) shouldBe Some(month)
    }
  }

  // The numeric spellings below are the ones the clients parsed through their
  // own `DateTimeFormatter.ofPattern` copies: Kino Paradox / Apollo / Falenica
  // ("dd.MM.yyyy"), Nowe Horyzonty / Sfinks ("dd-MM-yyyy"), Cyfrowe Kino / Tatry
  // ("dd/MM/yyyy"), GCF / Kino Forum ("yyyy-MM-dd").
  "parseDate" should "read the day-first spellings the cinema pages use" in {
    Seq("06.09.2026", "6.09.2026", "06-09-2026", "06/09/2026").foreach { s =>
      withClue(s)(ScraperParse.parseDate(s) shouldBe Some(LocalDate.of(2026, 9, 6)))
    }
  }

  it should "read an ISO date" in {
    ScraperParse.parseDate("2026-09-06") shouldBe Some(LocalDate.of(2026, 9, 6))
  }

  it should "pull the first date out of surrounding text" in {
    ScraperParse.parseDate("Data: 06.09.2026 18:00")       shouldBe Some(LocalDate.of(2026, 9, 6))
    ScraperParse.parseDate("wtorek, 2026-09-08 (sala 1)")  shouldBe Some(LocalDate.of(2026, 9, 8))
    ScraperParse.parseDate("/wydarzenie/2026-09-08-18-30") shouldBe Some(LocalDate.of(2026, 9, 8))
  }

  it should "return None for an impossible date, a bare time, or no date" in {
    Seq("31.02.2026", "12:30", "brak daty", "2026-13-01", "5.09").foreach { s =>
      withClue(s)(ScraperParse.parseDate(s) shouldBe None)
    }
  }

  // Bilety24 / Kino Orzeł / Kino Aurum ("yyyy-MM-dd HH:mm"), DCF / Kino Mikro /
  // Kino Fenomen ("dd.MM.yyyy HH:mm"), Prom Kępa ("d.MM.yyyy, HH:mm"), SCK
  // Stargard ("yyyy-MM-dd HH:mm:ss").
  "parseDateTime" should "pair the date with the HH:mm beside it" in {
    val expected = LocalDateTime.of(2026, 9, 6, 18, 0)
    Seq("2026-09-06 18:00", "06.09.2026 18:00", "6.09.2026, 18:00", "2026-09-06 18:00:00",
        "Tytuł; Miejsce: Sala 1; Data: 06.09.2026 18:00", "18:00 06.09.2026").foreach { s =>
      withClue(s)(ScraperParse.parseDateTime(s) shouldBe Some(expected))
    }
  }

  it should "return None when either half is missing" in {
    ScraperParse.parseDateTime("2026-09-06")        shouldBe None
    ScraperParse.parseDateTime("18:00")             shouldBe None
    ScraperParse.parseDateTime("2026-09-06-18-30")  shouldBe None  // `-HH-mm` is not an HH:mm
  }

  "polishMonth" should "resolve genitive, nominative and abbreviated spellings in any case" in {
    Seq("września" -> 9, "Września" -> 9, "WRZEŚNIA" -> 9, "wrzesień" -> 9, "wrz" -> 9, "Wrz" -> 9,
        "października" -> 10, "paź" -> 10, "maja" -> 5, "maj" -> 5, "Czerwiec" -> 6, "cze " -> 6).foreach {
      case (token, month) => withClue(token)(ScraperParse.polishMonth(token) shouldBe Some(month))
    }
    ScraperParse.polishMonth("sala") shouldBe None
  }

  // Kino Amok / CSW Toruń / Pod Baranami ("5 września"), Iluzjon ("5 Czerwca -
  // Piątek"), Kinomuzeum / Ekobilet ("10 cze"), Kino Bułgarska.
  "parseDayMonth" should "read a day followed by a Polish month name in any spelling" in {
    Seq("5 września" -> MonthDay.of(9, 5), "Piątek, 5 września" -> MonthDay.of(9, 5),
        "5 Czerwca - Piątek" -> MonthDay.of(6, 5), "10 cze" -> MonthDay.of(6, 10),
        "1 Styczeń" -> MonthDay.of(1, 1), "31 października, 18:00" -> MonthDay.of(10, 31)).foreach {
      case (text, expected) => withClue(text)(ScraperParse.parseDayMonth(text) shouldBe Some(expected))
    }
  }

  it should "return None for an impossible day or a word that is not a month" in {
    Seq("31 kwietnia", "5 sala", "godz. 18:00", "").foreach { s =>
      withClue(s)(ScraperParse.parseDayMonth(s) shouldBe None)
    }
  }

  // Kino Zorza / Kino Roma spell the day header "5.09" with no year.
  "parseNumericDayMonth" should "read a dotted day.month" in {
    ScraperParse.parseNumericDayMonth("Piątek 5.09") shouldBe Some(MonthDay.of(9, 5))
    ScraperParse.parseNumericDayMonth("31.12")       shouldBe Some(MonthDay.of(12, 31))
    ScraperParse.parseNumericDayMonth("12.30")       shouldBe None
    ScraperParse.parseNumericDayMonth("sala 3")      shouldBe None
  }

  // Kino Agrafka / OKF Iluzja / Sokół Brzozów / System Biletowy / Kino Zbyszek
  // ("4 września 2026", "Piątek, 12 Października 2026").
  "parseDayMonthYear" should "read the explicit year" in {
    ScraperParse.parseDayMonthYear("4 września 2026")             shouldBe Some(LocalDate.of(2026, 9, 4))
    ScraperParse.parseDayMonthYear("Piątek, 12 Października 2026") shouldBe Some(LocalDate.of(2026, 10, 12))
    ScraperParse.parseDayMonthYear("9 czerwca 2026, 17:00")       shouldBe Some(LocalDate.of(2026, 6, 9))
  }

  it should "return None without a year, or for an impossible date" in {
    ScraperParse.parseDayMonthYear("4 września")      shouldBe None
    ScraperParse.parseDayMonthYear("31 lutego 2026")  shouldBe None
  }

  "upcomingDate" should "keep a recent date this year and roll an older one into next year" in {
    val today = LocalDate.of(2026, 9, 6)
    ScraperParse.upcomingDate(MonthDay.of(9, 5), today)  shouldBe Some(LocalDate.of(2026, 9, 5))
    ScraperParse.upcomingDate(MonthDay.of(12, 24), today) shouldBe Some(LocalDate.of(2026, 12, 24))
    ScraperParse.upcomingDate(MonthDay.of(7, 20), today)  shouldBe Some(LocalDate.of(2026, 7, 20))  // 48 days back
    ScraperParse.upcomingDate(MonthDay.of(7, 1), today)   shouldBe Some(LocalDate.of(2027, 7, 1))   // 67 days back
    ScraperParse.upcomingDate(MonthDay.of(1, 13), today)  shouldBe Some(LocalDate.of(2027, 1, 13))
  }

  it should "honour a caller's own grace window" in {
    val today = LocalDate.of(2026, 9, 6)
    ScraperParse.upcomingDate(MonthDay.of(7, 1), today, grace = Period.ofMonths(6)) shouldBe Some(LocalDate.of(2026, 7, 1))
    ScraperParse.upcomingDate(MonthDay.of(8, 25), today, grace = Period.ofWeeks(1)) shouldBe Some(LocalDate.of(2027, 8, 25))
  }

  "upcomingMonthDate" should "keep the current month this year and roll any earlier month forward" in {
    val today = LocalDate.of(2026, 9, 20)
    ScraperParse.upcomingMonthDate(MonthDay.of(9, 1), today)  shouldBe Some(LocalDate.of(2026, 9, 1))
    ScraperParse.upcomingMonthDate(MonthDay.of(9, 25), today) shouldBe Some(LocalDate.of(2026, 9, 25))
    ScraperParse.upcomingMonthDate(MonthDay.of(8, 31), today) shouldBe Some(LocalDate.of(2027, 8, 31))
    ScraperParse.upcomingMonthDate(MonthDay.of(1, 5), today)  shouldBe Some(LocalDate.of(2027, 1, 5))
  }

  it should "return None for 29 February in a non-leap year rather than clamp it" in {
    ScraperParse.upcomingDate(MonthDay.of(2, 29), LocalDate.of(2026, 1, 10)) shouldBe None
  }

  "cssUrl" should "unwrap a plain url()" in {
    ScraperParse.cssUrl("background-image: url(/img/a.jpg)") shouldBe Some("/img/a.jpg")
  }

  it should "unwrap single, double and &quot; quoting" in {
    ScraperParse.cssUrl("""url('a.jpg')""")            shouldBe Some("a.jpg")
    ScraperParse.cssUrl("""url("b.jpg")""")            shouldBe Some("b.jpg")
    ScraperParse.cssUrl("""url(&quot;c.jpg&quot;)""")  shouldBe Some("c.jpg")
  }

  it should "return None when there is no url()" in {
    ScraperParse.cssUrl("color: red") shouldBe None
  }

  "ddField" should "return the <dd> after the matching <dt> (case-insensitive label)" in {
    val document = Jsoup.parse(
      "<dl><dt>Rok produkcji</dt><dd>2024</dd><dt>Reżyseria</dt><dd>Jan Kowalski</dd></dl>"
    )
    ScraperParse.ddField(document, "rok produkcji") shouldBe Some("2024")
    ScraperParse.ddField(document, "reżyseria")     shouldBe Some("Jan Kowalski")
  }

  it should "honour a custom dt selector and ignore dt outside it" in {
    val document = Jsoup.parse(
      "<dl class='x'><dt>Czas</dt><dd>90 min</dd></dl><dl><dt>Czas</dt><dd>WRONG</dd></dl>"
    )
    ScraperParse.ddField(document, "czas", "dl.x dt") shouldBe Some("90 min")
  }

  it should "return None for a missing label or an empty dd" in {
    val document = Jsoup.parse("<dl><dt>Rok</dt><dd>   </dd></dl>")
    ScraperParse.ddField(document, "rok")     shouldBe None // empty dd → None
    ScraperParse.ddField(document, "gatunek") shouldBe None // no such dt
  }

  "extractFormatTags" should "split a parenthesised format tag into clean title + display tokens" in {
    ScraperParse.extractFormatTags("Film (2D NAPISY)")  shouldBe (("Film", List("2D", "NAP")))
    ScraperParse.extractFormatTags("Film (2D DUBBING)") shouldBe (("Film", List("2D", "DUB")))
    ScraperParse.extractFormatTags("Film (3D)")         shouldBe (("Film", List("3D")))
  }

  it should "handle a separator-suffix tag (` / 2D dubbing`, ` - napisy`)" in {
    ScraperParse.extractFormatTags("Straszny film / 2D dubbing") shouldBe (("Straszny film", List("2D", "DUB")))
    ScraperParse.extractFormatTags("Straszny film - napisy")     shouldBe (("Straszny film", List("NAP")))
    ScraperParse.extractFormatTags("Film lektor")                shouldBe (("Film", List("LEK")))
  }

  // Nove Kino (Atlantic/Wisła) separates the format with an EN-DASH, not a hyphen
  // ("Wielkie piękno – napisy"); the bespoke ` - `-only splitter it used to have
  // missed that and left the suffix in the title (lost the badge, risked the
  // dub/napisy slot flip-flop). A real dash-bearing title still survives.
  it should "strip a format suffix after an en-dash, leaving a real dash-bearing title intact" in {
    ScraperParse.extractFormatTags("Wielkie piękno – napisy") shouldBe (("Wielkie piękno", List("NAP")))
    ScraperParse.extractFormatTags("Mission - Impossible")    shouldBe (("Mission - Impossible", Nil))
  }

  // Planet Cinema Oświęcim glues the separator to the LAST title word, then
  // space-separates the format: "Straszny Film- 2D dubbing". Dropping the format
  // words must not leave a dangling "Film-", or the dubbed/subtitled variants
  // fragment into their own row instead of merging with the canonical title.
  it should "trim a separator glued to the last title word (`Film- 2D dubbing`)" in {
    ScraperParse.extractFormatTags("Straszny Film- 2D dubbing")      shouldBe (("Straszny Film", List("2D", "DUB")))
    ScraperParse.extractFormatTags("Władcy Wszechświata- 2D napisy") shouldBe (("Władcy Wszechświata", List("2D", "NAP")))
  }

  // Forum Bolesławiec (bilety24) glues the version word to the title with an
  // underscore — "Supergirl_dubbing", "Spider-Man. Całkiem nowy dzień_3D" — so
  // the dubbed/subtitled/3D variants must un-glue, merge to one title, and yield
  // the format token; a non-version underscore must be left intact.
  it should "un-glue an underscore-glued format word into clean title + token" in {
    ScraperParse.extractFormatTags("Supergirl_dubbing") shouldBe (("Supergirl", List("DUB")))
    ScraperParse.extractFormatTags("Supergirl_napisy")  shouldBe (("Supergirl", List("NAP")))
    ScraperParse.extractFormatTags("Spider-Man. Całkiem nowy dzień_3D") shouldBe
      (("Spider-Man. Całkiem nowy dzień", List("3D")))
  }

  it should "leave a non-version underscore (date, programme tag) intact" in {
    ScraperParse.extractFormatTags("Seans w ciemno_7.26") shouldBe (("Seans w ciemno_7.26", Nil))
    ScraperParse.extractFormatTags("Monterey Pop_DKF")    shouldBe (("Monterey Pop_DKF", Nil))
  }

  // Kino Oskard (bilety24) glues the version word with a SLASH and no spaces —
  // "Supergirl/dubbing", "…dzień/napisy" — so the variants must un-glue + merge
  // like the underscore case; a real slashed title ("AC/DC", "Face/Off") whose
  // word after the slash isn't a version word must be left whole.
  it should "un-glue a slash-glued version word but keep a real slashed title" in {
    ScraperParse.extractFormatTags("Supergirl/dubbing")             shouldBe (("Supergirl", List("DUB")))
    ScraperParse.extractFormatTags("Spider-Man. Nowy dzień/napisy") shouldBe (("Spider-Man. Nowy dzień", List("NAP")))
    ScraperParse.extractFormatTags("AC/DC")                         shouldBe (("AC/DC", Nil))
    ScraperParse.extractFormatTags("Face/Off")                      shouldBe (("Face/Off", Nil))
  }

  // Screening-type labels (premiere / special / pre-premiere / a cinema's own
  // code) are NOT format — they keep the screening its own card, so
  // extractFormatTags leaves them (and any format word sitting behind them) in
  // place rather than stripping/partial-stripping.
  it should "keep a screening-type label in the title (not treated as a format tag)" in {
    ScraperParse.extractFormatTags("Spider-Man - Pokaz specjalny")     shouldBe (("Spider-Man - Pokaz specjalny", Nil))
    ScraperParse.extractFormatTags("Zaproszenie - przedpremiera")      shouldBe (("Zaproszenie - przedpremiera", Nil))
    ScraperParse.extractFormatTags("Ojczyzna - pokaz przedpremierowy") shouldBe (("Ojczyzna - pokaz przedpremierowy", Nil))
    ScraperParse.extractFormatTags("Backrooms - UROCZYSTA POLSKA PREMIERA") shouldBe (("Backrooms - UROCZYSTA POLSKA PREMIERA", Nil))
  }

  // Ukrainian-screening guard: a dub/lektor word directly after "ukraiński"/
  // "ukrainian" is NOT a format tag — that screening is a distinct audience and
  // keeps its whole title (stays its own card). Must hold in bare, paren AND
  // bracket shapes (the paren form otherwise collapses onto the base). Only
  // dub/lektor are guarded — napisy/2D still strip.
  it should "keep a Ukrainian dub/lektor screening whole in every shape (guard)" in {
    ScraperParse.extractFormatTags("Toy Story 5 ukraiński dubbing")   shouldBe (("Toy Story 5 ukraiński dubbing", Nil))
    ScraperParse.extractFormatTags("Toy Story 5 (ukraiński dubbing)") shouldBe (("Toy Story 5 (ukraiński dubbing)", Nil))
    ScraperParse.extractFormatTags("Toy Story 5 [ukraiński dubbing]") shouldBe (("Toy Story 5 [ukraiński dubbing]", Nil))
    ScraperParse.extractFormatTags("Straszny film ukrainian lektor")  shouldBe (("Straszny film ukrainian lektor", Nil))
    // a NON-guarded format word (napisy) still strips even next to "ukraiński"
    ScraperParse.extractFormatTags("Toy Story 5 ukraiński napisy")    shouldBe (("Toy Story 5 ukraiński", List("NAP")))
  }

  // "dolby" is the bare carrier word and yields no token; "atmos" carries the
  // ATMOS badge, so "Dolby Atmos", a bare trailing "Atmos", and a standalone
  // "(Atmos)" paren all surface exactly one ATMOS token (deduplicated).
  it should "emit an ATMOS token for atmos but not for the bare 'dolby' carrier" in {
    ScraperParse.extractFormatTags("Dzień objawienia (2D NAPISY DOLBY ATMOS)") shouldBe
      (("Dzień objawienia", List("2D", "NAP", "ATMOS")))
    ScraperParse.extractFormatTags("Spider-Man - 2D NAP ATMOS") shouldBe
      (("Spider-Man", List("2D", "NAP", "ATMOS")))
    ScraperParse.extractFormatTags("Vaiana (Dolby Atmos)") shouldBe (("Vaiana", List("ATMOS")))
    ScraperParse.extractFormatTags("Vaiana (Atmos)")       shouldBe (("Vaiana", List("ATMOS")))
  }

  it should "return (title, Nil) for a title with no format tag" in {
    ScraperParse.extractFormatTags("Mavka. Prawdziwy mit") shouldBe (("Mavka. Prawdziwy mit", Nil))
  }

  it should "not mangle a real dash/colon title" in {
    ScraperParse.extractFormatTags("Tom i Jerry: Przygoda w muzeum") shouldBe
      (("Tom i Jerry: Przygoda w muzeum", Nil))
  }

  it should "agree byte-for-byte with stripFormatTags on the cleaned title" in {
    val inputs = Seq(
      "Film (2D NAPISY)", "Straszny film / 2D dubbing", "Straszny film - napisy",
      "Dzień objawienia (2D NAPISY DOLBY ATMOS)", "Mavka. Prawdziwy mit",
      "Tom i Jerry: Przygoda w muzeum", "Toy story 5 (2D DUBBING DOLBY ATMOS)",
      "Film [2D napisy]", "DKF - Some film"
    )
    inputs.foreach(in => ScraperParse.extractFormatTags(in)._1 shouldBe ScraperParse.stripFormatTags(in))
  }

  "canonicalTrailer" should "canonicalise YouTube embed / watch / youtu.be to the watch form" in {
    ScraperParse.canonicalTrailer("https://www.youtube.com/embed/AYq1ljpbNfA?feature=oembed") shouldBe
      Some("https://www.youtube.com/watch?v=AYq1ljpbNfA")
    ScraperParse.canonicalTrailer("https://www.youtube.com/watch?v=C-h48bml6k0") shouldBe
      Some("https://www.youtube.com/watch?v=C-h48bml6k0")
    ScraperParse.canonicalTrailer("https://youtu.be/H1jQ_5vNGYk") shouldBe
      Some("https://www.youtube.com/watch?v=H1jQ_5vNGYk")
  }

  it should "pass a Vimeo URL through unchanged and drop anything else" in {
    ScraperParse.canonicalTrailer("https://player.vimeo.com/video/12345") shouldBe
      Some("https://player.vimeo.com/video/12345")
    ScraperParse.canonicalTrailer("https://example.com/not-a-trailer") shouldBe None
  }

  "stripUrls" should "remove http(s) and bare www tokens and tidy the whitespace they leave" in {
    ScraperParse.stripUrls("Świetny film. Więcej: www.example.pl")  shouldBe "Świetny film. Więcej:"
    ScraperParse.stripUrls("Zobacz https://foo.bar/x teraz")        shouldBe "Zobacz teraz"
    ScraperParse.stripUrls("Instagram: https://instagram.com/abc")  shouldBe "Instagram:"
    ScraperParse.stripUrls("Opis bez żadnych linków.")              shouldBe "Opis bez żadnych linków."
  }

  "cleanSynopsis" should "drop the named junk sub-trees and strip residual plain-text URLs" in {
    val html =
      """<div class="tresc">
        |  <div class="trailer"><a href="x">https://youtube.com/watch?v=abc</a></div>
        |  <p>Prawdziwy opis filmu o miłości i stracie.</p>
        |  <div class="terminy"><h2>Dostępne terminy</h2><a>Kup bilet</a></div>
        |  <p>Więcej: www.kino.pl</p>
        |</div>""".stripMargin
    val out = ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.tresc"), "div.terminy", "div.trailer")
    out should include ("Prawdziwy opis filmu o miłości i stracie")
    out should not include "Dostępne terminy"
    out should not include "Kup bilet"
    out should not include "youtube.com"
    out should not include "www.kino.pl"
  }

  it should "operate on a clone, leaving the live DOM intact for other fields" in {
    val el = Jsoup.parse("""<div class="c"><span class="junk">x</span>opis</div>""").selectFirst("div.c")
    ScraperParse.cleanSynopsis(el, "span.junk")
    Option(el.selectFirst("span.junk")) should not be empty // original untouched
  }

  it should "preserve paragraph breaks between sibling <p> blocks" in {
    val html = """<div class="t"><p>Pierwszy akapit opisu filmu.</p><p>Drugi akapit, osobny.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe
      "Pierwszy akapit opisu filmu.\n\nDrugi akapit, osobny."
  }

  it should "drop a URL-only paragraph without leaving a blank-line gap" in {
    val html = """<div class="t"><p>Prawdziwy opis.</p><p>https://youtu.be/x</p><p>Ostatni akapit.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe
      "Prawdziwy opis.\n\nOstatni akapit."
  }

  it should "render <br> as a single line break and <p> as a blank line" in {
    val html = """<div class="t"><p>Linia jeden<br>Linia dwa</p><p>Akapit dwa.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe
      "Linia jeden\nLinia dwa\n\nAkapit dwa."
  }

  "blockText" should "fall back to flat text when the element has no block children" in {
    ScraperParse.blockText(Jsoup.parse("<span>Jeden akapit bez struktury.</span>").selectFirst("span")).trim shouldBe
      "Jeden akapit bez struktury."
  }

  it should "preserve <b>/<strong> as **bold** and <i>/<em> as *italic*" in {
    val html = """<div class="t"><p>Zwykły <b>pogrubiony</b> i <em>kursywa</em> tekst.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe
      "Zwykły **pogrubiony** i *kursywa* tekst."
  }

  it should "skip empty emphasis tags so they can't emit a bare ****" in {
    val html = """<div class="t"><p>Opis<b></b> filmu.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe "Opis filmu."
  }

  it should "move whitespace inside an emphasis tag outside the markers (so CommonMark renders it)" in {
    // `<b>pogrubiony </b>` would otherwise emit `**pogrubiony **`, which iOS
    // (strict CommonMark) renders literally — the space must hop outside.
    val html = """<div class="t"><p>A <b>pogrubiony </b>tekst.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe "A **pogrubiony** tekst."
  }

  it should "drop an emphasis tag whose content is only whitespace" in {
    val html = """<div class="t"><p>A <b> </b>B.</p></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe "A B."
  }

  it should "skip block-spanning emphasis (can't bold across a paragraph) and keep the prose" in {
    val html = """<div class="t"><b><p>Pierwszy akapit.</p><p>Drugi akapit.</p></b></div>"""
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe
      "Pierwszy akapit.\n\nDrugi akapit."
  }

  it should "skip nested emphasis (only the leaf wins, no broken ***…* **)" in {
    val html = """<div class="t"><p>W filmie <b><i>Tytuł</i></b> coś.</p></div>"""
    // The <b> contains an <i>, so bold is skipped; the leaf <i> still emphasises.
    ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t")) shouldBe "W filmie *Tytuł* coś."
  }

  it should "not throw when an emphasis tag wraps only a Unicode space (em-space survives .text.trim)" in {
    // U+2003 passes the insertion filter (trim keeps chars > 0x20) but counts as
    // whitespace to the tidy pass — the all-whitespace pair must collapse, not
    // index past the segment end.
    val html = "<div class=\"t\"><p>A <b>\u2003</b>B.</p></div>"
    val out = ScraperParse.cleanSynopsis(Jsoup.parse(html).selectFirst("div.t"))
    out should not include "**"
    out should startWith ("A")
    out should endWith ("B.")
  }
}
