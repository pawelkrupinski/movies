package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextNormalizationSpec extends AnyFlatSpec with Matchers {

  "titleCaseIfAllCaps" should "leave properly-cased strings alone" in {
    TextNormalization.titleCaseIfAllCaps("Karl Urban")  shouldBe "Karl Urban"
    TextNormalization.titleCaseIfAllCaps("Anne Hathaway, Meryl Streep") shouldBe "Anne Hathaway, Meryl Streep"
  }

  it should "leave mixed-case strings alone even if mostly uppercase" in {
    // A single lowercase letter is enough signal that the source already
    // cased the string and we shouldn't second-guess.
    TextNormalization.titleCaseIfAllCaps("MARTYN Ford") shouldBe "MARTYN Ford"
  }

  it should "title-case an ALL CAPS comma-list of names" in {
    TextNormalization.titleCaseIfAllCaps("KARL URBAN, ADELINE RUDOLPH, LEWIS TAN") shouldBe
      "Karl Urban, Adeline Rudolph, Lewis Tan"
  }

  it should "preserve dotted initials when title-casing" in {
    // "C.J." reads as the start of a fresh word after each dot, so each
    // initial letter stays uppercase.
    TextNormalization.titleCaseIfAllCaps("C.J. BLOOMFIELD")  shouldBe "C.J. Bloomfield"
    TextNormalization.titleCaseIfAllCaps("F.B.I. AGENT")     shouldBe "F.B.I. Agent"
  }

  it should "handle apostrophes and hyphens" in {
    TextNormalization.titleCaseIfAllCaps("O'BRIEN")    shouldBe "O'Brien"
    TextNormalization.titleCaseIfAllCaps("MARY-ANN")   shouldBe "Mary-Ann"
  }

  it should "return empty input unchanged" in {
    TextNormalization.titleCaseIfAllCaps("") shouldBe ""
  }

  // ── dropTrailingPartialNameIfLong ──────────────────────────────────────────

  "dropTrailingPartialNameIfLong" should "leave short strings alone" in {
    TextNormalization.dropTrailingPartialNameIfLong("Karl Urban, Meryl Streep") shouldBe
      "Karl Urban, Meryl Streep"
  }

  it should "strip the trailing partial name when the cast string is at Cinema City's truncation length" in {
    // Exact production case: Cinema City's filmDetails JSON caps at ~232
    // chars and `C.J. BLOOMFI` is "C.J. Bloomfield" cut mid-word.
    val truncated = "KARL URBAN, ADELINE RUDOLPH, LEWIS TAN, JESSICA MCNAMEE, JOSH LAWSON, " +
      "TADANOBU ASANO, MEHCAD BROOKS, LUDI LIN, DAMON HERRIMAN, TATI GABRIELLE, MARTYN FORD, " +
      "CHIN HAN, JOE TASLIM, HIROYUKI SANADA, DESMOND CHIAM, ANA THU NGUYEN, MAX HUANG, C.J. BLOOMFI"
    TextNormalization.dropTrailingPartialNameIfLong(truncated) should endWith ("MAX HUANG")
    TextNormalization.dropTrailingPartialNameIfLong(truncated) should not include "BLOOMFI"
  }

  it should "leave the input alone when there's no comma to cut on (single-name suspected truncation)" in {
    // 240 chars of one word — no comma to fall back on; preserve and move on.
    val noComma = "A" * 240
    TextNormalization.dropTrailingPartialNameIfLong(noComma) shouldBe noComma
  }

  // ── stripHtml ─────────────────────────────────────────────────────────────

  "stripHtml" should "strip div and br wrappers from a Multikino synopsis" in {
    val raw = "<div>Młodzi ludzie są świadkami wypadku.</div><div><br></div>"
    TextNormalization.stripHtml(raw) shouldBe "Młodzi ludzie są świadkami wypadku."
  }

  it should "return plain text unchanged" in {
    TextNormalization.stripHtml("No tags here.") shouldBe "No tags here."
  }

  it should "collapse whitespace left by removed tags" in {
    TextNormalization.stripHtml("<p>First paragraph.</p>  <p>Second.</p>") shouldBe "First paragraph. Second."
  }

  it should "decode HTML entities" in {
    TextNormalization.stripHtml("caf&eacute; &amp; bar") shouldBe "café & bar"
  }

  // ── stripUrls ─────────────────────────────────────────────────────────────

  "stripUrls" should "drop a leading YouTube watch URL folded into a synopsis" in {
    // Exact production shape (confirmed in prod Mongo): KINOkawiarnia Stacja
    // Falenica's detail page has the distributor's "watch on YouTube" link
    // pasted into the synopsis block (`div.section.tresc`), so `FalenicaClient`'s
    // `.text` read prepends the bare URL to the blurb. This was the longest
    // synopsis across 19 cinemas, so longest-wins surfaced it on every city's
    // film page (Orły Republiki). `embeds_referring_euri` points at the
    // distributor site the editor copied the link from.
    val raw = "https://www.youtube.com/watch?v=ERysio3sHjw&source_ve_path=MjM4NTE&embeds_referring_euri=" +
      "https%3A%2F%2Faurorafilms.pl%2F Nowy film laureata Złotej Palmy George Fahmy to największa produkcja."
    TextNormalization.stripUrls(raw) shouldBe
      "Nowy film laureata Złotej Palmy George Fahmy to największa produkcja."
  }

  it should "drop a URL embedded mid-text and close the gap" in {
    TextNormalization.stripUrls("Zwiastun: https://youtu.be/abc123 — świetny film.") shouldBe
      "Zwiastun: — świetny film."
  }

  it should "drop a bare www. URL" in {
    TextNormalization.stripUrls("Więcej na www.kinomuza.pl/orly już dziś.") shouldBe
      "Więcej na już dziś."
  }

  it should "leave URL-free text unchanged" in {
    TextNormalization.stripUrls("Nowy film laureata Złotej Palmy.") shouldBe
      "Nowy film laureata Złotej Palmy."
  }

  it should "preserve paragraph breaks (\\n\\n) that the mobile apps render" in {
    // Kino Muza joins `<p>` blocks with "\n\n"; iOS/Android render those as
    // paragraph separators via /api/details, so the cleaner must not flatten
    // them — only horizontal gaps left by a removed URL get collapsed.
    val raw = "Pierwszy akapit. https://youtu.be/x\n\nDrugi akapit."
    TextNormalization.stripUrls(raw) shouldBe "Pierwszy akapit.\n\nDrugi akapit."
  }

  it should "return empty when the text is nothing but a URL" in {
    TextNormalization.stripUrls("https://www.youtube.com/watch?v=ERysio3sHjw") shouldBe ""
  }
}
