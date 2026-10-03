package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.SearchTitles

/** Search-title candidates for festival/preview "decorated" titles. */
class MovieServiceTmdbHintsSpec extends AnyFlatSpec with Matchers {

  // ── Festival/preview "decorated" titles resolve independently of siblings ──
  // "Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "Ojczyzna (pokaz przedpremierowy)" and
  // the like don't match TMDB by their decorated title, so they must resolve on
  // their own, not by copying a tmdbId from a sibling: search the cinema-provided original title + each side
  // of the "X | Y" pipe + the de-parenthesised title.

  "searchTitleCandidates" should "offer the original title, each pipe side, and the de-parenthesised title" in {
    SearchTitles.candidates("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", Some("Possession")) should
      contain allOf ("Opętanie | ŻUŁAWSKI. KINO EKSTAZY", "Possession", "Opętanie", "ŻUŁAWSKI. KINO EKSTAZY")
    SearchTitles.candidates("Ojczyzna (pokaz przedpremierowy)", None) should contain ("Ojczyzna")
    SearchTitles.candidates("Plain Title", None) shouldBe Seq("Plain Title")
  }

  // A banner is joined with a dash as often as a pipe. "500 mil" is the worked
  // example: TMDB's Polish title is exactly "500 mil", so the film should resolve
  // on its title alone — but the whole decorated string was the only candidate, so
  // it fell through to the year-pinned branch instead. Both dash forms occur.
  it should "split a dash-joined programme banner, without touching hyphenated words" in {
    SearchTitles.candidates("Filmoczule Dla Edukacji z Odn i WZiSS Ump – 500 mil", None) should contain ("500 mil")
    SearchTitles.candidates("Ladies Night - Narodziny gwiazdy", None) should contain ("Narodziny gwiazdy")
    SearchTitles.candidates("Spider-Man", None) shouldBe Seq("Spider-Man")
  }

  it should "also draw on the row's other reported titles (cinemaTitles + slot originals), de-decorated" in {
    // Every title the cinemas reported for the row becomes a search candidate.
    SearchTitles.candidates(
      title = "KINO SENIORA | Opętanie", originalTitle = None,
      extraTitles = Seq("Opętanie (pokaz)", "Possession")
    ) should contain allOf ("Opętanie", "Possession")
  }
}
