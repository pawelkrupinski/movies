package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Pins the badge vocabulary against the labels PRODUCTION actually holds.
 *
 * Every raw label below was measured on 2026-09-02 by grouping
 * `web_screenings.showtimes.format` across the five country databases — 59
 * distinct tokens in the UK, 17 in the US, 13 in Poland. They are the input a
 * hundred scrapers really produce, which is why they are the input here rather
 * than a list of plausible-looking strings.
 */
class ScreeningTokensSpec extends AnyFlatSpec with Matchers {

  private def one(raw: String): List[String] = ScreeningTokens.canonical(raw)

  // The defect that motivated the whole vocabulary: the UK shipped the same
  // concept under two and three spellings at once, so a visitor filtering or
  // scanning for it saw a different badge on each chain.
  "spellings of one concept" should "collapse onto a single token" in {
    one("Audio Described") shouldBe List("AD")
    one("AD")              shouldBe List("AD")

    one("Subtitled") shouldBe List("SUB")
    one("SUB")       shouldBe List("SUB")
    one("Subbed")    shouldBe List("SUB")

    one("70mm")           shouldBe List("70MM")
    one("70mm Screening") shouldBe List("70MM")
    one("70MM")           shouldBe List("70MM")

    one("4K")           shouldBe List("4K")
    one("4K Screening") shouldBe List("4K")

    one("Open Caps") shouldBe List("OC")
    one("OC")        shouldBe List("OC")
  }

  // Poland's own drift: three clients emit the Polish word, one the abbreviation,
  // one a misspelling of the abbreviation.
  it should "collapse Poland's spelled-out words onto its abbreviations" in {
    one("napisy")     shouldBe List("NAP")
    one("2d")         shouldBe List("2D")
    one("LEKT")       shouldBe List("LEK")
    one("lektor")     shouldBe List("LEK")
    one("oryginalny") shouldBe List("ORG")
  }

  // The three things a badge is for. Each of these was live in production.
  "a screening attribute" should "survive as its vocabulary token" in {
    val formats = Seq("2D", "3D", "IMAX", "4DX", "SCREENX", "LASER", "PLF", "HDR",
                      "ATMOS", "iSense", "EPIC", "INFINITY", "Dolby", "35MM", "16MM")
    formats.foreach(t => withClue(s"$t: ")(one(t) should have size 1))

    // The market version abbreviations pass through untouched — each is the
    // spelling that country's cinemagoers read.
    Seq("NAP", "DUB", "LEK", "ORG", "VO", "VOSE", "VOSI", "DOB", "CAT", "OV", "OmU", "OmeU", "DF")
      .foreach(t => withClue(s"$t: ")(one(t) shouldBe List(t)))

    // Accessibility a visitor seeks out by name.
    one("AD") shouldBe List("AD")
    one("OC") shouldBe List("OC")
  }

  // A source that names the AUDIO LANGUAGE is naming a version — at a UK
  // multiplex "Hindi" is the whole difference between two screenings of one
  // film. Read off the JDK's ISO language names so a new one needs no edit here.
  it should "keep an audio language as its own token" in {
    one("Hindi")      shouldBe List("HINDI")
    one("Telugu")     shouldBe List("TELUGU")
    one("Malayalam")  shouldBe List("MALAYALAM")
    one("japanese")   shouldBe List("JAPANESE")
    one("Lithuanian") shouldBe List("LITHUANIAN")
  }

  // THE 43,225-BADGE BUG. `Wheelchair Accessible` is a property of the venue,
  // true of nearly every screening in it, and it was the single commonest badge
  // in the UK database. The audience/pricing labels beside it are events, not
  // ways of showing a film — the same distinction `FormatTags` already makes
  // when it refuses to strip "premiera" off a title.
  "a label that is not a screening attribute" should "never become a badge" in {
    val dropped = Seq(
      "Wheelchair Accessible", "Recliner",
      "Parent & Baby Club", "Parent & Baby Only", "Baby & Me", "Baby Friendly", "Carers & Babies",
      "Toddler Time", "Toddler Club", "Kids", "Kids Club",
      "Relaxed", "Relaxed Screening", "Sensory Screening", "Dementia Friendly",
      "Silver Cinema", "Silver Screen",
      "£5 Tickets", "Free", "Q&A", "Glasgow Film Club", "Private Theatre Rental")
    dropped.foreach(t => withClue(s"$t: ")(one(t) shouldBe Nil))
  }

  // Passing an unknown label through is exactly how `Glasgow Film Club` became a
  // badge on a showtime, so an unrecognised one is dropped (and logged once).
  it should "drop a label the vocabulary does not know" in {
    one("Totally New Marketing Name") shouldBe Nil
    one("")                           shouldBe Nil
    one("   ")                        shouldBe Nil
  }

  // AMC brands each attribute with its own name and sells two in one label; its
  // recorded fixture is where these spellings come from.
  "a label naming two attributes" should "yield both tokens" in {
    one("IMAX with Laser at AMC") shouldBe List("IMAX", "LASER")
    one("IMAX at AMC")            shouldBe List("IMAX")
    one("Laser at AMC")           shouldBe List("LASER")
    one("Dolby Cinema at AMC")    shouldBe List("DOLBY")
    one("RealD 3D")               shouldBe List("3D")
  }

  "normalize" should "map, drop and dedupe a whole screening's labels in source order" in {
    ScreeningTokens.normalize(
      Seq("IMAX at AMC", "Wheelchair Accessible", "Audio Described", "AD", "RealD 3D")) shouldBe
      List("IMAX", "AD", "3D")

    ScreeningTokens.normalize(Nil)                          shouldBe Nil
    ScreeningTokens.normalize(Seq("Wheelchair Accessible")) shouldBe Nil
  }
}
