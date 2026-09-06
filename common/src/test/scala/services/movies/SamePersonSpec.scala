package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The one table of "same person" cases. Every row is a shape a real feed produced;
 * the ones marked with a commit landed in `CinemaCorroboration` one at a time on
 * 2026-09-05, while the search-hit verifier in `MovieService` never learned them.
 * Both now ask here, so a case added for one caller holds for the other.
 */
class SamePersonSpec extends AnyFlatSpec with Matchers {

  private val same: Seq[(String, String)] = Seq(
    "Zhang Yimou"                      -> "Yimou Zhang",                 // token order (MovieServiceSpec)
    "Helgestad"                        -> "Asgeir Helgestad",            // bare surname (MovieServiceSpec)
    "Pedro Almodovar"                  -> "Pedro Almodóvar",             // diacritics
    "Enyedi Ildikó"                    -> "Ildikó Enyedi",               // surname-first
    "Szabó István"                     -> "István Szabó",
    "Neele Vollmar"                    -> "Neele Leana Vollmar",         // extra middle name
    "Alejandro G. Iñárritu"            -> "Alejandro González Iñárritu", // initial
    "Michael Gottlieb"                 -> "Michael Gottli",              // feed truncation
    "Pedro Almodóvar"                  -> "Pedro Almod",
    "Paul Verhoeven"                   -> "Paul Verhoven",               // one-letter miss
    "Fatih Akin"                       -> "Fatih Akın",                  // undecomposed letter
    "Joachim Trier"                    -> "Joachim Trıer",
    "Søren Kragh-Jacobsen"             -> "Soren Kragh-Jacobsen",
    "Đorđe Kadijević"                  -> "Dorde Kadijevic",
    "Rainer Weiß"                      -> "Rainer Weiss",
    "Andrei Tarkowski"                 -> "Andreï Tarkovsky",            // transliteration
    "Alexander Nikolajewitsch Sokurow" -> "Alexandre Sokourov",
    "Kosara Mitik"                     -> "Kosara Mitić",
    "Christian Nyby"                   -> "Christian Niby",
    "Thomas Michael Donnelly"          -> "Tom Donnelly",                // familiar form
    "David G. Derrick Jr."             -> "Dave Derrick Jr.",
    "Amrou Al-Kadhi"                   -> "Amrou Alkadhi",               // joined surname
    "Mathi Maran"                      -> "Pugazhendhi Mathimaran",
    "Loriot"                           -> "Vicco von Bülow",             // DirectorAliases
    "Anthony M. Dawson"                -> "Antonio Margheriti",
    "Lau Kar-leung"                    -> "Liu Chia-Liang",
    "Kukla"                            -> "Katarina Rešek",
    "DK Welchman"                      -> "Dorota Kobiela",
    "Bruce Le"                         -> "Huang Kin-Lung"
  )

  private val different: Seq[(String, String)] = Seq(
    "Christopher Nolan" -> "Steven Spielberg",
    "John Smith"        -> "John Doe",
    "A. Wajda"          -> "Louisa Proske",     // a bare initial is not a match on its own
    "Bong Joon Ho"      -> "Bong Joon Il",      // short tokens stay strict
    "Andrzej Wajda"     -> "Andrzej Żuławski",  // shared first name only
    "Andrzej Wajda"     -> "Louisa Proske",
    "Loriot"            -> "Bruce Le"           // both listed, different groups
  )

  "SamePerson" should "recognise every shape a feed has mangled a credit into" in {
    for ((a, b) <- same) withClue(s"$a / $b: ") {
      SamePerson(a, b) shouldBe true
      SamePerson(b, a) shouldBe true
    }
  }

  it should "keep genuinely different people apart" in {
    for ((a, b) <- different) withClue(s"$a / $b: ") {
      SamePerson(a, b) shouldBe false
      SamePerson(b, a) shouldBe false
    }
  }

  it should "still match two identical credits the fold cannot read" in {
    // A site that prints the director in the original script, against a cinema
    // that prints the same string: the substring tests this replaced said yes.
    SamePerson("王家衛", "王家衛") shouldBe true
    SamePerson("王家衛", " 王家衛 ") shouldBe true
  }

  it should "deny rather than match when a credit folds away to nothing" in {
    // The CALLER that must abstain ("王家衛" is not evidence either way) checks
    // `tokens` for emptiness; the bare answer is false so nothing matches on nothing.
    SamePerson.tokens("王家衛") shouldBe empty
    SamePerson("王家衛", "Wong Kar Wai") shouldBe false
  }

  "withoutMiddleNames" should "drop the middle names of a three-token credit" in {
    SamePerson.withoutMiddleNames("David Kerrick Hand") shouldBe Some("David Hand")
  }

  it should "drop a generational suffix before taking first and last" in {
    SamePerson.withoutMiddleNames("David G. Derrick Jr.") shouldBe Some("David Derrick")
  }

  it should "not shorten a two-name credit, with or without a suffix" in {
    // "Robert Downey Jr." minus its suffix is his father.
    SamePerson.withoutMiddleNames("Robert Downey Jr.") shouldBe None
    SamePerson.withoutMiddleNames("Robert Downey") shouldBe None
  }

  it should "not shorten across a nobiliary particle" in {
    SamePerson.withoutMiddleNames("Lars von Trier") shouldBe None
    SamePerson.withoutMiddleNames("Lars Von Trier") shouldBe None
  }
}
