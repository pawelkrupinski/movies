package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonNameSpec extends AnyFlatSpec with Matchers {

  behavior of "PersonName.capitalized"

  it should "title-case a plain all-lowercase name" in {
    PersonName.capitalized("christoph waltz")  shouldBe "Christoph Waltz"
    PersonName.capitalized("sandra bullock")   shouldBe "Sandra Bullock"
    PersonName.capitalized("steve carell")     shouldBe "Steve Carell"
  }

  it should "capitalise each half of a hyphenated name" in {
    PersonName.capitalized("joseph gordon-levitt") shouldBe "Joseph Gordon-Levitt"
    PersonName.capitalized("daniel day-lewis")     shouldBe "Daniel Day-Lewis"
    PersonName.capitalized("jean-luc godard")      shouldBe "Jean-Luc Godard"
  }

  it should "capitalise after a one- or two-letter apostrophe prefix" in {
    PersonName.capitalized("peter o'toole")       shouldBe "Peter O'Toole"
    PersonName.capitalized("vincent d'onofrio")   shouldBe "Vincent D'Onofrio"
    PersonName.capitalized("chiwetel o'sullivan") shouldBe "Chiwetel O'Sullivan"
    PersonName.capitalized("youssou n'dour")      shouldBe "Youssou N'Dour"
  }

  it should "leave a mid-word apostrophe alone rather than invent a capital" in {
    // The prefix is 5 letters, so this is a word with an apostrophe in it, not
    // an elided particle. Safer to under-capitalise than to mangle.
    PersonName.capitalized("keali'i reichel") shouldBe "Keali'i Reichel"
  }

  it should "apply the Mc rule but never a Mac rule" in {
    PersonName.capitalized("matthew mcconaughey") shouldBe "Matthew McConaughey"
    PersonName.capitalized("james mcavoy")        shouldBe "James McAvoy"
    PersonName.capitalized("frances mcdormand")   shouldBe "Frances McDormand"
    // Mac is three ordinary letters — `macy` must never become `MacY`.
    PersonName.capitalized("william h. macy")     shouldBe "William H. Macy"
    PersonName.capitalized("andie macdowell")     shouldBe "Andie Macdowell"
    PersonName.capitalized("gustavo machado")     shouldBe "Gustavo Machado"
  }

  it should "keep a nobiliary particle lowercase in the middle of a name" in {
    PersonName.capitalized("ludwig van beethoven")    shouldBe "Ludwig van Beethoven"
    PersonName.capitalized("robert de niro")          shouldBe "Robert de Niro"
    PersonName.capitalized("alexander von humboldt")  shouldBe "Alexander von Humboldt"
    PersonName.capitalized("lee van cleef")           shouldBe "Lee van Cleef"
    PersonName.capitalized("guillermo del toro")      shouldBe "Guillermo del Toro"
  }

  it should "capitalise a particle that leads the name" in {
    PersonName.capitalized("al pacino")        shouldBe "Al Pacino"
    PersonName.capitalized("van damme")        shouldBe "Van Damme"
    PersonName.capitalized("de niro")          shouldBe "De Niro"
    PersonName.capitalized("della reese")      shouldBe "Della Reese"
  }

  it should "capitalise initials, dotted or spaced" in {
    PersonName.capitalized("samuel l. jackson") shouldBe "Samuel L. Jackson"
    PersonName.capitalized("j.k. simmons")      shouldBe "J.K. Simmons"
    PersonName.capitalized("michael j. fox")    shouldBe "Michael J. Fox"
    PersonName.capitalized("h.g. wells")        shouldBe "H.G. Wells"
  }

  it should "capitalise non-ASCII letters correctly" in {
    PersonName.capitalized("józef piłsudski")  shouldBe "Józef Piłsudski"
    PersonName.capitalized("renée zellweger")  shouldBe "Renée Zellweger"
    PersonName.capitalized("łukasz simlat")    shouldBe "Łukasz Simlat"
    PersonName.capitalized("émile françois")   shouldBe "Émile François"
    PersonName.capitalized("ángela molina")    shouldBe "Ángela Molina"
  }

  it should "pass an already-correct name through byte-identical" in {
    // The most important property: the helper runs over a MIXED corpus, so a
    // name any source already cased must survive untouched.
    val alreadyCorrect = Seq(
      "Christoph Waltz", "Joseph Gordon-Levitt", "Peter O'Toole", "Vincent D'Onofrio",
      "Matthew McConaughey", "William H. Macy", "Andie MacDowell", "Ludwig van Beethoven",
      "Robert De Niro", "Alexander von Humboldt", "Samuel L. Jackson", "J.K. Simmons",
      "Józef Piłsudski", "Renée Zellweger", "Ke Huy Quan", "Daniel Day-Lewis",
      "Lupita Nyong'o", "Sacha Baron Cohen", "Bong Joon-ho", "Ryûsuke Hamaguchi"
    )
    alreadyCorrect.foreach(name => PersonName.capitalized(name) shouldBe name)
  }

  it should "leave an ALL-CAPS name exactly as the source sent it" in {
    // Documented decision: an all-caps token can't be told from a stage name or
    // acronym that is genuinely upper, and no cast source we ingest shouts — so
    // there is nothing to fix and only damage to do.
    PersonName.capitalized("SMITH")         shouldBe "SMITH"
    PersonName.capitalized("JEAN DUPONT")   shouldBe "JEAN DUPONT"
    PersonName.capitalized("RZA")           shouldBe "RZA"
    PersonName.capitalized("JAY-Z")         shouldBe "JAY-Z"
  }

  it should "leave a partly-cased name alone" in {
    // Any uppercase letter at all is evidence the source cased deliberately.
    PersonName.capitalized("ludwig van Beethoven") shouldBe "ludwig van Beethoven"
    PersonName.capitalized("k.D. lang")            shouldBe "k.D. lang"
  }

  it should "handle blank and degenerate input" in {
    PersonName.capitalized("")     shouldBe ""
    PersonName.capitalized(" ")    shouldBe " "
    PersonName.capitalized("123")  shouldBe "123"
    PersonName.capitalized("-")    shouldBe "-"
  }

  it should "preserve the exact whitespace and punctuation of the input" in {
    PersonName.capitalized("  christoph   waltz ") shouldBe "  Christoph   Waltz "
    PersonName.capitalized("waltz, christoph")     shouldBe "Waltz, Christoph"
  }

  behavior of "PersonName.capitalizedAll"

  it should "recase only the lowercase members of a mixed list" in {
    PersonName.capitalizedAll(Seq("christoph waltz", "Sandra Bullock", "SMITH")) shouldBe
      Seq("Christoph Waltz", "Sandra Bullock", "SMITH")
  }

  it should "be idempotent" in {
    val once  = PersonName.capitalizedAll(Seq("christoph waltz", "peter o'toole", "ludwig van beethoven"))
    val twice = PersonName.capitalizedAll(once)
    twice shouldBe once
  }
}
