package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The tidy-up every scraped title gets before anything reads it.
 *
 * Both halves are drawn from one film. The André Rieu concert broadcast screens
 * at dozens of Polish cinemas and had scattered across FIVE rows, one of them
 * keyed `andrerieuquotniechzyjemaastrichtquot` — the listing published `&quot;`,
 * nothing decoded it, and `strippedKey` drops punctuation but keeps letters, so
 * the entity's bare name welded itself into the identity. Another listing ran the
 * sentence marks flush against the next word, and the display title came out as
 * "Andre rieu.niech żyje maastricht".
 */
class TitleTidySpec extends AnyFlatSpec with Matchers {

  private val pl = TitleNormalizer.forCountry(models.Country.Poland)

  "decodeEntities" should "resolve the entities cinema listings actually publish" in {
    TitleText.decodeEntities("""André Rieu &quot;Niech żyje Maastricht&quot;""") shouldBe
      """André Rieu "Niech żyje Maastricht""""
    TitleText.decodeEntities("Andr&#233; Rieu")   shouldBe "André Rieu"
    TitleText.decodeEntities("Andr&#xE9; Rieu")   shouldBe "André Rieu"
    TitleText.decodeEntities("Kubu&#347; i przyjaciele") shouldBe "Kubuś i przyjaciele"
    TitleText.decodeEntities("Lilo &amp; Stitch") shouldBe "Lilo & Stitch"
  }

  it should "decode &amp; last, so a double-encoded entity resolves one level" in {
    TitleText.decodeEntities("Rieu &amp;quot;Maastricht&amp;quot;") shouldBe
      """Rieu &quot;Maastricht&quot;"""
  }

  it should "leave a title alone when it carries no entity" in {
    val plain = "André Rieu. Niech żyje Maastricht!"
    TitleText.decodeEntities(plain) shouldBe plain
  }

  "the key" should "not weld an entity's name into a film's identity" in {
    val quoted = """André Rieu &quot;Niech żyje Maastricht&quot;"""
    pl.sanitize(quoted) should not include "quot"
    // …and it lands on the same identity as the same film written plainly.
    pl.sanitize(quoted) shouldBe pl.sanitize("André Rieu Niech żyje Maastricht")
  }

  "spaceAfterSentenceMark" should "put back the space a listing dropped" in {
    TitleText.spaceAfterSentenceMark("André Rieu.Niech żyje Maastricht") shouldBe
      "André Rieu. Niech żyje Maastricht"
    TitleText.spaceAfterSentenceMark("…Maastricht!”Retransmisja letniego koncertu") shouldBe
      "…Maastricht!” Retransmisja letniego koncertu"
    TitleText.spaceAfterSentenceMark("Co się zdarzyło?Baby Jane") shouldBe
      "Co się zdarzyło? Baby Jane"
  }

  it should "leave initialisms, decimals and abbreviations untouched" in {
    val unchanged = Seq(
      "S.W.A.T.",                      // single letters either side of each dot
      "Gwiezdne wojny Vol.2",          // a digit follows, not a letter
      "E.T.",
      "Faraon 4K",
      "Wall·E",
      "André Rieu. Niech żyje Maastricht!")   // already spaced
    unchanged.foreach(t => withClue(s"$t\n")(TitleText.spaceAfterSentenceMark(t) shouldBe t))
  }

  "tidy" should "decode before spacing, so a decoded mark still gets its space" in {
    TitleText.tidy("Rieu&#33;Niech żyje Maastricht") shouldBe "Rieu! Niech żyje Maastricht"
  }

  "frontArticle" should "put back at the front the article a catalogue filed at the end" in {
    // German venues' box-office systems name films library-style, as Cineamo serves them (2026-10-12 DE corpus).
    TitleText.frontArticle("Piano Tuner, The") shouldBe "The Piano Tuner"
    TitleText.frontArticle("Perfekte Urlaub, Der") shouldBe "Der Perfekte Urlaub"
    TitleText.frontArticle("Krankheit wie ein Gedicht, Eine") shouldBe "Eine Krankheit wie ein Gedicht"
    TitleText.frontArticle("Bola negra, La") shouldBe "La Bola negra"
    TitleText.frontArticle("Tribute von Panem - Catching Fire (WA: 2026), The") shouldBe
      "The Tribute von Panem - Catching Fire (WA: 2026)"
    TitleText.frontArticle("PIANO TUNER, THE") shouldBe "THE PIANO TUNER"
  }

  it should "leave a title whose last word is no article, or a lower-case one, alone" in {
    val unchanged = Seq("Sorry, Baby", "Crazy, Stupid, Love", "Eins, zwei, drei", "Powiedz mi, a", "The Piano Tuner", "Die Hard")
    unchanged.foreach(t => withClue(s"$t\n")(TitleText.frontArticle(t) shouldBe t))
  }

  "tidy" should "front a filed-away article, so the title reads and keys as the film's own" in {
    TitleText.tidy("Life of Chuck, The") shouldBe "The Life of Chuck"
  }
}
