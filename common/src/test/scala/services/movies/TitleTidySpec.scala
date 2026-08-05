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
    TitleNormalizer.decodeEntities("""André Rieu &quot;Niech żyje Maastricht&quot;""") shouldBe
      """André Rieu "Niech żyje Maastricht""""
    TitleNormalizer.decodeEntities("Andr&#233; Rieu")   shouldBe "André Rieu"
    TitleNormalizer.decodeEntities("Andr&#xE9; Rieu")   shouldBe "André Rieu"
    TitleNormalizer.decodeEntities("Kubu&#347; i przyjaciele") shouldBe "Kubuś i przyjaciele"
    TitleNormalizer.decodeEntities("Lilo &amp; Stitch") shouldBe "Lilo & Stitch"
  }

  it should "decode &amp; last, so a double-encoded entity resolves one level" in {
    TitleNormalizer.decodeEntities("Rieu &amp;quot;Maastricht&amp;quot;") shouldBe
      """Rieu &quot;Maastricht&quot;"""
  }

  it should "leave a title alone when it carries no entity" in {
    val plain = "André Rieu. Niech żyje Maastricht!"
    TitleNormalizer.decodeEntities(plain) shouldBe plain
  }

  "the key" should "not weld an entity's name into a film's identity" in {
    val quoted = """André Rieu &quot;Niech żyje Maastricht&quot;"""
    pl.sanitize(quoted) should not include "quot"
    // …and it lands on the same identity as the same film written plainly.
    pl.sanitize(quoted) shouldBe pl.sanitize("André Rieu Niech żyje Maastricht")
  }

  "spaceAfterSentenceMark" should "put back the space a listing dropped" in {
    TitleNormalizer.spaceAfterSentenceMark("André Rieu.Niech żyje Maastricht") shouldBe
      "André Rieu. Niech żyje Maastricht"
    TitleNormalizer.spaceAfterSentenceMark("…Maastricht!”Retransmisja letniego koncertu") shouldBe
      "…Maastricht!” Retransmisja letniego koncertu"
    TitleNormalizer.spaceAfterSentenceMark("Co się zdarzyło?Baby Jane") shouldBe
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
    unchanged.foreach(t => withClue(s"$t\n")(TitleNormalizer.spaceAfterSentenceMark(t) shouldBe t))
  }

  "tidy" should "decode before spacing, so a decoded mark still gets its space" in {
    TitleNormalizer.tidy("Rieu&#33;Niech żyje Maastricht") shouldBe "Rieu! Niech żyje Maastricht"
  }
}
