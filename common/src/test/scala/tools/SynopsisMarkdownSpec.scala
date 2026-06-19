package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SynopsisMarkdownSpec extends AnyFlatSpec with Matchers {

  "toInlineHtml" should "render **bold** and *italic* as <strong>/<em>" in {
    SynopsisMarkdown.toInlineHtml("Zwykły **pogrubiony** i *kursywa* tekst.") shouldBe
      "Zwykły <strong>pogrubiony</strong> i <em>kursywa</em> tekst."
  }

  it should "render ***bold+italic*** as nested <strong><em>" in {
    SynopsisMarkdown.toInlineHtml("To jest ***ważne***.") shouldBe "To jest <strong><em>ważne</em></strong>."
  }

  it should "escape HTML in the prose before introducing its own tags" in {
    // A literal < in the synopsis must not become a live tag; only our markers do.
    SynopsisMarkdown.toInlineHtml("a < b & **c**") shouldBe "a &lt; b &amp; <strong>c</strong>"
  }

  it should "preserve newlines (the .synopsis pre-wrap renders them)" in {
    SynopsisMarkdown.toInlineHtml("Akapit *raz*.\n\nAkapit dwa.") shouldBe
      "Akapit <em>raz</em>.\n\nAkapit dwa."
  }

  it should "leave an unpaired marker as literal text" in {
    SynopsisMarkdown.toInlineHtml("3 * 4 = 12") shouldBe "3 * 4 = 12"
  }

  "strip" should "remove emphasis markers, keeping the words and newlines" in {
    SynopsisMarkdown.strip("**Pierwszy** akapit z *kursywą*.\n\nDrugi.") shouldBe
      "Pierwszy akapit z kursywą.\n\nDrugi."
  }

  it should "strip ***bold+italic*** to plain" in {
    SynopsisMarkdown.strip("To jest ***ważne***.") shouldBe "To jest ważne."
  }

  "sanitize" should "keep well-formed inline emphasis untouched" in {
    SynopsisMarkdown.sanitize("Zwykły **pogrubiony** i *kursywa* tekst.") shouldBe
      "Zwykły **pogrubiony** i *kursywa* tekst."
  }

  it should "strip a stray half-marker left by malformed source HTML" in {
    // The valid `**Patronują…3.**` survives; the dangling `**` before it goes.
    SynopsisMarkdown.sanitize("Więcej informacji: **\n\n**Patronują nam: TVP 3.**") shouldBe
      "Więcej informacji:\n\n**Patronują nam: TVP 3.**"
  }

  it should "strip emphasis that straddles a line break (can't render)" in {
    SynopsisMarkdown.sanitize("**Linia jeden\nLinia dwa**") shouldBe "Linia jeden\nLinia dwa"
  }

  it should "not let the italic pass split a bold run" in {
    SynopsisMarkdown.sanitize("**bold** zwykły") shouldBe "**bold** zwykły"
  }

  it should "cap blank-line runs and trim" in {
    SynopsisMarkdown.sanitize("  Akapit.\n\n\n\nDrugi.  ") shouldBe "Akapit.\n\nDrugi."
  }
}
