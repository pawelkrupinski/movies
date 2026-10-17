package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The name a title closes on, bracketed, where nobody is credited: a director's, whose films are then walked — never a
 *  note a venue brackets after the title, which names no one. */
class TitleCreditSpec extends AnyFlatSpec with Matchers {

  "a closing bracket" should "name the person it holds" in {
    TitleCredit.bracketedName("ES (ULRICH SCHAMONI)") shouldBe Some("ULRICH SCHAMONI")
    TitleCredit.bracketedName("Der Bockerer (Franz Antel)") shouldBe Some("Franz Antel")
    TitleCredit.bracketedName("Le Mépris (Jean-Luc Godard)") shouldBe Some("Jean-Luc Godard")
    TitleCredit.bracketedName("Paris, Texas (Wim van der Wenders)") shouldBe Some("Wim van der Wenders")
  }

  it should "name no one where it holds a cut, a score, subtitles, a version or a year" in {
    Seq("Blade Runner (Director's Cut)", "Nosferatu (Live Score)", "Metropolis (mit Untertiteln)", "Metropolis (Mit Untertiteln)",
      "Perfect Days (OmU)", "Es (1966)", "Aliens (Special Edition)", "Faust (Live Music)")
      .foreach(title => withClue(title)(TitleCredit.bracketedName(title) shouldBe None))
  }

  "a title's credit" should "be the director its search suggests by the bracket's name whole" in {
    TitleCredit.named("ES (ULRICH SCHAMONI)", Seq("Ulrich Schamoni")) shouldBe Some("Ulrich Schamoni")
    TitleCredit.named("ES (ULRICH SCHAMONI)", Seq("Ulrich Schamoni Jr.")) shouldBe None
    TitleCredit.named("Blade Runner (Director's Cut)", Seq("Director's Cut")) shouldBe None
  }
}
