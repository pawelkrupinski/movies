package settings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdentityFocusSettingSpec extends AnyFlatSpec with Matchers {

  private def focus(vars: (String, String)*): Option[IdentityFocus] =
    new ProcessConfiguration(tools.Env.of(vars*)).identityFocus

  "KINOWO_IDENTITY_FOCUS" should "name comma-separated title phrases a focused measurement resolves" in {
    val tokens = (s: String) => services.movies.TitleContainment.tokens(s).toSet
    focus("KINOWO_IDENTITY_FOCUS" -> "pieśni lasu, Vincent") shouldBe Some(IdentityFocus(Seq(tokens("pieśni lasu"), tokens("vincent"))))
  }

  it should "cover a title only when it holds every word of one phrase" in {
    val f      = focus("KINOWO_IDENTITY_FOCUS" -> "dark city").get
    val tokens = (s: String) => services.movies.TitleContainment.tokens(s).toSet
    f.covers(tokens("Dark City: Director's Cut")) shouldBe true
    f.covers(tokens("Dancer in the Dark")) shouldBe false
    f.covers(tokens("Asteroid City")) shouldBe false
  }

  it should "leave a run unfocused when unset or blank" in {
    focus() shouldBe None
    focus("KINOWO_IDENTITY_FOCUS" -> " , ") shouldBe None
  }

  it should "resolve the whole corpus and show only the focus, unless asked to resolve the focus alone" in {
    // Resolved alone, UK "Lalka (The Doll)" matched; in the full run its family vetoed it: only the
    // whole resolve answers as the measurement does.
    focus("KINOWO_IDENTITY_FOCUS" -> "lalka").map(_.alone) shouldBe Some(false)
    focus("KINOWO_IDENTITY_FOCUS" -> "lalka", "KINOWO_IDENTITY_FOCUS_ALONE" -> "true").map(_.alone) shouldBe Some(true)
  }
}
