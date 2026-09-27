package settings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdentityFocusSettingSpec extends AnyFlatSpec with Matchers {

  private def focus(vars: (String, String)*): Option[IdentityFocus] =
    new ProcessConfiguration(tools.Env.of(vars*)).identityFocus

  "KINOWO_IDENTITY_FOCUS" should "name the title words a focused measurement resolves, comma-separated" in {
    focus("KINOWO_IDENTITY_FOCUS" -> "pieśni lasu, Vincent") shouldBe
      Some(IdentityFocus(services.movies.TitleContainment.tokens("pieśni lasu vincent").toSet))
  }

  it should "leave a run unfocused when unset or blank" in {
    focus() shouldBe None
    focus("KINOWO_IDENTITY_FOCUS" -> " , ") shouldBe None
  }
}
