package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.EnvGatedFeature

/**
 * The worker's Telegram alerters are wired only when their env vars are present, and
 * when the workers moved from Fly to k3s the chat/topic ids were left behind: the pods
 * had `TELEGRAM_BOT_TOKEN` and nothing else, and all three alerters were OFF for weeks
 * with nothing to say so. This pins what [[AlertingWiring.alerters]] reports for exactly
 * that environment, so the gauge and the boot WARN built from it name the gap.
 */
class AlertingWiringSpec extends AnyFlatSpec with Matchers {

  private val tokenOnly = Map("TELEGRAM_BOT_TOKEN" -> "bot:token")

  private val afterTheGitopsFix = tokenOnly ++ Map(
    "KINOWO_FALLBACK_TG_CHAT_ID"     -> "-1003950886618",
    "KINOWO_FALLBACK_TG_TOPIC_ID"    -> "2",
    "KINOWO_FILMWEB_DROP_TG_CHAT_ID" -> "-1003950886618",
    "KINOWO_FILMWEB_DROP_TG_TOPIC_ID" -> "5")

  "AlertingWiring.alerters" should "report every alerter off, naming its missing key, on the env k3s shipped with" in {
    AlertingWiring.alerters(tokenOnly.get, filmwebEnabled = true) shouldBe Seq(
      EnvGatedFeature("filmweb_fallback", Seq("KINOWO_FALLBACK_TG_CHAT_ID")),
      EnvGatedFeature("filmweb_drop", Seq("KINOWO_FILMWEB_DROP_TG_CHAT_ID")),
      EnvGatedFeature("staging_stuck", Seq("KINOWO_STAGING_STUCK_TG_CHAT_ID or KINOWO_FALLBACK_TG_CHAT_ID")))
  }

  it should "report every alerter on once the chat ids are back" in {
    val alerters = AlertingWiring.alerters(afterTheGitopsFix.get, filmwebEnabled = true)
    alerters.map(_.name) shouldBe Seq("filmweb_fallback", "filmweb_drop", "staging_stuck")
    alerters.filterNot(_.enabled) shouldBe empty
  }

  it should "not report the Filmweb alerters at all for a country that has no Filmweb path" in {
    // Off by design there, not by misconfiguration: exporting a 0 would page for nothing.
    AlertingWiring.alerters(afterTheGitopsFix.get, filmwebEnabled = false).map(_.name) shouldBe Seq("staging_stuck")
  }
}
