package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnvKnobClassifierSpec extends AnyFlatSpec with Matchers {

  "EnvKnobClassifier" should "treat tuning knobs as non-secret" in {
    Seq(
      "KINOWO_ENRICHMENT_TICK_INTERVAL_SECONDS",
      "KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK",
      "KINOWO_SCRAPE_FRESHNESS_MINUTES",
      "KINOWO_BG_CONCURRENCY"
    ).foreach(k => withClue(k)(EnvKnobClassifier.isSecret(k) shouldBe false))
  }

  it should "treat credentials and structural infra as secret" in {
    Seq(
      "ZYTE_API_KEY", "TMDB_API_KEY", "TELEGRAM_BOT_TOKEN", "MONGODB_URI",
      "GOOGLE_CLIENT_SECRET", "KINOWO_PROXY_PASS", "ADMIN_ALLOWLIST"
    ).foreach(k => withClue(k)(EnvKnobClassifier.isSecret(k) shouldBe true))
  }
}
