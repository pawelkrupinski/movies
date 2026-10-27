package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Who judged a judgement row, read from its evidence: an agent's marker in each spelling agents used, and the user's
 *  own word overriding it. */
class ReviewDigestSpec extends AnyFlatSpec with Matchers {

  "an agent's judgement" should "be read by each spelling of the marker" in {
    ReviewDigest.agentJudged("agent judged 2026-10-22 (prodreg rose): kinoheld credits Aurélie Saada") shouldBe true
    ReviewDigest.agentJudged("judged 10-22 (premiere bound, AGENT-JUDGED, flagged for the user): the venue's Cineamo id") shouldBe true
    ReviewDigest.agentJudged("judged 10-22 (agent, parser-query): NT Live's relay") shouldBe true
    ReviewDigest.agentJudged("judged 10-13: Ichikawa's Enjō (1958)") shouldBe false
  }

  it should "be the user's once the user confirmed or overruled it" in {
    val confirmed = "review page: wrong: Patrick Monahan: Live (2011) (was: wrong: agent judged 2026-10-22: a stand-up night)"
    ReviewDigest.userJudged(confirmed) shouldBe true
    ReviewDigest.agentJudged(confirmed) shouldBe false
    ReviewDigest.userJudged("hand label: the venue's own page") shouldBe true
  }
}
