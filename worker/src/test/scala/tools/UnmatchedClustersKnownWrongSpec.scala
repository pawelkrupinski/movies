package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters.Take

/** The shrink-only tolerance of the WRONG takes no safe fix removes (the judgements' known-wrong rows of scope `capture`,
 *  [[IdentityJudgements.tolerated]]): a wrong take it lists stands, a wrong take it does not list fails, and a billing no
 *  wrong take is any more fails as stale — they only shrink. */
class UnmatchedClustersKnownWrongSpec extends AnyFlatSpec with Matchers {

  private def take(venue: String, raw: String, tmdb: Int) = Take("de", venue, raw, Some(tmdb), None, "Agreed", s"film $tmdb")

  "the known-wrong takes" should "let a listed wrong take stand at every venue billing it, and fail an unlisted one" in {
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"))
    val audit = UnmatchedClusters.auditWrong(Seq(take("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933),
      take("Neues Maxim Kino München", "WHEN PIGS FLY", 149933), take("Lumière", "40 Jahre  Lumière - Die Geburtstagsfeier", 403248)), known)
    audit.fresh.map(_.rawTitle) shouldBe Seq("40 Jahre  Lumière - Die Geburtstagsfeier")
    audit.stale shouldBe empty
  }

  it should "let a listed wrong take stand when its venue re-spaces the billing" in {
    val audit = UnmatchedClusters.auditWrong(Seq(take("Neue Post Lichtspiele", "WHEN  PIGS FLY ", 149933)), Set(("de", "WHEN PIGS FLY", "tmdb:149933")))
    audit.fresh shouldBe empty
    audit.stale shouldBe empty
  }

  it should "name a billing no wrong take is any more as stale, to strike" in {
    // a fix cures it (or takes another wrong film): the row goes, so they only shrink
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"), ("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
    val audit = UnmatchedClusters.auditWrong(Seq(take("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933)), known)
    audit.fresh shouldBe empty
    audit.stale shouldBe Seq(("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
  }
}
