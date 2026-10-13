package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.UnmatchedClusters.Take

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The shrink-only baseline of the WRONG takes no safe fix removes (`known-wrong.tsv`): a wrong take it lists stands, a
 *  wrong take it does not list fails, and a line no wrong take is any more fails as stale — the list only shrinks. */
class UnmatchedClustersKnownWrongSpec extends AnyFlatSpec with Matchers {

  private def take(venue: String, raw: String, tmdb: Int) = Take("de", venue, raw, Some(tmdb), None, "Agreed", s"film $tmdb")

  "the known-wrong baseline" should "let a listed wrong take stand at every venue billing it, and fail an unlisted one" in {
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"))
    val audit = UnmatchedClusters.auditWrong(Seq(take("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933),
      take("Neues Maxim Kino München", "WHEN PIGS FLY", 149933), take("Lumière", "40 Jahre  Lumière - Die Geburtstagsfeier", 403248)), known)
    audit.fresh.map(_.rawTitle) shouldBe Seq("40 Jahre  Lumière - Die Geburtstagsfeier")
    audit.stale shouldBe empty
  }

  it should "name a line no wrong take is any more as stale, to strike" in {
    // a fix cures it (or takes another wrong film): the line goes, so the list only shrinks
    val known = Set(("de", "WHEN PIGS FLY", "tmdb:149933"), ("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
    val audit = UnmatchedClusters.auditWrong(Seq(take("Neue Post Lichtspiele", "WHEN PIGS FLY", 149933)), known)
    audit.fresh shouldBe empty
    audit.stale shouldBe Seq(("de", "Begegnungen der Seele 2", "imdb:tt37057962"))
  }

  it should "read the baseline as the suspect-take audit's: country, raw title, film, title, reason — and none when absent" in {
    val file = Files.createTempFile("known-wrong", ".tsv")
    Files.writeString(file, "country\trawTitle\tfilm\ttitle\treasons\nde\tWHEN PIGS FLY\ttmdb:149933\tWhen Pigs Fly (1993)\tera: no fact the venue states\n",
      StandardCharsets.UTF_8)
    UnmatchedClusters.readKnownWrong(file) shouldBe Set(("de", "WHEN PIGS FLY", "tmdb:149933"))
    Files.delete(file)
    UnmatchedClusters.readKnownWrong(file) shouldBe empty
  }
}
