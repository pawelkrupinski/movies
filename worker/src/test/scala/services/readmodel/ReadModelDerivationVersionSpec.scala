package services.readmodel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.ReadModelSnapshot

/**
 * What keeps [[ReadModelProjection.DerivationVersion]] honest: it must be the fingerprint of the
 * checked-in read-model snapshot, the projected output of the whole fixture corpus.
 *
 * A change to what the projection derives from a row — a field, a pick among sources, an order —
 * moves that snapshot (`FilmScheduleEndToEndSpec` fails until it is regenerated), and this spec
 * then fails until the version is moved with it. So no one has to judge whether their change
 * "counts" as a derivation change, and none can ship without the worker re-projecting its stored
 * corpus once on boot. A snapshot that moved for another reason (a scraper, the enrichment)
 * costs one paced pass that writes little: the price of never missing one.
 */
class ReadModelDerivationVersionSpec extends AnyFlatSpec with Matchers {

  "the derivation version" should "be the fingerprint of the checked-in read-model snapshot" in {
    val fingerprint = ReadModelDerivationVersionSpec.fingerprint(ReadModelSnapshot.read())
    withClue(
      "The checked-in read-model snapshot has moved, so what ReadModelProjection derives may have too. Set\n" +
      s"  ReadModelProjection.DerivationVersion = \"$fingerprint\"\n" +
      "so every worker re-projects its stored corpus once after the deploy (see ReadModelDerivationMarker).\n") {
      ReadModelProjection.DerivationVersion shouldBe fingerprint
    }
  }
}

object ReadModelDerivationVersionSpec {
  /** SHA-256 over the snapshot as `orderIndependent` renders it, so a regeneration that only
   *  re-minted film ids (they follow the scrape's arrival order) does not count as a move. */
  def fingerprint(snapshotJson: String): String = {
    val canonical = ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(ReadModelSnapshot.parse(snapshotJson)))
    java.security.MessageDigest.getInstance("SHA-256")
      .digest(canonical.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      .take(8).map(b => f"${b & 0xff}%02x").mkString
  }
}
