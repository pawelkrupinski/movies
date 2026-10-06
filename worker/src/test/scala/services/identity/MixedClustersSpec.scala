package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer
import tools.UnmatchedClusters

/**
 * The resolver over the unmatched clusters' fixture's listings and recorded answers (`UnmatchedClusters`): a title joins
 * listings of different films, and their own facts must keep them apart (docs/design/identity-resolver.md §20.10).
 */
class MixedClustersSpec extends AnyFlatSpec with Matchers {

  private lazy val pl = UnmatchedClusters.read(UnmatchedClusters.fixturePath(models.Country.Poland))

  /** The resolver over the capture's listings `titled`, answered by the capture. */
  private def resolved(capture: UnmatchedClusters.Capture)(titled: Listing => Boolean): (Seq[Listing], Resolution) = {
    val listings = capture.listings.filter(titled)
    listings -> IdentityResolver.resolve(listings, new UnmatchedClusters.Replay(capture), TitleNormalizer.forCountry(capture.country))
  }

  "PL \"Dyrygent\"" should "keep Patria's Provazník apart from Kino Marzenie's Wajda" in {
    // Patria's page credits Ondřej Provazník (106′: his 2025 "Broken Voices"); Kino Marzenie's "DYRYGENT - POŁĄCZONY Z
    // KONCERTEM MUZYKI NA ŻYWO" credits Andrzej Wajda (1980, 97′: his "Dyrygent"); Kozienice bills the title alone. The
    // title and its segment joined all three in the capture, whose corpus recorded Patria's listing before its scraper read
    // the director: on the facts the listings publish now, the learned listing-listing cannot-link keeps them apart and
    // each part takes its own director's film. A family resolves on its own, so these three alone are the whole corpus's.
    val (listings, r) = resolved(pl)(_.rawTitle.toLowerCase.startsWith("dyrygent"))
    val Seq(patria)   = listings.filter(_.venue == "Patria")
    val Seq(marzenie) = listings.filter(_.venue == "Kino Marzenie")
    val Seq(kozienice) = listings.filter(_.venue == "Kozienicki Dom Kultury")
    withClue(listings.map(l => s"${l.venue}: ${r.decisionOf(l.key).render}").mkString("\n")) {
      listings.map(_.venue).toSet shouldBe Set("Patria", "Kozienicki Dom Kultury", "Kino Marzenie")
      (r.decisionOf(patria.key) eq r.decisionOf(marzenie.key)) shouldBe false
      r.decisionOf(patria.key).film shouldBe Some(1483477)
      r.decisionOf(marzenie.key).film shouldBe Some(95269)
      // the bare title joins the listing whose facts it does not contradict: Filmweb's Kozienice programme links Broken Voices
      r.decisionOf(kozienice.key) shouldBe r.decisionOf(patria.key)
    }
  }
}
