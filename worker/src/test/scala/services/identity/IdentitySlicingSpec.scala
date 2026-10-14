package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models.{Cinema, Helios, Multikino}
import services.movies.ListingKey

/** What an identity slice cuts out of a whole corpus ([[IdentitySlicing.select]]): the seeds' whole families as the
 *  core, and as context every other listing a core node shares a corpus-context key with. */
class IdentitySlicingSpec extends AnyFlatSpec with Matchers {

  private def key(venue: String, title: String) = ListingKey.Published(venue, title, None, Nil)
  private val seed       = key("A", "Wolf")
  private val sibling    = key("B", "Wolf (OmU)")      // the seed's family, another node
  private val namesake   = key("C", "Wolf Hall")       // another family, sharing a title shape with the seed's node
  private val neighbour  = key("D", "Wolf Hall Live")  // the namesake's family
  private val stranger   = key("E", "Heat")            // nothing in common

  private val index = IdentitySlicing.Index(Nil,
    familyOf = Map(seed -> 1, sibling -> 1, namesake -> 2, neighbour -> 2, stranger -> 3),
    nodeOf = Map(seed -> "n1", sibling -> "n2", namesake -> "n3", neighbour -> "n4", stranger -> "n5"),
    keysOf = Map("n1" -> Set("t:wolf", "s:wolf"), "n2" -> Set("t:wolf", "s:wolf omu"), "n3" -> Set("t:wolf hall", "s:wolf"),
      "n4" -> Set("t:wolf hall live"), "n5" -> Set("t:heat")),
    nodeListings = Map("n1" -> Seq(seed), "n2" -> Seq(sibling), "n3" -> Seq(namesake), "n4" -> Seq(neighbour), "n5" -> Seq(stranger)))

  "A slice" should "resolve the seed's whole family, and read a listing sharing a context key as context only" in {
    IdentitySlicing.select(index, Set(seed)) shouldBe IdentitySlicing.Selection(core = Set(seed, sibling), context = Set(namesake))
  }

  it should "join the context's own families to the core when widened" in {
    val widened = IdentitySlicing.select(index, Set(seed), widen = 1)
    widened.core shouldBe Set(seed, sibling, namesake, neighbour)
    widened.context shouldBe empty
  }

  it should "hold a seed the whole resolve never placed in a family" in {
    val lone = key("F", "Unindexed")
    IdentitySlicing.select(index, Set(lone)).core shouldBe Set(lone)
  }

  private def listing(venue: Cinema, title: String): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, None, Nil), title, title, title, None, Nil, None, None, None)

  "A changed projection's read" should "hand the agreement every listing the intake holds, the re-scraped venues' as republished" in {
    // IdentityListingIntake's Read.Changed keeps each venue it did not read again (IdentityListingIntakeProjectedSpec:
    // "read again, between two reads of the stamps, only the venues it took a scrape of" — the whole corpus, one row
    // read), so tickChanged's agreement reads a cluster's listings at EVERY venue: a read of the re-scraped venues alone
    // digests a cluster as half its listings and decides it again on them, a flip no worker can make.
    val rescraped = listing(Helios, "CBeebies Panto")
    val kept      = listing(Multikino, "CBeebies Panto")
    val read = IdentitySlicing.changedRead(Seq(rescraped, kept), rescraped = Set(Helios.displayName))
    read.listingOf(kept.key) shouldBe Some(kept)
    read.listingOf(rescraped.key) shouldBe Some(rescraped)
    read.republished.map(_.toSet) shouldBe Some(Set(rescraped.key))
  }
}
