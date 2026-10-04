package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FixtureTestWiring, SuiteConfiguration}

/** An allowlisted listing: the venue (its `displayName`, the wire key) and the title it published. */
final case class AllowedListing(venue: String, listing: String)

object AllowedListing {
  /** The venue of an entry that holds wherever the title is listed — a chain-wide event billing. */
  val AnyVenue = "*"
  def anywhere(listing: String): AllowedListing = AllowedListing(AnyVenue, listing)
}

/**
 * A rule every listing of every corpus is held to — the Polish fixture boot and each country's
 * recorded corpus ([[ListingCorpora]]) — with an allowlist that carries a why per entry and may
 * only shrink.
 *
 * Each corpus is its own pair of tests: one names every listing that breaks the rule, one every
 * allowlist entry whose listing is in that corpus and no longer breaks it. An entry whose listing a
 * corpus does not carry (a later recording, another country) says nothing about it there — the
 * recorded corpora move nightly, and a film that stopped screening is not a fix. A recorded corpus
 * that is not on disk CANCELS its tests with where to get it, never passes them.
 */
abstract class CorpusShapeSpec extends AnyFlatSpec with Matchers with SuiteConfiguration {

  /** What the rule holds a corpus to, as the test name says it ("hold every field to its shape"). */
  protected def rule: String
  /** What `listing` breaks, one line each — empty when it keeps the rule. */
  protected def findings(corpus: ListingCorpus, listing: CorpusListing): Seq[String]
  /** Listing → why it may break the rule. */
  protected def allowlist: Map[AllowedListing, String]
  /** What the failure tells the reader to do. */
  protected def remedy: String

  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026")
    w.bootStartup()
    w
  }

  /** What a corpus's two tests read of it, computed in one pass — the listings themselves are not
   *  kept, so a spec holds five countries' findings rather than five whole corpora. */
  private final class Checked(corpus: ListingCorpus) {
    val name: String = s"${corpus.label} (${corpus.country.code})"
    val size: Int = corpus.listings.size
    val carried: Set[AllowedListing] = corpus.listings.iterator.map(l => AllowedListing(l.cinema.displayName, l.raw)).toSet
    val found: Seq[(AllowedListing, String)] = corpus.listings.flatMap { listing =>
      findings(corpus, listing).map(AllowedListing(listing.cinema.displayName, listing.raw) -> _)
    }.distinct
  }

  /** The allowlist entry `at` falls under, if any — its own venue's, or one for any venue. */
  private def entryFor(at: AllowedListing): Option[AllowedListing] =
    Seq(at, AllowedListing.anywhere(at.listing)).find(allowlist.contains)

  private def register(label: String, corpus: () => Checked): Unit = {
    s"the $label corpus" should rule in {
      val checked = corpus()
      checked.size should be > 100
      val unexplained = checked.found.collect {
        case (at, finding) if entryFor(at).isEmpty => s"${at.venue}: '${at.listing}' $finding"
      }.distinct.sorted
      withClue(s"${checked.name}: $remedy\n${unexplained.mkString("\n")}\n")(unexplained shouldBe empty)
    }
    it should "keep every allowlist entry it carries still breaking the rule (the backlog only shrinks)" in {
      val checked = corpus()
      val carried = checked.carried ++ checked.carried.map(at => AllowedListing.anywhere(at.listing))
      val stale   = (allowlist.keySet & carried) -- checked.found.flatMap(f => entryFor(f._1))
      withClue("Allowlisted but no longer breaking the rule — drop the entry: ")(stale.toSeq.map(_.toString).sorted shouldBe empty)
    }
  }

  private lazy val boot = new Checked(ListingCorpora.fixtureBoot(wiring))
  register(ListingCorpora.FixtureBootLabel, () => boot)

  Country.all.foreach { country =>
    lazy val recorded = ListingCorpora.recordedPath(country, configuration.identityCorpusDirectory.map(_.value))
      .map(path => new Checked(ListingCorpora.recorded(country, path)))
    register(ListingCorpora.recordedLabel(country), () => recorded.getOrElse(cancel(ListingCorpora.absent(country))))
  }
}
