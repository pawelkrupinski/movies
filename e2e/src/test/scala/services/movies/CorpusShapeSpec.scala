package services.movies

import tools.SpecClock.given

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{ConvergenceDiagnostics, FixtureTestWiring, SuiteConfiguration}

/** An allowlisted listing: the venue (its `displayName`, the wire key) and the title it published. */
final case class AllowedListing private (venue: String, listing: String)

object AllowedListing {
  /** An entry for `listing` at `venue`, both single-spaced ([[tools.UnmatchedClusters.singleSpaced]]): a venue that
   *  re-spaces a billing between recordings bills the same listing. */
  def apply(venue: String, listing: String): AllowedListing =
    new AllowedListing(tools.UnmatchedClusters.singleSpaced(venue), tools.UnmatchedClusters.singleSpaced(listing))

  /** The venue of an entry that holds wherever the title is listed — a chain-wide event billing. */
  val AnyVenue = "*"
  def anywhere(listing: String): AllowedListing = AllowedListing(AnyVenue, listing)

  /** The entries a corpus proves stale: allowlisted, carried by the corpus, no longer breaking the rule — less
   *  those awaiting a re-record, which a fresh recording cures by design. */
  def stale(allowlist: Set[AllowedListing], carried: Set[AllowedListing], stillBreaking: Set[AllowedListing],
            awaitingReRecord: Set[AllowedListing]): Set[AllowedListing] =
    (allowlist & carried) -- stillBreaking -- awaitingReRecord
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
abstract class CorpusShapeSpec extends AnyFlatSpec with Matchers with SuiteConfiguration with StopsItsWirings {

  /** What the rule holds a corpus to, as the test name says it ("hold every field to its shape"). */
  protected def rule: String
  /** What `listing` breaks, one line each — empty when it keeps the rule. */
  protected def findings(corpus: ListingCorpus, listing: CorpusListing): Seq[String]
  /** Listing → why it may break the rule. */
  protected def allowlist: Map[AllowedListing, String]
  /** What the failure tells the reader to do. */
  protected def remedy: String
  /** Allowlisted listings whose breakage is in a corpus recorded BEFORE the parser fix that cures it: a fresh
   *  recording stops carrying the bad value, which is the fix arriving, not a stale entry — so these are reported,
   *  not failed, once a corpus no longer breaks them (drop them by hand when every corpus is re-recorded). Every
   *  one must also be an allowlist entry. */
  protected def awaitingReRecord: Set[AllowedListing] = Set.empty

  private lazy val wiring: FixtureTestWiring = {
    val w = stoppedAfterAll(new FixtureTestWiring("08-06-2026"))
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
    /** The findings no allowlist entry explains. */
    val unexplained: Seq[(AllowedListing, String)] =
      found.filter { case (at, _) => entryFor(at).isEmpty }.sortBy { case (at, finding) => (at.venue, at.listing, finding) }
    private val explained       = found.flatMap { case (at, _) => entryFor(at) }.toSet
    private val carriedAnywhere = carried ++ carried.map(at => AllowedListing.anywhere(at.listing))
    /** Allowlisted, carried, re-recorded past their fix: reported, never failed. */
    val cured: Set[AllowedListing] = ((allowlist.keySet & carriedAnywhere) -- explained) & awaitingReRecord
    /** Allowlisted, carried, no longer breaking the rule: the entry is to drop. */
    val stale: Seq[AllowedListing] =
      AllowedListing.stale(allowlist.keySet, carriedAnywhere, explained, awaitingReRecord).toSeq.sortBy(_.toString)

    // Both lists as the leg's diagnostics, one `finding|stale <venue> <listing> <what>` line each, tab-separated: what the
    // identity takes check diffs base against head (scripts/ci/corpus_shape_diff.py), so a hermetic leg fails only on a
    // finding the CODE made, and what the Identity findings report reads off a recording.
    ConvergenceDiagnostics.write(s"corpus-shape-${corpus.label.replace(':', '-').replace(' ', '-')}-$specName",
      unexplained.map { case (at, finding) => s"finding\t${at.venue}\t${at.listing}\t$finding" } ++
        stale.map(at => s"stale\t${at.venue}\t${at.listing}\tallowlisted but no longer breaking the rule"))
  }

  /** The spec's name in its findings' file: `SearchQueryMarkers`, `CinemaSlotInvariants`. */
  private def specName: String = getClass.getSimpleName.stripSuffix("Spec")

  /** The allowlist entry `at` falls under, if any — its own venue's, or one for any venue. */
  private def entryFor(at: AllowedListing): Option[AllowedListing] =
    Seq(at, AllowedListing.anywhere(at.listing)).find(allowlist.contains)

  private def register(label: String, corpus: () => Checked): Unit = {
    s"the $label corpus" should rule in {
      val checked = corpus()
      checked.size should be > 100
      val unexplained = checked.unexplained.map { case (at, finding) => s"${at.venue}: '${at.listing}' $finding" }
      withClue(s"${checked.name}: $remedy\n${unexplained.mkString("\n")}\n")(unexplained shouldBe empty)
    }
    it should "keep every allowlist entry it carries still breaking the rule (the backlog only shrinks)" in {
      val checked = corpus()
      if (checked.cured.nonEmpty)
        info(s"${checked.name}: re-recorded past their fix (drop once every corpus is): ${checked.cured.toSeq.map(_.toString).sorted.mkString(", ")}")
      withClue("Allowlisted but no longer breaking the rule — drop the entry: ")(checked.stale.map(_.toString) shouldBe empty)
    }
  }

  "the allowlist" should "hold every listing awaiting a re-record" in {
    (awaitingReRecord -- allowlist.keySet).toSeq.map(_.toString).sorted shouldBe empty
  }

  private lazy val boot = new Checked(ListingCorpora.fixtureBoot(wiring))
  register(ListingCorpora.FixtureBootLabel, () => boot)

  Country.all.foreach { country =>
    lazy val recorded = ListingCorpora.recordedPath(country, configuration.identityCorpusDirectory.map(_.value), configuration.identityCache.map(_.value))
      .map(path => new Checked(ListingCorpora.recorded(country, path)))
    register(ListingCorpora.recordedLabel(country), () => recorded.getOrElse(cancel(ListingCorpora.absent(country))))
  }
}
