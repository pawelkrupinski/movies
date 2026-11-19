package services.closure

import services.scrapes.VenueClosure.{FallbackEvidence, SourceDrop, VenueSignal}
import services.scrapes.{OwnSiteEvidence, VenueClosure, VenueWebsites}
import tools.HttpFetch

import scala.util.Try

/** A page the retirement PR's live re-check reads again: the venue's own `website`,
 *  or a second `listing` that dropped it. */
final case class LiveCheck(kind: String, url: String)

/**
 * Asks about the venue ITSELF once its sources have dropped it: what its second
 * listing's answer already says ([[VenueClosure.listingSignal]]), and what its own
 * website answers now ([[OwnSiteEvidence]]), where we know the site.
 */
final class VenueChecks(websites: VenueWebsites, fetch: HttpFetch) {

  def signals(candidate: ClosureCandidate, drop: SourceDrop): Seq[VenueSignal] =
    VenueClosure.listingSignal(drop).toSeq ++
      websites.of(candidate.name).map(url => OwnSiteEvidence.read(url, Try(fetch.get(url))))

  /** The retirement to ask for of `candidate`, closed on `evidence`: none for a venue in no data-driven roster. */
  def retirement(candidate: ClosureCandidate, drop: SourceDrop, evidence: String): Option[RetirementRequest] =
    candidate.rosterEntry.map(RetirementRequest(_, candidate.name, evidence, live(candidate, drop), candidate.page))

  /** The pages the live re-check reads again for `candidate`. */
  private def live(candidate: ClosureCandidate, drop: SourceDrop): Seq[LiveCheck] =
    websites.of(candidate.name).map(LiveCheck("website", _)).toSeq ++ (drop.fallback match {
      case FallbackEvidence.Dropped(_, _, _, error) => VenueChecks.UrlIn.findFirstIn(error).map(LiveCheck("listing", _))
      case _                                        => None
    })
}

object VenueChecks {
  private val UrlIn = """https?://\S+""".r
}
