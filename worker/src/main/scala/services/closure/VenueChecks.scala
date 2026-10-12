package services.closure

import models.Cinema
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

  def signals(cinema: Cinema, drop: SourceDrop): Seq[VenueSignal] =
    VenueClosure.listingSignal(drop).toSeq ++
      websites.of(cinema.displayName).map(url => OwnSiteEvidence.read(url, Try(fetch.get(url))))

  /** The pages the live re-check reads again for `cinema`. */
  def live(cinema: Cinema, drop: SourceDrop): Seq[LiveCheck] =
    websites.of(cinema.displayName).map(LiveCheck("website", _)).toSeq ++ (drop.fallback match {
      case FallbackEvidence.Dropped(_, _, _, error) => VenueChecks.UrlIn.findFirstIn(error).map(LiveCheck("listing", _))
      case _                                        => None
    })
}

object VenueChecks {
  private val UrlIn = """https?://\S+""".r
}
