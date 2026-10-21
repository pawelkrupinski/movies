package services.identity

import models.{Cinema, CinemaMovie}
import services.movies.TitleNormalizer
import services.scrapes.LeanListing

/** How the identity model reads a venue's films — ONE read, wherever they come from: prod's take-up of the scrape
 *  archives (`IdentityListingIntake.identities`: the lean read, [[LeanListing.leanFilm]], done server-side in Mongo),
 *  each scrape the intake hands the model since (`IdentityModelService.venueScraped`), and every recording a measurement
 *  replays (`integration.IdentityShadow.listingsOf`: identity-measure, the takes check, the slices, the captures).
 *
 *  The lean read keeps the screening DAYS of a film billing a stage work alone ([[LeanStageWorks]]), every other film's
 *  season and no day. Recordings were replayed whole until 2026-10-21, so a rule reading what the lean read drops
 *  measured cured and was dead in prod: the festival catalogue namesake, the author credit, the current-release veto
 *  and the title season, 10-15 to 10-21 (17da3656b). Reading every source through here, a rule reading anything the
 *  lean read drops fails the same way in measurement as in prod; [[whole]] is for the guard that names those rules
 *  (`integration.LeanWholeGuard`), never for a measurement. */
object ModelRead {

  /** Every listing of `byCinema` as the model reads it, keys not yet made unique ([[Listing.all]]). */
  def all(byCinema: Iterable[(Cinema, Seq[CinemaMovie])], normalizer: TitleNormalizer): Seq[Listing] = {
    val keepsDays = new LeanListing.ByTitle(LeanStageWorks.billsStageWork)
    Listing.all(byCinema.map { case (cinema, fs) => cinema -> fs.map(LeanListing.leanFilm(_, keepsDays(_))) }, normalizer)
  }

  /** The resolver's listing set of `byCinema` as the model reads it: one per key ([[Listing.corpus]]). */
  def corpus(byCinema: Iterable[(Cinema, Seq[CinemaMovie])], normalizer: TitleNormalizer): Seq[Listing] =
    Listing.distinct(all(byCinema, normalizer))

  /** SECONDARY — `byCinema` read whole, every showtime's day kept: what no source of the model reads. Only the guard
   *  comparing it with [[corpus]] reads it, to name the rules that fire on whole data alone (dead in prod). */
  def whole(byCinema: Iterable[(Cinema, Seq[CinemaMovie])], normalizer: TitleNormalizer): Seq[Listing] =
    Listing.corpus(byCinema, normalizer)
}
