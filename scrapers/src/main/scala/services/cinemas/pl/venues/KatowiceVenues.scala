package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Katowice: see [[PolishVenueRegistry]]. */
object KatowiceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1065", CinemaCityPunkt44),
    cinemaCity("1079", CinemaCitySilesia),
    multikino("0035", MultikinoKatowice),
    helios(HeliosVenues.Katowice),
    // Silesia Film's art-house trio, all Bilety24-hosted: listing at `/repertuar/`
    // linking per-film `/wydarzenie/?id=N` pages, so they reuse the shared Bilety24Client.
    bilety24("https://www.bilety24.pl/kino/organizator/kino-kosmos-1501", KinoKosmos),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503", KinoSwiatowid),
    bilety24Subdomain(KinoteatrRialto, "https://kinoteatrrialto.bilety24.pl"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    cinemaCity("1062", CinemaCity),
    own(kit => new KinoPatriaClient(kit.http, KinoPatria, kit.today)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoGrajfkaClient(kit.http, KinoGrajfka)),   // Chorzów
    // Kino Frajda (Starochorzowski Dom Kultury) shares its bilety.chck.pl
    // systembiletowy portal with Chorzowskie Centrum Kultury's own events —
    // filmGroups keeps only the "Imprezy SDK" rows, same pattern as
    // KinoBCKBytom below but with this venue's own data-group value.
    systemBiletowy(KinoFrajda, "https://bilety.chck.pl", filmGroup = Some(EventCategory("Imprezy SDK"))),   // Chorzów
  )
}
