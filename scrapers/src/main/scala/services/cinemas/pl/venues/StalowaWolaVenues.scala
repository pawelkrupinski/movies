package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Stalowa Wola: see [[PolishVenueRegistry]]. */
object StalowaWolaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.StalowaWola),   // Stalowa Wola
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-stalowa-wola-1419", KinoWrzos),   // Stalowa Wola
    own(kit => new ModernEventsCalendarClient(kit.http, ModernEventsCalendarPage("https://nck.nisko.pl/kino-sokol/repertuar/"), KinoSokolNisko, kit.today)),   // Nisko
    own(kit => new KinoMetalowiecNowaDebaClient(kit.http, KinoMetalowiecNowaDeba, kit.today)),   // Nowa Dęba
    own(kit => new IksorisBookingClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilet.bck.lbl.pl")), KinoBCK)),   // Biłgoraj
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoJOKClient(kit.http, KinoJOK, kit.today)),   // Janów Lubelski
    systemBiletowy(KinoCKiBNowaSarzyna, "https://oks.systembiletowy.pl"),   // Nowa Sarzyna
  )
}
