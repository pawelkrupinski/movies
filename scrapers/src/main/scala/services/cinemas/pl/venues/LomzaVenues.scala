package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Lomza: see [[PolishVenueRegistry]]. */
object LomzaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Lomza),   // Łomża
    bilety24("https://www.bilety24.pl/kino/organizator/ostroleckie-centrum-kultury-1068", KinoJantar),   // Ostrołęka
    own(kit => new ModernEventsCalendarClient(kit.http, ModernEventsCalendarPage("https://kino.mokzambrow.pl/"), KinoCKZambrow, kit.today)),   // Zambrów
    msi(KinoOstrovia, "https://bilety.kinoostrovia.pl"),   // Ostrów Mazowiecka
    own(kit => new KinoWarsClient(kit.http, KinoWars)),   // Wysokie Mazowieckie
    biletyna(KinoDomKulturyGrajewo, "https://biletyna.pl/Grajewo/Grajewskie-Centrum-Kultury"),   // Grajewo
  )
}
