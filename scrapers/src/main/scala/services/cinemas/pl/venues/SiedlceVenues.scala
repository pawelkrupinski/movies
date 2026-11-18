package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Siedlce: see [[PolishVenueRegistry]]. */
object SiedlceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Siedlce),   // Siedlce
    noveKino("siedlce", NoveKinoSiedlce),   // Siedlce
    filmweb(2073, KinoSokolSokolowPodlaski),   // Sokołów Podlaski
    // One screen Filmweb split across two ids (1658 "Kino Sława", 1850 "Kino za
    // Rogiem", the room's older name), sharing slots; biletyna sells all of it.
    biletyna(KinoSlawa, "https://biletyna.pl/Miedzyrzec-Podlaski/Kino-Slawa"),   // Międzyrzec Podlaski
    biletyna(KinoKongres, "https://biletyna.pl/Wegrow/Wegrowski-Osrodek-Kultury"),   // Węgrów
    own(kit => new KinoLukowClient(kit.http, KinoLukow)),   // Łuków
    cinemaCity("1100", CinemaCityBialaPodlaska),   // Biała Podlaska
    systemBiletowy(KinoWilga, "https://grl.systembiletowy.pl"),   // Garwolin
  )
}
