package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Ciechanow: see [[PolishVenueRegistry]]. */
object CiechanowVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/powiatowe-centrum-kultury-i-sztuki-im-marii-konopnickiej-1307", KinoLydynia),   // Ciechanów
    biletyna(KinoKosmosMlawa, "https://biletyna.pl/Mlawa/Miejski-Dom-Kultury-kino-Kosmos"),   // Mława
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-im-stanislawa-ostoi-kotkowskiego-w-przasnyszu-1482", KinoSwiatowidPrzasnysz),   // Przasnysz
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-makowie-mazowieckim-1485", KinoMDKMakow),   // Maków Mazowiecki
    biletyna(KinoApolloDzialdowo, "https://biletyna.pl/Dzialdowo/Miejski-Dom-Kultury"),   // Działdowo
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoNOKNasielsk, "https://biletyna.pl/Nasielsk/Nasielski-Osrodek-Kultury"),   // Nasielsk
    ekobilet("kinoton", KinoTon),   // Żuromin
  )
}
