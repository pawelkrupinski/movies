package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Zlocieniec: see [[PolishVenueRegistry]]. */
object ZlocieniecVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/zlocieniecki-osrodek-kultury-1710", KinoMewaZlocieniec),   // Złocieniec
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-w-drawsku-pomorskim-1680", KinoOKDrawsko),   // Drawsko Pomorskie
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoChDKChoszczno, "https://biletyna.pl/Choszczno/Choszczenski-Dom-Kultury"),   // Choszczno
    biletyna(KinoDOKDrawno, "https://biletyna.pl/Drawno/Drawienski-Osrodek-Kultury"),   // Drawno
    biletyna(KinoMGOKRecz, "https://biletyna.pl/Recz/Miejsko-Gminny-Osrodek-Kultury"),   // Recz
    msi(KinoWolnoscSzczecinek, "https://bilety.sapik.pl"),   // Szczecinek
  )
}
