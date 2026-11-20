package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Opole: see [[PolishVenueRegistry]]. */
object OpoleVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.OpoleKarolinka),
    helios(HeliosVenues.OpoleSolaris),
    ekobilet("opolskielamy", KinoMeduza),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.KedzierzynKozle),
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-aktywnosci-lokalnej-w-kluczborku-kino-bajka-1467", KinoBajkaKluczbork),
    msi(KinoChemik, "https://bilety.mok.com.pl", titlePrefix = Some("Chemik")),
    own(kit => new KinoDianaClient(kit.http, KinoDiana)),
    own(kit => new KdkKrapkowiceClient(kit.http, KinoKrapkowice)),
    own(kit => new KinoStudioClient(kit.http, KinoStudio, kit.today)),
    msi(KinoTwierdza, "https://bilety.mok.com.pl", titlePrefix = Some("TWIERDZA")),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/brzeskie-centrum-kultury-im-i-j-paderewskiego-1672", KinoCentrumBrzeg),   // Brzeg
    systemBiletowy(KinoCinemaN, "https://bilety.kino.nysa.pl"),   // Nysa
  )
}
