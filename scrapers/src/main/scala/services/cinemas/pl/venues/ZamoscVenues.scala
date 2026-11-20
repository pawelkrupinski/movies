package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Zamosc: see [[PolishVenueRegistry]]. */
object ZamoscVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    own(kit => new KinoStylowyClient(kit.http, KinoStylowy, kit.today)),   // Zamość
    bilety24("https://www.bilety24.pl/kino/organizator/chelmski-dom-kultury-1718", KinoZorzaChelm),   // Chełm
    biletyna(KinoMorskieOko, "https://biletyna.pl/Krasnystaw/Kino-Morskie-Oko"),   // Krasnystaw
    own(kit => new IksorisRepertoireClient(kit.http, IksorisOrigin("https://kinoplon.pl"), KinoPlon, kit.today)),   // Hrubieszów
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoKadrTomaszowLubelskiClient(kit.http, KinoKadrTomaszowLubelski, kit.today)),   // Tomaszów Lubelski
  )
}
