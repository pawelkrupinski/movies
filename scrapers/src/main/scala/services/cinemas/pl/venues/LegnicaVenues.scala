package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Legnica: see [[PolishVenueRegistry]]. */
object LegnicaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Legnica),
    bilety24Subdomain(KinoPiast, "https://kino-piast.bilety24.pl"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Lubin),
    own(kit => new KinoAurumClient(kit.http, KinoAurum)),
    own(kit => new CyfroweKinoClient(kit.http, KinoCyfroweKino, titles = kit.titles)),
    bilety24("https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-w-boleslawcu-1586", KinoForumBoleslawiec),
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-dolny-slask-1375", KinoMuzaLubin),
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoMOKGlogow, "https://biletyna.pl/Glogow/Miejski-Osrodek-Kultury"),   // Głogów
    biletyna(TeatrGryphius, "https://biletyna.pl/Glogow/Teatr-im-Andreasa-Gryphiusa"),   // Głogów
    multikino("0048", MultikinoGlogow),   // Głogów
    own(kit => new KinoKulturaChojnowClient(kit.http, KinoKulturaChojnow)),   // Chojnów
  )
}
