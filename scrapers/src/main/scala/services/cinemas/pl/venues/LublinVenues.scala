package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Lublin: see [[PolishVenueRegistry]]. */
object LublinVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1094", CinemaCityLublinFelicity),
    cinemaCity("1084", CinemaCityLublinPlaza),
    multikino("0034", MultikinoLublin),
    own(kit => new KinoBajkaClient(kit.http, KinoBajka, titles = kit.titles)),
    bilety24Subdomain(KinoCkLublin, "https://ck-lublin.bilety24.pl"),
    own(kit => new KinoChatkaZakaClient(kit.http, KinoChatkaZaka)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/lubartowski-osrodek-kultury-1382", KinoLewart),
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-promocji-i-turystyki-w-poniatowej-1220", KinoCzyn),   // Poniatowa
    ekobilet("kino-milenium", KinoMilenium),   // Milejów
    // MOK Świdnik's own page embeds a biletyna.pl "widget" iframe (ifid=22) —
    // an UNRELATED biletyna integration from Kino Lot Jelenia Góra, which is a
    // bilety24.pl subdomain instead. Distinct client because it's biletyna's
    // HTML widget template, not the JSON-LD place page BiletynaClient parses.
    own(kit => new KinoLotSwidnikClient(kit.http, KinoLotSwidnik)),   // Świdnik
  )
}
