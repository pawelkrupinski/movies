package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Kielce: see [[PolishVenueRegistry]]. */
object KielceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Kielce),
    multikino("0029", MultikinoKielce),
    // iframe639.biletyna.pl 403s our Fly datacenter IP (Cloudflare) on the
    // per-film /artist/view/id detail pages, so route through the biletyna seam
    // (residential proxy → direct) like every other biletyna venue — else the
    // deferred detail enrichment fetches all 403 and the enrichment bar goes red.
    own(kit => new KinoFenomenClient(kit.biletynaFetch, KinoFenomen)),
    own(kit => new KinoMoskwaClient(kit.http, KinoMoskwa, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458", KinoCK),
    // Końskie's culture centre lists each hall on a biletyna page of its own. The
    // stage hall is mostly cabaret and concerts but does screen the odd film
    // ("Popiełuszko - wolność jest w nas", 2026-10-20), so both halls are ONE
    // cinema, each showtime tagged with its hall.
    biletynaHalls(KinoKoneckieCentrumKultury, Seq(
        "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa"      -> "sala kinowa",
        "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-widowiskowa" -> "sala widowiskowa",
      )),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoZdrojBusko, "https://biletyna.pl/Busko-Zdroj/BSCK"),   // Busko-Zdrój
    biletyna(KinoMuzaWloszczowa, "https://biletyna.pl/Wloszczowa/Dom-Kultury-we-Wloszczowie"),   // Włoszczowa
  )
}
