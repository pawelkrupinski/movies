package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import java.time.LocalDate
import scala.util.Try

/** One-shot: record the own-site responses of the cinemas switched off Filmweb
 *  into the 08-06-2026 whole-corpus fixture set, so the e2e + page snapshots
 *  render their real films. Run:
 *    sbt 'worker/Test/runMain clients.tools.RecordSwitchedToCorpus'
 *  then delete + regenerate the snapshots. today is pinned to the capture date. */
object RecordSwitchedToCorpus {
  def main(args: Array[String]): Unit = {
    val record   = new RecordingHttpFetch("08-06-2026", new RealHttpFetch())
    val today = LocalDate.of(2026, 6, 8)
    def rep(label: String)(n: => Int): Unit =
      println(f"$label%-34s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName}", x => s"$x films")}")
    rep("KinoEcho")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-echo-w-jarocinie-1159", KinoEcho).fetch().size)
    rep("KinoBajkaKluczbork")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-bajka-w-kluczborku-1467", KinoBajkaKluczbork).fetch().size)
    rep("KinoLewart")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-lewart-w-lubartowie-1382", KinoLewart).fetch().size)
    rep("KinoWawel")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-wawel-w-lubaniu-1489", KinoWawel).fetch().size)
    rep("KinoMuzaLubin")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-1375", KinoMuzaLubin).fetch().size)
    rep("KinoFregata")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-kino-fregata-1683", KinoFregata).fetch().size)
    rep("KinoNarie")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-narie-w-moragu-1682", KinoNarie).fetch().size)
    rep("KinoPiastOstrzeszow")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-piast-w-ostrzeszowie-601", KinoPiastOstrzeszow).fetch().size)
    rep("KinoHel")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-hel-dom-kultury-w-pleszewie-1255", KinoHel).fetch().size)
    rep("KinoStaryMlyn")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn).fetch().size)
    rep("KinoPionierZary")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-pionier-w-zarach-1492", KinoPionierZary).fetch().size)
    rep("KinoKDK")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-kutnowskiego-domu-kultury-1474", KinoKDK).fetch().size)
    rep("KinoSokolDabrowaTarnowska")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-sokol-w-dabrowie-tarnowskiej-1303", KinoSokolDabrowaTarnowska).fetch().size)
    rep("KinoWislaBrzeszcze")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-wisla-w-brzeszczach-1539", KinoWislaBrzeszcze).fetch().size)
    rep("KinoKolory")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory).fetch().size)
    rep("KinoPrzedwiosnieKrotoszyn")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn).fetch().size)
    rep("KinoKlaps")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps).fetch().size)
    rep("KinoPCA")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA).fetch().size)
    rep("KinoMDK")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546", KinoMDK).fetch().size)
    rep("KinoMOKCentrum")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-centrum-im-adama-mickiewicza-w-zawierciu-1305", KinoMOKCentrum).fetch().size)
    rep("KinoSokol")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/malopolskie-centrum-kultury-sokol-w-nowym-saczu-1225", KinoSokol).fetch().size)
    rep("KinoNadWarta")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoNadWarta).fetch().size)
    rep("Kino60Krzesel")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel).fetch().size)
    rep("KinoGornik")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-gornik-szydlowiec-1320", KinoGornik).fetch().size)
    rep("KinoJanosik")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-janosik-1500", KinoJanosik).fetch().size)
    rep("KinoBaltyk")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk).fetch().size)
    rep("KinoBaszta")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBaszta).fetch().size)
    rep("KinoCentrumBialogard")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/kino-centrum-w-bialogardzie-1685", KinoCentrumBialogard).fetch().size)
    rep("KinoEuropa")(new Bilety24OrganizerClient(record, "https://www.bilety24.pl/kino/organizator/nowosolski-dom-kultury-1679", KinoEuropa).fetch().size)
    rep("KinoKinomax")(new MsiClient(record, "https://bilety.kinomax.info.pl", KinoKinomax, today).fetch().size)
    rep("KinoMCK")(new MsiClient(record, "https://bilety.kinolezajsk.pl", KinoMCK, today).fetch().size)
    rep("KinoSniezka")(new MsiClient(record, "https://bilety.mokdebica.pl", KinoSniezka, today).fetch().size)
    rep("KinoCinemaLumiere")(new MsiClient(record, "https://bilety.kinoszczytno.pl", KinoCinemaLumiere, today).fetch().size)
    rep("KinoIgnacy")(new MsiClient(record, "https://www.biletyignacy.pl", KinoIgnacy, today).fetch().size)
    rep("KinoBajkaDarlowo")(new MsiClient(record, "https://darlowo.vectorsoft.pl", KinoBajkaDarlowo, today).fetch().size)
    rep("KinoGoplana")(new MsiClient(record, "https://bilety.ckpolczyn.pl", KinoGoplana, today).fetch().size)
    rep("KinoWybrzeze")(new MsiClient(record, "https://bilety.rck.kolobrzeg.pl", KinoWybrzeze, today).fetch().size)
    rep("KinoJutrzenka")(new MsiClient(record, "https://kino.sierpc.pl", KinoJutrzenka, today).fetch().size)
    rep("KinoZaRogiem")(new MsiClient(record, "https://bilety.pokis.pl", KinoZaRogiem, today).fetch().size)
    rep("KinoIkar")(new MsiClient(record, "https://kinoikar.mok-jar.pl", KinoIkar, today).fetch().size)
    rep("KinoSDKSwiebodzin")(new MsiClient(record, "https://bilety.kino.swiebodzin.pl:4433", KinoSDKSwiebodzin, today).fetch().size)
    rep("KinoNaBiegunach")(new MsiClient(record, "https://jaroslaw.kinonabiegunach.pl", KinoNaBiegunach, today).fetch().size)
    rep("KinoKalejdoskop")(new SystemBiletowyClient(record, "https://ckp.systembiletowy.pl", KinoKalejdoskop).fetch().size)
    rep("KinoTur")(new BiletynaClient(record, "https://biletyna.pl/Turek/Kino-Tur", KinoTur).fetch().size)
    rep("KinoMok")(new BiletynaClient(record, "https://biletyna.pl/Zagorow/Gminny-Osrodek-Kultury", KinoMok).fetch().size)
    rep("KinoPDK")(new BiletynaClient(record, "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury", KinoPDK).fetch().size)
    rep("KinoSCK")(new BiletynaClient(record, "https://biletyna.pl/Stargard/Stargardzkie-Centrum-Kultury", KinoSCK).fetch().size)
    rep("KinoSokolSokolka")(new BiletynaClient(record, "https://biletyna.pl/Sokolka/Kino-Sokol", KinoSokolSokolka).fetch().size)
    rep("KinoNaSzekspirowskim")(new BiletynaClient(record, "https://biletyna.pl/Gdansk/Kino-na-Szekspirowskim", KinoNaSzekspirowskim).fetch().size)
    rep("KinoZacisze")(new BiletynaClient(record, "https://biletyna.pl/Piekary-Slaskie/Kino-Zacisze", KinoZacisze).fetch().size)
    rep("KinoMiejskieCentrumKultury")(new BiletynaClient(record, "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury", KinoMiejskieCentrumKultury).fetch().size)
    rep("KinoZdroj")(new BiletynaClient(record, "https://biletyna.pl/Ciechocinek/Kino-Zdroj", KinoZdroj).fetch().size)
    rep("KinoKoneckieCentrumKultury")(new BiletynaClient(record, "https://biletyna.pl/Konskie/Koneckie-Centrum-Kultury-sala-kinowa", KinoKoneckieCentrumKultury).fetch().size)
    rep("KinoTeatrElektryczny")(new BiletynaClient(record, "https://biletyna.pl/Skoczow/Teatr-Elektryczny", KinoTeatrElektryczny).fetch().size)
    rep("KinoDK")(new BiletynaClient(record, "https://biletyna.pl/Slawno/Slawienski-Dom-Kultury", KinoDK).fetch().size)
    rep("KinoZbyszek")(new BiletynaClient(record, "https://biletyna.pl/Dzierzoniow/Kinoteatr-Zbyszek", KinoZbyszek).fetch().size)
    rep("KinoGCK")(new BiletynaClient(record, "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj", KinoGCK).fetch().size)
    rep("KinoZulawskiOsrodekKultury")(new BiletynaClient(record, "https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury", KinoZulawskiOsrodekKultury).fetch().size)
    rep("KinoNawojka")(new BiletynaClient(record, "https://biletyna.pl/Lipno/Kino-Nawojka", KinoNawojka).fetch().size)
    rep("KinoPlaneta")(new MsiClient(record, "https://rezerwacja.planetabrzesko.pl", KinoPlaneta, today, mvcPath = "/Rezerwacja/mvc/pl").fetch().size)
    rep("KinoSwitCzechowiceDziedzice")(new BiletynaClient(record, "https://biletyna.pl/Czechowice-Dziedzice/Kino-Swit", KinoSwitCzechowiceDziedzice).fetch().size)
  }
}
