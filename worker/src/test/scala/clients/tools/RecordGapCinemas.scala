package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the art-house / independent "gap" cinemas added for Filmweb parity,
 * each into its own per-cinema fixture dir (replayed by the matching
 * `clients.*` specs). 17 bespoke scrapers + 3 reuses (Kino Wisła via NoveKino,
 * Helios Outlet Park via Helios, Kino CK Lublin via Bilety24). Run:
 *
 *   sbt 'worker/Test/runMain clients.tools.RecordGapCinemas'
 */
object RecordGapCinemas {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def rec(dir: String) = new RecordingHttpFetch(dir, real)
    def report(label: String)(n: => Int): Unit =
      println(f"$label%-28s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName} ${e.getMessage}", x => s"$x films")}")

    // ── Kraków ──
    report("Kino Pod Baranami")(new KinoPodBaranamiClient(rec("kino-pod-baranami"), KinoPodBaranami).fetch().size)
    report("Kino Kijów")(new KinoKijowClient(rec("kino-kijow"), KinoKijow).fetch().size)
    report("Kino Kika")(new KinoKikaClient(rec("kino-kika"), KinoKika).fetch().size)
    report("Kino Agrafka")(new KinoAgrafkaClient(rec("kino-agrafka"), KinoAgrafka).fetch().size)
    report("Kino Paradox")(new KinoParadoxClient(rec("kino-paradox"), KinoParadox).fetch().size)
    // ── Trójmiasto / Szczecin ──
    report("Cinema1 (Gdańsk)")(new MsiClient(rec("cinema1"), "https://bilety.cinemaone.pl", Cinema1Gdansk).fetch().size)
    report("Kino Zamek (Szczecin)")(new KinoZamekClient(rec("kino-zamek"), KinoZamekSzczecin).fetch().size)
    report("Helios Outlet Park")(new HeliosClient(rec("helios-outlet-park"), HeliosNuxt.SzczecinOutletPark).fetch().size)
    // ── Łódź ──
    report("Kinematograf (Łódź)")(new KinematografLodzClient(rec("kinematograf-lodz"), KinematografLodz).fetch().size)
    report("NCKF (Łódź)")(new NckfClient(rec("nckf"), Nckf).fetch().size)
    report("Kino Spójnia (Aleksandrów)")(new KinoSpojniaClient(rec("kino-spojnia"), KinoSpojnia).fetch().size)
    report("Kino Ślęża (Sobótka)")(new KinoSlezaClient(rec("kino-sleza"), KinoSleza).fetch().size)
    report("Cyfrowe Kino (Środa Śl.)")(new CyfroweKinoClient(rec("cyfrowe-kino"), KinoCyfroweKino).fetch().size)
    report("Kino Kuźnica (Suchedniów)")(new SystemBiletowyClient(rec("kino-kuznica"), "https://shd.systembiletowy.pl", KinoKuznica).fetch().size)
    // ── Lublin ──
    report("Kino CK Lublin")(new Bilety24Client(rec("kino-ck-lublin"), "https://ck-lublin.bilety24.pl", KinoCkLublin).fetch().size)
    // ── Częstochowa / Radom ──
    report("OKF Iluzja")(new OkfIluzjaClient(rec("okf-iluzja"), OkfIluzja).fetch().size)
    report("MCSW Elektrownia")(new McswElektrowniaCinemaClient(rec("mcsw-elektrownia"), McswElektrowniaCinema).fetch().size)
    // ── Kielce ──
    report("Kino Fenomen")(new KinoFenomenClient(rec("kino-fenomen"), KinoFenomen).fetch().size)
    report("Kino Moskwa")(new KinoMoskwaClient(rec("kino-moskwa"), KinoMoskwa).fetch().size)
    // ── Toruń / Rzeszów / Gliwice / Zabrze ──
    report("Kino Centrum CSW")(new KinoCentrumCswClient(rec("csw-torun"), KinoCentrumCsw).fetch().size)
    report("Kino za Rogiem Cafe")(new KinoZaRogiemCafeClient(rec("kzr-cafe"), KinoZaRogiemCafe).fetch().size)
    report("Kino Amok")(new KinoAmokClient(rec("kino-amok"), KinoAmok).fetch().size)
    report("Kino Roma")(new KinoRomaClient(rec("kino-roma"), KinoRoma).fetch().size)
    // ── Warszawa ──
    report("Kino Wisła")(new NoveKinoClient(rec("kino-wisla"), "wisla", KinoWisla).fetch().size)
    // ── MSI-platform venues Filmweb silently stopped carrying ──
    report("Kino GOK (Tychowo)")(new MsiClient(rec("kino-gok"), "https://bilety.goktychowo.pl", KinoGOK).fetch().size)
    report("Kino MOK Nowa Ruda")(new MsiClient(rec("kino-mok-nowa-ruda"), "https://bilety.nowaruda.pl", KinoMOKNowaRuda).fetch().size)
    report("Kino Warszawa (Przeworsk)")(new MsiClient(rec("kino-warszawa-przeworsk"), "https://bilety-kino.przeworsk.um.gov.pl", KinoWarszawa).fetch().size)
    report("Kino Powiśle (Sztum)")(new MsiClient(rec("kino-powisle-sztum"), "https://kinosztumbilety.pl", KinoPowisle).fetch().size)
  }
}
