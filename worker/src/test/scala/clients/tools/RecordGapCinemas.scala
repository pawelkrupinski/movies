package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the art-house / independent "gap" cinemas added for Filmweb parity,
 * each into its own per-cinema fixture directory (replayed by the matching
 * `clients.*` specs). 17 bespoke scrapers + 3 reuses (Kino Wisła via NoveKino,
 * Helios Outlet Park via Helios, Kino CK Lublin via Bilety24). Run:
 *
 *   sbt 'worker/Test/runMain clients.tools.RecordGapCinemas'
 */
object RecordGapCinemas {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def record(directory: String) = new RecordingHttpFetch(directory, real)
    def report(label: String)(n: => Int): Unit =
      println(f"$label%-28s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName} ${e.getMessage}", x => s"$x films")}")

    // ── Kraków ──
    report("Kino Pod Baranami")(new KinoPodBaranamiClient(record("kino-pod-baranami"), KinoPodBaranami).fetch().size)
    report("Kino Kijów")(new KinoKijowClient(record("kino-kijow"), KinoKijow).fetch().size)
    report("Kino Kika")(new KinoKikaClient(record("kino-kika"), KinoKika).fetch().size)
    report("Kino Agrafka")(new KinoAgrafkaClient(record("kino-agrafka"), KinoAgrafka).fetch().size)
    report("Kino Paradox")(new KinoParadoxClient(record("kino-paradox"), KinoParadox).fetch().size)
    // ── Trójmiasto / Szczecin ──
    report("Cinema1 (Gdańsk)")(new MsiClient(record("cinema1"), "https://bilety.cinemaone.pl", Cinema1Gdansk).fetch().size)
    report("Kino Zamek (Szczecin)")(new KinoZamekClient(record("kino-zamek"), KinoZamekSzczecin).fetch().size)
    report("Helios Outlet Park")(new HeliosClient(record("helios-outlet-park"), HeliosNuxt.SzczecinOutletPark).fetch().size)
    // ── Łódź ──
    report("Kinematograf (Łódź)")(new KinematografLodzClient(record("kinematograf-lodz"), KinematografLodz).fetch().size)
    report("NCKF (Łódź)")(new NckfClient(record("nckf"), Nckf).fetch().size)
    report("Kino Spójnia (Aleksandrów)")(new KinoSpojniaClient(record("kino-spojnia"), KinoSpojnia).fetch().size)
    report("Kino Ślęża (Sobótka)")(new KinoSlezaClient(record("kino-sleza"), KinoSleza).fetch().size)
    report("Cyfrowe Kino (Środa Śl.)")(new CyfroweKinoClient(record("cyfrowe-kino"), KinoCyfroweKino).fetch().size)
    report("Kino Kuźnica (Suchedniów)")(new SystemBiletowyClient(record("kino-kuznica"), "https://shd.systembiletowy.pl", KinoKuznica).fetch().size)
    // ── Lublin ──
    report("Kino CK Lublin")(new Bilety24Client(record("kino-ck-lublin"), "https://ck-lublin.bilety24.pl", KinoCkLublin).fetch().size)
    // ── Częstochowa / Radom ──
    report("OKF Iluzja")(new OkfIluzjaClient(record("okf-iluzja"), OkfIluzja).fetch().size)
    report("MCSW Elektrownia")(new McswElektrowniaCinemaClient(record("mcsw-elektrownia"), McswElektrowniaCinema).fetch().size)
    // ── Kielce ──
    report("Kino Fenomen")(new KinoFenomenClient(record("kino-fenomen"), KinoFenomen).fetch().size)
    report("Kino Moskwa")(new KinoMoskwaClient(record("kino-moskwa"), KinoMoskwa).fetch().size)
    // ── Toruń / Rzeszów / Gliwice / Zabrze ──
    report("Kino Centrum CSW")(new KinoCentrumCswClient(record("csw-torun"), KinoCentrumCsw).fetch().size)
    report("Kino za Rogiem Cafe")(new KinoZaRogiemCafeClient(record("kzr-cafe"), KinoZaRogiemCafe).fetch().size)
    report("Kino Amok")(new KinoAmokClient(record("kino-amok"), KinoAmok).fetch().size)
    report("Kino Roma")(new KinoRomaClient(record("kino-roma"), KinoRoma).fetch().size)
    // ── Warszawa ──
    report("Kino Wisła")(new NoveKinoClient(record("kino-wisla"), "wisla", KinoWisla).fetch().size)
    // ── MSI-platform venues Filmweb silently stopped carrying ──
    report("Kino GOK (Tychowo)")(new MsiClient(record("kino-gok"), "https://bilety.goktychowo.pl", KinoGOK).fetch().size)
    report("Kino MOK Nowa Ruda")(new MsiClient(record("kino-mok-nowa-ruda"), "https://bilety.nowaruda.pl", KinoMOKNowaRuda).fetch().size)
    report("Kino Warszawa (Przeworsk)")(new MsiClient(record("kino-warszawa-przeworsk"), "https://bilety-kino.przeworsk.um.gov.pl", KinoWarszawa).fetch().size)
    report("Kino Powiśle (Sztum)")(new MsiClient(record("kino-powisle-sztum"), "https://kinosztumbilety.pl", KinoPowisle).fetch().size)
    // ── Wave 3: more Filmweb-dropped venues, on existing platform clients ──
    report("Kino Centrum (Skarżysko)")(new MsiClient(record("kino-centrum-skarzysko"), "https://bilet-mck.skarzysko.pl", KinoCentrumSkarzyskoKamienna).fetch().size)
    report("Nowe Kino Warszawa (Gostynin)")(new MsiClient(record("nowe-kino-warszawa"), "https://bilety.mck-gostynin.pl", KinoNoweKinoWarszawa).fetch().size)
    report("Farys (Biecz)")(new SystemBiletowyClient(record("kino-farys"), "https://kfb.systembiletowy.pl", KinoFarys).fetch().size)
    val b24 = "https://www.bilety24.pl/kino/organizator"
    report("Kino CK (Jędrzejów)")(new Bilety24OrganizerClient(record("kino-ck"), s"$b24/centrum-kultury-w-jedrzejowie-1458", KinoCK).fetch().size)
    report("Metalowiec (Kraśnik)")(new Bilety24OrganizerClient(record("kino-metalowiec"), s"$b24/centrum-kultury-i-promocji-w-krasniku-1529", KinoMetalowiec).fetch().size)
    report("Kino Sokolnia (Słupca)")(new Bilety24OrganizerClient(record("kino-sokolnia"), s"$b24/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia).fetch().size)
    // ── Wave 3: bespoke own-site parsers ──
    report("Kino Krapkowice")(new KdkKrapkowiceClient(record("kino-krapkowice"), KinoKrapkowice).fetch().size)
    report("Kino Aurum (Złotoryja)")(new KinoAurumClient(record("kino-aurum"), KinoAurum).fetch().size)
    report("Kino Sokół (Brzozów)")(new KinoSokolBrzozowClient(record("kino-sokol-brzozow"), KinoSokolBrzozow).fetch().size)
    report("Kino Karolinka (Lubliniec)")(new KinoKarolinkaClient(record("kino-karolinka"), KinoKarolinka).fetch().size)
    report("Kino Jedność (Sędziszów)")(new KinoJednoscClient(record("kino-jednosc"), KinoJednosc).fetch().size)
  }
}
