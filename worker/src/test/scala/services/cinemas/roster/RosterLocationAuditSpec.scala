package services.cinemas.roster

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.roster.RosterAuditFixtures._
import services.cinemas.roster.RosterFinding._
import services.cinemas.roster.SourceReading._
import tools.{FetchedPage, HttpStatusException}

/** The online audit's decisions, replayed on the source pages behind the three
 *  roster mistakes found 2026-09-23 — each as it was wired, then as fixed. */
class RosterLocationAuditSpec extends AnyFlatSpec with Matchers {

  private val NoBody = ""

  /** A recorded web: URL → (where its redirects end, which fixture it serves). */
  private def web(pages: (String, (String, String))*): String => FetchedPage = {
    val byUrl = pages.toMap
    url => byUrl.get(url).map { case (finalUrl, fixture) => FetchedPage(finalUrl, if (fixture == NoBody) "" else page(fixture)) }
      .getOrElse(throw new HttpStatusException(404, "GET", url, None))
  }

  private def audit(fetch: String => FetchedPage, venues: AuditedVenue*): Seq[RosterFinding] =
    RosterLocationAudit.findings(venues.map(v => v -> RosterSourceReader.read(fetch)(v)))

  private val SrodaUrl = "https://www.bilety24.pl/kino/organizator/kino-baszta-477"

  "the online roster audit" should "catch Braniewo's Baszta read off Środa Wielkopolska's organiser" in {
    val asWired = "https://www.bilety24.pl/kino/organizator/kino-baszta-w-braniewie-477"
    val found = audit(web(asWired -> (SrodaUrl, Sroda477)), AuditedVenue("elblag", "Baszta", asWired, Seq("Braniewo")))
    found.collect { case TownMismatch(_, p) => p.town } shouldBe Seq("Środa Wielkopolska")
    found.collect { case StaleUrl(_, finalUrl) => finalUrl } shouldBe Seq(SrodaUrl)
  }

  it should "pass Braniewo's Baszta read off its own Filmweb page" in {
    val fixed = "https://www.filmweb.pl/cinema/-2352"
    audit(web(filmwebInfo(2352) -> (filmwebInfo(2352), Braniewo2352)),
      AuditedVenue("elblag", "Baszta", fixed, Seq("Braniewo"))) shouldBe empty
  }

  it should "catch Koło's Kino nad Wartą read off Konin's culture centre, and pass it on its own Filmweb id" in {
    val asWired = "https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626"
    audit(web(asWired -> (asWired, Konin1626)), AuditedVenue("konin", "Kino nad Wartą", asWired, Seq("Koło")))
      .collect { case TownMismatch(_, p) => p.town } shouldBe Seq("Konin")

    val fixed = "https://www.filmweb.pl/cinema/-1526"
    audit(web(filmwebInfo(1526) -> (filmwebInfo(1526), Kolo1526)),
      AuditedVenue("konin", "Kino nad Wartą", fixed, Seq("Koło"))) shouldBe empty
  }

  it should "catch Kino Etiuda wired twice, by the one address both sources publish" in {
    val bilety24 = "https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-w-ostrowcu-swietokrzyskim-1389"
    val filmweb  = "https://www.filmweb.pl/cinema/-3024"
    val ostrowiec = Seq("Ostrowiec Świętokrzyski")
    val found = audit(
      web(bilety24 -> (bilety24, Ostrowiec1389),
          filmwebInfo(3024) -> (filmwebInfo(3024), EtiudaObk3024)),
      AuditedVenue("ostrowiec-swietokrzyski", "Kino Etiuda", bilety24, ostrowiec),
      AuditedVenue("ostrowiec-swietokrzyski", "Kino Etiuda OBK", filmweb, ostrowiec))
    found.collect { case SharedAddress(venues, _, street) => (venues.map(_.cinema).sorted, street) } shouldBe
      Seq((Seq("Kino Etiuda", "Kino Etiuda OBK"), "Siennieńska 54"))
    found.filter(_.failing) should have size 1
  }

  it should "pass the two Międzyrzec screens that share Warszawska 37 but not a programme" in {
    val miedzyrzec = Seq("Międzyrzec Podlaski")
    audit(web(filmwebInfo(1658) -> (filmwebInfo(1658), Slawa1658), filmwebInfo(1850) -> (filmwebInfo(1850), ZaRogiem1850)),
      AuditedVenue("siedlce", "Kino Sława", "https://www.filmweb.pl/cinema/-1658", miedzyrzec),
      AuditedVenue("siedlce", "Kino za Rogiem Międzyrzec", "https://www.filmweb.pl/cinema/-1850", miedzyrzec)) shouldBe empty
  }

  it should "pass a source spelling the town's qualifier with another ending" in {
    audit(web(filmwebInfo(2348) -> (filmwebInfo(2348), Wars2348)),
      AuditedVenue("lomza", "Kino Wars", "https://www.filmweb.pl/cinema/-2348", Seq("Wysokie Mazowieckie"))) shouldBe empty
  }

  it should "report a venue its source dropped — a 404, or a redirect off every venue page" in {
    val dropped  = "https://www.bilety24.pl/kino/organizator/kino-zamkniete-999999"
    val unknown  = "https://www.filmweb.pl/cinema/-999999"
    val found = audit(
      web(dropped -> ("https://www.bilety24.pl/", Konin1626),       // bilety24 302s an unknown id home
          filmwebInfo(999999) -> (filmwebInfo(999999), NoBody)),   // Filmweb answers 204
      AuditedVenue("konin", "Gone", dropped, Seq("Konin")), AuditedVenue("konin", "Unknown", unknown, Seq("Konin")))
    found.collect { case SourceGone(v, _) => v.cinema } shouldBe Seq("Gone", "Unknown")
  }

  it should "list a page it could not fetch this time without failing on it, and hand throttling back to the pool" in {
    val url = "https://www.filmweb.pl/cinema/-1526"
    val venue = AuditedVenue("konin", "Kino nad Wartą", url, Seq("Koło"))
    val flaky: String => FetchedPage = u => throw new HttpStatusException(500, "GET", u, None)
    val found = audit(flaky, venue)
    found.map(_.failing) shouldBe Seq(false)

    val throttled: String => FetchedPage = u => throw new HttpStatusException(429, "GET", u, None)
    an [HttpStatusException] should be thrownBy RosterSourceReader.read(throttled)(venue)
  }

  // The first live run read no Filmweb page at all (an http:// redirect hop the
  // client refused) and still came out clean. A run that checked almost nothing
  // fails instead.
  it should "fail a run that read no town off most of its pages" in {
    val venues = (1 to 5).map(i => AuditedVenue("konin", s"V$i", s"https://www.filmweb.pl/cinema/-$i", Seq("Konin")))
    val readings = venues.take(3).map(_ -> Unlocated("https://www.filmweb.pl/showtimes/Konin/V-1")) ++
                   venues.drop(3).map(_ -> Unreachable("HTTP 301"))
    RosterLocationAudit.findings(readings).collect { case c: LowCoverage => c.unchecked } shouldBe Seq(5)
  }

  it should "catch a venue whose source places it in another region, however its town is spelled" in {
    val url = "https://www.filmweb.pl/cinema/-1526"
    val koloWebs = web(filmwebInfo(1526) -> (filmwebInfo(1526), Kolo1526))
    // Koło as a /krakow/ venue — the town agrees with the annotation, the map does not.
    audit(koloWebs, AuditedVenue("krakow", "Kino nad Wartą", url, Seq("Koło")))
      .collect { case FarFromHub(v, _, km) => (v.cinema, km.round) } shouldBe Seq("Kino nad Wartą" -> 254L)
    // …and as the /konin/ venue it is, 27 km out.
    audit(koloWebs, AuditedVenue("konin", "Kino nad Wartą", url, Seq("Koło"))) shouldBe empty
  }

  "streetKey" should "read bilety24's and Filmweb's spellings of one street alike" in {
    RosterLocationAudit.streetKey("ul. Siennieńska 54") shouldBe RosterLocationAudit.streetKey("Siennieńska 54")
  }
}
