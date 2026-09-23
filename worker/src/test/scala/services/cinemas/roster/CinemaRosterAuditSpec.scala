package services.cinemas.roster

import clients.tools.FakeHttpFetch
import models.{City, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{Json, Reads}
import services.cinemas.CinemaScraperCatalog
import services.cinemas.common.{CinemaScraper, MultiListingScraper}
import services.cinemas.pl.{Bilety24OrganizerClient, FilmwebShowtimesClient}
import tools.Slugify

import java.net.URI
import java.time.LocalDate
import scala.io.{Codec, Source}
import scala.util.{Try, Using}

/**
 * The OFFLINE half of the cinema-roster audit: pure checks over the catalog and
 * the roster data, with no network. Each is a class of mistake that shipped:
 *
 *   - Braniewo's Baszta was wired to bilety24 organiser 477 — Środa
 *     Wielkopolska's Kino Baszta — under a different slug, so one programme was
 *     scraped into two cities. → one scraper per [[CinemaScraper.sourceKey]].
 *   - "Kino Etiuda OBK" (Filmweb 3024) was Kino Etiuda (bilety24 1389) listed a
 *     second time, same address. → no two venues of one town whose names differ
 *     only by what the other adds.
 *   - Kino nad Wartą (Koło) was wired to Konin's culture centre. That one only
 *     the ONLINE half can see — the source's own page names the town — which is
 *     `tools.RosterAudit`, run weekly by `.github/workflows/roster-audit.yml`.
 *
 * The town checks read [[City.townsOf]] (the venue table built from the
 * catalog's `// <Town> — <source>` annotations, see `data/pl/README.md`), so they
 * cover Poland, the hand-written roster where these mistakes are made.
 */
class CinemaRosterAuditSpec extends AnyFlatSpec with Matchers {

  private val catalog = new CinemaScraperCatalog(new FakeHttpFetch("does-not-exist"), LocalDate.of(2026, 6, 6))

  /** Every upstream listing with the city it is wired under — every modelled
   *  city, including ones currently disabled. A venue read off several listings
   *  counts each, so each is held to the checks on its own. */
  private val placed: Seq[(String, CinemaScraper)] =
    catalog.byCity.toSeq.flatMap { case (slug, scrapers) =>
      scrapers.flatMap {
        case venue: MultiListingScraper => venue.listings
        case single                     => Seq(single)
      }.map(slug -> _)
    }

  private val polishCities: Map[String, City] = Country.Poland.cities.map(c => c.slug -> c).toMap
  private val polish: Seq[(City, CinemaScraper)] =
    placed.collect { case (slug, s) if polishCities.contains(slug) => polishCities(slug) -> s }

  private def describe(city: String, s: CinemaScraper): String = s"${s.cinema.displayName} ($city)"

  // ── Source identity ────────────────────────────────────────────────────────

  "the cinema roster" should "give every scraper an upstream identity" in {
    val anonymous = placed.collect { case (city, s) if s.sourceKey.isEmpty => describe(city, s) }
    withClue(s"scrapers without a sourceKey: ${anonymous.sorted}") { anonymous shouldBe empty }
  }

  it should "never wire two venues to the same upstream listing, in any source" in {
    val shared = placed.groupBy(_._2.sourceKey).collect {
      case (Some(key), wired) if wired.size > 1 => key -> wired.map(describe).sorted
    }
    withClue(s"listings wired more than once: $shared") { shared shouldBe empty }
  }

  // Filmweb is the one index of (nearly) every Polish cinema, and the venue
  // table remembers each venue's Filmweb id even after the scrape moved off it
  // (`// Koło — filmweb 1526`). A venue scraped from Filmweb id N while ANOTHER
  // venue's annotation says it is N is the same screen twice under two names.
  it should "never scrape a Filmweb id another venue is annotated with" in {
    val annotated = venueTable.flatMap(v => v.filmwebId.map(_ -> v.cinemaObject)).groupMap(_._1)(_._2)
    val twice = annotated.filter(_._2.distinct.size > 1)
    withClue(s"Filmweb ids annotated on more than one venue: $twice") { twice shouldBe empty }

    val clashes = polish.collect { case (city, s: FilmwebShowtimesClient) =>
      filmwebIdOf(s).flatMap(annotated.get).getOrElse(Nil).filterNot(_ == s.cinema.toString)
        .map(other => s"${describe(city.slug, s)} scrapes the Filmweb id annotated on $other")
    }.flatten
    withClue(clashes.mkString("\n")) { clashes shouldBe empty }
  }

  it should "scrape a Filmweb-backed venue off the Filmweb id its annotation names" in {
    val byObject = venueTable.map(v => v.cinemaObject -> v).toMap
    val wrong = polish.collect { case (city, s: FilmwebShowtimesClient) =>
      (filmwebIdOf(s), byObject.get(s.cinema.toString).flatMap(_.filmwebId)) match {
        case (Some(wired), Some(noted)) if wired != noted => Some(s"${describe(city.slug, s)}: wired $wired, annotated $noted")
        case _                                            => None
      }
    }.flatten
    withClue(wrong.mkString("\n")) { wrong shouldBe empty }
  }

  // ── Well-formed source ids ─────────────────────────────────────────────────

  it should "point every source URL at a host the scraper declares" in {
    val bad = placed.flatMap { case (city, s) =>
      s.sourceUrl.flatMap { url =>
        val host = Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase)
        val scheme = Try(URI.create(url).getScheme).toOption
        Option.when(!scheme.exists(Set("http", "https")) || !host.exists(s.scrapeHosts))(s"${describe(city, s)}: $url")
      }
    }
    withClue(bad.mkString("\n")) { bad shouldBe empty }
  }

  it should "spell every multi-venue source's venue id in that source's own shape" in {
    val bad = placed.flatMap { case (city, s) =>
      s.sourceUrl.flatMap { url =>
        CinemaRosterAuditSpec.IdShapes.collectFirst { case (host, shape) if url.contains(s"//$host/") => shape }
          .filterNot(_.matches(url)).map(_ => s"${describe(city, s)}: $url")
      }
    }
    withClue(bad.mkString("\n")) { bad shouldBe empty }
  }

  it should "address every bilety24 organiser by the slug bilety24 publishes, not one it only redirects" in {
    val retired = Using.resource(Source.fromFile("worker/src/test/resources/roster/bilety24-retired-organiser-slugs.txt")(using Codec.UTF8)) {
      _.getLines().map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSet
    }
    retired should not be empty
    val stale = placed.collect { case (city, s: Bilety24OrganizerClient) =>
      s.sourceUrl.flatMap(url => Bilety24OrganizerClient.organiserSlug(url).map(_ + "-" + Bilety24OrganizerClient.organiserId(url).getOrElse("")))
        .filter(retired).map(slug => s"${describe(city, s)}: $slug")
    }.flatten
    withClue(stale.mkString("\n")) { stale shouldBe empty }
  }

  // ── Towns (Poland) ─────────────────────────────────────────────────────────

  it should "keep the venue table in step with the catalog" in {
    val wired = polish.map { case (city, s) => (s.cinema.toString, city.slug, s.cinema.displayName) }.toSet
    val table = venueTable.map(v => (v.cinemaObject, v.citySlug, v.displayName)).toSet
    withClue("wired but not in data/pl/venues.json (rebuild it — see data/pl/README.md): ") {
      (wired diff table).toSeq.sorted shouldBe empty
    }
    withClue("in data/pl/venues.json but not wired so (rebuild it — see data/pl/README.md): ") {
      (table diff wired).toSeq.sorted shouldBe empty
    }
  }

  // A town with its own city page is that city's, not a hub's: filing it under
  // a neighbour hides it from the page a visitor from that town opens.
  it should "file every venue under its own town's city, when that town has one" in {
    val homeOf: Map[String, City] = polishCities.values.toSeq.flatMap(c => c.homeTowns.map(Slugify.stable(_) -> c)).toMap
    val misfiled = polish.flatMap { case (city, s) =>
      city.townsOf(s.cinema).flatMap(t => homeOf.get(Slugify.stable(t))).filterNot(_ == city)
        .map(own => s"${describe(city.slug, s)} is in ${own.labels.nominative}, which is /${own.slug}/")
    }
    withClue(misfiled.mkString("\n")) { misfiled shouldBe empty }
  }

  // biletyna.pl and Helios put the venue's town in the URL itself.
  it should "wire each venue to a source filed under the venue's own town" in {
    val wrong = polish.flatMap { case (city, s) =>
      s.sourceUrl.flatMap(CinemaRosterAuditSpec.townInUrl).flatMap { urlTown =>
        val expected = city.townsOf(s.cinema)
        Option.when(!expected.exists(TownName.same(_, urlTown)))(
          s"${describe(city.slug, s)}: source files it under '$urlTown', we say ${expected.mkString(" / ")}")
      }
    }
    withClue(wrong.mkString("\n")) { wrong shouldBe empty }
  }

  it should "not list one screen twice in a town under two names" in {
    val byTown = polish.groupBy { case (city, s) => Slugify.stable(city.townsOf(s.cinema).head) }
    val twins = byTown.toSeq.flatMap { case (town, venues) =>
      val named = venues.map(_._2.cinema).distinct.map(c => c -> CinemaRosterAuditSpec.distinctiveTokens(c.displayName, town))
      for {
        (a, i) <- named.zipWithIndex
        b      <- named.drop(i + 1)
        if a._2.nonEmpty && b._2.nonEmpty && (a._2.subsetOf(b._2) || b._2.subsetOf(a._2))
        pair = Set(a._1.toString, b._1.toString)
        if !CinemaRosterAuditSpec.DistinctNamesakes(pair)
      } yield s"$town: '${a._1.displayName}' and '${b._1.displayName}'"
    }
    withClue(twins.mkString("\n")) { twins shouldBe empty }
  }

  // ── Fixtures ───────────────────────────────────────────────────────────────

  private case class Venue(citySlug: String, cinemaObject: String, displayName: String, town: String, annotation: String) {
    def filmwebId: Option[String] = CinemaRosterAuditSpec.FilmwebAnnotation.findFirstMatchIn(annotation).map(_.group(1))
  }
  private given Reads[Venue] = Json.reads[Venue]

  private lazy val venueTable: Seq[Venue] =
    Using.resource(Source.fromFile("data/pl/venues.json")(using Codec.UTF8))(src => Json.parse(src.mkString).as[Seq[Venue]])

  private def filmwebIdOf(s: FilmwebShowtimesClient): Option[String] =
    s.sourceUrl.flatMap(u => CinemaRosterAuditSpec.FilmwebUrl.findFirstMatchIn(u).map(_.group(1)))
}

object CinemaRosterAuditSpec {

  private val FilmwebAnnotation = """\bfilmweb (\d+)\b""".r
  private val FilmwebUrl        = """/cinema/-(\d+)$""".r

  /** The URL shape of each source that serves many venues, by host. */
  private val IdShapes: Seq[(String, scala.util.matching.Regex)] = Seq(
    "www.filmweb.pl"      -> """https://www\.filmweb\.pl/cinema/-\d+""".r,
    "www.bilety24.pl"     -> """https://www\.bilety24\.pl/kino/organizator/[a-z0-9]+(-[a-z0-9]+)*-\d+""".r,
    "www.cinema-city.pl"  -> """https://www\.cinema-city\.pl/kina/cinema-city/\d{4}""".r,
    "helios.pl"           -> """https://helios\.pl/[a-z0-9-]+/(kino-)?helios(-[a-z0-9-]+)?""".r,
    "biletyna.pl"         -> """https://biletyna\.pl/[A-Za-z-]+/[A-Za-z0-9-]+""".r,
    "www.biletyna.pl"     -> """https://www\.biletyna\.pl/[A-Za-z-]+/[A-Za-z0-9-]+""".r,
    "www.flicks.co.uk"    -> """https://www\.flicks\.co\.uk/cinema/[a-z0-9-]+/""".r,
    "www.flicks.us"       -> """https://www\.flicks\.us/cinema/[a-z0-9-]+/""".r,
    "www.sensacine.com"   -> """https://www\.sensacine\.com/cines/cine/[A-Z0-9]{5}/""".r,
    "www.filmstarts.de"   -> """https://www\.filmstarts\.de/kinoprogramm/kino/[A-Z0-9]{5}/""".r,
    "www.cineworld.co.uk" -> """https://www\.cineworld\.co\.uk/cinemas/[a-z0-9]{5}-[a-z0-9-]+/""".r,
  )

  private val BiletynaTown = """^https://(?:www\.)?biletyna\.pl/([^/]+)/""".r.unanchored
  private val HeliosTown   = """^https://helios\.pl/([^/]+)/""".r.unanchored

  /** The town a source files the venue under in its own URL, where it does. */
  private def townInUrl(url: String): Option[String] = url match {
    case BiletynaTown(town) => Some(town)
    case HeliosTown(town)   => Some(town)
    case _                  => None
  }

  /** Words any venue name may carry — chain brands and the institution words of
   *  a Polish culture centre — so what is left says WHICH venue it is. */
  private val GenericWords: Set[String] = Set(
    "kino", "kinoteatr", "kinokawiarnia", "cinema", "city", "multikino", "helios", "nove",
    "centrum", "kultury", "dom", "osrodek", "miejski", "miejskie", "gminny", "im", "w", "we",
    "the", "3d", "studyjne", "sala", "kinowa", "cafe")

  private def distinctiveTokens(displayName: String, townSlug: String): Set[String] =
    Slugify.stable(displayName).split('-').toSet -- GenericWords -- townSlug.split('-') - ""

  /** Pairs the name check flags that are genuinely two venues. */
  private val DistinctNamesakes: Set[Set[String]] = Set(
    Set("KinoMikro", "MikroBronowice"),   // Kino Mikro (Juliusza Lea) and its second screen in Bronowice
  )
}
