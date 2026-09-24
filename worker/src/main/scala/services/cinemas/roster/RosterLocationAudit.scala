package services.cinemas.roster

import models.{City, GeoPoint}
import tools.Slugify

import java.time.LocalDate

/** One venue the online audit checks: where we file it, and the source page
 *  that should agree. `expectedTowns` is [[models.City.townsOf]]. */
final case class AuditedVenue(citySlug: String, cinema: String, sourceUrl: String, expectedTowns: Seq[String]) {
  def label: String = s"$cinema (/$citySlug/)"
  /** The centre of the city page the venue is listed on. */
  def hub: Option[GeoPoint] = City.bySlug(citySlug).map(_.centre)
}

/** What fetching a venue's source page came to. */
sealed trait SourceReading
object SourceReading {
  /** The page names a location. `finalUrl` is where the fetch's redirects ended. */
  final case class Located(published: PublishedVenue, finalUrl: String) extends SourceReading
  /** A venue page, but one naming no town — a layout change, most likely. */
  final case class Unlocated(finalUrl: String) extends SourceReading
  /** The source no longer knows the venue: a 404/410, or a redirect off every venue page. */
  final case class Gone(detail: String) extends SourceReading
  /** A fetch that failed for reasons of the moment (timeout, 5xx, throttling). */
  final case class Unreachable(detail: String) extends SourceReading
}

/** A problem the audit reports. A `failing` one fails the run; the rest are
 *  context for whoever reads the summary. */
sealed trait RosterFinding {
  def failing: Boolean
  def describe: String
}

object RosterFinding {
  final case class TownMismatch(venue: AuditedVenue, published: PublishedVenue) extends RosterFinding {
    val failing = true
    def describe: String =
      s"${venue.label}: source says **${published.town}**${published.street.fold("")(s => s" ($s)")}, " +
        s"we say ${venue.expectedTowns.mkString(" / ")} — ${venue.sourceUrl}"
  }
  final case class SharedAddress(venues: Seq[AuditedVenue], town: String, street: String) extends RosterFinding {
    val failing = true
    def describe: String = s"$street, $town is published for ${venues.size} venues: " +
      venues.map(v => s"${v.label} ${v.sourceUrl}").mkString("; ")
  }
  /** Coordinates the source publishes that put the venue further from the city
   *  page it is listed on than any venue we file on purpose — a venue wired
   *  under the wrong hub, or a source id that is another town's venue. */
  final case class FarFromHub(venue: AuditedVenue, published: PublishedVenue, km: Double) extends RosterFinding {
    val failing = true
    def describe: String =
      f"${venue.label}: source places it in **${published.town}**, $km%.0f km from /${venue.citySlug}/ — ${venue.sourceUrl}"
  }
  final case class StaleUrl(venue: AuditedVenue, finalUrl: String) extends RosterFinding {
    val failing = true
    def describe: String = s"${venue.label}: ${venue.sourceUrl} redirects to $finalUrl — wire the address the source publishes"
  }
  final case class SourceGone(venue: AuditedVenue, detail: String) extends RosterFinding {
    val failing = true
    def describe: String = s"${venue.label}: source no longer lists it ($detail) — ${venue.sourceUrl}"
  }
  /** Too many pages read no town: the audit, not the roster, is what broke — a
   *  source changed its layout, or started refusing us — and a run that checked
   *  almost nothing must not read as a clean one. */
  final case class LowCoverage(unchecked: Int, located: Int) extends RosterFinding {
    val failing = true
    def describe: String = s"$unchecked venue pages read no town (vs $located that did) — has a source changed its layout or started blocking us?"
  }
  final case class NoTownOnPage(venue: AuditedVenue) extends RosterFinding {
    val failing = false
    def describe: String = s"${venue.label}: page names no town — ${venue.sourceUrl}"
  }
  final case class NotChecked(venue: AuditedVenue, detail: String) extends RosterFinding {
    val failing = false
    def describe: String = s"${venue.label}: not checked ($detail) — ${venue.sourceUrl}"
  }
  /** A chain's venue list that could not be read this time — one finding for
   *  all its venues. Not failing when read from a datacenter address alone:
   *  Multikino's list sits behind Cloudflare, which refuses those outright, CI's
   *  among them. `tools.ChainListEgress` fails it once the residential proxy was
   *  tried too. */
  final case class DirectoryNotRead(directory: String, venues: Int, detail: String, failing: Boolean = false) extends RosterFinding {
    def describe: String = s"$directory's venue list: not read ($detail) — its $venues venues not checked"
  }
}

/**
 * The pure half of the ONLINE roster audit: given what each venue's source page
 * said, name every place the roster and the sources disagree. `tools.RosterAudit`
 * does the fetching; this decides, so the decisions are testable on recorded
 * pages.
 */
object RosterLocationAudit {
  import RosterFinding._
  import SourceReading._

  /** Venues that share a street address and are nonetheless two listings,
   *  checked by comparing their programmes, not their names. */
  private val SharingAnAddress: Set[Set[String]] = Set(
    // Warszawska 37, Międzyrzec Podlaski: Filmweb 1658 and 1850 carry disjoint
    // programmes (different films, different days — 14 days compared 2026-09-23),
    // Sława's sold through biletyna and za Rogiem's not: two screens of one
    // culture centre, not one screen twice.
    Set("Kino Sława", "Kino za Rogiem Międzyrzec"),
  )

  /** How far from its city page's centre a venue may sit. Picked from the PL
   *  roster's real spread, measured 2026-09-23 over the 174 venues whose source
   *  publishes coordinates (Filmweb, Helios, Cinema City): median 9.5 km, and the
   *  furthest venue filed on purpose is Cinema City Biała Podlaska on /siedlce/
   *  at 59 km, then Grajewo on /lomza/ (58), Międzyzdroje on /szczecin/ (56).
   *  75 km clears that tail with room for a hub's next nearby town. It does not
   *  separate neighbouring hubs (median 53 km apart) — the town check does that;
   *  this catches a source id that is a venue in another region altogether,
   *  which a town spelled like ours (two Środas, two Ostrówów) would slip past. */
  val MaxKmFromHub = 75.0

  /** Pages allowed to read no town before the run blames itself. */
  private def coverageTolerance(pages: Int): Int = math.max(2, pages / 10)

  def findings(readings: Seq[(AuditedVenue, SourceReading)]): Seq[RosterFinding] = {
    val located = readings.collect { case (v, l: Located) => v -> l }

    val towns = located.collect {
      case (v, Located(p, _)) if !v.expectedTowns.exists(TownName.same(_, p.town)) => TownMismatch(v, p)
    }
    val far = located.flatMap { case (v, Located(p, _)) =>
      for { hub <- v.hub; at <- p.location; km = hub.kmTo(at) if km > MaxKmFromHub } yield FarFromHub(v, p, km)
    }
    val stale = located.collect {
      case (v, Located(_, finalUrl)) if VenueSourcePage.forUrl(v.sourceUrl).exists(p =>
                                          finalUrl.stripSuffix("/") != p.pageUrl(v.sourceUrl).stripSuffix("/")) => StaleUrl(v, finalUrl)
    }
    val shared = located
      .flatMap { case (v, Located(p, _)) => p.street.map(s => (Slugify.stable(p.town), streetKey(s)) -> (v, p)) }
      .groupMap(_._1)(_._2).toSeq
      .collect { case (_, all) if all.map(_._1.cinema).distinct.size > 1 && !SharingAnAddress(all.map(_._1.cinema).toSet) =>
        SharedAddress(all.map(_._1), all.head._2.town, all.head._2.street.getOrElse(""))
      }
    val gone       = readings.collect { case (v, Gone(detail)) => SourceGone(v, detail) }
    val unlocated  = readings.collect { case (v, Unlocated(_)) => NoTownOnPage(v) }
    val unchecked  = readings.collect { case (v, Unreachable(detail)) => NotChecked(v, detail) }
    val uncovered  = unlocated.size + unchecked.size
    val coverage   = Option.when(uncovered > coverageTolerance(readings.size))(LowCoverage(uncovered, located.size))

    towns ++ far ++ shared ++ stale ++ gone ++ coverage ++ unlocated ++ unchecked
  }

  /** A street as a comparable key: folded, the `ul.`/`al.`/`pl.` prefix
   *  dropped, so bilety24's "ul. Siennieńska 54" is Filmweb's "Siennieńska 54". */
  private[roster] def streetKey(street: String): String =
    Slugify.stable(street).split('-').dropWhile(StreetPrefixes).mkString("-")
  private val StreetPrefixes = Set("ul", "ulica", "al", "aleja", "aleje", "pl", "plac", "os", "osiedle")

  /** The run as a GitHub job summary. */
  def markdown(readings: Seq[(AuditedVenue, SourceReading)], findings: Seq[RosterFinding], throughput: String): String = {
    val (failing, notes) = findings.partition(_.failing)
    val located = readings.count(_._2.isInstanceOf[Located])
    val header =
      s"""## Cinema roster audit
         |
         |${readings.size} venue source pages fetched, $located named a town. $throughput
         |""".stripMargin
    def section(title: String, items: Seq[RosterFinding]): String =
      if (items.isEmpty) "" else s"\n### $title (${items.size})\n\n" + items.map(f => s"- ${f.describe}").mkString("\n") + "\n"
    header +
      (if (failing.isEmpty) "\n**No roster mismatches.**\n" else section("Mismatches", failing)) +
      section("Not checked", notes)
  }
}

/** Reads one venue's source page into a [[SourceReading]]. */
object RosterSourceReader {
  import RosterFinding.DirectoryNotRead
  import SourceReading._

  /** Statuses that mean "ask again more slowly" — rethrown so the caller's pool
   *  can back off, never recorded as the venue's answer. */
  def isThrottle(e: Throwable): Boolean = e match {
    case s: tools.HttpStatusException => s.code == 429 || s.code == 503
    case _                            => false
  }

  /** Every venue of these cities whose source page names its town. */
  def venuesOf(cities: Seq[models.City], scrapersOf: String => Seq[services.cinemas.common.CinemaScraper]): Seq[AuditedVenue] =
    for {
      city    <- cities
      scraper <- scrapersOf(city.slug)
      url     <- scraper.sourceUrl.toSeq if VenueSourcePage.forUrl(url).isDefined
    } yield AuditedVenue(city.slug, scraper.cinema.displayName, url, city.townsOf(scraper.cinema))

  /** Every venue of these cities a [[ChainDirectory]] lists, with its id there. */
  def chainVenuesOf(cities: Seq[models.City], scrapersOf: String => Seq[services.cinemas.common.CinemaScraper]): Seq[(ChainDirectory, String, AuditedVenue)] =
    for {
      city          <- cities
      scraper       <- scrapersOf(city.slug)
      (directory, id) <- ChainDirectory.of(scraper).toSeq
    } yield (directory, id, AuditedVenue(city.slug, scraper.cinema.displayName,
                                         scraper.sourceUrl.getOrElse(directory.listUrl(LocalDate.now())), city.townsOf(scraper.cinema)))

  /** Read a chain's venue list once and look each of its venues up in it — or,
   *  when the list cannot be read, why not. */
  def readDirectory(fetch: String => tools.FetchedPage, today: LocalDate)(directory: ChainDirectory, venues: Seq[(String, AuditedVenue)])
      : Either[DirectoryNotRead, Seq[(AuditedVenue, SourceReading)]] =
    try {
      val url    = directory.listUrl(today)
      val listed = directory.parse(fetch(url).body)
      if (listed.isEmpty) Left(DirectoryNotRead(directory.name, venues.size, "no venues in the answer"))
      else Right(venues.map { case (id, venue) =>
        venue -> listed.get(id).fold[SourceReading](Gone(s"id $id not in ${directory.name}'s venue list"))(Located(_, url))
      })
    } catch {
      case scala.util.control.NonFatal(e) => Left(DirectoryNotRead(directory.name, venues.size, s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }

  def read(fetch: String => tools.FetchedPage)(venue: AuditedVenue): SourceReading =
    VenueSourcePage.forUrl(venue.sourceUrl) match {
      case None => Unreachable("no reader for this source")
      case Some(page) =>
        try {
          val fetched = fetch(page.pageUrl(venue.sourceUrl))
          if (page.dropped(fetched)) Gone(s"answered from ${fetched.finalUrl} with no venue")
          else page.read(fetched.body).fold[SourceReading](Unlocated(fetched.finalUrl))(Located(_, fetched.finalUrl))
        } catch {
          case e: tools.HttpStatusException if tools.HttpStatusException.isDurable(e.code) => Gone(s"HTTP ${e.code}")
          case e if isThrottle(e)                                                           => throw e
          case scala.util.control.NonFatal(e)                                               => Unreachable(s"${e.getClass.getSimpleName}: ${e.getMessage}")
        }
    }
}
