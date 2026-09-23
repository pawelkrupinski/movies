package services.cinemas.roster

import org.jsoup.Jsoup
import services.cinemas.pl.{Bilety24OrganizerClient, FilmwebShowtimesClient}
import tools.FetchedPage

/** Where a venue's source says the venue is. `street` is the street address
 *  without postcode or town, when the page gives one. */
final case class PublishedVenue(town: String, street: Option[String])

/**
 * A multi-venue source whose venue page names the venue's town — read by the
 * ONLINE roster audit (`tools.RosterAudit`) to check that page against the town
 * we file the venue under. Only the two Polish sources the three 2026-09 roster
 * mistakes came through publish one: a bilety24 organiser page (Braniewo's
 * Baszta on Środa's organiser, Koło's Kino nad Wartą on Konin's culture centre)
 * and a Filmweb cinema (Kino Etiuda wired a second time as Filmweb's "Etiuda
 * OBK").
 */
sealed trait VenueSourcePage {
  def matches(sourceUrl: String): Boolean

  /** The URL to fetch for a source URL we wire. A fetch that ends anywhere
   *  else was redirected: the address we wire is one the source retired. */
  def pageUrl(sourceUrl: String): String = sourceUrl

  /** Whether the source answered as it does for a venue it no longer knows —
   *  neither says so with a 404. */
  def dropped(page: FetchedPage): Boolean

  /** The town and street the page publishes, `None` when it names none. */
  def read(body: String): Option[PublishedVenue]
}

object VenueSourcePage {

  val all: Seq[VenueSourcePage] = Seq(Bilety24Organiser, Filmweb)

  def forUrl(sourceUrl: String): Option[VenueSourcePage] = all.find(_.matches(sourceUrl))

  /** `…/kino/organizator/<slug>-<id>`: `<h1>` names the organiser, the `<p>`
   *  under it reads "ul. Dąbrowskiego 19, 63-000 Środa Wielkopolska". */
  case object Bilety24Organiser extends VenueSourcePage {
    // Postcode as "63-000", or "64 840" where the organiser typed it so.
    private val StreetPostcodeTown = """^(.*?),?\s*(\d{2}[- ]\d{3})\s+(.+)$""".r

    def matches(sourceUrl: String): Boolean  = Bilety24OrganizerClient.organiserId(sourceUrl).isDefined
    // An id bilety24 does not know 302s to the home page.
    def dropped(page: FetchedPage): Boolean = Bilety24OrganizerClient.organiserId(page.finalUrl).isEmpty

    def read(body: String): Option[PublishedVenue] =
      Option(Jsoup.parse(body).selectFirst(".cinema-view-info .header p")).map(_.text.trim).collect {
        case StreetPostcodeTown(street, _, town) => PublishedVenue(town.trim, Option(street.trim).filter(_.nonEmpty))
      }
  }

  /** A Filmweb cinema, read off the JSON API (`/api/v1/cinema/<id>/info`)
   *  rather than the `/cinema/-<id>` page we link to, which redirects through an
   *  `http://` hop Java's client refuses to follow off https. */
  case object Filmweb extends VenueSourcePage {
    private val CinemaUrl = """^https://www\.filmweb\.pl/cinema/-(\d+)$""".r

    def matches(sourceUrl: String): Boolean = CinemaUrl.matches(sourceUrl)
    override def pageUrl(sourceUrl: String): String = sourceUrl match {
      case CinemaUrl(id) => FilmwebShowtimesClient.cinemaInfoUrl(id.toInt)
      case other         => other
    }
    // An unknown id answers 204, no body.
    def dropped(page: FetchedPage): Boolean = page.body.trim.isEmpty

    def read(body: String): Option[PublishedVenue] =
      FilmwebShowtimesClient.parseCinemaInfo(body).map(info => PublishedVenue(info.city, info.street))
  }
}
