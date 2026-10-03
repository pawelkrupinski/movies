package services.movies

import models.{Cinema, CinemaMovie, Source, SourceData}

/**
 * A venue's LISTING, identified by what the venue itself published — never by a title the
 * pipeline normalised or a year it derived. The identity resolver's unit of evidence
 * (docs/design/identity-resolver.md, "Listing identity"): slots and showtimes are to be keyed
 * by it, so a resolution that moves a listing between films can never re-key, move or delete
 * its showtimes.
 *
 * Two shapes, by what the venue gives:
 *
 *  - [[ListingKey.Native]] — the venue publishes a page for the listing (`filmUrl`). The page
 *    alone is NOT enough: KinoPort, Kino Studio Opole and DK Łapy link every film of a month
 *    to one repertoire page (the recorded PL corpus holds "Opętanie" 1981 and "Zawieście
 *    czerwone latarnie" 1991 under one URL), so the key is the page AND the venue's raw title.
 *  - [[ListingKey.Published]] — no page. The raw title is not enough either: Cinema-Arthouse
 *    and Schauburg Karlsruhe list "Sinn und Sinnlichkeit" twice (Ang Lee 1995 and Georgia
 *    Oakley 2026), Club Manufaktur "Bad Apples" twice (2018 and 2025), with no page and one
 *    spelling each. What tells them apart is the year and director the VENUE printed, so they
 *    are part of the key. That is a published year, not a derived one: a TMDB answer, a
 *    bracket the pipeline parsed, or a fold never reaches it.
 *
 * The price of the second shape: when a page-less venue corrects its own year or director the
 * listing gets a new key, and its film keeps its id only through its other listings (the
 * resolver's overlap rule). A page-bearing venue's listing survives any such correction.
 *
 * `ListingKeyCorpusSpec` proves the key unique per distinct listing over every recorded corpus,
 * and that each naive key (the production slot key, venue + raw title, venue + page) is not.
 */
sealed trait ListingKey {
  def venue: String
  def rawTitle: String
}

object ListingKey {

  final case class Native(venue: String, nativeId: String, rawTitle: String) extends ListingKey

  final case class Published(venue: String, rawTitle: String, year: Option[Int], directors: Seq[String]) extends ListingKey

  /** The key of `cm` as `cinema` lists it. Directors are sorted, because a venue's credit
   *  order is presentation, not identity; blank page and names count as absent. */
  def of(cinema: Cinema, cm: CinemaMovie): ListingKey =
    of(cinema, cm.filmUrl, cm.movie.rawTitle.getOrElse(cm.movie.title), cm.movie.releaseYear, cm.director)

  /** The key of the listing a stored venue `slot` holds — the same fields [[of]] reads, as the
   *  landing copied them onto the slot. */
  def ofSlot(cinema: Cinema, slot: SourceData): ListingKey =
    of(cinema, slot.filmUrl, slot.rawTitle.orElse(slot.title).getOrElse(""), slot.releaseYear, slot.director)

  /** The key of the listing a stored slot ROW holds, from the row's wire key
   *  (`Source.displayName`: a bare cinema or `"<cinema>␟<titleKey>"`) and its slot. `None` for a
   *  row that is no venue's listing: an enrichment slot (TMDB, IMDb), a chain's network-level
   *  detail slot (a `Cinema` that is no venue — not in `Cinema.all` — holding one film's detail
   *  for every branch, with no title of its own), or a venue no longer on the roster, whose wire
   *  key names no cinema. The one derivation both `movie_slots` and `screenings` stamp their
   *  `listingKey` with, so the two collections cannot disagree. */
  def ofSlotRow(slotKey: String, slot: SourceData): Option[ListingKey] =
    Source.byWireKey(slotKey).flatMap(ofSource(_, slot))

  /** Whether a row under wire key `slotKey` holds a venue listing, i.e. whether [[ofSlotRow]]
   *  gives it a key whatever its slot says. False for exactly the rows the dual write leaves
   *  unstamped on purpose: enrichment slots (TMDB, IMDb, Filmweb), chain network-level detail
   *  slots and retired venues. */
  def isVenueRow(slotKey: String): Boolean =
    Source.byWireKey(slotKey).flatMap(Source.cinemaOf).exists(isVenue)

  /** The key of `cinema`'s `slot`, when `cinema` is a venue on the roster — the key [[ofSlotRow]]
   *  stamps that slot's rows with, for a caller holding the resolved cinema (the read-model
   *  projection). */
  def ofVenueSlot(cinema: Cinema, slot: SourceData): Option[ListingKey] =
    Option.when(isVenue(cinema))(ofSlot(cinema, slot))

  /** [[ofSlotRow]] for a slot already keyed by its `Source`. */
  def ofSource(source: Source, slot: SourceData): Option[ListingKey] =
    Source.cinemaOf(source).filter(isVenue).map(ofSlot(_, slot))

  private def isVenue(cinema: Cinema): Boolean = Cinema.byDisplayName.get(cinema.displayName).contains(cinema)

  /** The stored form: total and injective, NUL-separated (no venue, page, title or name carries
   *  a NUL). The identity model's traces and families key listings by the same string, so a stored
   *  slot and the model's decision on its listing join on it. */
  def serialised(key: ListingKey): String = key match {
    case Native(venue, page, raw)          => Seq("N", venue, page, raw).mkString(Separator)
    case Published(venue, raw, year, dirs) => (Seq("P", venue, raw, year.fold("")(_.toString)) ++ dirs).mkString(Separator)
  }

  /** The inverse of [[serialised]]; `None` for a string no key serialises to. */
  def parse(stored: String): Option[ListingKey] = stored.split(Separator, -1).toList match {
    case "N" :: venue :: page :: raw :: Nil => Some(Native(venue, page, raw))
    case "P" :: venue :: raw :: year :: dirs =>
      if (year.isEmpty) Some(Published(venue, raw, None, dirs))
      else year.toIntOption.map(y => Published(venue, raw, Some(y), dirs))
    case _ => None
  }

  private val Separator = "\u0000"

  private def of(cinema: Cinema, page: Option[String], raw: String, year: Option[Int], directors: Seq[String]): ListingKey =
    page.map(_.trim).filter(_.nonEmpty) match {
      case Some(p) => Native(cinema.displayName, p, raw)
      case None    => Published(cinema.displayName, raw, year, directors.map(_.trim).filter(_.nonEmpty).distinct.sorted)
    }

  /** By venue, a page's listing before a published one, then the page (native) or the title, year as text and the
   *  directors (published) — the order of the tuple `(venue, kind, page, raw, year as text, directors joined by NUL)`,
   *  compared field by field rather than built per comparison: sorting a US projection's ~100k keys made two tuples,
   *  a year string and a joined string at every one of its ~2M comparisons. */
  implicit val ordering: Ordering[ListingKey] = new Ordering[ListingKey] {
    def compare(a: ListingKey, b: ListingKey): Int = {
      val byVenue = a.venue.compareTo(b.venue)
      if (byVenue != 0) byVenue
      else a match {
        case Native(_, pa, ra) => b match {
          case Native(_, pb, rb) => val byPage = pa.compareTo(pb); if (byPage != 0) byPage else ra.compareTo(rb)
          case _: Published      => -1
        }
        case Published(_, ra, ya, da) => b match {
          case _: Native => 1
          case Published(_, rb, yb, db) =>
            val byRaw = ra.compareTo(rb)
            if (byRaw != 0) byRaw
            else {
              val byYear = years(ya, yb)
              if (byYear != 0) byYear else directors(da, db)
            }
        }
      }
    }
    // As the years' text compares: none first; two of as many digits as their numbers do.
    private def years(a: Option[Int], b: Option[Int]): Int =
      if (a.isEmpty) (if (b.isEmpty) 0 else -1)
      else if (b.isEmpty) 1
      else {
        val x = a.get
        val y = b.get
        if (x == y) 0
        else if (x >= 0 && y >= 0 && digits(x) == digits(y)) Integer.compare(x, y)
        else x.toString.compareTo(y.toString)
      }
    private def digits(n: Int): Int = if (n < 10) 1 else 1 + digits(n / 10)
    // As the directors joined by NUL compare: name by name, a list that runs out first ordering first.
    private def directors(a: Seq[String], b: Seq[String]): Int =
      if (a.isEmpty || b.isEmpty) java.lang.Boolean.compare(a.nonEmpty, b.nonEmpty)
      else if (a eq b) 0
      else {
        val ia = a.iterator
        val ib = b.iterator
        var result = 0
        while (result == 0 && ia.hasNext && ib.hasNext) result = ia.next().compareTo(ib.next())
        if (result != 0) result else java.lang.Boolean.compare(ia.hasNext, ib.hasNext)
      }
  }
}
