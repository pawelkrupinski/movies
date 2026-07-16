package services.movies

import scala.util.matching.Regex

/** Extracts a per-SCREENING discriminator from a booking URL, so
 *  [[MovieRecordMerge.dedupShowtimes]] can tell duplicate ticket links apart from
 *  genuinely parallel screenings that happen to share `(dateTime, room, format)`.
 *
 *  The two look identical structurally — same slot, different booking URLs — but
 *  mean opposite things, and only the host knows which:
 *    - Kino Nowe Horyzonty lists ONE screening under several `?eventId=` links;
 *      bilety24 exposes a "view" and a "buy" URL for the same session. Distinct
 *      URLs, SAME screening → collapse (the historical default).
 *    - Helios opens a premiere in several halls AT THE SAME TIME, each a distinct
 *      `/screen/<uuid>` and `room=None` (the hall isn't scraped). Distinct URLs,
 *      DIFFERENT screenings → must NOT collapse, or real sessions vanish.
 *
 *  So the default stays "collapse" (discriminator `""`, preserving every existing
 *  aggregator's behaviour); a host is added below ONLY when its URL carries a
 *  true per-screening id. Data, not per-host `if` branches. */
object BookingUrlScreening {

  /** (host substring, regex whose group(1) is the screening id). A URL matching a
   *  host here contributes its screening id to the dedup identity, so two same-slot
   *  showings with DIFFERENT ids are kept as the distinct screenings they are. */
  private val PerScreeningId: Seq[(String, Regex)] = Seq(
    "bilety.helios.pl" -> raw"/screen/([0-9a-fA-F-]{36})".r
  )

  /** `""` for the historical "collapse this slot" default; a stable per-screening
   *  token for hosts whose URL identifies the screening. */
  def discriminator(bookingUrl: Option[String]): String =
    bookingUrl.flatMap { url =>
      PerScreeningId.collectFirst {
        case (host, re) if url.contains(host) => re.findFirstMatchIn(url).map(_.group(1))
      }.flatten
    }.getOrElse("")
}
