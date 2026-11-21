package services.cinemas.us

import models.GeneratedTable

/**
 * Regal's US estate: each venue's `data/us/venues.json` slug — the same slug
 * [[models.UsRoster.flicksSlugByCinema]] carries, and therefore the stable key a
 * `Cinema` is found by — paired with its Regal `TheatreCode`.
 *
 * GENERATED: the table `generated/us-regal-venues.tsv` (venue slug, theatre code), from
 * `docs/venue-maps/REGAL-VENUE-MAP.tsv`, which was built by
 * matching Regal's own roster (`GET /api/theatres`) to our venue rows by
 * coordinates + name tokens and then cross-checking every pairing against the
 * theatre slug in `regmovies.com/sitemap.xml` (`/theatres/<slug>-<code>`).
 * Keyed by SLUG rather than display name because a display name is qualified on
 * collision (`UsRoster` appends the state), while the slug never moves.
 *
 * 401 of our 408 Regal venues appear here. The other seven are locations Regal's
 * own roster no longer lists (closed sites); they keep flicks.us as their only
 * source, which is what the catalogue does with any venue absent from this map.
 */
object RegalVenues {

  /** Venue slug -> Regal `TheatreCode`. */
  val theatreCodeBySlug: Map[String, String] = GeneratedTable.lookup("generated/us-regal-venues.tsv")

  /** The chain's theatre codes, ascending — the order the request batches below
   *  are cut from, so a batch's membership (and therefore its URL, and therefore
   *  its cache key) is stable across processes and restarts. */
  val allCodes: Seq[String] = theatreCodeBySlug.values.toSeq.sorted

  /** Upper bound on how many theatre codes ride in one `getShowtimes` request.
   *
   *  The endpoint takes a COMMA-SEPARATED `theatres=` list and answers one
   *  `shows[]` entry per theatre, so one request can serve many venues. Measured
   *  2026-08-30: the whole 396-code estate in a single request answers in 3.4s
   *  with a 2.5MB body, and a venue's slice of that response is IDENTICAL to the
   *  response its own single-theatre request returns — batching is lossless, not
   *  a summary.
   *
   *  100 rather than "all of them" because the response body is the unit the
   *  shared cache stores and every venue in a batch re-reads: at ~80 codes a
   *  batch answers in 1.8s with a ~510KB body, where the full estate is 2.5MB.
   *  Since each of the ~400 venues reads its batch's body once per date, the
   *  bigger batch would push ~5x more bytes through Mongo's (256MB-pinned)
   *  WiredTiger cache for a saving of 444 requests per sweep against a total
   *  that is already ~2% of the unbatched shape. */
  val BatchSize = 100

  /** The codes cut into balanced batches — `ceil(n / BatchSize)` groups of as
   *  near-equal size as possible, rather than `grouped(BatchSize)`, which would
   *  leave a 1-code remainder batch spending a whole request on one venue. */
  val batches: Seq[Seq[String]] = {
    val codes = allCodes
    val count = math.ceil(codes.size.toDouble / BatchSize).toInt.max(1)
    (0 until count).map(i => codes.slice(i * codes.size / count, (i + 1) * codes.size / count))
  }

  private val batchByCode: Map[String, Seq[String]] =
    batches.flatMap(batch => batch.map(_ -> batch)).toMap

  /** The batch `code` rides in — the set of theatre codes its requests name, so
   *  every venue in the batch issues the byte-identical URL and the shared cache
   *  collapses them to one upstream fetch. A code outside the map batches alone
   *  (it still works, it just shares nothing). */
  def batchFor(code: String): Seq[String] = batchByCode.getOrElse(code, Seq(code))
}
