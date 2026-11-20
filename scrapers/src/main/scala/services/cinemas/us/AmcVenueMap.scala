package services.cinemas.us

import models.GeneratedTable

/**
 * Our US venues that AMC serves from its OWN origin, mapped to the two slugs its
 * public URL and GraphQL are addressed by: the market its theatre page sits under
 * and the theatre itself (`/movie-theatres/<market>/<theatre>`).
 *
 * GENERATED — the table `data/us/scripts/generate_amc_map.py` writes to `generated/us-amc-venues.tsv` (flicks.us
 * slug, AMC market slug, AMC theatre slug); `docs/venue-maps/AMC-VENUE-MAP.tsv` is the same table with the
 * provenance columns. It is
 * built from AMC's OWN roster (`viewer.theatres`, 523 theatres) matched to
 * `data/us/venues.json` by coordinates, and a row exists only where that join was
 * verified — 519 of our 532 AMC venues. The 13 that did not match are absent
 * from AMC's current roster (closed or sold sites the aggregator still lists) and
 * stay on flicks.us, which is why this is a lookup rather than a rule: a venue with
 * no row simply keeps its aggregator scraper.
 *
 * Keyed by the venue's flicks.us slug rather than its `Cinema`, because that slug is
 * the stable identifier `UsRoster` already carries per venue
 * (`UsRoster.flicksSlugByCinema`) — keying by `Cinema` would mean materialising 519
 * roster lookups here just to invert them again in the catalogue.
 */
object AmcVenueMap {
  /** flicks.us venue slug -> (AMC market slug, AMC theatre slug). */
  val byFlicksSlug: Map[String, (String, String)] =
    GeneratedTable.rows("generated/us-amc-venues.tsv").map(row => row(0) -> (row(1), row(2))).toMap
}
