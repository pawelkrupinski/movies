package models

/** Provenance record for one (cinema, raw title, raw year) tuple that has
 *  scraped into a `MovieRecord`. Stored as a Set on the record so the cache
 *  can recognise "we already saw this exact scrape" without firing a fresh
 *  enrichment event. */
case class CinemaScrape(cinema: Cinema, title: String, year: Option[Int])
