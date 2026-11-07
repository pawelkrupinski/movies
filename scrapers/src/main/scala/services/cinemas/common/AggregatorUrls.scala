package services.cinemas.common

/** Recognising an aggregator's URLs without depending on the client that reads it. */
object AggregatorUrls {

  /** Whether `url` is a Filmweb film page (`FilmwebShowtimesClient.filmPageUrl`) — i.e. a
   *  Filmweb FALLBACK row's detail reference. A cinema in Filmweb fallback serves Filmweb
   *  data under its OWN source slot with such a URL; the cinema's native `DetailEnricher`
   *  parses its own event page and can never read filmweb.pl, so no detail-fetch site may
   *  point `fetchFilmDetail` at one (the synopsis/cast/hints those rows need come from TMDB
   *  enrichment, not the cinema). */
  def isFilmwebFilmUrl(url: String): Boolean = url.contains("filmweb.pl/film/")
}
