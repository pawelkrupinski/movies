// GENERATED from data/germany/venue-sources.json by data/germany/scripts/generate_sources.py
// -- do NOT edit by hand. Measured -: 0 rostered venues served other than
// Filmstarts-then-kinoprogramm.com. Regenerate after re-running harvest_sources.py; see data/germany/README.md.
package services.cinemas.de

import services.cinemas.common.{CineamoClient, KinoheldVenue}

private[de] object GermanSourcesData {
  private def f(theaterId: String): GermanFeed = GermanFeed.Filmstarts(theaterId)
  private def c(id: String): GermanFeed = GermanFeed.Cineamo(CineamoClient.CinemaId(id))
  private def k(id: String, path: String): GermanFeed = GermanFeed.Kinoheld(KinoheldVenue(id, path))
  private def p(path: String): GermanFeed = GermanFeed.Kinoprogramm(path)

  /** Each venue's feeds, primary first, by its Filmstarts theaterId. */
  val byTheaterId: Map[String, Seq[GermanFeed]] = (Seq.empty).toMap
}
