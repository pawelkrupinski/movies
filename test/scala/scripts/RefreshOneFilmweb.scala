package scripts

import services.enrichment.{EnrichmentCache, EnrichmentRepo, FilmwebClient, FilmwebRatings}

/** Force a one-row Filmweb refresh for `(title, year)`. Used after wiring
 *  `onImdbIdMissing` on Filmweb so existing rows that were missed at first
 *  enrichment (Polish indies whose TMDB hit had no IMDb id) get filled now
 *  rather than waiting for the next hourly walk. */
object RefreshOneFilmweb {
  def main(args: Array[String]): Unit = {
    val (title, year) = ("Chłopiec na krańcach świata", Some(2026))
    val repo  = new EnrichmentRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }
    val cache   = new EnrichmentCache(repo)
    val ratings = new FilmwebRatings(cache, new FilmwebClient())

    def show(label: String): Unit =
      repo.findAll().find { case (t, y, _) => t == title && y == year }.foreach { case (_, _, e) =>
        println(s"$label  filmwebUrl=${e.filmwebUrl.getOrElse("None")}  filmwebRating=${e.filmwebRating.getOrElse("None")}")
      }

    show("before:")
    ratings.refreshOneSync(title, year)
    show("after: ")
    repo.close()
  }
}
