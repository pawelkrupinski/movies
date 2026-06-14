package scripts

import clients.TmdbClient
import services.enrichment.{FilmwebClient, FilmwebRatings}
import services.movies.{CaffeineMovieCache, MongoMovieRepository}
import tools.RealHttpFetch

/** Force a one-row Filmweb refresh for `(title, year)`. Used after wiring
 *  `onImdbIdMissing` on Filmweb so existing rows that were missed at first
 *  enrichment (Polish indies whose TMDB hit had no IMDb id) get filled now
 *  rather than waiting for the next hourly walk. */
object RefreshOneFilmweb {
  def main(args: Array[String]): Unit = {
    val (title, year) = ("Chłopiec na krańcach świata", Some(2026))
    val repository  = new MongoMovieRepository()
    if (!repository.enabled) { println("MONGODB_URI not set."); sys.exit(1) }
    val cache   = new CaffeineMovieCache(repository)
    val ratings = new FilmwebRatings(cache, new TmdbClient(new RealHttpFetch), new FilmwebClient(new RealHttpFetch))

    def show(label: String): Unit =
      repository.findAll().find(r => r.title == title && r.year == year).foreach { r =>
        val e = r.record
        println(s"$label  filmwebUrl=${e.filmwebUrl.getOrElse("None")}  filmwebRating=${e.filmwebRating.getOrElse("None")}")
      }

    show("before:")
    ratings.refreshOneSync(title, year)
    show("after: ")
    repository.close()
  }
}
