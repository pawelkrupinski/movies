package controllers

import models.{ResolvedMovie, ResolvedRatings}

/** The films of the `/debug/tune` samples ([[TuneSamples]]), each with the ratings spread its card is there to show. */
private[controllers] object TuneSampleMovies {

  // Build a resolved-movie sample directly (the web no longer holds
  // MovieRecords). Rating hrefs are placeholders — this page tunes layout, not
  // links — and `weightedRating` uses the production formula so the grid's
  // data-rating sort behaves as in prod.
  def resolved(
    title:     String,
    genres:    Seq[String],
    runtime:   Option[Int],
    year:      Option[Int],
    imdb:      Option[Double] = None,
    metascore: Option[Int]    = None,
    rt:        Option[Int]    = None,
    filmweb:   Option[Double] = None
  ): ResolvedMovie = {
    val ns       = Seq(imdb, filmweb, metascore.map(_ / 10.0), rt.map(_ / 10.0)).flatten
    val weighted = if (ns.isEmpty) 0.0 else ns.sum / ns.size
    ResolvedMovie(
      _id = title, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
      runtimeMinutes = runtime, releaseYear = year, genres = genres, countries = Seq.empty,
      directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
      ratings = ResolvedRatings(
        imdb = imdb, imdbUrl = imdb.map(_ => "https://www.imdb.com/"),
        metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
        rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
        filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
      ),
      weightedRating = weighted
    )
  }
}
