package services.identity

import models.Cinema
import services.movies.{ListingKey, TitleNormalizer}

/** A film database of `films`: search by all-words containment of a title or an alternative title,
 *  the directors' filmographies, the films IMDb lists under a title (its own, exactly), and each
 *  film's record. */
final class FilmTable(films: Seq[FilmTable.F], normalizer: TitleNormalizer) extends IdentityLookups {
  import FilmTable.F
  private def words(s: String) = services.movies.TitleContainment.tokens(normalizer.searchQuery(s)).toSet
  private def hit(f: F) = Hit(f.id, f.title, None, Some(f.year), f.popularity)
  override def hasDetail(l: Listing): Boolean = false
  override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
  private val inTmdb = films.filterNot(_.imdbOnly)
  private def titledIn(f: F, t: String) = (f.title +: f.imdbTitles).exists(IdentityMeasures.key(_) == IdentityMeasures.key(t))
  override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = Answer.Known(q match {
    case CandidateQuery.Title(text) =>
      val want = words(text)
      inTmdb.filter(f => f.searched && want.nonEmpty && (f.title +: f.alternatives).exists(t => want.subsetOf(words(t)))).sortBy(-_.popularity).map(hit)
    case CandidateQuery.Director(name) => inTmdb.filter(f => f.director == name || f.directorAliases.contains(name)).map(hit)
    case CandidateQuery.Imdb(title)    => inTmdb.filter(f => words(f.title) == words(title) || f.imdbTitles.exists(words(_) == words(title))).map(hit)
    // and the films only IMDb holds, under their fallback ids
    case CandidateQuery.ImdbTitled(t)  => inTmdb.filter(titledIn(_, t)).map(hit) ++
      films.filter(f => f.imdbOnly && titledIn(f, t)).flatMap(f => FallbackIds.of(FallbackIds.Source.Imdb, f.id).map(id => hit(f).copy(tmdbId = id, popularity = 0.0)))
  })
  // A record crediting nobody (an empty director) and with no runtime (0), as a broadcast's is.
  override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
    val found = FallbackIds.unapply(id).fold(inTmdb.find(_.id == id)) { case (_, number) => films.find(f => f.imdbOnly && f.id == number) }
    Answer.Known(found.map(f =>
      IdentityMeasures.Film(f.title, None, f.alternatives ++ (if (f.imdbOnly) f.imdbTitles else Nil), Some(f.year), Some(f.runtime).filter(_ > 0),
        Some(Seq(f.director).filter(_.nonEmpty)), Some(f.countries).filter(_.nonEmpty), if (f.imdbOnly) None else Some(f.popularity),
        imdbNumber = f.id, released = f.released, releaseCountries = f.releaseCountries)))
  }
}

object FilmTable {
  /** `searched`: TMDB's search returns the film (a record its index misses is reached only by the
   *  IMDb id IMDb lists under its title). `directorAliases`: the other names TMDB's person search finds its
   *  director by — a Latin spelling of one its credits write in another script. `imdbTitles`: the titles IMDb lists it
 *  under besides its own (its AKAs), which IMDb's suggestions match a query against. `imdbOnly`: a film TMDB holds no
 *  record of — only the IMDb-titled question answers it, under its fallback id ([[FallbackIds]]). `released`: the day TMDB
 *  dates its release, a broadcast's air date. `releaseCountries`: the countries TMDB dates a release in, codes run
 *  together ([[IdentityMeasures.Film.releaseCountries]]). */
  final case class F(id: Int, title: String, year: Int, director: String, runtime: Int, popularity: Double = 10.0,
                     alternatives: Seq[String] = Nil, searched: Boolean = true, countries: Seq[String] = Nil,
                     directorAliases: Seq[String] = Nil, imdbTitles: Seq[String] = Nil, imdbOnly: Boolean = false,
                     released: Option[java.time.LocalDate] = None, releaseCountries: Option[String] = None)

  /** A listing publishing only its title and what it is given, keyed as a page-less venue keys it. */
  def listing(venue: Cinema, title: String, year: Option[Int] = None, director: Option[String] = None,
              runtime: Option[Int] = None): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director.toSeq), title, title, title, year,
      director.toSeq, runtime, None, None)
}
