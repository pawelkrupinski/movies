package services.identity

import models.Cinema
import services.movies.{ListingKey, TitleNormalizer}

/** A film database of `films`: search by all-words containment of a title or an alternative title,
 *  the directors' filmographies, the films IMDb lists under a title (its own, exactly), and each
 *  film's record. */
final class FilmTable(films: Seq[FilmTable.F], normalizer: TitleNormalizer, tmdbNumbered: Boolean = true) extends IdentityLookups {
  import FilmTable.F
  private def words(s: String) = services.movies.TitleContainment.tokens(normalizer.searchQuery(s)).toSet
  private def hit(f: F) = Hit(f.id, f.title, None, Some(f.year), f.popularity)
  override def hasDetail(l: Listing): Boolean = false
  /** `tmdbNumbered` false: the table's ids are a family's own numbers, as `agreement.FamilyLookups`' are. */
  override def numbersFilmsByTmdbId: Boolean = tmdbNumbered
  override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
  private val inTmdb = films.filterNot(_.imdbOnly)
  private def titledIn(f: F, t: String) = (f.title +: f.imdbTitles).exists(IdentityMeasures.key(_) == IdentityMeasures.key(t))
  override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = Answer.Known(q match {
    case CandidateQuery.Title(text) =>
      val want = words(text)
      inTmdb.filter(f => f.searched && want.nonEmpty && (f.title +: (f.original.toSeq ++ f.alternatives.diff(f.unindexed))).exists(t => want.subsetOf(words(t))))
        .sortBy(-_.popularity).map(hit)
    case CandidateQuery.Director(name) => inTmdb.filter(f => f.director == name || f.directorAliases.contains(name)).map(hit)
    case CandidateQuery.Imdb(title)    => inTmdb.filter(f => f.suggested && (words(f.title) == words(title) || f.imdbTitles.exists(words(_) == words(title)))).map(hit)
    // every director the table holds is one IMDb would suggest by name
    case CandidateQuery.TitleCredit(t) => TitleCredit.named(t, inTmdb.map(_.director).filter(_.nonEmpty)).toSeq
      .flatMap(name => inTmdb.filter(f => f.director == name)).map(hit)
    // and the films only IMDb holds, under their fallback ids
    case CandidateQuery.ImdbTitled(t)  => inTmdb.filter(titledIn(_, t)).map(hit) ++
      films.filter(f => f.imdbOnly && titledIn(f, t)).flatMap(f => FallbackIds.of(FallbackIds.Source.Imdb, f.id).map(id => hit(f).copy(tmdbId = id, popularity = 0.0)))
  })
  // A record crediting nobody (an empty director) and with no runtime (0), as a broadcast's is.
  override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
    val found = FallbackIds.unapply(id).fold(inTmdb.find(_.id == id)) { case (_, number) => films.find(f => f.imdbOnly && f.id == number) }
    Answer.Known(found.map(f =>
      IdentityMeasures.Film(f.title, f.original, f.alternatives ++ (if (f.imdbOnly) f.imdbTitles else Nil), Some(f.year), Some(f.runtime).filter(_ > 0),
        Some(Seq(f.director).filter(_.nonEmpty)), Some(f.countries).filter(_.nonEmpty), if (f.imdbOnly) None else Some(f.popularity),
        imdbNumber = f.id, released = f.released, releaseCountries = f.releaseCountries, releases = f.releases)))
  }
}

object FilmTable {
  /** `searched`: TMDB's search returns the film (a record its index misses is reached only by the
   *  IMDb id IMDb lists under its title). `directorAliases`: the other names TMDB's person search finds its
   *  director by — a Latin spelling of one its credits write in another script. `imdbTitles`: the titles IMDb lists it
 *  under besides its own (its AKAs), which IMDb's suggestions match a query against. `imdbOnly`: a film TMDB holds no
 *  record of — only the IMDb-titled question answers it, under its fallback id ([[FallbackIds]]). `released`: the day TMDB
 *  dates its release, a broadcast's air date. `releaseCountries`: the countries TMDB dates a release in, codes run
 *  together ([[IdentityMeasures.Film.releaseCountries]]). `original`: its original title, when TMDB's localised one is
 *  another ("RBO 2026/27: Schwanensee (The Royal Ballet)" of "Royal Ballet & Opera 2026/27: Swan Lake"). `unindexed`: the
 *  alternatives among its titles TMDB's search does not match a query against (Kiselyov's "Mira", 864101, which a search
 *  for "Mira" does not return, only one for "Last Signal"). `suggested`: IMDb's suggestions for a title it carries list it —
 *  they list a title's best-known films only (Kiselyov's "Mira" alone, of the eight). */
  final case class F(id: Int, title: String, year: Int, director: String, runtime: Int, popularity: Double = 10.0,
                     alternatives: Seq[String] = Nil, searched: Boolean = true, countries: Seq[String] = Nil,
                     directorAliases: Seq[String] = Nil, imdbTitles: Seq[String] = Nil, imdbOnly: Boolean = false,
                     released: Option[java.time.LocalDate] = None, releaseCountries: Option[String] = None,
                     releases: Option[String] = None, original: Option[String] = None, unindexed: Seq[String] = Nil,
                     suggested: Boolean = true)

  /** A listing publishing only its title and what it is given, keyed as a page-less venue keys it. */
  def listing(venue: Cinema, title: String, year: Option[Int] = None, director: Option[String] = None,
              runtime: Option[Int] = None): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director.toSeq), title, title, title, year,
      director.toSeq, runtime, None, None)
}
