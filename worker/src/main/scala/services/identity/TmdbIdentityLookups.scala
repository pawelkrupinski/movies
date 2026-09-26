package services.identity

import clients.TmdbClient
import models.Cinema
import services.cinemas.common.DetailEnricher
import services.movies.TmdbCandidateSearch

import scala.util.Try

/**
 * [[IdentityLookups]] answered by TMDB and the venues' own detail pages — the raw primitives
 * only: one title search's results, a person's filmography, a film's record. No choice between
 * candidates is made here (that is the resolver's score), and nothing is memoised across calls,
 * so an answer is a function of its argument and of what the source holds.
 *
 * `misses` counts the requests the source could NOT answer (a hermetic replay's gaps); a lookup
 * during which it grew is [[Answer.Unknown]], never an empty answer — `TmdbClient` turns a failed
 * read into an empty one, which would read as "no such film". A lookup that throws is `Unknown`
 * too. Until the observation store (phase 1) replaces it, this is the resolver's source in the
 * shadow harness; nothing in production constructs it.
 */
final class TmdbIdentityLookups(tmdb: TmdbClient, enrichers: Seq[DetailEnricher], misses: () => Long = () => 0L)
    extends IdentityLookups {

  private val enricherOf: Map[Cinema, DetailEnricher] = enrichers.map(e => e.cinema -> e).toMap

  /** `read` as an [[Answer]]: `Unknown` when it threw or a request inside it went unanswered. */
  private def answered[A](read: => A): Answer[A] = synchronized {
    val before = misses()
    Try(read).toOption.filter(_ => misses() == before).fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
  }

  private def hit(r: TmdbClient.SearchResult): Hit = Hit(r.id, r.title, r.originalTitle, r.releaseYear, r.popularity)

  override def hasDetail(listing: Listing): Boolean = listing.page.isDefined && enricherOf.contains(listing.cinema)

  override def detail(listing: Listing): Answer[Option[DetailFacts]] =
    (listing.page, enricherOf.get(listing.cinema)) match {
      case (Some(page), Some(e)) => answered(e.fetchFilmDetail(page).map(d =>
        DetailFacts(d.releaseYear, d.director.map(_.trim).filter(_.nonEmpty), d.runtimeMinutes, d.originalTitle, d.countries)))
      case _                     => Answer.Known(None)
    }

  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text, year) => answered(tmdb.search(text, year).map(hit))
    case CandidateQuery.Director(name)    =>
      // Every person the name could mean, each with what they directed — or, with no directing
      // credit, wrote (a venue may print the writer): the walk the pipeline makes, without its pick.
      answered(tmdb.findPersonCandidates(TmdbCandidateSearch.ImdbDisambiguatorSuffix.replaceFirstIn(name, "").trim)
        .flatMap { person =>
          val directed = tmdb.personDirectorCredits(person)
          if (directed.nonEmpty) directed else tmdb.personWriterCredits(person)
        }.map(hit).distinctBy(_.tmdbId))
  }

  override def film(tmdbId: Int): Answer[Option[FilmFacts]] =
    answered(tmdb.fullDetails(tmdbId).map(d => FilmFacts(tmdbId, d.title, d.originalTitle, d.releaseYear,
      d.director, d.runtimeMinutes, d.countries)))
}
