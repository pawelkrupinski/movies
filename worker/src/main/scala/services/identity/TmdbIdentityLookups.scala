package services.identity

import clients.TmdbClient
import models.Cinema
import services.cinemas.common.DetailEnricher
import services.enrichment.ImdbClient
import services.movies.TmdbCandidateSearch

import scala.util.Try

/**
 * [[IdentityLookups]] answered by TMDB, IMDb's title suggestions and the venues' own detail pages
 * — the raw primitives only: one yearless title search's results, a person's filmography, the
 * films IMDb lists under a title found in TMDB by their IMDb ids, a film's record
 * (`TmdbClient.identityRecord`, parsed by the calibration's own `TmdbFilmRecord`). `tmdb` and `imdb`
 * draw from one fetch, so the store observes, and a replay answers, both alike. No choice between
 * candidates is made here (that is the resolver's score), and nothing is memoised across calls,
 * so an answer is a function of its argument and of what the source holds.
 *
 * A lookup that throws, or that met a request its source could not answer (`gaps`: a hermetic
 * replay's), is [[Answer.Unknown]], never an empty answer — `TmdbClient` turns a failed read into
 * an empty one, which would read as "no such film". Production builds it over the observation
 * store (`ObservedIdentityLookups`, `CutoverIdentityLookups`), where lookups run side by side on
 * the prefetch's threads; the offline harness over a recorded replay, one lookup at a time.
 */
final class TmdbIdentityLookups(tmdb: TmdbClient, imdb: ImdbClient, enrichers: Seq[DetailEnricher],
                                gaps: TmdbIdentityLookups.Gaps = TmdbIdentityLookups.NoGaps)
    extends IdentityLookups {

  private val enricherOf: Map[Cinema, DetailEnricher] = enrichers.map(e => e.cinema -> e).toMap

  private def answered[A](read: => A): Answer[A] = gaps.answered(read)

  private def hit(r: TmdbClient.SearchResult): Hit = Hit(r.id, r.title, r.originalTitle, r.releaseYear, r.popularity)

  override def hasDetail(listing: Listing): Boolean = listing.page.isDefined && enricherOf.contains(listing.cinema)

  override def detail(listing: Listing): Answer[Option[DetailFacts]] =
    (listing.page, enricherOf.get(listing.cinema)) match {
      case (Some(page), Some(e)) => answered(e.fetchFilmDetail(page).map(d =>
        DetailFacts(d.releaseYear, d.director.map(_.trim).filter(_.nonEmpty), d.runtimeMinutes, d.originalTitle, d.countries)))
      case _                     => Answer.Known(None)
    }

  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text)    =>
      // TMDB's OWN order: a film's rank in it is the `search.rank` measure, fitted on that order.
      answered(tmdb.searchAsRanked(text).getOrElse(throw new IllegalStateException("no TMDB key")).map(hit))
    case CandidateQuery.Director(name)    =>
      // Every person the name could mean, each with what they directed — or, with no directing
      // credit, wrote (a venue may print the writer): the walk the pipeline makes, without its pick.
      answered(tmdb.findPersonCandidates(TmdbCandidateSearch.ImdbDisambiguatorSuffix.replaceFirstIn(name, "").trim)
        .flatMap { person =>
          val directed = tmdb.personDirectorCredits(person)
          if (directed.nonEmpty) directed else tmdb.personWriterCredits(person)
        }.map(hit).distinctBy(_.tmdbId))
    case CandidateQuery.Imdb(title)       =>
      answered(imdb.titledIds(title).flatMap(tmdb.findByImdbId).map(hit).distinctBy(_.tmdbId))
  }

  override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = answered(tmdb.identityRecord(tmdbId))
}

object TmdbIdentityLookups {
  /** `read` as an [[Answer]]: `Unknown` when it threw or met a gap in its source. */
  trait Gaps {
    def answered[A](read: => A): Answer[A]
  }

  /** A source that answers every request it is asked (a live one, or the observation store with
   *  its own fallbacks): only a throw is `Unknown`, and lookups read side by side. */
  object NoGaps extends Gaps {
    def answered[A](read: => A): Answer[A] = Try(read).fold(_ => Answer.Unknown, Answer.Known(_))
  }

  /** A replay counting the requests it could not answer in one counter (`misses`): a lookup during
   *  which it grew met a gap. The counter is the replay's, not the lookup's, so lookups take turns. */
  final class CountedGaps(misses: () => Long) extends Gaps {
    def answered[A](read: => A): Answer[A] = synchronized {
      val before = misses()
      Try(read).toOption.filter(_ => misses() == before).fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
    }
  }
}
