package services.identity
package adapters

import clients.TmdbClient
import models.Cinema
import services.cinemas.common.DetailEnricher
import services.enrichment.ImdbClient

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
 * an empty one, which would read as "no such film". Production builds a cut-over model's
 * live fallback over it (`StoredFirstLookups`), where lookups run side by side on the prefetch's
 * threads; the offline harness over a recorded replay (`IdentityLookupSweep`), one
 * lookup at a time or, in a convergence leg, on its pool of prefetch threads.
 */
final class TmdbIdentityLookups(tmdb: TmdbClient, imdb: ImdbClient, enrichers: Seq[DetailEnricher],
                                gaps: TmdbIdentityLookups.Gaps = TmdbIdentityLookups.NoGaps, oneReadPerTitle: Boolean = false)
    extends IdentityLookups {

  private val venueDetails = new VenueDetails(enrichers, gaps)

  private def answered[A](read: => A): Answer[A] = gaps.answered(read)

  override def hasDetail(listing: Listing): Boolean = venueDetails.hasDetail(listing)

  override def detail(listing: Listing): Answer[Option[DetailFacts]] = venueDetails.detail(listing)

  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = query match {
    case CandidateQuery.Title(text)    =>
      // TMDB's OWN order: a film's rank in it is the `search.rank` measure, fitted on that order.
      answered(tmdb.searchAsRanked(text).getOrElse(throw new IllegalStateException("no TMDB key")).map(TmdbIdentityLookups.hitOf))
    case CandidateQuery.Director(name)    => directed(name)
    case CandidateQuery.TitleCredit(title) =>
      // the director IMDb suggests for the very title, named whole by its closing bracket — his films, as a credit's walk
      suggested(title).flatMap(s => TitleCredit.named(title, s.directors).fold[Answer[Seq[Hit]]](Answer.Known(Nil))(directed))
    case CandidateQuery.Imdb(title)       =>
      suggestedMovies(title).flatMap(movies =>
        answered(ImdbClient.suggested(movies).flatMap(tmdb.findByImdbId).map(TmdbIdentityLookups.hitOf).distinctBy(_.tmdbId)))
    case CandidateQuery.ImdbTitled(title) =>
      suggestedMovies(title).flatMap(movies => answered {
        val found = ImdbClient.titled(title, movies, id => Some(imdb.titlesOf(id))).getOrElse(Nil).map(tt => tt -> tmdb.findByImdbId(tt).take(1).toSeq.map(TmdbIdentityLookups.hitOf))
        TmdbIdentityLookups.titledHits(found.map(_._2), TmdbIdentityLookups.fallbackHits(found.collect { case (tt, Nil) => tt }, movies))
      })
  }

  // A title's IMDb suggestions answer three questions — the films IMDb suggests (`Imdb`), those it lists under the title
  // (`ImdbTitled`) and the director its closing bracket credits (`TitleCredit`) — which a prefetch asks side by side:
  // one read serves every question asking while it runs, AS ANSWERED (a read that met a gap or threw is Unknown to all
  // of them, never an empty list). Nothing is kept once it returns: what was answered is filed in the TMDB store, and
  // what was not is remembered per question, by `QuestionMemory`. Unless `oneReadPerTitle`: a lookup fill's round, made
  // for the round alone, asks a title's questions one after another and owes one ask per question — its reads serve
  // every question of the round.
  private val reading = new java.util.concurrent.ConcurrentHashMap[String, java.util.concurrent.CompletableFuture[Answer[ImdbClient.Suggested]]]()
  private def suggested(title: String): Answer[ImdbClient.Suggested] = {
    val mine    = new java.util.concurrent.CompletableFuture[Answer[ImdbClient.Suggested]]()
    val running = reading.putIfAbsent(title, mine)
    if (running ne null) running.join()
    else try { val read = answered(imdb.suggestionsFor(title)); mine.complete(read); read }
    finally { mine.complete(Answer.Unknown); if (!oneReadPerTitle) reading.remove(title, mine) }   // a read that threw: Unknown to its waiters
  }
  private def suggestedMovies(title: String): Answer[Seq[ImdbClient.Suggestion]] = suggested(title).map(_.movies)

  /** Every person the name could mean, each with what they directed — or, with no directing credit, wrote (a venue may
   *  print the writer): the walk the pipeline makes, without its pick. */
  private def directed(name: String): Answer[Seq[Hit]] =
    answered(tmdb.findPersonCandidates(CandidateQuery.personName(name))
      .flatMap(tmdb.personFilmography).map(TmdbIdentityLookups.hitOf).distinctBy(_.tmdbId))

  override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
    if (FallbackIds.isFallback(tmdbId)) FallbackIds.imdbId(tmdbId).fold[Answer[Option[IdentityMeasures.Film]]](Answer.Known(None))(tt => answered(imdb.identityRecord(tt)))
    else answered(tmdb.identityRecord(tmdbId))

  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] =
    if (FallbackIds.isFallback(tmdbId)) Answer.Known(None) else answered(tmdb.identityCast(tmdbId))

  override def overview(tmdbId: Int): Answer[Option[ProseWords]] =
    if (FallbackIds.isFallback(tmdbId)) Answer.Known(None) else answered(tmdb.identityOverview(tmdbId))
}

object TmdbIdentityLookups {
  /** The films IMDb lists under a title, each found in TMDB by its IMDb id — none at all when one of them is not:
   *  the title then names a film TMDB has no record of beside the one it has (Hayakawa's 2025 "Renoir" beside
   *  Bourdos's 2012 one), and the one found is not the ONE film IMDb lists under it. ONE reading for the live
   *  lookups and the store's. */
  private def everyTitled(found: Seq[Seq[Hit]]): Seq[Hit] = if (found.exists(_.isEmpty)) Nil else found.flatten.distinctBy(_.tmdbId)

  /** An IMDb-titled answer: the TMDB films found ([[everyTitled]]), and the ones TMDB holds no record of as fallback
   *  candidates ([[fallbackHits]]) — which no TMDB rule weighs. ONE reading for the live lookups and the store's. */
  def titledHits(found: Seq[Seq[Hit]], fallbacks: Seq[Hit]): Seq[Hit] = everyTitled(found) ++ fallbacks

  /** The IMDb titles `ids` (those TMDB holds no record of) as fallback candidates, each as IMDb's suggestion names it —
   *  no popularity, since IMDb's suggestion order is no TMDB popularity. ONE reading for the live lookups and the store's. */
  def fallbackHits(ids: Seq[String], movies: Seq[ImdbClient.Suggestion]): Seq[Hit] =
    ids.distinct.flatMap(tt => FallbackIds.ofImdbId(tt).map { id =>
      val named = movies.find(_.id == tt)
      Hit(id, named.flatMap(_.title).getOrElse(tt), None, named.flatMap(_.year), 0.0)
    })

  /** A TMDB film row as the model's candidate — from a live answer here, or a normalized one. */
  def hitOf(r: TmdbClient.SearchResult): Hit = Hit(r.id, r.title, r.originalTitle, r.releaseYear, r.popularity)

  /** `read` as an [[Answer]]: `Unknown` when it threw or met a gap in its source. */
  trait Gaps {
    def answered[A](read: => A): Answer[A]
  }

  /** A source that answers every request it is asked (a live one, or the TMDB store with
   *  live fallbacks): only a throw is `Unknown`, and lookups read side by side. */
  object NoGaps extends Gaps {
    def answered[A](read: => A): Answer[A] = Try(read).fold(_ => Answer.Unknown, Answer.Known(_))
  }
}

/** A listing's venue detail through its cinema's enricher: the page's facts as the model reads them. */
final class VenueDetails(enrichers: Seq[DetailEnricher], gaps: TmdbIdentityLookups.Gaps) {
  private val enricherOf: Map[Cinema, DetailEnricher] = enrichers.map(e => e.cinema -> e).toMap

  def hasDetail(listing: Listing): Boolean = listing.page.isDefined && enricherOf.contains(listing.cinema)

  def detail(listing: Listing): Answer[Option[DetailFacts]] =
    (listing.page, enricherOf.get(listing.cinema)) match {
      case (Some(page), Some(e)) => gaps.answered(e.fetchFilmDetail(page).map(d =>
        DetailFacts(d.releaseYear, d.director.map(_.trim).filter(_.nonEmpty), d.runtimeMinutes, d.originalTitle, d.countries, d.synopsis, d.cast)))
      case _                     => Answer.Known(None)
    }
}

