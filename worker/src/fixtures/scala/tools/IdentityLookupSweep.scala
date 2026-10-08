package tools

import services.identity.{Answer, CandidateQuery, DetailFacts, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures,
  IdentityResolver, Listing, ObservationReads, TmdbIdentityLookups, TrackedLookups}
import services.movies.TitleNormalizer

/**
 * The identity resolver's QUERY SET over one corpus, issued once, so a recording pass captures
 * every answer the resolver will ask a replay for (docs/design/identity-resolver.md, "Recording
 * the resolver's queries").
 *
 * The sweep IS a resolve: it runs `IdentityResolver` itself over the corpus's raw listings, with
 * `TmdbIdentityLookups` over the wiring's recording chains. So it asks exactly what the resolver
 * asks — every listing's own detail page, every `CandidateQueries` search (the calibration's
 * `IdentityMeasures.searchQueries`, yearless) and director filmography, and the identity record
 * (`TmdbClient.identityRecord`) of every film any of them names — and nothing else. There is no
 * second list to drift from the first. A function of the listing SET alone (A1): never of arrival
 * order or of a memo.
 */
object IdentityLookupSweep {

  final case class Summary(listings: Int, details: Int, detailsUnanswered: Int, queries: Int, queriesUnanswered: Int,
                           films: Int, filmsUnanswered: Int) {
    override def toString: String =
      s"$listings listing(s): $details detail page(s) ($detailsUnanswered unanswered), $queries candidate quer(ies) " +
        s"($queriesUnanswered unanswered), $films film record(s) ($filmsUnanswered unanswered)"
  }

  def enabledIn(configuration: settings.ProcessConfiguration): Boolean = configuration.identityLookupSweep.value

  /** The sweep over a booted replay wiring: its archived listings, its venues' detail enrichers,
   *  its TMDB client and an IMDb suggestion client over the TMDB client's fetch, all fetching
   *  through the wiring's recording chain — which is what files the answers into the leg's tree.
   *  `onLookup` hears each logical lookup's name as soon as the resolver has asked it.
   *
   *  `threads` above one asks each of the resolver's read phases side by side (its `prefetch`), the
   *  way production's take-up does: a recording leg meets every lookup its tree lacks live, and one
   *  at a time those cost the US leg 274 s of a sweep that replays hermetically in 41 s. The wiring's
   *  live chain still paces and gates each host (`RateLimitedHttpFetch`, `ThrottledHttpFetch`). One
   *  thread (the default) keeps every request on the thread of the lookup that made it, issued
   *  right before `onLookup` names it — which is how `IdentityQueryCoverage.RequestLog` attributes
   *  requests to lookups. */
  def over(w: ArchiveReplayWiring, onLookup: String => Unit = _ => (), threads: Int = 1): Summary = {
    val normalizer = w.movieCache.normalizer
    val lookups    = new TmdbIdentityLookups(w.tmdbClient, new services.enrichment.ImdbClient(w.identityLookupFetch), w.detailEnrichers)
    val pool = Option.when(threads > 1)(java.util.concurrent.Executors.newFixedThreadPool(threads,
      Thread.ofPlatform().daemon().name(s"identity-sweep-${w.country.code}-", 0).factory()))
    try run(Listing.corpus(w.archivedListings, normalizer), lookups, normalizer, onLookup, pool = pool)
    finally pool.foreach(_.shutdownNow())
  }

  /** How many of a read phase's lookups a convergence leg's sweep has in flight: the 5-10 a script
   *  against these services runs (`external-api-rate-limits`), under the live chain's host pacing. */
  val LookupThreads = 8

  /** Issue the resolver's whole query set against `lookups`. */
  def run(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
          onLookup: String => Unit = _ => (), calibration: IdentityCalibration = IdentityCalibration.resolver,
          pool: Option[java.util.concurrent.ExecutorService] = None): Summary = {
    val named = new Named(pool.fold(lookups)(threads => new TrackedLookups(lookups, ObservationReads.Untracked, Some(threads))), onLookup)
    val r = IdentityResolver.resolve(listings, named, normalizer, calibration)
    Summary(listings.size, named.details, r.unknownDetails, r.queries.size, r.unknownQueries, r.filmLookups, r.unknownFilms)
  }

  /** `inner`, announcing each lookup by name once it returns. A resolve's [[IdentityLookups.prefetch]]
   *  is passed on (to the pool [[run]] put under it) with a detail page several listings share named
   *  once — the resolver reads such a page for the first of them (`CandidateGeneration.detailOf`),
   *  which is the one kept. */
  private final class Named(inner: IdentityLookups, onLookup: String => Unit) extends IdentityLookups {
    var details = 0
    private def named[A](name: String)(answer: => Answer[A]): Answer[A] = { val a = answer; onLookup(name); a }
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], pages: Iterable[Listing]): Unit =
      inner.prefetch(queries, films, pages.toSeq.distinctBy(l => (l.venue, l.page)))
    override def prefetchAnswered(): Unit = inner.prefetchAnswered()
    override def detail(l: Listing): Answer[Option[DetailFacts]] = {
      details += 1; named(Named.detail(l))(inner.detail(l))
    }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = named(Named.query(q))(inner.candidates(q))
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = named(Named.film(id))(inner.film(id))
  }

  /** Each lookup's name, one per line of the marker. */
  private object Named {
    private def line(name: String): String = name.replaceAll("[\r\n]", " ")
    def detail(l: Listing): String     = line(s"detail ${l.venue} ${l.page.getOrElse("")}")
    def query(q: CandidateQuery): String = line(s"query ${q.sortKey.replace('\u0000', ' ')}")
    def film(id: Int): String          = line(s"film $id")
  }
}
