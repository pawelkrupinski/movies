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
                           films: Int, filmsUnanswered: Int, unrecorded: Int = 0) {
    override def toString: String =
      s"$listings listing(s): $details detail page(s) ($detailsUnanswered unanswered), $queries candidate quer(ies) " +
        s"($queriesUnanswered unanswered), $films film record(s) ($filmsUnanswered unanswered)" +
        (if (unrecorded > 0) s"; $unrecorded lookup(s) the tree was recorded without, not asked — the resolver's " +
          "query set moved since the recording, and the next recording files them" else "")
  }

  def enabledIn(configuration: settings.ProcessConfiguration): Boolean = configuration.identityLookupSweep.value

  /** Left at the root of a tree whose recording ran the sweep, holding the NAME of every lookup the
   *  recording asked, one per line. A HERMETIC leg replaying the tree runs the sweep too, and fails
   *  on any gap — which keeps the phase-1 gate enforced by every verdict leg without anyone turning
   *  it on.
   *
   *  The list is what versions the mark by the query set. A hermetic leg asks only the lookups it
   *  names: a question the resolver has learned to ask since the recording (a new search shape, a
   *  season query) is answered unknown and counted as `unrecorded`, never requested. So a change to
   *  the resolver's queries can never turn a verdict leg red on a tree recorded before it, and never
   *  makes the leg issue a request its recording did not — the next recording asks and files it.
   *  Before the list (`-v2` and the unversioned `.identity-lookups`) the version was a number bumped
   *  by hand, and a query-set change that forgot to bump it failed every hermetic leg until the
   *  nightly recording caught up; a tree carrying only such a mark does not run the sweep. */
  val RecordedMarker = ".identity-lookups-v3"

  /** Whether a leg runs the sweep: when asked to, or when it replays a tree recorded with it. */
  def runsIn(requested: Boolean, hermetic: Boolean, treeRoot: java.nio.file.Path): Boolean =
    requested || (hermetic && java.nio.file.Files.exists(treeRoot.resolve(RecordedMarker)))

  /** The lookups `treeRoot`'s recording asked (its [[RecordedMarker]]), when it was recorded with the sweep. */
  def recordedIn(treeRoot: java.nio.file.Path): Option[Set[String]] = {
    val marker = treeRoot.resolve(RecordedMarker)
    Option.when(java.nio.file.Files.exists(marker))(
      scala.jdk.CollectionConverters.ListHasAsScala(java.nio.file.Files.readAllLines(marker)).asScala.filter(_.nonEmpty).toSet)
  }

  /** Mark `treeRoot` as recorded with the sweep that asked `asked` — by a RECORDING leg, once the
   *  sweep has run. A tree is recorded by more than one leg (the sample, then the full leg over the
   *  same tree), so the names already there are kept: the tree answers every one of them. */
  def markRecorded(treeRoot: java.nio.file.Path, asked: Iterable[String]): Unit = {
    java.nio.file.Files.createDirectories(treeRoot)
    val names = (recordedIn(treeRoot).getOrElse(Set.empty) ++ asked).toSeq.sorted
    java.nio.file.Files.writeString(treeRoot.resolve(RecordedMarker), names.mkString("", "\n", "\n"))
    ()
  }


  /** The sweep over a booted replay wiring: its archived listings, its venues' detail enrichers,
   *  its TMDB client and an IMDb suggestion client over the TMDB client's fetch, all fetching
   *  through the wiring's recording chain — which is what files the answers into the leg's tree.
   *  `onLookup` hears each logical lookup's name as soon as the resolver has asked it. With
   *  `recorded` (a hermetic replay of a marked tree), a lookup it does not name is not issued.
   *
   *  `threads` above one asks each of the resolver's read phases side by side (its `prefetch`), the
   *  way production's take-up does: a recording leg meets every lookup its tree lacks live, and one
   *  at a time those cost the US leg 274 s of a sweep that replays hermetically in 41 s. The wiring's
   *  live chain still paces and gates each host (`RateLimitedHttpFetch`, `ThrottledHttpFetch`). One
   *  thread (the default) keeps every request on the thread of the lookup that made it, issued
   *  right before `onLookup` names it — which is how `IdentityQueryCoverage.RequestLog` attributes
   *  requests to lookups. */
  def over(w: ArchiveReplayWiring, onLookup: String => Unit = _ => (), recorded: Option[Set[String]] = None,
           threads: Int = 1): Summary = {
    val normalizer = w.movieCache.normalizer
    val lookups    = new TmdbIdentityLookups(w.tmdbClient, new services.enrichment.ImdbClient(w.identityLookupFetch), w.detailEnrichers)
    val pool = Option.when(threads > 1)(java.util.concurrent.Executors.newFixedThreadPool(threads,
      Thread.ofPlatform().daemon().name(s"identity-sweep-${w.country.code}-", 0).factory()))
    try run(Listing.corpus(w.archivedListings, normalizer), lookups, normalizer, onLookup, recorded = recorded, pool = pool)
    finally pool.foreach(_.shutdownNow())
  }

  /** How many of a read phase's lookups a convergence leg's sweep has in flight: the 5-10 a script
   *  against these services runs (`external-api-rate-limits`), under the live chain's host pacing. */
  val LookupThreads = 8

  /** Issue the resolver's whole query set against `lookups` — or, with `recorded`, the part of it
   *  those lookup names cover (the rest answered unknown and counted as `unrecorded`). */
  def run(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
          onLookup: String => Unit = _ => (), calibration: IdentityCalibration = IdentityCalibration.resolver,
          recorded: Option[Set[String]] = None, pool: Option[java.util.concurrent.ExecutorService] = None): Summary = {
    val named = new Named(pool.fold(lookups)(threads => new TrackedLookups(lookups, ObservationReads.Untracked, Some(threads))), onLookup, recorded)
    val r = IdentityResolver.resolve(listings, named, normalizer, calibration)
    Summary(listings.size, named.details, r.unknownDetails, r.queries.size, r.unknownQueries, r.filmLookups, r.unknownFilms,
      named.unrecorded)
  }

  /** `inner`, announcing each lookup by name once it returns — and, with `recorded`, answering a
   *  lookup it does not name as unknown without asking `inner`. A resolve's [[IdentityLookups.prefetch]]
   *  is passed on (to the pool [[run]] put under it), less what `recorded` leaves unasked and with a
   *  detail page several listings share named once — the resolver reads such a page for the first
   *  of them (`CandidateGeneration.detailOf`), which is the one kept. */
  private final class Named(inner: IdentityLookups, onLookup: String => Unit, recorded: Option[Set[String]]) extends IdentityLookups {
    var details    = 0
    var unrecorded = 0
    private def asks(name: String): Boolean = !recorded.exists(!_(name))
    private def named[A](name: String)(answer: => Answer[A]): Answer[A] =
      if (!asks(name)) { unrecorded += 1; Answer.Unknown }
      else { val a = answer; onLookup(name); a }
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], pages: Iterable[Listing]): Unit =
      inner.prefetch(queries.filter(q => asks(Named.query(q))), films.filter(id => asks(Named.film(id))),
        pages.toSeq.distinctBy(l => (l.venue, l.page)).filter(l => asks(Named.detail(l))))
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
