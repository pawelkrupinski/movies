package tools

import services.identity.{Answer, CandidateQuery, DetailFacts, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures,
  IdentityResolver, Listing, TmdbIdentityLookups}
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
   *  through the wiring's recording chain — which is what files
   *  the answers into the leg's tree. `onLookup` hears each logical lookup's name as soon as it has
   *  been issued (they run one at a time), so a caller can attribute every request to it. With
   *  `recorded` (a hermetic replay of a marked tree), a lookup it does not name is not issued. */
  def over(w: ArchiveReplayWiring, onLookup: String => Unit = _ => (), recorded: Option[Set[String]] = None): Summary = {
    val normalizer = w.movieCache.normalizer
    run(Listing.corpus(w.archivedListings, normalizer), new TmdbIdentityLookups(w.tmdbClient, new services.enrichment.ImdbClient(w.identityLookupFetch), w.detailEnrichers), normalizer,
      onLookup, recorded = recorded)
  }

  /** Issue the resolver's whole query set against `lookups` — or, with `recorded`, the part of it
   *  those lookup names cover (the rest answered unknown and counted as `unrecorded`). */
  def run(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
          onLookup: String => Unit = _ => (), calibration: IdentityCalibration = IdentityCalibration.resolver,
          recorded: Option[Set[String]] = None): Summary = {
    val named = new Named(lookups, onLookup, recorded)
    val r = IdentityResolver.resolve(listings, named, normalizer, calibration)
    Summary(listings.size, named.details, r.unknownDetails, r.queries.size, r.unknownQueries, r.filmLookups, r.unknownFilms,
      named.unrecorded)
  }

  /** `inner`, announcing each lookup by name once it returns — and, with `recorded`, answering a
   *  lookup it does not name as unknown without asking `inner`. */
  private final class Named(inner: IdentityLookups, onLookup: String => Unit, recorded: Option[Set[String]]) extends IdentityLookups {
    var details    = 0
    var unrecorded = 0
    private def named[A](name: String)(answer: => Answer[A]): Answer[A] =
      val line = name.replaceAll("[\r\n]", " ") // one name per line of the marker
      if (recorded.exists(!_(line))) { unrecorded += 1; Answer.Unknown }
      else { val a = answer; onLookup(line); a }
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = {
      details += 1; named(s"detail ${l.venue} ${l.page.getOrElse("")}")(inner.detail(l))
    }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = named(s"query ${q.sortKey.replace('\u0000', ' ')}")(inner.candidates(q))
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = named(s"film $id")(inner.film(id))
  }
}
