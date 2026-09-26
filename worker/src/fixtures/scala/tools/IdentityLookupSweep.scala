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
                           films: Int, filmsUnanswered: Int) {
    override def toString: String =
      s"$listings listing(s): $details detail page(s) ($detailsUnanswered unanswered), $queries candidate quer(ies) " +
        s"($queriesUnanswered unanswered), $films film record(s) ($filmsUnanswered unanswered)"
  }

  def enabledIn(configuration: settings.ProcessConfiguration): Boolean = configuration.identityLookupSweep.value

  /** Left at the root of a tree whose recording ran the sweep: the tree answers the resolver's
   *  query set, so a HERMETIC leg replaying it runs the sweep too — and fails on any gap. That
   *  is what keeps the phase-1 gate enforced by every verdict leg without anyone turning it on,
   *  and without failing a leg that replays a tree recorded before the sweep existed.
   *
   *  VERSIONED by the query set: `-v2` is the resolver's own set (calibrated search shapes,
   *  filmographies, identity records). A tree marked for the earlier set (`.identity-lookups`,
   *  the per-evidence `resolveStagingRecord`) does not answer it, so its hermetic legs do not run
   *  the sweep until a recording re-marks the tree. */
  val RecordedMarker = ".identity-lookups-v2"

  /** Whether a leg runs the sweep: when asked to, or when it replays a tree recorded with it. */
  def runsIn(requested: Boolean, hermetic: Boolean, treeRoot: java.nio.file.Path): Boolean =
    requested || (hermetic && java.nio.file.Files.exists(treeRoot.resolve(RecordedMarker)))

  /** Mark `treeRoot` as recorded with the sweep — by a RECORDING leg, once the sweep has run. */
  def markRecorded(treeRoot: java.nio.file.Path): Unit = {
    java.nio.file.Files.createDirectories(treeRoot)
    java.nio.file.Files.writeString(treeRoot.resolve(RecordedMarker),
      "the identity resolver's query set (IdentityResolver over TmdbIdentityLookups) is recorded in this tree\n")
    ()
  }


  /** The sweep over a booted replay wiring: its archived listings, its venues' detail enrichers
   *  and its TMDB client, both fetching through the wiring's recording chain — which is what files
   *  the answers into the leg's tree. `onLookup` hears each logical lookup's name as soon as it has
   *  been issued (they run one at a time), so a caller can attribute every request to it. */
  def over(w: ArchiveReplayWiring, onLookup: String => Unit = _ => ()): Summary = {
    val normalizer = w.movieCache.normalizer
    run(Listing.corpus(w.archivedListings, normalizer), new TmdbIdentityLookups(w.tmdbClient, w.detailEnrichers), normalizer, onLookup)
  }

  /** Issue the resolver's whole query set against `lookups`. */
  def run(listings: Seq[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
          onLookup: String => Unit = _ => (), calibration: IdentityCalibration = IdentityCalibration.default): Summary = {
    val named = new Named(lookups, onLookup)
    val r = IdentityResolver.resolve(listings, named, normalizer, calibration)
    Summary(listings.size, named.details, r.unknownDetails, r.queries.size, r.unknownQueries, r.filmLookups, r.unknownFilms)
  }

  /** `inner`, announcing each lookup by name once it returns. */
  private final class Named(inner: IdentityLookups, onLookup: String => Unit) extends IdentityLookups {
    var details = 0
    private def named[A](name: String)(answer: Answer[A]): Answer[A] = { onLookup(name); answer }
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = {
      details += 1; named(s"detail ${l.venue} ${l.page.getOrElse("")}")(inner.detail(l))
    }
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = named(s"query ${q.sortKey.replace('\u0000', ' ')}")(inner.candidates(q))
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = named(s"film $id")(inner.film(id))
  }
}
