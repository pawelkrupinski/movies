package services.identity.agreement

import services.identity.{ContentHash, Dependency, FamilyIndex, IdentityLookups, Listing, Resolution}
import services.movies.ListingKey
import tools.{MutableClock, UnmatchedClusters}

/**
 * The FINAL take's side of the family index ([[FamilyIndex]]): what the agreement stage read, settled to its fixpoint
 * after the model, as its verdicts' receipts record it ([[StoredVerdict.reads]]): every family answer (a venue's
 * programme, `VenueListings`; a search, a record), every TMDB and venue lookup (Broadcast's namesake records, a cast, a
 * description), every attempt the ask memory holds and the year -- each a [[Dependency]] with its digest. Plus every
 * lookup the stage made through its lookups ([[FamilyIndex]]'s recorder), receipted or not.
 *
 * A fresh stage per settle, unbudgeted: stored verdicts found by digest and apply budgets are PATH state, never inputs
 * (docs/design/identity-no-drift.md, "One family index") -- the property proves the take independent of them.
 */
object FamilyFinalReads {

  /** A settled stage: each listing's final take (`None` pending), the receipts its verdicts hold, and the stage itself,
   *  whose current reads a receipt is compared against ([[moved]]). */
  final case class Settled(takes: Map[ListingKey, Option[String]], receipts: Map[Dependency, Long], lookups: Map[Dependency, Long],
                           members: Map[ListingKey, Long],
                           stage: AgreementStage, source: IdentityLookups, listingOf: ListingKey => Option[Listing])

  /** `listings`, their model `resolution`, settled through a fresh stage over the recording's answers `reads`. */
  def settle(reads: scripts.IdentitySlice.Reads, listings: Seq[Listing], resolution: Resolution, lookups: IdentityLookups,
             recordedAt: java.time.Instant): Settled = {
    val clock    = new MutableClock(recordedAt)
    val answers  = new scripts.IdentityDrift.Answers(reads, scripts.IdentityDrift.Noted(reads.filed), clock, _ => None)
    val verdicts = new InMemoryAgreementVerdicts
    val recorded = new FamilyIndex.RecordingLookups(lookups)
    val stage    = answers.stage(recorded, verdicts, budgets = false)
    val byKey    = listings.map(l => l.key -> l).toMap
    val model    = UnmatchedClusters.resolutionOf(resolution.decisions.filter(_.members.exists(byKey.contains)))
    val result   = scripts.IdentityDrift.settle(stage, () => stage.apply(model, byKey.get, answers.store.version), answers, clock, _ => false,
      log = false)
    val pending  = scripts.IdentityDrift.pendingOf(result.decisions)
    val takes    = scripts.IdentityDrift.takesOf(result).collect { case (key, take) if byKey.contains(key) => key -> Option.when(!pending(key))(take.film) }
    Settled(takes, verdicts.all().flatMap(_.reads).toMap, recorded.reads, listings.map(l => l.key -> ContentHash.of(l)).toMap, stage,
      lookups, byKey.get)
  }

  /** What `read`, a receipt's, digests to in `now`'s stage. */
  def digestNow(read: Dependency, now: Settled): Option[Long] = now.stage.digestNow(read, now.listingOf)

  /** What of `was`'s reads digests otherwise in `now`: a receipted read through `now`'s stage (a filed answer, an attempt,
   *  the year, a lookup), every lookup through `now`'s lookups, and each member's whole listing (its days, poster, names). `Asked` names a question to read again on its filing,
   *  no value: its answer is its own receipt entry. */
  def moved(was: Settled, now: Settled): Seq[String] =
    was.members.toSeq.collect { case (key, digest) if !now.listingOf(key).map(ContentHash.of).contains(digest) => s"member|$key" } ++
    was.receipts.toSeq.collect {
      case (read, digest) if !read.isInstanceOf[Dependency.Asked] && !digestNow(read, now).contains(digest) => Dependency.encode(read)
    } ++ was.lookups.toSeq.collect {
      case (read, digest) if !FamilyIndex.Lookups.digestIn(read, now.source, now.listingOf).contains(digest) => "lookup|" + Dependency.encode(read)
    }
}
