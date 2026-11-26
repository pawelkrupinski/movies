package tools

import testsupport.SourceTrees
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Never key a decision by a mutable id (docs/design/identity-no-drift.md).
 *
 * A cluster's key, a listing's key, a family's number stay while what they name changes: a scrape reshapes a cluster
 * under the key it keeps, a listing's page moves under its key. State the identity code keeps BY such an id and decides
 * from is stale the moment the content moves, unless the value says what it was decided for. Each of these was that bug:
 *  - the agreement's inline asks, kept by cluster key: a reshaped cluster under its old key was never asked again (now
 *    `askedNow`, a set of the shapes' digests: no key at all);
 *  - verdicts found by key, not by the listings they were decided for (now `heldFor`, by `StoredVerdict.listings`);
 *  - the dirty index's `checked` by id, which a reshaped cluster inherited (now checked under a stamp, [[services.identity.vocabulary.Memo]]);
 *  - a cluster's waits and a correction's waits, kept by key: a cluster back to a shape whose verdict stands, or a
 *    reshaped take its budget left unread, went on waiting on what another shape asked (now `Waiting.listings`).
 *
 * The rule: every mutable map, set or Memo the identity code KEEPS in a class or object ([[StatefulFieldScan]]) is
 * reviewed here, each with what makes it safe:
 *  - [[Content]]: its key is the content, or a digest of it — a question, a query, a TMDB id, a decision object;
 *  - [[Digested]]: keyed by an id, and its value carries the digest (or the content) it was decided for, which its
 *    readers compare — the field is named and must exist on the value's type;
 *  - [[Name]]: a pure name or structure index (an id's members, a reverse index), moved with the structure, deciding
 *    nothing about content;
 *  - [[Pass]]: lives one apply, resolve or round — its owner is built per pass, or it is cleared at each;
 *  - [[Store]]: an in-memory store's rows, or a write-behind of them: persisted inputs, keyed by their own ids.
 * A map keyed by a listing or cluster key type is never [[Content]]. A new kept map fails here until it is reviewed;
 * one gone fails until its entry goes.
 */
class NoDecisionKeyedByMutableIdSpec extends AnyFlatSpec with Matchers {
  import StatefulFieldScan.Field

  private sealed trait Kept { def why: String }
  private final case class Content(why: String)                  extends Kept
  private final case class Digested(carrier: String, why: String) extends Kept
  private final case class Name(why: String)                     extends Kept
  private final case class Pass(why: String)                     extends Kept
  private final case class Store(why: String)                    extends Kept

  private val Roots = SourceTrees("identity/src/main/scala/services/identity", "worker/src/main/scala/services/identityadapters",
    "worker/src/main/scala/services/tmdb", "worker/src/main/scala/services/agreementasks", "worker/src/main/scala/services/identitystaging", "worker/src/main/scala/services/lookupfill", "worker/src/main/scala/services/identityvenuepages", "worker/src/main/scala/services/identityprojection", "worker/src/main/scala/services/identitymodel")

  /** Key types that are content by construction: what they name cannot change under them. */
  private val ContentKeys = Set("Question", "PosterQuestion", "CandidateQuery", "Dependency", "ResolverDecision",
    "MeasuredListing.TitleInputs", "(MeasuredListing.TitleInputs, Int)")

  /** Key types that name a listing or a cluster: what they name moves under them. */
  private val IdKey = """\b(?:ListingKey|ClusterId|NodeKey)\b""".r

  private val I = "identity/src/main/scala/services/identity/"
  private val A = I + "agreement/"

  private val PerResolve = "its owner is built per resolve"
  private val PerApply   = "a field of AgreementPass, built per apply"
  private val PerRefresh = "a field of LiveCorpus's Refresh, built per refresh"
  private val PerRound   = "a field of LookupFillBudget, built per fill round"
  private val PerDraft   = "a field of LeanVenueSlots, built per draft (ScopeDrafting)"
  private val Lookups    = "a field of Agreement's FamilyLookups, built per resolve"
  private val CorpusIndex = "LiveCorpus's structure index, moved as nodes are linked, unlinked and rebuilt"

  /** `file#field` → why the kept map is safe. */
  private val Reviewed: Map[String, Kept] = Map(
    // ── the agreement stage
    s"${A}ClusterKeys.scala#live"              -> Name("each live cluster key's model decision, re-keyed every apply"),
    s"${A}ClusterKeys.scala#firsts"            -> Store("the key store's rows: each live key's first listing"),
    s"${A}VerdictIndex.scala#memo"             -> Digested("listings", "a Memo: heldFor compares StoredVerdict.listings, and an entry is checked under the listings' digest"),
    s"${A}VerdictIndex.scala#byDigest"         -> Content("the held verdicts by their listings' digest"),
    s"${A}InlineAsker.scala#asked"             -> Content("the shapes (listings' digests) asked inline, no key"),
    s"${A}AgreementPass.scala#shapes"          -> Pass(PerApply),
    s"${A}ClusterVerdicts.scala#waiting"        -> Digested("listings", "Waiting.listings: a wait for another shape is dropped (verdictOf) and never read"),
    s"${A}AgreementCandidates.scala#candidateFilms" -> Digested("digest", "a Memo: CandidateFilms.digest compared, its receipt checked"),
    s"${A}StandingTakes.scala#settledTakes"    -> Digested("members", "ResolverDecision.members: a take settled for other listings under the key stands for none (standing)"),
    s"${A}StandingTakes.scala#holds"           -> Digested("members", "Hold.members: outcomeOf and waitingOn read a hold only for the members it was made for; standing drops a reshaped one"),
    s"${A}AgreementCorrections.scala#correctionWaits" -> Digested("listings", "Waiting.listings: a correction's waits for another shape of its take are dropped (corrected)"),
    s"${A}AgreementTakes.scala#takeWaits"       -> Digested("listings", "Waiting.listings: what an agreed take pending asked, read only for the cluster shape it was asked for (outcomeOf compares the decision's digest)"),
    s"${A}TmdbReadTracking.scala#recordsReading" -> Content("TMDB ids the apply read"),
    s"${A}TmdbReadTracking.scala#findsReading"   -> Content("the IMDb ids whose TMDB finds the apply read"),
    s"${A}AgreementPass.scala#seen"            -> Pass(PerApply),
    s"${A}AgreementPass.scala#correcting"      -> Pass(PerApply),
    s"${A}AgreementPass.scala#inline"          -> Pass(PerApply),
    s"${A}AgreementPass.scala#backfilled"      -> Pass(PerApply),
    s"${A}AgreementPass.scala#venues"          -> Pass("a field of AgreementPass.Read, built per apply: poster answers by URL"),
    s"${A}AgreementPass.scala#films"           -> Pass("a field of AgreementPass.Read, built per apply: poster answers by TMDB id"),
    s"${A}AgreementVerdicts.scala#kept"        -> Store("InMemoryAgreementVerdicts' rows, each carrying its listings' digest"),
    s"${A}AgreementKeys.scala#kept"            -> Store("InMemoryAgreementKeys' rows"),
    s"${A}FamilyVerdicts.scala#kept"           -> Digested("listings", "Kept.listings compared whole, and every answer it read by digest"),
    s"${A}FamilyLookups.scala#numbers"         -> Pass(Lookups),
    s"${A}FamilyLookups.scala#named"           -> Pass(Lookups),
    s"${A}FamilyLookups.scala#bestRank"        -> Pass(Lookups),
    s"${A}FamilyLookups.scala#read"            -> Pass(Lookups),
    // ── the Memo itself: each Memo a class keeps is reviewed where it is kept
    s"${I}vocabulary/Memo.scala#entries"                  -> Digested("value", "the Memo's own entries: each user's value carries its receipt, and is reviewed as that user's field"),
    s"${I}vocabulary/Memo.scala#checks"                   -> Digested("value", "each check holds the stamp (the caller's content digest) it was found standing under"),
    s"${I}vocabulary/Memo.scala#readers"                  -> Content("the dirty index: by dependency filing"),
    s"${I}vocabulary/Memo.scala#filings"                  -> Pass("the filings since a version, read once per pass (newPass clears it)"),
    // ── the family index: a measure, recorded per family resolve, deciding nothing
    s"${I}FamilyIndex.scala#values"   -> Pass("the reads one family's resolve made, by what they read (a context fact or a lookup); its recorder is built per family resolve"),
    s"${I}RecordingLookups.scala#detailed" -> Pass("whether each member's venue publishes a detail page, as one family's resolve asked; its recorder is built per family resolve"),
    // ── the model
    s"${I}IncrementalResolver.scala#byKey"         -> Name("each held listing's current listing and family, kept current by hold/release/remember/forget"),
    s"${I}IncrementalResolver.scala#atVenue"       -> Name("each venue's held listing keys"),
    s"${I}IncrementalResolver.scala#families"      -> Digested("digest", "Family.digest of the corpus slice it read; update compares it"),
    s"${I}IncrementalResolver.scala#readersOf"     -> Name("reverse index: read key → families"),
    s"${I}IncrementalResolver.scala#filmReadersOf" -> Name("reverse index: film → families"),
    s"${I}IncrementalResolver.scala#familiesOfKey" -> Name("reverse index: block key → families"),
    s"${I}IncrementalResolver.scala#freedIds"      -> Name("family numbers free to reuse; forget clears every index entry of one"),
    s"${I}IncrementalResolver.scala#titlesOf"      -> Pass("a field of UpdateRounds, built per update: each settled family's region's title-form inputs, stored with it at settle"),
    s"${I}IdentityModelStore.scala#keys"           -> Pass("a field of MongoIdentityModelStore's TitleFields, built per decoded family document"),
    s"${I}TitleRuleChange.scala#keyed"             -> Pass("a field of RecordingTitleForms, built per region: the titles whose key form it read"),
    s"${I}TitleRuleChange.scala#searched"          -> Pass("a field of RecordingTitleForms, built per region: the titles whose search form it read"),
    s"${I}TitleRuleChange.scala#keyAlike"          -> Pass("a field of TitleRuleChange, built per restore: by title, whether its key form reads alike under both rules"),
    s"${I}TitleRuleChange.scala#searchAlike"       -> Pass("a field of TitleRuleChange, built per restore: by title, whether its search form reads alike under both rules"),
    s"${I}TitleRuleChange.scala#firedWas"          -> Pass("a field of TitleRuleChange, built per restore: firedRules' memo of the cleaned titles' tiers under the old rules"),
    s"${I}TitleRuleChange.scala#firedNow"          -> Pass("a field of TitleRuleChange, built per restore: firedRules' memo of the cleaned titles' tiers under the new rules"),
    s"${I}TakeFlips.scala#last"          -> Name("a replay's record of each listing's last served take: it watches takes, decides none"),
    s"${I}TakeFlips.scala#changes"       -> Name("a replay's record of each listing's served takes, the flip check's input: it decides no take"),
    s"${I}LiveCorpus.scala#listings"     -> Name("each listing's node, moved as it arrives and leaves"),
    s"${I}LiveCorpus.scala#heads"        -> Name("each node's head listing and its evidence, derived again whenever the head arrives"),
    s"${I}LiveCorpus.scala#members"      -> Name("each node's member listings"),
    s"${I}LiveCorpus.scala#nodes"        -> Name("each node, rebuilt for every touched, re-asked or re-recorded node"),
    s"${I}LiveCorpus.scala#bestHits"     -> Content("by TMDB id"),
    s"${I}LiveCorpus.scala#records"      -> Content("TMDB records by TMDB id"),
    s"${I}LiveCorpus.scala#base"         -> Content("candidates by TMDB id"),
    s"${I}LiveCorpus.scala#reachers"     -> Name("reverse index: TMDB id → nodes reaching it"),
    s"${I}LiveCorpus.scala#seasonNodes"  -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#seasonFilms"  -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#translatedBy" -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#originsOfKey" -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#keysOfOrigin" -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#filmsByKey"   -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#venueTitlesOf" -> Content("by TMDB id"),
    s"${I}LiveCorpus.scala#titledBy"     -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#candidates"   -> Content("candidates by TMDB id"),
    s"${I}LiveCorpus.scala#billingsOf"   -> Name("each node's billings, rebuilt for the nodes rebuilt (rebill)"),
    s"${I}LiveCorpus.scala#byBanner"     -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#houseOf"      -> Name("banner → house, learnt again for every banner rebilled"),
    s"${I}LiveCorpus.scala#rankingOf"    -> Name("banner → contenders, learnt again for every banner rebilled"),
    s"${I}LiveCorpus.scala#groups"       -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#wholes"       -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#spreads"      -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#reachedByKey" -> Name(CorpusIndex),
    s"${I}LiveCorpus.scala#groupView"    -> Name("a view of `groups`, dropped for every title key refreshed"),
    s"${I}LiveCorpus.scala#reachedView"  -> Name("a view of `reachedByKey`, dropped for every title key refreshed"),
    s"${I}LiveCorpus.scala#films"        -> Pass(PerRefresh),
    s"${I}LiveCorpus.scala#titleKeys"    -> Pass(PerRefresh),
    s"${I}LiveCorpus.scala#segments"     -> Pass(PerRefresh),
    s"${I}LiveCorpus.scala#moved"        -> Pass(PerRefresh),
    s"${I}LiveCorpus.scala#bannersBefore" -> Pass(PerRefresh),
    s"${I}vocabulary/ObservationReads.scala#readers" -> Name("store key → the questions that read it, forgotten when a question is asked again"),
    s"${I}TrackedLookups.scala#films"   -> Content("prefetched records by TMDB id"),
    s"${I}TrackedLookups.scala#details" -> Pass("prefetched venue pages: cleared at each prefetch and consumed in the same read"),
    // ── a resolve's scratch
    // ── one solve, update or stored decision's read: the state each builds and drops
    s"${I}ConstraintSolver.scala#cannot"    -> Pass("a field of ConstraintSolver's Partition, built per solve: each root's cannot-links"),
    s"${I}ConstraintSolver.scala#film"      -> Pass("a field of ConstraintSolver's Partition, built per solve: each root's film"),
    s"${I}ConstraintSolver.scala#ambiguous" -> Pass("a field of ConstraintSolver's TieredJoins, built per solve"),
    s"${I}ConstraintSolver.scala#sides"     -> Pass("a field of ConstraintSolver's TieredJoins, built per solve"),
    s"${I}IncrementalResolver.scala#settled"      -> Pass("a field of IncrementalResolver's UpdateRounds, built per update: the families it settled"),
    s"${I}IncrementalResolver.scala#settledOfKey" -> Pass("a field of IncrementalResolver's UpdateRounds, built per update: its settled families by block key"),
    s"${I}ResolverDecisionBson.scala#agreed"      -> Pass("a field of ResolverDecisionBson's DecisionFields, built per stored decision read"),
    s"${I}ResolverDecisionBson.scala#filmTitled"  -> Pass("a field of ResolverDecisionBson's DecisionFields, built per stored decision read"),
    s"${I}FeedLinkOverrule.scala#overruledMemo" -> Pass("a field of a FamilyScope's FeedLinkOverrule, built with its scope per resolve"),
    s"${I}FamilyScope.scala#reachMemo"      -> Pass(PerResolve),
    s"${I}FamilyScope.scala#memo"           -> Pass(PerResolve),
    s"${I}FamilyScope.scala#takenAloneMemo" -> Pass(PerResolve),
    s"${I}BillingDenials.scala#billingMemo" -> Pass("a field of a FamilyScope's BillingDenials (its NodeDenials'), built with its scope per resolve"),
    s"${I}FamilyScope.scala#pooledMemo"     -> Pass(PerResolve),
    s"${I}NodeDenials.scala#denialMemo"     -> Pass("a field of a FamilyScope's NodeDenials, built with its scope per resolve"),
    s"${I}VenueBacking.scala#memo"          -> Pass("one per resolve or calibration pass"),
    s"${I}VenueBacking.scala#read"          -> Pass("one per resolve or calibration pass"),
    s"${I}CandidateGeneration.scala#details" -> Pass(PerResolve),
    s"${I}CandidateGeneration.scala#held"    -> Pass(PerResolve),
    s"${I}NamingPieces.scala#piecesOf"       -> Pass(PerResolve),
    s"${I}NamingPieces.scala#placesOfVenue"   -> Pass(PerResolve),
    s"${I}ListingCannotLinks.scala#apart"       -> Pass("a field of ConstraintEdges' ListingCannotLinks, built with it per ResolverRun"),
    s"${I}ListingCannotLinks.scala#eventOfNode" -> Pass("a field of ConstraintEdges' ListingCannotLinks, built with it per ResolverRun; a node's event read off its own listings' titles"),
    s"${A}UnifiedEvidence.scala#ids"         -> Pass("a field of Building, built per contenders call"),
    s"${I}TitleLinks.scala#sanitizedOf"      -> Content("by the title text"),
    s"${I}TitleLinks.scala#searchFormOf"     -> Content("by the title text"),
    s"${I}TitleLinks.scala#keysOf"           -> Content("by the title text"),
    s"${I}TitleLinks.scala#formsOf"          -> Pass("one TitleLinks per resolve's stages: each node's title forms (TitleRelation), for that resolve"),
    // ── the projection
    s"${I}LiveProjectionIndex.scala#venueListings" -> Name("each venue's listings as last read: compared by value to see a move"),
    s"${I}ListingEntries.scala#entries"            -> Name("each listing's entry, kept equal to the builder's index (ScopedProjectionEquivalenceSpec)"),
    s"${I}LiveProjectionIndex.scala#slots"         -> Name("each slot's published listings and stored films, moved with entries"),
    s"${I}LiveProjectionIndex.scala#decisionsOf"   -> Name("each cluster's current decisions"),
    s"${I}LiveProjectionIndex.scala#afterWrites"   -> Name("the listings moved by writes, consumed by the next update"),
    s"${I}LeanVenueSlots.scala#slotsByFilm"       -> Pass(PerDraft),
    s"${I}LeanVenueSlots.scala#venueSlots"        -> Pass(PerDraft),
    s"${I}VenueSlots.scala#current"                -> Content("by the hash of the venue's rows, priors, keys and pages"),
    s"${I}VenueSlots.scala#seenNow"                -> Content("by the venue's listings, its value the full content key"),
    s"${I}IdentityProjectionPlan.scala#drafted"    -> Digested("members", "FilmShape.members and its venues' inputs are compared before a shape is reused"),
    s"${I}IdentityProjectionPlan.scala#priorsUsed" -> Digested("_1", "the MovieRecord it was read from, reused only while the same object"),
    s"${I}IdentityListingIntake.scala#dirty"       -> Name("the venues written since the last read"),
    // ── stores and their write-behinds
    s"${I}IdentityTraceStore.scala#removed"   -> Store("write-behind: the trace rows to delete"),
    s"${I}IdentityTraceStore.scala#pending"   -> Store("write-behind: the trace rows to write"),
    s"${I}IdentityModelStore.scala#kept"      -> Store("InMemoryIdentityModelStore's rows, each carrying its digest"),
    s"${I}staging/IdentityStaging.scala#held" -> Store("InMemoryStagingDocuments' rows"),
    s"${I}measures/ListingFilmMeasures.scala#Slots"    -> Content("a constant table, never written"),
    s"${I}ProjectionScope.scala#seen"       -> Pass("one close's worklist of what it reached, dropped when the close returns"),
    s"worker/src/main/scala/services/identityadapters/IdentityModelService.scala#venues"       -> Store("the latest scrape per venue, until drained"),
    s"worker/src/main/scala/services/identityadapters/IdentityModelService.scala#observations" -> Pass("an event queue, drained by the model's thread"),
    s"worker/src/main/scala/services/identityadapters/ListingRetainer.scala#departed"          -> Store("every listing that left, its last-seen facts and departure under its own key (an input, identity_retained in memory): replaced whole when it leaves again, removed when billed again"),
    s"worker/src/main/scala/services/identityadapters/ListingRetainer.scala#retained"          -> Store("the departed listings the model holds as kept (ListingRetention.retainedAt, recomputed whole each drain): membership only, the listing itself the engine's, by its key"),
    s"worker/src/main/scala/services/identityadapters/ModelInputGate.scala#pending"            -> Store("the listings the input gate holds (an input, identity_pending in memory): each its newest version itself, its ready version and what it waits on under its own key; whether it is ready is read afresh from the answers on every drain"),
    s"worker/src/main/scala/services/identityadapters/ModelInputGate.scala#unwritten"          -> Store("the keys whose last identity_pending write failed: membership only, each written again from `pending` (or released) by the next write, cleared once one lands"),
    s"worker/src/main/scala/services/lookupfill/StoredTmdbLookups.scala#held"   -> Content("by the store document's id, a TMDB id or a question; released around every prefetch"),
    s"worker/src/main/scala/services/lookupfill/StoredTmdbLookups.scala#films"  -> Content("by TMDB id, released with held"),
    s"worker/src/main/scala/services/lookupfill/StoredTmdbLookups.scala#hits"   -> Content("by TMDB id, released with held"),
    s"worker/src/main/scala/services/lookupfill/TmdbIdentityLookups.scala#reading" -> Content("single-flight by title, removed when the read completes"),
    s"worker/src/main/scala/services/tmdb/TmdbStore.scala#byKind"         -> Store("InMemoryTmdbDocuments' rows"),
    s"worker/src/main/scala/services/identityvenuepages/VenuePageIndex.scala#pending"   -> Name("the pages to read again, removed as each is read"),
    s"worker/src/main/scala/services/lookupfill/LookupFill.scala#overloaded"    -> Pass(PerRound),
    s"worker/src/main/scala/services/lookupfill/LookupFill.scala#wanted"        -> Pass(PerRound),
    s"worker/src/main/scala/services/lookupfill/LookupFill.scala#byHost"        -> Pass(PerRound),
    s"${I}vocabulary/EventTrigger.scala#bursts"              -> Pass("each debounce window's burst since the last run: timing, no decision"),
    s"${I}IdentityListingIntake.scala#entries"    -> Store("each venue's latest listings as written, with their stamp"),
    s"${I}IdentityProjectionPlan.scala#kept"      -> Digested("members", "as `drafted`, the shapes it commits"),
    s"${I}IdentityProjectionPlan.scala#priorsHeld" -> Digested("_1", "as `priorsUsed`, the priors it commits"),
    s"${I}LiveProjectionIndex.scala#storedById"   -> Store("the stored film rows by their id"),
    s"${I}LiveProjectionIndex.scala#listingsOf"   -> Name("each stored film's listings"),
    s"${I}LiveProjectionIndex.scala#clusters"     -> Name("each cluster's current structure"),
    s"${I}LiveProjectionIndex.scala#replaced"     -> Pass("replaced whole by each update: the decisions it replaced"),
    s"${I}ListingEntries.scala#owners"            -> Name("a field of an Entry: the stored films holding its key"),
    s"${I}LiveProjectionIndex.scala#keys"         -> Name("a field of a Slot: the listings published at it"),
    s"${I}LiveProjectionIndex.scala#films"        -> Name("a field of a Slot: the stored films with a slot there"),
    s"${I}VenueSlotFingerprints.scala#kept"       -> Content("venue slot fingerprints"),
    s"${I}VenueSlots.scala#recorded"              -> Content("venue slot fingerprints"),
    s"worker/src/main/scala/services/identityadapters/RecordedSlotFingerprints.scala#recorded" -> Content("venue slot fingerprints"),
    s"worker/src/main/scala/services/identityadapters/ProjectionWrite.scala#lastUnwritten"    -> Pass("the films the last write left unwritten, replaced by each"),
    s"worker/src/main/scala/services/identitystaging/IdentityStagingPublisher.scala#published" -> Store("what the last publish left in the store, compared by value"),
    s"worker/src/main/scala/services/identityadapters/RuleFiringCount.scala#titleRules"       -> Content("by venue and raw title: the title is the content its rules are read from"),
    s"worker/src/main/scala/services/identityvenuepages/VenuePageIndex.scala#answers"           -> Store("each page's answer as read, compared by value to see it move"),
    s"worker/src/main/scala/services/lookupfill/LookupGaps.scala#kinds"         -> Content("a counter per gap kind"),
    s"worker/src/main/scala/services/tmdb/IdentityHistoryReplay.scala#written" -> Store("a replay's own writes, by document id: it decides nothing, the replay is built per moment"),
    s"worker/src/main/scala/services/tmdb/FailureHistory.scala#failedAt"     -> Content("by redacted URL and failure: when each was last kept, to keep one a minute; decides no take"),
    s"worker/src/main/scala/services/identityadapters/IdentityModelGauges.scala#holding" -> Content("by country and hold (gate, stage): the held gauges' last count and oldest wait; decides no take")
  )

  private lazy val fields: Seq[Field] = ScalaSourceScan.scalaFiles(Roots).flatMap(StatefulFieldScan.fieldsIn)

  private def contentKeyed(f: Field): Boolean = ContentKeys(f.key)

  "the kept-state scan" should "find a class's maps, sets and Memos, keyed as declared, and no method's locals" in {
    val src =
      """final class Stage(keyStore: Keys = new InMemoryKeys, n: Int = 2) extends A, B {
        |  private val waiting = TrieMap.empty[String, Stage.Waiting]
        |  @volatile private var keyed = new java.util.IdentityHashMap[ResolverDecision, String]()
        |  private[agreement] lazy val held: Memo[String, StoredVerdict] = {
        |    val memo = new Memo[String, StoredVerdict](_.reads)
        |    memo
        |  }
        |  private var awaiting = Set.empty[Question]
        |  var validated = services.identity.vocabulary.Memo.Validation.None
        |  def apply(): Unit = {
        |    val local = mutable.HashMap.empty[ListingKey, Int]
        |    val fresh = new Object {
        |      private val inner = mutable.Set.empty[String]
        |    }
        |    if (local.isEmpty) { val scratch = mutable.Set.empty[Int] }
        |  }
        |  private val byVenue = scala.collection.mutable.LongMap.empty[Int]
        |}
        |object Stage {
        |  final class Pass(v: Long, f: Int => Int = _ + 1) extends Base(v) with Seen {
        |    val seen = mutable.Set.empty[String]
        |  }
        |}""".stripMargin
    StatefulFieldScan.fieldsIn("S.scala", src).map(f => (f.name, f.key, f.value)) shouldBe Seq(
      ("waiting", "String", "Stage.Waiting"), ("keyed", "ResolverDecision", "String"), ("held", "String", "StoredVerdict"),
      ("awaiting", "Question", "Question"), ("inner", "String", "String"), ("byVenue", "Long", "Int"), ("seen", "String", "String"))
  }

  "every map, set or Memo the identity code keeps" should "be reviewed: what makes its key, or its value, safe from a moved id" in {
    fields.nonEmpty shouldBe true
    val unreviewed = fields.filterNot(f => Reviewed.contains(f.id) || contentKeyed(f))
      .map(f => s"${f.path}:${f.line}: ${f.name} keyed by ${f.key}: ${f.text}")
    withClue("Kept state not reviewed. If it is keyed by a cluster, listing or family id and decides anything, make the value " +
      "carry the content digest it was decided for and compare it on read (Digested); else name why it is safe:\n" +
      unreviewed.mkString("\n") + "\n")(unreviewed shouldBe empty)
  }

  it should "never call a listing- or cluster-keyed map content-keyed" in {
    val wrong = fields.filter(f => IdKey.findFirstIn(f.key).isDefined)
      .filter(f => Reviewed.get(f.id).forall(_.isInstanceOf[Content]))
      .map(f => s"${f.path}:${f.line}: ${f.name} keyed by ${f.key}")
    withClue("A ListingKey / ClusterId / NodeKey names content that moves under it:\n" + wrong.mkString("\n") + "\n")(wrong shouldBe empty)
  }

  it should "carry, where it is Digested, the field it is said to carry" in {
    val sources = ScalaSourceScan.scalaFiles(Roots).map(ScalaSourceScan.codeOf).mkString("\n")
    val byId    = fields.map(f => f.id -> f).toMap
    val missing = Reviewed.toSeq.sortBy(_._1).collect { case (id, Digested(carrier, _)) if byId.contains(id) =>
      val f     = byId(id)
      val value = Option(f.value).filter(_ != "?")
      val ok = carrier match {
        case "value" => value.forall(v => v == "Long" || v == "V" || v.startsWith("(")) // the digest, or a Memo's generic V
        case "_1"    => value.exists(_.startsWith("("))
        case field   => value.exists { v =>
          val simple = v.split('.').last.trim
          s"""case class $simple\\b\\s*(?:\\[[^\\]]*\\])?\\(([^)]*)\\)""".r.findAllMatchIn(sources)
            .exists(m => s"""\\b$field\\s*:""".r.findFirstIn(m.group(1)).isDefined)
        }
      }
      Option.when(!ok)(s"$id: value ${value.getOrElse("?")} has no `$carrier`")
    }.flatten
    withClue(missing.mkString("\n") + "\n")(missing shouldBe empty)
  }

  it should "list no field that is gone, nor one whose key is content by construction" in {
    val live  = fields.map(_.id).toSet
    val stale = Reviewed.keys.toSeq.sorted.filter(id => !live(id) || fields.exists(f => f.id == id && contentKeyed(f)))
    withClue("Drop these entries:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
    Reviewed.keys.map(_.takeWhile(_ != '#')).toSeq.distinct.filterNot(p => Files.exists(Paths.get(p))) shouldBe empty
  }
}
