package services.identityadapters

import services.identity.*
import services.titles.TitleNormalizer

import models.{ListingKey, MovieRecord}

import scala.util.{Failure, Success, Try}

/** How the identity projection drafts the films a scope names ([[IdentityProjection]]): each film's record from its venues'
 *  slots (`drafts`, [[IdentityProjectionPlan.draftOf]]) — the venue slot memo's where nothing a slot is built from moved,
 *  its fingerprints recorded (`identity_slot_fingerprints`) so a restarted worker reuses them, and each film as last
 *  drafted ([[FilmShapes]]) so a scoped projection works out only the venues that moved — then its TMDB details fetched
 *  by id ([[ProjectionDrafter.detailed]]), then titled and keyed ([[IdentityProjectionPlan.finish]]). `details` is a
 *  film's record with the TMDB details of `film`. Logs under the projection's `logger`. */
final class ProjectionDrafter(drafts: IdentityProjectionPlan.Drafts, normalizer: TitleNormalizer,
                              details: (MovieRecord, Int) => Option[MovieRecord], fingerprints: VenueSlotFingerprints,
                              logger: play.api.Logger,
                              /** The venue slot memo, made under its environment (`VenueSlotMemo.environment`). */
                              slotMemoOf: Long => VenueSlotMemo = new VenueSlotMemo(_)) {
  /** The venue slots the last projection built, so this one rebuilds only the films whose listings changed. */
  private val slotMemo = slotMemoOf(VenueSlotMemo.environment(normalizer))
  /** Each film as last drafted, so a scoped projection works out only the venues of a film that moved. */
  private var shapes = FilmShapes()
  /** The fingerprints the store holds, as far as this worker knows: none read yet until the first projection. */
  private var recordedFingerprints: Option[Set[Long]] = None

  /** Seed the memo, before the first projection after a boot, with the slots the last run kept. */
  def seed(): Unit = if (recordedFingerprints.isEmpty) {
    val recorded = Try(fingerprints.all()).fold(e => { logger.warn("identity projection: reading the venue slot fingerprints failed; " +
      "the first projection builds every slot", e); Set.empty[Long] }, identity)
    slotMemo.seed(recorded)
    recordedFingerprints = Some(recorded)
  }

  /** Record the fingerprints of the slots the memo now keeps, writing only those that moved. One that fails is
   *  tried again by the next projection; one left behind is a true fact, costing only its space. */
  private def recordFingerprints(): Unit = {
    val known = recordedFingerprints.getOrElse(Set.empty)
    val now   = slotMemo.fingerprints
    if (now != known) Try(fingerprints.update(now -- known, known -- now)) match {
      case Success(_) => recordedFingerprints = Some(now)
      case Failure(e) => logger.warn("identity projection: recording the venue slot fingerprints failed; the next projection retries", e)
    }
  }

  /** Forget each film as last drafted: the projection has nothing written to build on. */
  def forget(): Unit = shapes = FilmShapes()

  /** The last drafts were written: kept for the next projection's ([[FilmShapes.commit]]). */
  def commit(whole: Boolean): Unit = shapes.commit(whole)

  /** The last drafts were not written. */
  def discard(): Unit = shapes.discard()

  /** Close the slot memo's tick (recording its fingerprints on a periodic projection) and note what it reused and
   *  built: the reused and built counts, and the misses by cause. */
  def slotReport(periodic: Boolean, scope: ProjectionScope, phases: ProjectionPhases): ((Int, Int), (Int, Int, Int)) = {
    val slotCounts = slotMemo.endTick(retainUnseen = !scope.whole)
    if (periodic) phases("fingerprints")(recordFingerprints())
    val misses = slotMemo.lastMisses()
    phases.note(s"venue slots reused ${slotCounts._1}, built ${slotCounts._2} " +
      s"(rows moved ${misses._1}, priors moved ${misses._2}, new ${misses._3})")
    if (!scope.whole) phases.note(s"scope ${scope.films.size} stored films, ${scope.clusters.size} clusters, ${scope.listings.size} listings")
    (slotCounts, misses)
  }

  /** The drafts of `scope`, detailed and titled. A scope is closed again over every stored film outside it under one of
   *  its films' plain title keys, new or old — the older of two films of one title and year keeps the plain key, so
   *  their keys are decided together — until none is left outside. */
  def drafted(start: ProjectionScope, freed: Set[String], changed: Set[ListingKey], index: ProjectionIndex,
              phases: ProjectionPhases): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
    @scala.annotation.tailrec
    def loop(scope: ProjectionScope): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
      val draft    = phases("draft")(drafts.draftOf(index, scope, slotMemo, shapes, changed))
      val detailed = phases("details")(draft.withDrafts(ProjectionDrafter.detailed(draft.drafts, details)))
      val plan     = phases("finish")(drafts.finish(detailed, index))
      (if (scope.whole) None else ProjectionScope.widened(index, scope, plan.films.map(_.key), freed, normalizer)) match {
        case Some(wider) => loop(wider)
        case None        => (scope, draft, detailed, plan)
      }
    }
    loop(start)
  }
}

object ProjectionDrafter {
  /** How many films' TMDB details a projection fetches at once — within what TMDB tolerates (see the
   *  external-api-rate-limits budgets). */
  private[services] val DetailsConcurrency = 8

  /** Each draft whose film is new to its record, with that film's TMDB details — fetched side by
   *  side, since each is its own film's: one at a time, a country's first projection waited on
   *  ~2,250 of them in a row (a US boot). The details builder owns the TMDB side; what the projection
   *  derived stays the projection's, and a fetch that fails leaves its draft as it was. */
  private[services] def detailed(drafts: Seq[FilmDraft], details: (MovieRecord, Int) => Option[MovieRecord]): Seq[FilmDraft] =
    tools.BoundedParallel.map("identity-projection-details", drafts, DetailsConcurrency) { d =>
      d.needsDetails.fold(d)(film => Try(details(d.record, film)).toOption.flatten.fold(d)(d.withDetails))
    }
}
