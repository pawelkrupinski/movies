package services.identityprojection

import scala.util.Try

import models.{ListingKey, MovieRecord}
import services.evidence.LatestTitleYear
import services.identity.*
import services.titles.TitleNormalizer

/** The DRAFT stage of THE IDENTITY PROJECTION ([[IdentityProjection]]): how it drafts the films a scope names — each film's
 *  record from its venues' slots ([[IdentityProjectionPlan.draftOf]], built by `slotSource`) — the venue slot memo's where nothing a slot is built from moved,
 *  its fingerprints recorded ([[RecordedSlotFingerprints]]) so a restarted worker reuses them, and each film as last
 *  drafted ([[FilmShapes]]) so a scoped projection works out only the venues that moved — then its TMDB details fetched
 *  by id ([[ProjectionDrafter.detailed]]), then titled and keyed ([[FilmKeys.finish]]). `details` is a
 *  film's record with the TMDB details of `film`. */
final class ProjectionDrafter(normalizer: TitleNormalizer, slotSource: VenueSlotSource,
                              details: (MovieRecord, Int) => Option[MovieRecord], fingerprints: RecordedSlotFingerprints,
                              /** The venue slot memo, made under its environment (`VenueSlotMemo.environment`). */
                              slotMemoOf: Long => VenueSlotMemo = new VenueSlotMemo(_))(using LatestTitleYear) {
  /** The venue slots the last projection built, so this one rebuilds only the films whose listings changed. */
  private val slotMemo = slotMemoOf(VenueSlotMemo.environment(normalizer))
  /** Each film as last drafted, so a scoped projection works out only the venues of a film that moved. */
  private var shapes = FilmShapes()
  /** Forget each film as last drafted: the projection has nothing written to build on. */
  def forget(): Unit = shapes = FilmShapes()

  /** The last drafts were written: kept for the next projection's ([[FilmShapes.commit]]). */
  def commit(whole: Boolean): Unit = shapes.commit(whole)

  /** The last drafts were not written. */
  def discard(): Unit = shapes.discard()

  /** Close the slot memo's tick (recording its fingerprints on a periodic projection) and note what it reused and
   *  built: the reused and built counts, and the misses by cause. */
  private def slotReport(periodic: Boolean, scope: ProjectionScope, phases: ProjectionPhases): ((Int, Int), (Int, Int, Int)) = {
    val slotCounts = slotMemo.endTick(retainUnseen = !scope.whole)
    if (periodic) phases("fingerprints")(fingerprints.record(slotMemo))
    val misses = slotMemo.lastMisses()
    phases.note(s"venue slots reused ${slotCounts._1}, built ${slotCounts._2} " +
      s"(rows moved ${misses._1}, priors moved ${misses._2}, new ${misses._3})")
    if (!scope.whole) phases.note(s"scope ${scope.films.size} stored films, ${scope.clusters.size} clusters, ${scope.listings.size} listings")
    (slotCounts, misses)
  }

  /** The drafts of `start`, detailed and titled, and what the venue slot memo reused and built for them (its
   *  fingerprints recorded on a `periodic` projection). A scope is closed again over every stored film outside it under
   *  one of its films' plain title keys, new or old — the older of two films of one title and year keeps the plain key,
   *  so their keys are decided together — until none is left outside. */
  def drafted(start: ProjectionScope, freed: Set[String], changed: Set[ListingKey], index: ProjectionIndex, periodic: Boolean,
              phases: ProjectionPhases): ProjectionDrafter.Drafted = {
    phases("seed")(fingerprints.seed(slotMemo))
    @scala.annotation.tailrec
    def loop(scope: ProjectionScope): (ProjectionScope, ProjectionDraft, ProjectionDraft, ProjectionPlan) = {
      val draft    = phases("draft")(IdentityProjectionPlan.draftOf(index, scope, normalizer, slotSource, slotMemo, shapes, changed))
      val detailed = phases("details")(draft.withDrafts(ProjectionDrafter.detailed(draft.drafts, details)))
      // a fresh id never one a film stored in `index` holds
      val plan     = phases("finish")(FilmKeys.finish(detailed, normalizer, index.storedById.valuesIterator.map(_.id).toSet))
      (if (scope.whole) None else ProjectionScope.widened(index, scope, plan.films.map(_.key), freed, normalizer)) match {
        case Some(wider) => loop(wider)
        case None        => (scope, draft, detailed, plan)
      }
    }
    val (scope, draft, detailed, plan) = loop(start)
    val (slots, misses) = slotReport(periodic, scope, phases)
    ProjectionDrafter.Drafted(scope, draft, detailed, plan, slots, misses)
  }
}

object ProjectionDrafter extends play.api.Logging {
  /** One projection's drafts: the scope they ended on, the draft (what the guard reads), the draft with its films' TMDB
   *  details (what the write completes), the plan, and the venue slots reused and built and the misses by cause. */
  final case class Drafted(scope: ProjectionScope, draft: ProjectionDraft, detailed: ProjectionDraft, plan: ProjectionPlan,
                           slots: (Int, Int), slotMisses: (Int, Int, Int))

  /** How many films' TMDB details a projection fetches at once — within what TMDB tolerates (see the
   *  external-api-rate-limits budgets). */
  private[services] val DetailsConcurrency = 8

  /** Each draft whose film is new to its record, with that film's TMDB details — fetched side by
   *  side, since each is its own film's: one at a time, a country's first projection waited on
   *  ~2,250 of them in a row (a US boot). The details builder owns the TMDB side; what the projection
   *  derived stays the projection's, and a fetch that fails leaves its draft as it was, said out loud. */
  private[services] def detailed(drafts: Seq[FilmDraft], details: (MovieRecord, Int) => Option[MovieRecord]): Seq[FilmDraft] =
    tools.BoundedParallel.map("identity-projection-details", drafts, DetailsConcurrency) { d =>
      d.needsDetails.fold(d) { film =>
        Try(details(d.record, film)).fold(e => { logger.warn(s"identity projection: TMDB details of $film failed, drafted without them: $e"); None },
          identity).fold(d)(d.withDetails)
      }
    }
}
