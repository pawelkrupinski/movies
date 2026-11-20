package services.identity

import models.FilmId
import models.ListingKey
import services.titles.TitleNormalizer

/** One run of [[IdentityProjectionPlan.draftOf]]: the drafts of the films `scope` names over `index`, worked out in its
 *  steps — the films' ids by overlap and their venues' groups ([[FilmPlans]]), the venue slots each film's venues need built
 *  ([[LeanVenueSlots]]), each film's record ([[FilmRecordDrafts]]) and the projection's draft as a whole ([[projectionDraft]]).
 *  The mutable state a run reads and writes — the memo of venue slots, the films' shapes — is written in that order. */
private[identity] final class ScopeDrafting(index: ProjectionIndex, scope: ProjectionScope, normalizer: TitleNormalizer,
                                                 slotSource: VenueSlotSource, memo: VenueSlotMemo,
                                                 shapes: FilmShapes, changed: Set[ListingKey]) {
  import index.covered
  import FilmPlans.Planned

  // Within the scope: its stored films' listings, and its clusters. Everything outside it is a film no change reached,
  // whose draft is the film as stored.
  private val within        = index.within(scope)
  import within.{previousOf, stored}
  private val previousFilms = within.listingsOf.map { case (id, ls) => IdSeeding.Film(id, ls) }
  private val clusters      = within.clusters.map(c => c.members -> c.film)
  private val numbered      = previousFilms.map(f => covered.counterOf(f.id).get -> f.listings)

  // 2. Ids by overlap.
  private val assigned = IdAssigner.assign(numbered, clusters.map(_._1), covered.nextCounter)
  private def previousIdOf(c: Long): Option[String] = Option.when(c < covered.nextCounter)(covered.filmIdOf(c)).flatten

  // 3. Each film's record. Its venue slots are LEAN, each venue's from the memo when nothing it is built from
  // moved, else built from the venue's rows, read a batch of venues at a time and let go ([[LeanVenueSlots]]); the
  // record over them ([[FilmRecordDrafts]]).
  private val venueSlots = new LeanVenueSlots(index, previousOf, normalizer, slotSource, memo)
  private val records    = new FilmRecordDrafts(index, within, shapes, normalizer, venueSlots, previousIdOf)
  /** The projection's draft of `planned` films drafted as `drafts`: the films retired and the regroupings. */
  private def projectionDraft(planned: Seq[Planned], drafts: Seq[FilmDraft]): ProjectionDraft = {
    val venues = planned.map(plan => plan.counter -> plan.groups.map {
      case (cinema, Left(venue))           => cinema -> venue.keys
      case (cinema, Right((group, _, _)))  => cinema -> group.rows.map(_.listing.key)
    }.toMap).toMap

    // Retired: a previous film no cluster kept, and a stored film none of whose listings is published.
    val kept    = drafts.flatMap(_.inherited).toSet
    val retired = stored.map(_.id).filterNot(kept).distinct.sortBy(_.value)

    ProjectionDraft(drafts, retired, retired.filterNot(id => index.placed(id.value)), covered, index.additions,
      regroupings(drafts, retired), venues)
  }

  private def regroupings(drafts: Seq[FilmDraft], retired: Seq[FilmId]): Regroupings = {
    val newIdOf = assigned.idOfListing
    Regroupings(
      merges  = clusters.map { case (ms, _) => ms.flatMap(previousOf.get).map(_.id).size - 1 }.filter(_ > 0).sum,
      splits  = previousFilms.count(f => f.listings.flatMap(newIdOf.get).sizeIs > 1),
      moves   = previousOf.count { case (l, ref) => newIdOf.get(l).map(c => previousIdOf(c).getOrElse(s"#$c")) != Some(ref.id) },
      fresh   = drafts.count(_.inherited.isEmpty),
      retired = retired.size)
  }

  /** The draft: plan every film, build the venue slots the plans need, then draft each film's record. */
  def draft: ProjectionDraft = {
    val planned = new FilmPlans(index, scope, shapes, slotSource, changed, assigned).all
    venueSlots.buildAll(planned.flatMap(_.groups.collect { case (_, Left(venue)) => venue }),
      planned.flatMap(_.groups.collect { case (_, Right((group, _, _))) => group }))
    projectionDraft(planned, planned.map(records.filmDraftOf))
  }
}
