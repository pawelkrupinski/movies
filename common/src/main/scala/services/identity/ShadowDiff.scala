package services.identity

import services.movies.LatestTitleYear

import java.util.Locale

import models.{CinemaShowing, SourceData}
import services.movies.{EmbeddedYear, ListingConstraints, ListingKey, ScrapeListing, StoredMovieRecord, TitleNormalizer}
import services.resolution.YearWindow

/*
 * THE SHADOW DIFF (docs/design/identity-resolver.md §8, "Phase 1: shadow mode"): how the
 * resolver's clusters relate to the films stored for the same listings. Pure; the projection's
 * canary ([[IdentityProjectionPlan]]) counts it against the films stored before each projection, and
 * the offline harness (`IdentityShadowIntegrationSpec`) maps listings to stored films through the
 * same [[PipelineFilms]].
 */

/** A film today's pipeline made, as the shadow diff names it: its film id and TMDB id. */
final case class PipelineFilmRef(id: String, tmdbId: Option[Int])

object PipelineFilmRef {
  /** The order a listing's films are picked in (`PipelineFilms.pick`: the smallest). */
  given Ordering[PipelineFilmRef] = Ordering.by(r => (r.id, r.tmdbId.getOrElse(0)))
}

/** How one resolver cluster relates to the pipeline's films, over its listings the pipeline has
 *  placed on a film (a listing still in staging is on none, and is left out):
 *
 *  - `Identical`: exactly one pipeline film's listings, and the same TMDB film;
 *  - `Moved`: exactly one pipeline film's listings, but another film (or none on one side);
 *  - `Split`: part of one pipeline film — the resolver keeps apart what the pipeline joined;
 *  - `Merged`: listings of two or more pipeline films — the resolver joins what it kept apart. */
enum ShadowRelation {
  case Identical, Split, Merged, Moved

  /** The metric label and the stored form. */
  def label: String = toString.toLowerCase(Locale.ROOT)
}

/** One resolver cluster of a shadow run: the decision, its family, its relation to the pipeline
 *  (`None` when the pipeline placed none of its listings), and the pipeline films its listings are on. */
final case class ShadowCluster(decision: ResolverDecision, family: Int, relation: Option[ShadowRelation],
                               pipelineFilms: Seq[PipelineFilmRef]) {

  /** Whether the pipeline, today's reference, agrees with the decision: yes when the cluster is
   *  identical to a pipeline film; no when both name a film for exactly the same listings and
   *  the films differ. A split or merge is a partition question no film comparison settles, and
   *  "no film" against a film is a coverage question, so neither is a verdict. */
  def agrees: Option[Boolean] = relation match {
    case Some(ShadowRelation.Identical) => Some(true)
    case Some(ShadowRelation.Moved) if decision.film.isDefined && pipelineFilms.exists(_.tmdbId.isDefined) => Some(false)
    case _ => None
  }
}

/** One family where the resolver and the pipeline part ways: every resolver cluster and pipeline
 *  film of it, with their listings, the listings the pipeline has not placed, and how many of
 *  its clusters stand in each relation. */
final case class ShadowFamily(family: Int, resolver: Seq[(Option[Int], Seq[ListingKey])],
                              pipeline: Seq[(PipelineFilmRef, Seq[ListingKey])], unplaced: Seq[ListingKey],
                              relations: Map[ShadowRelation, Int])

object ShadowDiff {

  /** Every cluster of `resolution` with its relation, and every family whose clusters are not
   *  all identical to a pipeline film (or that holds a listing the pipeline has not placed). */
  def of(resolution: Resolution, pipelineOf: Map[ListingKey, PipelineFilmRef])(using LatestTitleYear): (Seq[ShadowCluster], Seq[ShadowFamily]) = {
    val membersOf = membersOfFilms(pipelineOf)
    val clusters = resolution.decisions.map(d => cluster(d, d.members.headOption.flatMap(resolution.familyOf.get).getOrElse(-1), pipelineOf, membersOf))
    val families = clusters.groupBy(_.family).toSeq.sortBy(_._1).flatMap { case (family, cs) =>
      val listings = cs.flatMap(_.decision.members)
      val unplaced = listings.filterNot(pipelineOf.contains).sorted
      Option.when(unplaced.nonEmpty || cs.exists(!_.relation.contains(ShadowRelation.Identical)))(ShadowFamily(
        family,
        resolver  = cs.map(c => c.decision.film -> c.decision.members.sorted),
        pipeline  = cs.flatMap(_.pipelineFilms).distinct.sortBy(_.id).map(p => p -> membersOf(p).toSeq.sorted),
        unplaced  = unplaced,
        relations = cs.flatMap(_.relation).groupMapReduce(identity)(_ => 1)(_ + _)))
    }
    (clusters, families)
  }

  /** `decisions` as clusters, each with its relation to the pipeline's films — what the canary counts. Every listing
   *  `pipelineOf` places on one of the films the decisions' listings are on must be in it: a film's split is read off
   *  all of its listings. */
  def clustersOf(decisions: Seq[ResolverDecision], pipelineOf: Map[ListingKey, PipelineFilmRef]): Seq[ShadowCluster] = {
    val membersOf = membersOfFilms(pipelineOf)
    decisions.map(cluster(_, -1, pipelineOf, membersOf))
  }

  private def membersOfFilms(pipelineOf: Map[ListingKey, PipelineFilmRef]): Map[PipelineFilmRef, Set[ListingKey]] =
    pipelineOf.toSeq.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap

  private def cluster(d: ResolverDecision, family: Int, pipelineOf: Map[ListingKey, PipelineFilmRef],
                      membersOf: Map[PipelineFilmRef, Set[ListingKey]]): ShadowCluster = {
    val placed = d.members.filter(pipelineOf.contains)
    val films  = placed.map(pipelineOf).distinct.sortBy(_.id)
    val relation = films match {
      case Seq()  => None
      case Seq(p) =>
        if (membersOf(p) != placed.toSet) Some(ShadowRelation.Split)
        else if (p.tmdbId == d.film) Some(ShadowRelation.Identical)
        else Some(ShadowRelation.Moved)
      case _ => Some(ShadowRelation.Merged)
    }
    ShadowCluster(d, family, relation, films)
  }

  /** Clusters per relation, every relation present (zero when none) — what the gauge exports. */
  def counts(clusters: Seq[ShadowCluster]): Map[ShadowRelation, Int] =
    ShadowRelation.values.map(r => r -> clusters.count(_.relation.contains(r))).toMap
}

/**
 * Which of the pipeline's films each listing is on: found by its SLOT — the venue and the slot key
 * its title folds to — and, where one venue's two films share that key (Belle 2013/2021), the slot
 * whose own year and directors the listing's agree with, the discriminator the slot fold itself
 * splits on (`ScrapeListing.filmsOf`). A listing still in staging is on no film.
 */
object PipelineFilms {

  /** `films` are anything carrying a film's venue slots `(venue, slot key, slot)`. */
  def assign[F](listings: Seq[Listing], films: Seq[(F, Seq[(String, String, SourceData)])],
                normalizer: TitleNormalizer)(using Ordering[F], LatestTitleYear): Map[ListingKey, F] = {
    val bySlot = films.flatMap { case (f, slots) => slots.map { case (v, k, sd) => (v, k) -> (f, sd) } }.groupMap(_._1)(_._2)
    listings.flatMap(l => bySlot.get(slotOf(l, normalizer)).flatMap(pick(l, _, normalizer)).map(l.key -> _)).toMap
  }

  /** The venue slot `listing` is on: its venue, and the slot key its title folds to. */
  def slotOf(listing: Listing, normalizer: TitleNormalizer): (String, String) = (listing.venue, normalizer.sanitize(listing.cleanTitle))

  /** The film `listing` is on among those holding a slot at [[slotOf]] it (`held`, never empty): the one film, or of
   *  several the one whose slot's year and directors the listing's agree with, the smallest first. */
  def pick[F](listing: Listing, held: Seq[(F, SourceData)], normalizer: TitleNormalizer)(using Ordering[F], LatestTitleYear): Option[F] = held match {
    case Seq((f, _)) => Some(f)
    case several =>
      val year = listing.year.orElse(EmbeddedYear.ofAll(Seq(listing.rawTitle, listing.cleanTitle), LatestTitleYear.current))
      val fits = several.filter { case (_, sd) =>
        val slotYear = ScrapeListing.yearOf(sd)
        (year.isEmpty || slotYear.isEmpty || math.abs(year.get - slotYear.get) <= YearWindow.ProductionToRelease) &&
          ListingConstraints.venueCreditsApart(listing.directors, sd.director, normalizer).isEmpty
      }
      (if (fits.nonEmpty) fits else several).map(_._1).minOption
  }

  /** The venue slots of a stored film row. */
  def slotsOf(row: StoredMovieRecord): Seq[(String, String, SourceData)] =
    row.record.data.toSeq.collect { case (cs: CinemaShowing, sd) => (cs.cinema.displayName, cs.titleKey, sd) }

  /** Each listing's film among the pipeline's stored rows. */
  def of(listings: Seq[Listing], rows: Seq[StoredMovieRecord], normalizer: TitleNormalizer)(using LatestTitleYear): Map[ListingKey, PipelineFilmRef] =
    assign(listings, rows.map(r => PipelineFilmRef(r.id.value, r.record.tmdbId) -> slotsOf(r)), normalizer)
}
