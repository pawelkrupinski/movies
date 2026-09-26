package services.identity

import models.{CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import services.movies.{CacheKey, CinemaSlotBuilder, FilmId, ListingKey, MovieRecordMerge, ScrapeListing, ScreeningTokens,
  StoredMovieRecord, TitleNormalizer}
import services.resolution.TmdbAttempt

import java.time.Instant

/** One listing a cut-over venue publishes: the resolver's view of it and the row as scraped. */
final case class ProjectedListing(listing: Listing, row: CinemaMovie)

/** How the films moved between two projections: previous films absorbed into another (`merges`),
 *  previous films spread over two or more (`splits`), listings whose film id changed (`moves`),
 *  films with a new id (`fresh`), and ids no film carries any more (`retired`). */
final case class Regroupings(merges: Int, splits: Int, moves: Int, fresh: Int, retired: Int) {
  def isEmpty: Boolean = merges == 0 && splits == 0 && moves == 0 && fresh == 0 && retired == 0
}

/** One film of a projection before its key is chosen: the counter [[IdAssigner]] gave it, the id it
 *  inherits (none for a fresh one), its listings, the record the listings and the previous film
 *  make, and the title its slots mostly carry (`anchor`, the display ladder's fallback). */
final case class FilmDraft(counter: Long, inherited: Option[FilmId], members: Seq[ListingKey], record: MovieRecord,
                           anchor: String) {
  /** The TMDB film whose details the record still lacks: a film new to it, or one whose details
   *  never arrived. The projection fetches them by id before it writes (`resolved by id`, never a
   *  search). */
  def needsDetails: Option[Int] = record.tmdbId.filterNot(_ => record.data.contains(models.Tmdb))
}

/** A film as the projection writes it: its id and counter, display title and year, the unique
 *  lookup key it is stored under, the record, and its listings. */
final case class ProjectedFilm(id: FilmId, counter: Long, title: String, year: Option[Int], key: String,
                               record: MovieRecord, members: Seq[ListingKey])

/** Everything one projection decides before anything is fetched: the drafts, the ids retired, the
 *  FilmId map extended over the previous films, the regroupings, and the canary — how the resolver's
 *  clusters relate to the films stored before it ran ([[ShadowDiff]]'s relations). */
final case class ProjectionDraft(drafts: Seq[FilmDraft], retired: Seq[FilmId], counters: FilmIdCounters,
                                 additions: Seq[FilmIdCounter], regroupings: Regroupings, canary: Map[ShadowRelation, Int])

/** A projection ready to write: the films (every key unique, every TMDB id on one film), the ids to
 *  retire, the FilmId-map entries it adds, and the draft's regroupings and canary. */
final case class ProjectionPlan(films: Seq[ProjectedFilm], retired: Seq[FilmId], counterAdditions: Seq[FilmIdCounter],
                                regroupings: Regroupings, canary: Map[ShadowRelation, Int])

/**
 * THE IDENTITY PROJECTION's decisions (docs/design/identity-resolver.md §2 "Projection", §6, §8
 * phase 3 = programme phase 5), as pure functions of the accepted listings, the resolver's
 * decisions over them, the films stored before, and the persisted FilmId map:
 *
 *  1. every stored film is the set of today's listings its slots hold (`PipelineFilms`, the shadow
 *     diff's own mapping), numbered through the FilmId map ([[FilmIdCounters]], largest film first
 *     for a film not yet mapped) — so a legacy `title|year` id keeps its counter and its URL;
 *  2. the resolver's clusters, with the clusters of one TMDB film joined (`movies` holds one
 *     document per film — its unique `tmdbId` index), get ids by OVERLAP ([[IdAssigner]]): a merge
 *     keeps the older id, a split leaves it on the larger half, and a film no cluster overlaps is
 *     retired;
 *  3. each film's record is its listings' venue slots — the venue's rows of one slot unioned
 *     exactly as the landing's same-title fold unions them (`ScrapeListing.prepare`), every
 *     showtime of every listing kept — over the previous film's enrichment when the film is the
 *     same TMDB film, over none when it is not; a cluster matching no film is concluded as a
 *     no-match (`tmdbAttempt`), which is a verdict, not a failure;
 *  4. ([[finish]], after the details of a new film are fetched) the display title and year pick
 *     the key; two films whose title and year coincide keep their keys apart, the older plain.
 *
 * Nothing here reads a title to decide identity: titles only name what the resolver decided.
 * Every step sorts by keys of the data, so the plan is a function of the SETS it is given (P1),
 * and a second projection over its own output changes nothing (P2).
 */
object IdentityProjectionPlan {

  /** What a no-match concluded by the resolver records: not a search's fingerprint (the resolver's
   *  lookups are observations, re-asked on their own TTL), only that the verdict was reached. */
  val ResolverVerdict: String = "identity-resolver"

  def draft(listings: Seq[ProjectedListing], resolution: Resolution, stored: Seq[StoredMovieRecord], counters: FilmIdCounters,
            normalizer: TitleNormalizer, slots: CinemaSlotBuilder, tokens: ScreeningTokens, at: Instant): ProjectionDraft = {
    val byKey: Map[ListingKey, ProjectedListing] =
      listings.sortBy(_.listing).distinctBy(_.listing.key).map(p => p.listing.key -> p).toMap
    val storedById = stored.map(r => r.id.value -> r).toMap

    // 1. The stored films as listing sets, numbered.
    val previousOf: Map[ListingKey, PipelineFilmRef] = PipelineFilms.of(byKey.values.map(_.listing).toSeq, stored, normalizer)
    val previousFilms: Seq[IdSeeding.Film] = previousOf.toSeq.groupMap(_._2.id)(_._1).toSeq
      .map { case (id, ls) => IdSeeding.Film(id, ls.toSet) }.sortBy(_.id)
    val covered   = counters.covering(previousFilms)
    val additions = covered.entries.filterNot(e => counters.counterOf(e.filmId).isDefined)
    val numbered  = previousFilms.map(f => covered.counterOf(f.id).get -> f.listings)

    // 2. Clusters, one per film, and their ids by overlap.
    val (matched, unmatched) = resolution.decisions.filter(_.members.exists(byKey.contains)).partition(_.film.isDefined)
    val clusters: Seq[(Set[ListingKey], Option[Int])] =
      (matched.groupBy(_.film).toSeq.map { case (film, ds) => ds.flatMap(_.members).toSet -> film } ++
        unmatched.map(d => d.members.toSet -> None))
        .map { case (ms, film) => ms.filter(byKey.contains) -> film }
    val filmOf    = clusters.toMap
    val assigned  = IdAssigner.assign(numbered, clusters.map(_._1), covered.nextCounter)
    val previousIdOf: Long => Option[String] = c => Option.when(c < covered.nextCounter)(covered.filmIdOf(c)).flatten

    // 3. Each film's record.
    val drafts = assigned.ids.map { case (counter, members) =>
      val previous = previousIdOf(counter).flatMap(storedById.get)
      val film     = filmOf(members)
      val rows     = members.toSeq.sorted.map(byKey)
      val (venueSlots, anchor) = slotsOf(rows, previousOf, storedById, normalizer, slots, tokens)
      val sameFilm = previous.exists(_.record.tmdbId == film)
      val base = previous.filter(_ => sameFilm).map(_.record).getOrElse(
        MovieRecord(retainedSynopses = previous.map(_.record.retainedSynopses).getOrElse(Map.empty)))
      val record = base.copy(
        tmdbId        = film,
        tmdbAttempt   = if (film.isDefined) None else base.tmdbAttempt.orElse(Some(TmdbAttempt(ResolverVerdict, at))),
        detailPending = false,
        searchTitle   = base.searchTitle.orElse(Some(normalizer.apiQuery(normalizer.recase(anchor)))),
        data          = base.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty } ++ venueSlots)
      FilmDraft(counter, previous.map(_.id), members.toSeq.sorted, record, anchor)
    }

    // Retired: a previous film no cluster kept, and a stored film none of whose listings is published.
    val kept    = drafts.flatMap(_.inherited).toSet
    val retired = stored.map(_.id).filterNot(kept).distinct.sortBy(_.value)

    val newIdOf = assigned.idOfListing
    val regroupings = Regroupings(
      merges  = clusters.map { case (ms, _) => ms.flatMap(previousOf.get).map(_.id).size - 1 }.filter(_ > 0).sum,
      splits  = previousFilms.count(f => f.listings.flatMap(newIdOf.get).sizeIs > 1),
      moves   = previousOf.count { case (l, ref) => newIdOf.get(l).map(c => previousIdOf(c).getOrElse(s"#$c")) != Some(ref.id) },
      fresh   = drafts.count(_.inherited.isEmpty),
      retired = retired.size)
    ProjectionDraft(drafts, retired, covered, additions, regroupings, ShadowDiff.counts(ShadowDiff.of(resolution, previousOf)._1))
  }

  /** Choose every film's title, year and key, and mint the id of every fresh one. `taken` says
   *  whether an id is live already (a fresh id must not be one). */
  def finish(draft: ProjectionDraft, normalizer: TitleNormalizer, taken: FilmId => Boolean): ProjectionPlan = {
    val titled = draft.drafts.sortBy(_.counter).map { d =>
      val title = d.record.displayTitle(d.anchor, normalizer)
      (d, title, d.record.resolvedYear)
    }
    // Two films one title and year name: the older keeps the plain key, the other its own.
    val plainKey = titled.map { case (d, title, year) => d.counter -> StoredMovieRecord.keyFor(title, year, normalizer) }.toMap
    val firstHolder = titled.groupBy { case (d, _, _) => plainKey(d.counter) }.map { case (k, ds) => k -> ds.map(_._1.counter).min }
    val minted = scala.collection.mutable.Set.empty[FilmId]
    val films = titled.map { case (d, title, year) =>
      val key = if (firstHolder(plainKey(d.counter)) == d.counter) plainKey(d.counter)
                else s"${normalizer.sanitize(title)}~${d.counter}|${year.fold("")(_.toString)}"
      val id = d.inherited.getOrElse {
        val fresh = FilmId.fresh(CacheKey.stored(title, key), id => taken(id) || minted(id))
        minted += fresh
        fresh
      }
      ProjectedFilm(id, d.counter, title, year, key, d.record, d.members)
    }
    val freshEntries = films.filter(f => draft.counters.filmIdOf(f.counter).isEmpty).map(f => FilmIdCounter(f.id.value, f.counter))
    ProjectionPlan(films, draft.retired, draft.additions ++ freshEntries, draft.regroupings, draft.canary)
  }

  /** A film's venue slots from its listings: per venue, the rows of one slot unioned as the
   *  landing's fold unions them (every showtime kept), each built over the slot the representative
   *  listing's previous film held there. Also the title most of its listings carry. */
  private def slotsOf(rows: Seq[ProjectedListing], previousOf: Map[ListingKey, PipelineFilmRef],
                      storedById: Map[String, StoredMovieRecord], normalizer: TitleNormalizer,
                      slots: CinemaSlotBuilder, tokens: ScreeningTokens): (Seq[(Source, SourceData)], String) = {
    val built = rows.groupBy(_.listing.cinema).toSeq.sortBy(_._1.displayName).flatMap { case (cinema, ofVenue) =>
      val prepared = ScrapeListing.prepare(cinema, ofVenue.map(_.row), normalizer, tokens)
      prepared.movies.groupBy(cm => CinemaShowing.keyFor(cinema, prepared.cleaned(cm), normalizer)).toSeq
        .sortBy(_._1.titleKey).map { case (source, group) =>
          val representative =
            if (group.sizeIs == 1) group.head
            else MovieRecordMerge.slotRepresentative(group).copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
          val prior = previousOf.get(ListingKey.of(cinema, representative)).flatMap(ref => storedById.get(ref.id))
            .flatMap(_.record.data.get(source))
          (source: Source) -> slots.build(representative, prepared.cleaned(representative), prior, representative.movie.releaseYear)
        }
    }
    val anchor = rows.map(r => r.listing.cleanTitle).groupMapReduce(identity)(_ => 1)(_ + _).toSeq
      .sortBy { case (t, n) => (-n, t) }.headOption.map(_._1).getOrElse("")
    (built, anchor)
  }

}
