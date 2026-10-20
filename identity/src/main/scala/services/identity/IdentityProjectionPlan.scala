package services.identity

import services.movies.LatestTitleYear

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import services.movies.{CacheKey, CinemaSlotBuilder, FilmId, LeanRecords, ListingKey, ScreeningTokens,
  StoredMovieRecord, TitleNormalizer}

import java.time.Instant

/** One listing a venue publishes: the resolver's view of it, and digests of the row as scraped — of everything but
 *  its showtimes (`row`), and of its showtimes. A US projection held every listing's row and showtimes (1.7M, ~560 MB
 *  live) through each tick to build the slots of the few films that changed; a slot that must be built reads its
 *  venue's rows again (`IdentityProjectionPlan.draft`'s `rowsOf`), a batch of venues at a time. */
final case class ProjectedListing(listing: Listing, row: Int, showtimes: Int)

object ProjectedListing {
  def of(listing: Listing, row: CinemaMovie): ProjectedListing = of(listing, row, row.showtimes.##)

  /** `row`, read without its showtimes, whose digest (`showtimes.##`) is `showtimes`. */
  def of(listing: Listing, row: CinemaMovie, showtimes: Int): ProjectedListing = ProjectedListing(listing, rowDigest(row), showtimes)

  /** A digest of everything on `row` but its showtimes, the same in every JVM — the slot memo's fingerprints outlive
   *  the worker ([[VenueSlotFingerprints]]) — so its venue is read by name: a roster venue (`UsCinema`) is an
   *  instance, hashed by identity. */
  def rowDigest(row: CinemaMovie): Int =
    row.copy(showtimes = Nil).productIterator.map { case cinema: Cinema => cinema.displayName; case field => field }.toSeq.##
}

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
                           anchor: String, touched: Option[Set[Source]] = None) {
  /** The TMDB film whose details the record still lacks: a film new to it, or one whose details
   *  never arrived. The projection fetches them by id before it writes (`resolved by id`, never a
   *  search). */
  def needsDetails: Option[Int] = record.tmdbId.filterNot(_ => record.data.contains(models.Tmdb))
}

/** A film as the projection writes it: its id and counter, display title and year, the unique
 *  lookup key it is stored under, the record, and its listings — and, of a film drafted again from its last draft, the
 *  sources that may differ from what that draft wrote (`touched`: its non-venue sources and the venues it rebuilt, before
 *  and after); every other slot is the one written then. None: any may differ. */
final case class ProjectedFilm(id: FilmId, counter: Long, title: String, year: Option[Int], key: String,
                               record: MovieRecord, members: Seq[ListingKey], touched: Option[Set[Source]] = None)

/** Everything one projection decides before anything is fetched: the drafts, the ids retired (and
 *  among them the `vanished` ones — stored films no published listing is on any more), the FilmId
 *  map extended over the previous films, and the regroupings. */
final case class ProjectionDraft(drafts: Seq[FilmDraft], retired: Seq[FilmId], vanished: Seq[FilmId], counters: FilmIdCounters,
                                 additions: Seq[FilmIdCounter], regroupings: Regroupings,
                                 venues: Map[Long, Map[Cinema, Seq[ListingKey]]] = Map.empty,
                                 build: (Cinema, Seq[ListingKey], VenueSlots.VenueRows) => Seq[(Source, SourceData)] = (_, _, _) => Nil,
                                 rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]] = _ => Map.empty,
                                 takes: Seq[TakeChange] = Nil,
                                 // A venue's rows by the key each listing is published under (`VenueSlots.rowsByKey`, which
                                 // needs the drafting's normalizer); none without `rowsOf`'s rows.
                                 keyRows: (Cinema, Seq[CinemaMovie]) => VenueSlots.VenueRows = (_, _) => Map.empty) {
  /** `films` as they are written over the films stored before (`stored`). Every drafted slot is LEAN (no showtimes,
   *  only their digest and starts — enough to compare and guard by); a venue whose lean slot differs from the stored
   *  one is built in full from its rows, read a batch of venues at a time. One that matches a stored slot kept
   *  stripped is written lean — the write re-stitches a stripped slot from the stored screenings, which are its
   *  own — so a film at hundreds of venues that changed at one rebuilds that one. */
  def complete(films: Seq[ProjectedFilm], stored: FilmId => Option[MovieRecord]): Seq[ProjectedFilm] = {
    val wanted = films.map { film =>
      // Of a film whose untouched slots are the ones written last ([[ProjectedFilm.touched]]), only the touched can differ.
      val slots = film.touched.fold(film.record.data.iterator)(_.iterator.flatMap(s => film.record.data.get(s).map(s -> _)))
      film -> slots.collect {
        case (showing: CinemaShowing, slot) if IdentityProjectionPlan.isLean(slot) && !stored(film.id).flatMap(_.data.get(showing))
          .exists(held => IdentityProjectionPlan.isLean(held) && LeanRecords.slotsEqual(slot, held)) => showing.cinema
      }.toSet
    }
    val built = scala.collection.mutable.HashMap.empty[(Long, Cinema), Seq[(Source, SourceData)]]
    // Each venue's films that differ there, so a batch of venues visits only those, each venue's rows grouped once. Never
    // a set of the films: a film hashes its whole record, a wide release's thousands of slots.
    val filmsAt = wanted.flatMap { case (film, differing) => differing.iterator.map(_ -> film) }.groupMap(_._1)(_._2)
    filmsAt.keys.toSeq.sortBy(_.displayName).grouped(IdentityProjectionPlan.RowBatch).foreach { batch =>
      val rows = rowsOf(batch.toSet)
      batch.foreach { cinema =>
        val byKey = keyRows(cinema, rows.getOrElse(cinema, Nil))
        filmsAt(cinema).foreach { film =>
          built((film.counter, cinema)) = build(cinema, venues.getOrElse(film.counter, Map.empty).getOrElse(cinema, Nil), byKey)
        }
      }
    }
    wanted.map { case (film, differing) =>
      if (differing.isEmpty) film
      else film.copy(record = film.record.copy(data = film.record.data ++ differing.toSeq.flatMap(cinema => built((film.counter, cinema)))))
    }
  }
}

/** A projection ready to write: the films (every key unique, every TMDB id on one film), the ids to
 *  retire, the FilmId-map entries it adds, and the draft's regroupings and take changes. */
final case class ProjectionPlan(films: Seq[ProjectedFilm], retired: Seq[FilmId], counterAdditions: Seq[FilmIdCounter],
                                regroupings: Regroupings, takes: Seq[TakeChange] = Nil)

/**
 * THE IDENTITY PROJECTION's decisions (docs/design/identity-resolver.md §2 "Projection", §6, §8
 * phase 3 = programme phase 5), as pure functions of the accepted listings, the resolver's
 * decisions over them, the films stored before, and the persisted FilmId map:
 *
 *  1. every stored film is the set of today's listings its slots hold (the slot's own listing key,
 *     else the film whose slot its venue and title fold to, `StoredFilmSlots`), numbered through the FilmId map ([[FilmIdCounters]], largest film first
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
            normalizer: TitleNormalizer, slots: CinemaSlotBuilder, tokens: ScreeningTokens, at: Instant,
            rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]], memo: VenueSlotMemo = VenueSlotMemo.none)(using LatestTitleYear): ProjectionDraft = {
    val whole = index(listings, resolution, stored, counters, normalizer)
    draftOf(whole, whole.everything, normalizer, slots, tokens, at, rowsOf, memo)
  }

  /** What a projection reads off the whole listing set, the resolution and the stored films before it drafts any film:
   *  each a pass over the corpus, made every tick, and what [[ProjectionScope]] closes a tick's changes over. */
  def index(listings: Seq[ProjectedListing], resolution: Resolution, stored: Seq[StoredMovieRecord], counters: FilmIdCounters,
            normalizer: TitleNormalizer)(using LatestTitleYear): ProjectionIndex = {
    val byKey      = oneByKey(listings)
    val storedById = stored.map(r => r.id.value -> r).toMap

    // The stored films as listing sets, numbered.
    // A listing is on the stored film whose slot it IS (the slot's own listing key); a listing no slot
    // names — one a venue's same-title fold hid behind another's slot — on the film holding its slot.
    val slotOwner: Map[ListingKey, StoredFilmRef] = stored.flatMap { r =>
      val ref = StoredFilmRef(r.id.value, r.record.tmdbId)
      slotsOf(r).flatMap(_.key.map(_ -> ref))
    }.groupMap(_._1)(_._2).flatMap { case (k, refs) => ownerOf(refs.toSet).map(k -> _) }
    val previousOf: Map[ListingKey, StoredFilmRef] =
      StoredFilmSlots.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
    val listingsOf = previousOf.toSeq.groupMap(_._2.id)(_._1).view.mapValues(_.toSet).toMap
    val (covered, additions) = counters.cover(listingsOf.toSeq.map { case (id, ls) => IdSeeding.Film(id, ls) })

    // Clusters, one per film: the resolver's, with the clusters of one TMDB film joined.
    val clusters: Map[ClusterId, Cluster] = resolution.decisions.groupBy(clusterIdOf).flatMap { case (id, ds) =>
      IdentityProjectionPlan.clusterOf(id, ds, byKey.contains).map(id -> _)
    }
    val clusterOf = clusters.iterator.flatMap { case (id, c) => c.members.iterator.map(_ -> id) }.toMap
    ProjectionIndex(byKey, storedById, previousOf, listingsOf, covered, additions, clusters, clusterOf)(normalizer)
  }

  /** One listing per key — the smallest by the total order, as the resolver takes it — standing for every row
   *  published under that key (its slot is built from all of them: `merged`), so a venue printing one listing twice
   *  loses no showtime. */
  def oneByKey(listings: Seq[ProjectedListing]): Map[ListingKey, ProjectedListing] = {
    val first = scala.collection.mutable.HashMap.empty[ListingKey, ProjectedListing]
    val twice = scala.collection.mutable.HashMap.empty[ListingKey, List[ProjectedListing]]
    listings.foreach { l =>
      val key = l.listing.key
      first.get(key) match {
        case None          => first(key) = l
        case Some(earlier) => twice(key) = l :: twice.getOrElse(key, List(earlier))
      }
    }
    twice.foreach { case (key, rows) => first(key) = rows.minBy(_.listing).copy(showtimes = rows.map(_.showtimes).sorted.##) }
    first.toMap
  }

  /** One slot of a stored film: where a venue's same-title fold puts it (`at`, a venue slot's venue and slot key), its
   *  data, and the listing key it is the slot of, if any. */
  final case class Slot(at: Option[(String, String)], data: SourceData, key: Option[ListingKey])

  def slotsOf(r: StoredMovieRecord): Seq[Slot] = r.record.data.toSeq.map { case (source, sd) => slotOf(source, sd) }

  def slotOf(source: Source, sd: SourceData): Slot =
    Slot(source match { case cs: CinemaShowing => Some(cs.cinema.displayName -> cs.titleKey); case _ => None }, sd, ListingKey.ofSource(source, sd))

  /** The film a listing is the slot of, of the films whose slot it is: the smallest id. */
  def ownerOf(refs: Set[StoredFilmRef]): Option[StoredFilmRef] = refs.minByOption(_.id)

  /** The cluster a decision is part of: its TMDB film's, or for no film its own. */
  def clusterIdOf(d: ResolverDecision): ClusterId = d.film.fold[ClusterId](ClusterId.Unmatched(d.members.min))(ClusterId.Matched(_))

  /** Cluster `id` of `decisions`, over the listings `published`: none when it publishes none. */
  def clusterOf(id: ClusterId, decisions: Seq[ResolverDecision], published: ListingKey => Boolean): Option[Cluster] = {
    val members = decisions.iterator.flatMap(_.members).filter(published).toSet
    Option.when(members.nonEmpty)(id match {
      case ClusterId.Matched(film) => Cluster(members, Some(film))
      case _                       => Cluster(members, None, decisions.iterator.flatMap(_.fallback).nextOption(), decisions.exists(_.unanswered > 0),
                                        decisions.iterator.flatMap(_.leaning).nextOption())
    })
  }

  /** The drafts of the films `scope` names — every film of the corpus, or the ones a tick's changes reach, closed over
   *  everything that couples one film's draft to another's ([[ProjectionScope]]) — over `index`. Drafting a scope so
   *  closed gives each of its films exactly the draft the whole corpus would. */
  def draftOf(index: ProjectionIndex, scope: ProjectionScope, normalizer: TitleNormalizer, slots: CinemaSlotBuilder,
              tokens: ScreeningTokens, at: Instant, rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]],
              memo: VenueSlotMemo = VenueSlotMemo.none, shapes: FilmShapes = FilmShapes.none,
              changed: Set[ListingKey] = Set.empty)(using LatestTitleYear): ProjectionDraft =
    new ProjectionDrafting(index, scope, normalizer, slots, tokens, at, rowsOf, memo, shapes, changed).draft

  /** Choose every film's title, year and key, and mint the id of every fresh one. `taken` says
   *  whether an id is live already (a fresh id must not be one). */
  def finish(draft: ProjectionDraft, normalizer: TitleNormalizer, taken: FilmId => Boolean)(using LatestTitleYear): ProjectionPlan = {
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
      ProjectedFilm(id, d.counter, title, year, key, d.record, d.members, d.touched)
    }
    val freshEntries = films.filter(f => draft.counters.filmIdOf(f.counter).isEmpty).map(f => FilmIdCounter(f.id.value, f.counter))
    ProjectionPlan(films, draft.retired, draft.additions ++ freshEntries, draft.regroupings, draft.takes)
  }

  /** How many venues' rows a projection reads at once to build their slots: a batch's rows are let go before the next. */
  private[identity] val RowBatch = 200

  /** A slot holds its showtimes elsewhere: only their digest and starts are on it. */
  private[identity] def isLean(slot: SourceData): Boolean = slot.showtimes.isEmpty && slot.showtimesDigest.isDefined

  /** One film's listings at one venue, and what its slots are built from. */
  private[identity] final case class VenueGroup(cinema: Cinema, rows: Seq[ProjectedListing], key: VenueSlotMemo.Key, counter: Long)

}

/** One venue of a drafted film: its listings' keys (not the listings — a venue read again holds new ones, and the old
 *  are let go), its memo key, the slots built for it, and a digest of the film each of its listings was on and of what
 *  those films' slots at the venue were (their priors, [[VenueShape.inputs]]) — the venue is reused while each listing
 *  is on the same film and those slots are unmoved. The digest, not the two lists: kept for every venue of every film,
 *  the lists, their ids' options and boxed priors were ~6 MB of worker-us's ~100k venue shapes. */
private[identity] final case class VenueShape(keys: Seq[ListingKey], memoKey: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)],
                                              inputs: Long)

private[identity] object VenueShape {
  /** What a venue's slots are reused by besides its listings: each listing's previous film and those films' priors at
   *  the venue — 64 bits of them, a finer key than the memo's own (`VenueSlotMemo.keyOf` hashes the same to 32). */
  def inputs(previous: Seq[Option[String]], priors: Seq[Int], pages: Int): Long = ContentHash.of((previous, priors, pages))
}

/** A drafted film: the listings it was drafted with (and in key order), how many carry each clean title (its anchor),
 *  and each venue. Its venue slots as one map are worked out again per draft, not kept beside the venues' own. */
private[identity] final case class FilmShape(members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                                             venues: Map[Cinema, VenueShape]) {
  /** This shape over `now`, its members as other key objects (listings read again): each key as `canonical` holds it, so
   *  a shape kept for the next projection holds no key object the index has let go. */
  def over(now: Set[ListingKey], canonical: ListingKey => ListingKey): FilmShape =
    if (now eq members) this
    else {
      // Only what holds another key object is copied: a film's listings read again are mostly the same objects, and a
      // light projection put every film of its scope over its members anew — copying every venue of a wide film each
      // time (worker-us: ~35k venue shapes a projection, each kept until the next, so promoted and left to die old).
      val moved = venues.filter { case (_, venue) => venue.keys.exists(k => canonical(k) ne k) }
      FilmShape(now, FilmShape.canonicalised(keys, canonical), titles,
        if (moved.isEmpty) venues else venues ++ moved.map { case (cinema, venue) => cinema -> venue.copy(keys = venue.keys.map(canonical)) })
    }
}

private[identity] object FilmShape {
  /** `keys` as `canonical` holds them: `keys` itself when it holds each one already. */
  def canonicalised(keys: Seq[ListingKey], canonical: ListingKey => ListingKey): Seq[ListingKey] =
    if (keys.forall(k => canonical(k) eq k)) keys else keys.map(canonical)

  /** `drafted` — a venue as this projection drafted it — or `kept`, the one the last projection drafted, when they are
   *  alike: kept, so a venue drafted again the same is the object already held, not a copy promoted and left to die. */
  def reused(kept: Option[VenueShape], drafted: VenueShape): VenueShape =
    kept.filter(k => k.memoKey == drafted.memoKey && k.inputs == drafted.inputs && (k.lean eq drafted.lean) && k.keys == drafted.keys)
      .getOrElse(drafted)
}

/**
 * Each film as the last projection drafted it, so a scoped projection drafting a film again with the same listings
 * works out only the venues that moved — a film at hundreds of venues that moved at one is not regrouped, re-keyed and
 * re-assembled whole ([[IdentityProjectionPlan.draftOf]]). A venue moved when one of its listings did (the scope's
 * changes) or when a stored record its slots carry a prior from is another one. Held between projections and
 * committed only once one is written: a refused or failed projection's drafts are dropped.
 */
final class FilmShapes private (keeping: Boolean) {
  private var kept    = Map.empty[Long, FilmShape]
  private val drafted = scala.collection.mutable.HashMap.empty[Long, FilmShape]

  private[identity] def get(counter: Long): Option[FilmShape] = if (keeping) kept.get(counter) else None
  private[identity] def put(counter: Long, shape: FilmShape): Unit = if (keeping) drafted(counter) = shape

  // Each stored film's prior slots by venue, as the slot memo's key reads them: worked out once per record — a film
  // stored the same is read the same — not once per draft of every film it lends a listing to. A film written again is
  // read again, into the map it had wherever a venue reads the same: a wide film written for a few venues' showtimes
  // rebuilt its whole map (thousands of venues), kept until the next projection — promoted, to die old, every time.
  private var priorsHeld = Map.empty[String, (MovieRecord, Map[Cinema, Int])]
  private val priorsUsed = scala.collection.mutable.HashMap.empty[String, (MovieRecord, Map[Cinema, Int])]

  private[identity] def priorsOf(id: String, record: MovieRecord): Map[Cinema, Int] = {
    val was = priorsUsed.get(id).orElse(priorsHeld.get(id))
    val known = was.filter(_._1 eq record).map(_._2).getOrElse {
      val read = VenueSlots.byVenue(record).map { case (cinema, slots) => cinema -> VenueSlotMemo.priorsAt(slots) }
      was.fold(read) { case (_, held) => FilmShapes.sharing(held, read, _ == _) }
    }
    priorsUsed(id) = record -> known
    known
  }

  /** The last drafts were written: keep them — and, after a scoped projection, every film it did not draft. */
  def commit(whole: Boolean): Unit = {
    kept = if (whole) drafted.toMap else kept ++ drafted
    drafted.clear()
    endPriors()
  }

  /** The last drafts were not written. */
  def discard(): Unit = { drafted.clear(); endPriors() }

  // Only the records a projection read stay worked out: a record replaced is let go.
  private def endPriors(): Unit = { priorsHeld = priorsUsed.toMap; priorsUsed.clear() }
}

object FilmShapes {
  /** `now` as `held` with only what differs moved — a value `alike` the held one kept as held: `held` itself when nothing
   *  differs, else a map sharing all but the moved entries with it. */
  private[identity] def sharing[K, V](held: Map[K, V], now: Map[K, V], alike: (V, V) => Boolean): Map[K, V] = {
    val kept = if (held.keysIterator.forall(now.contains)) held else held.filter { case (k, _) => now.contains(k) }
    now.foldLeft(kept) { case (map, (k, v)) => if (map.get(k).exists(alike(_, v))) map else map.updated(k, v) }
  }

  def apply(): FilmShapes = new FilmShapes(keeping = true)
  /** Shapes of nothing: every film drafted whole. */
  def none: FilmShapes = new FilmShapes(keeping = false)
}
