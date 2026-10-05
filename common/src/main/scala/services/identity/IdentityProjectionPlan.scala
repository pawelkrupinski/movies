package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Filmweb, FilmwebPages, Imdb, MovieRecord, Source, SourceData}
import services.movies.{CacheKey, CinemaSlotBuilder, FilmId, LeanRecords, ListingKey, ScreeningTokens,
  ShowtimesDigest, StoredMovieRecord, TitleNormalizer}
import services.resolution.TmdbAttempt

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
 *  map extended over the previous films, the regroupings, and the canary — how the resolver's
 *  clusters relate to the films stored before it ran ([[ShadowDiff]]'s relations). */
final case class ProjectionDraft(drafts: Seq[FilmDraft], retired: Seq[FilmId], vanished: Seq[FilmId], counters: FilmIdCounters,
                                 additions: Seq[FilmIdCounter], regroupings: Regroupings, canary: Map[ShadowRelation, Int],
                                 venues: Map[Long, Map[Cinema, Seq[ListingKey]]] = Map.empty,
                                 build: (Cinema, Seq[ListingKey], IdentityProjectionPlan.VenueRows) => Seq[(Source, SourceData)] = (_, _, _) => Nil,
                                 rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]] = _ => Map.empty) {
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
        val byKey = IdentityProjectionPlan.rowsByKey(cinema, rows.getOrElse(cinema, Nil))
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
 *  retire, the FilmId-map entries it adds, and the draft's regroupings and canary. */
final case class ProjectionPlan(films: Seq[ProjectedFilm], retired: Seq[FilmId], counterAdditions: Seq[FilmIdCounter],
                                regroupings: Regroupings, canary: Map[ShadowRelation, Int])

/**
 * THE IDENTITY PROJECTION's decisions (docs/design/identity-resolver.md §2 "Projection", §6, §8
 * phase 3 = programme phase 5), as pure functions of the accepted listings, the resolver's
 * decisions over them, the films stored before, and the persisted FilmId map:
 *
 *  1. every stored film is the set of today's listings its slots hold (the slot's own listing key,
 *     else the shadow diff's mapping by slot, `PipelineFilms`), numbered through the FilmId map ([[FilmIdCounters]], largest film first
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
            rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]], memo: VenueSlotMemo = VenueSlotMemo.none): ProjectionDraft = {
    val whole = index(listings, resolution, stored, counters, normalizer)
    draftOf(whole, whole.everything, normalizer, slots, tokens, at, rowsOf, memo)
  }

  /** What a projection reads off the whole listing set, the resolution and the stored films before it drafts any film:
   *  each a pass over the corpus, made every tick, and what [[ProjectionScope]] closes a tick's changes over. */
  def index(listings: Seq[ProjectedListing], resolution: Resolution, stored: Seq[StoredMovieRecord], counters: FilmIdCounters,
            normalizer: TitleNormalizer): ProjectionIndex = {
    val byKey      = oneByKey(listings)
    val storedById = stored.map(r => r.id.value -> r).toMap

    // The stored films as listing sets, numbered.
    // A listing is on the stored film whose slot it IS (the slot's own listing key); a listing no slot
    // names — one a venue's same-title fold hid behind another's slot — on the film holding its slot.
    val slotOwner: Map[ListingKey, PipelineFilmRef] = stored.flatMap { r =>
      val ref = PipelineFilmRef(r.id.value, r.record.tmdbId)
      slotsOf(r).flatMap(_.key.map(_ -> ref))
    }.groupMap(_._1)(_._2).flatMap { case (k, refs) => ownerOf(refs.toSet).map(k -> _) }
    val previousOf: Map[ListingKey, PipelineFilmRef] =
      PipelineFilms.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
    val listingsOf = previousOf.toSeq.groupMap(_._2.id)(_._1).view.mapValues(_.toSet).toMap
    val covered    = counters.covering(listingsOf.toSeq.map { case (id, ls) => IdSeeding.Film(id, ls) })
    val additions  = covered.entries.filterNot(e => counters.counterOf(e.filmId).isDefined)

    // Clusters, one per film: the resolver's, with the clusters of one TMDB film joined.
    val clusters: Map[ClusterId, Cluster] = resolution.decisions.groupBy(clusterIdOf).flatMap { case (id, ds) =>
      IdentityProjectionPlan.clusterOf(id, ds, byKey.contains).map(id -> _)
    }
    val clusterOf = clusters.iterator.flatMap { case (id, c) => c.members.iterator.map(_ -> id) }.toMap
    ProjectionIndex(byKey, storedById, previousOf, listingsOf, covered, additions, clusters, clusterOf)
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
  def ownerOf(refs: Set[PipelineFilmRef]): Option[PipelineFilmRef] = refs.minByOption(_.id)

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
              changed: Set[ListingKey] = Set.empty): ProjectionDraft = {
    import index.{byKey, storedById, covered}
    // Within the scope: its stored films' listings, and its clusters. Everything outside it is a film no change reached,
    // whose draft is the film as stored.
    val previousOf    = if (scope.whole) index.previousOf else index.previousOf.filter { case (k, _) => scope.listings(k) }
    val previousFilms = (if (scope.whole) index.listingsOf.toSeq else scope.films.toSeq.flatMap(id => index.listingsOf.get(id).map(id -> _)))
      .map { case (id, ls) => IdSeeding.Film(id, ls) }
    val inScope       = if (scope.whole) index.clusters.values.toSeq else scope.clusters.toSeq.map(index.clusters)
    val clusters      = inScope.map(c => c.members -> c.film)
    val unmatchedOf   = inScope.filter(_.film.isEmpty).map(c => c.members -> c).toMap
    val stored        = if (scope.whole) storedById.values.toSeq else scope.films.toSeq.flatMap(storedById.get)
    val numbered      = previousFilms.map(f => covered.counterOf(f.id).get -> f.listings)

    // 2. Ids by overlap.
    val filmOf    = clusters.toMap
    val assigned  = IdAssigner.assign(numbered, clusters.map(_._1), covered.nextCounter)
    val previousIdOf: Long => Option[String] = c => Option.when(c < covered.nextCounter)(covered.filmIdOf(c)).flatten

    // 3. Each film's record. Its venue slots are LEAN, each venue's from the memo when nothing it is built from
    // moved, else built from the venue's rows, read a batch of venues at a time and let go.
    // Each stored film's prior slots by venue, as the slot memo's key reads them: worked out once per film, not
    // once per venue of every film it lends a listing to — a US film showing at hundreds of venues was scanned
    // whole for each of them.
    // Each stored film's venue slots by venue, worked out once per film for the same reason.
    val slotsByFilm = scala.collection.mutable.HashMap.empty[String, Map[Cinema, Seq[(Source, SourceData)]]]
    def slotsByVenueOf(id: String): Map[Cinema, Seq[(Source, SourceData)]] = slotsByFilm.getOrElseUpdate(id,
      storedById.get(id).fold(Map.empty[Cinema, Seq[(Source, SourceData)]])(_.record.data.toSeq.collect {
        case (showing: CinemaShowing, slot) => showing.cinema -> ((showing: Source) -> slot) }.groupMap(_._1)(_._2)))
    def priorsOf(id: String): Map[Cinema, Int] = storedById.get(id).fold(Map.empty[Cinema, Int])(r => shapes.priorsOf(r.record))
    // One venue's listings on a film, and what its slots are built from.
    def previousAt(keys: Seq[ListingKey]): Seq[Option[String]] = keys.map(k => index.previousOf.get(k).map(_.id))
    def priorsAt(cinema: Cinema, previous: Seq[Option[String]]): Seq[Int] =
      previous.flatten.distinct.sorted.map(id => priorsOf(id).getOrElse(cinema, 0))
    def groupOf(cinema: Cinema, ofVenue: Seq[ProjectedListing], counter: Long): (VenueGroup, Seq[Option[String]], Seq[Int]) = {
      // The prior slots any of these rows' previous films held at the venue: a superset of the one each built
      // slot carries forward, so a change to any of them is a change to the key — and which film each row is on,
      // since a slot carries forward its own listing's film's slot: two rows trading films trade the slot each
      // carries, the set of priors unchanged. Left out, a slot built over one film's prior was taken from the memo
      // for the other's (a venue's "Lalka" and "Lalka 2D" trading films kept the director only one of them had).
      val previous = previousAt(ofVenue.map(_.listing.key))
      val priors   = priorsAt(cinema, previous)
      // Each row's film by its counter, which a film keeps for good and has from its first draft: what the key of a
      // slot once written is worked out from ([[VenueSlotMemo.written]]), a film new to the store included.
      (VenueGroup(cinema, ofVenue, VenueSlotMemo.keyOf(cinema.displayName, ofVenue, priors, previous.map(_.flatMap(covered.counterOf))),
        counter), previous, priors)
    }
    // A film drafted again with the listings it was last drafted with is drafted again only where it moved: a venue one of
    // whose listings, or whose listings' previous films' stored slots, moved since ([[FilmShapes]]).
    val reuse   = !scope.whole
    lazy val placedAt: Map[ListingKey, Long] = assigned.idOfListing
    val dirtyAt = if (!reuse) Map.empty[Long, Set[ListingKey]]
                  else changed.iterator.filter(byKey.contains).flatMap(k => placedAt.get(k).map(_ -> k)).toSeq.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val planned = assigned.ids.map { case (counter, members) =>
      // The same listings: the same keys in the same order, the same titles (a key holds its listing's title). An equal set
      // of other key objects is the same listings read again — a venue's scrape reads every listing it prints, moved or not.
      shapes.get(counter).filter(sh => reuse && ((sh.members eq members) || sh.members == members))
        .map(_.over(members, k => byKey.get(k).fold(k)(_.listing.key))) match {
        case Some(was) =>
          val dirtyVenues = dirtyAt.getOrElse(counter, Set.empty).map(byKey(_).listing.cinema)
          val groups = was.venues.toSeq.sortBy(_._1.displayName).map { case (cinema, venue) =>
            // Moved: one of its listings did, or which film one is on, or the slots those films hold at the venue.
            val moved = dirtyVenues(cinema) || { val previous = previousAt(venue.keys); VenueShape.inputs(previous, priorsAt(cinema, previous)) != venue.inputs }
            if (!moved) cinema -> Left(venue)
            else cinema -> Right(groupOf(cinema, venue.keys.map(byKey), counter))
          }
          Planned(counter, members, was.keys, was.titles, groups, Some(was))
        case _ =>
          val keys   = members.toSeq.sorted
          val rows   = keys.map(byKey)
          val titles = rows.groupMapReduce(_.listing.cleanTitle)(_ => 1)(_ + _)
          val groups = rows.groupBy(_.listing.cinema).toSeq.sortBy(_._1.displayName).map { case (cinema, ofVenue) =>
            cinema -> Right(groupOf(cinema, ofVenue, counter))
          }
          Planned(counter, members, keys, titles, groups, None)
      }
    }
    val build: (Cinema, Seq[ListingKey], VenueRows) => Seq[(Source, SourceData)] =
      // Over the whole index: a slot's prior is its representative listing's film's, whichever of the venue's listings that is.
      (cinema, keys, rows) => VenueSlots.buildVenue(cinema, keys, rows, index.previousOf, storedById, normalizer, slots, tokens)
    val venueSlots = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, Seq[(Source, SourceData)]]
    // The slots the stored film of a group's listings holds at its venue: what a restarted worker's memo reuses
    // when they are recorded as built from the group's key.
    def storedAt(group: VenueGroup): Option[Seq[(Source, SourceData)]] =
      group.rows.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct match {
        case Seq(id) => storedById.get(id).map(_ => slotsByVenueOf(id).getOrElse(group.cinema, Nil))
        case _ => None
      }
    planned.foreach(_.groups.foreach { case (_, Left(venue)) => venueSlots(venue.memoKey) = venue.lean; case _ => () })
    val pending    = planned.flatMap(_.groups.collect { case (_, Right((group, _, _))) => group }).filter(group =>
      memo.lookup(group.key, storedAt(group)).fold(true) { lean => venueSlots(group.key) = lean; false })
    pending.groupBy(_.cinema).toSeq.sortBy(_._1.displayName).grouped(RowBatch).foreach { batch =>
      val fetched = rowsOf(batch.map(_._1).toSet)
      batch.foreach { case (cinema, groups) =>
        val rows = rowsByKey(cinema, fetched.getOrElse(cinema, Nil))
        groups.foreach { group =>
          val lean = build(cinema, group.rows.map(_.listing.key), rows).map { case (source, slot) => source -> ShowtimesDigest.stripSlot(slot) }
          venueSlots(group.key) = lean
          memo.store(group.key, lean, keep = VenueSlots.readAsListed(cinema, group.rows, rows, normalizer), writtenAs = Some(group.counter))
        }
      }
    }
    val drafts = planned.map { plan =>
      import plan.{counter, members, keys}
      val previous = previousIdOf(counter).flatMap(storedById.get)
      val film     = filmOf(members)
      val anchor   = plan.titles.toSeq.sortBy { case (t, n) => (-n, t) }.headOption.map(_._1).getOrElse("")
      // Each venue's slots — of a film drafted again, the venues it rebuilt replace theirs — and the film's as a whole.
      val venueShapes = plan.groups.iterator.map {
        case (cinema, Left(venue))            => cinema -> venue
        case (cinema, Right((group, previous, priors))) => cinema -> VenueShape(group.rows.map(_.listing.key), group.key, venueSlots(group.key), VenueShape.inputs(previous, priors))
      }.toMap
      val venueData = venueShapes.valuesIterator.flatMap(_.lean).toMap
      shapes.put(counter, FilmShape(members, keys, plan.titles, venueShapes))
      val sameFilm = previous.exists(_.record.tmdbId == film)
      val base = previous.filter(_ => sameFilm).map(_.record).getOrElse(
        MovieRecord(retainedSynopses = previous.map(_.record.retainedSynopses).getOrElse(Map.empty)))
      // A film TMDB has no record of carries the IMDb id of the fallback film the resolver took for it — else the one it
      // held, when that is the TMDB film its listings' evidence leans to though no rule took it ([[ResolverDecision.Leaning]]:
      // "Tatarak", Wajda's 2009 film below the rating cut), else none: an id a title search guessed (the pipeline's, the
      // former TMDB-less enrichments') is not the resolver's answer — PL "Lalka" (2026) held the 1968 film's, and its 6.9.
      // The rating and IMDb's slot go with an id that goes. A no-match reached before every question was answered is a
      // gap, not a verdict: the record keeps what it holds.
      // One standing on Filmweb's or Wikidata's own id (the agreement's, where no record links an IMDb id) links that
      // record instead: its Filmweb page — replacing a page a title search guessed, with its rating and slot — or its
      // Wikidata item.
      val answered = unmatchedOf.get(members).filterNot(_.unanswered)
      val imdb   = answered.map { cluster =>
        cluster.fallback.filter(_.source == "imdb").map(_.id)
          .orElse(base.imdbId.filter(id => cluster.leaning.exists(_.imdbNumber == IdentityMeasures.imdbNumber(id)))) }
      val standsOn = answered.flatMap(_.fallback)
      val linked = imdb.fold(base)(id => if (id == base.imdbId) base else base.copy(imdbId = id, imdbRating = None, data = base.data - Imdb))
      val held   = standsOn.fold(linked) {
        case ResolverDecision.Fallback("filmweb", id, _, title, year) if !linked.filmwebUrl.flatMap(FilmwebPages.idOf).map(_.toString).contains(id) =>
          linked.copy(filmwebUrl = id.toIntOption.map(FilmwebPages.url(_, "film", title.getOrElse(anchor), year)), filmwebRating = None,
            data = linked.data - Filmweb)
        case ResolverDecision.Fallback("wikidata", id, _, _, _) => linked.copy(wikidataId = Some(id))
        case _                                                  => linked
      }
      val nonVenue = held.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty }
      val networks = previous.fold(Map.empty[Source, SourceData])(_.record.data.filter { case (source, _) => Cinema.Networks.contains(source) })
      val previousNonVenue = previous.fold(Set.empty[Source])(_.record.data.keysIterator.filter(Source.cinemaOf(_).isEmpty).toSet)
      val record = held.copy(
        tmdbId        = film,
        tmdbAttempt   = if (film.isDefined) None else base.tmdbAttempt.orElse(Some(TmdbAttempt(ResolverVerdict, at))),
        searchTitle   = base.searchTitle.orElse(Some(normalizer.apiQuery(normalizer.recase(anchor)))),
        // A chain's network detail slot is venue source data no listing is published at: kept from the
        // stored film whatever it is matched to, as its venue slots are rebuilt from theirs.
        // The venue slots first, and the rest over them: no venue slot is a non-venue source or a chain's network one.
        data          = venueData ++ nonVenue ++ networks)
      // Of a film drafted again, what may differ from the last draft's write: its non-venue sources, and the venues it
      // rebuilt — their slots as written then and as built now. Every other venue's slot is the one written.
      val touched = plan.was.map { was =>
        val rebuilt = plan.groups.collect { case (cinema, Right(_)) => cinema }
        previousNonVenue ++ nonVenue.keySet ++ networks.keySet ++
          rebuilt.flatMap(cinema => was.venues(cinema).lean.map(_._1) ++ venueShapes(cinema).lean.map(_._1))
      }
      FilmDraft(counter, previous.map(_.id), keys, record, anchor, touched)
    }
    val venues = planned.map(plan => plan.counter -> plan.groups.map {
      case (cinema, Left(venue))           => cinema -> venue.keys
      case (cinema, Right((group, _, _)))  => cinema -> group.rows.map(_.listing.key)
    }.toMap).toMap

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
    ProjectionDraft(drafts, retired, retired.filterNot(id => index.placed(id.value)), covered, index.additions, regroupings,
      canaryOf(clusters, previousOf), venues, build, rowsOf)
  }

  /** The canary over every cluster of `index`: what a projection reports, whichever films it drafted. */
  def canary(index: ProjectionIndex): Map[ShadowRelation, Int] =
    canaryCounts(index.clusters.valuesIterator.map(relationOf(index, _)._1))

  /** `ShadowDiff.clustersOf`'s relation of `cluster`, read off the index: a film's listings are `listingsOf` it, so nothing
   *  is sorted into a decision or grouped by film again — on US that was ~100k keys sorted and grouped every five minutes.
   *  With it, the films it read: each stored film its members are on, and that film's listings as the index holds them. */
  private[identity] def relationOf(index: ProjectionIndex, cluster: Cluster): (Option[ShadowRelation], Seq[(String, Set[ListingKey])]) = {
    val placed = cluster.members.filter(index.previousOf.contains)
    val films  = placed.iterator.map(index.previousOf).toSet
    val read   = films.toSeq.map(f => f.id -> index.listingsOf.getOrElse(f.id, Set.empty))
    val relation =
      if (films.isEmpty) None
      else if (films.sizeIs > 1) Some(ShadowRelation.Merged)
      else if (index.listingsOf.getOrElse(films.head.id, Set.empty) != placed) Some(ShadowRelation.Split)
      else if (films.head.tmdbId == cluster.film) Some(ShadowRelation.Identical)
      else Some(ShadowRelation.Moved)
    (relation, read)
  }

  private[identity] def canaryCounts(relations: Iterator[Option[ShadowRelation]]): Map[ShadowRelation, Int] = {
    val counts = scala.collection.mutable.HashMap.empty[ShadowRelation, Int]
    relations.foreach(_.foreach(r => counts(r) = counts.getOrElse(r, 0) + 1))
    ShadowRelation.values.map(r => r -> counts.getOrElse(r, 0)).toMap
  }

  /** The canary compares the films as STORED — one per TMDB film — with the films before. */
  private def canaryOf(clusters: Seq[(Set[ListingKey], Option[Int])], previousOf: Map[ListingKey, PipelineFilmRef]): Map[ShadowRelation, Int] =
    ShadowDiff.counts(ShadowDiff.clustersOf(clusters.map { case (members, film) =>
      ResolverDecision(members.toSeq.sorted, film, 1.0, ResolverDecision.Basis.OwnMatch, Nil)()
    }, previousOf))

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
      ProjectedFilm(id, d.counter, title, year, key, d.record, d.members, d.touched)
    }
    val freshEntries = films.filter(f => draft.counters.filmIdOf(f.counter).isEmpty).map(f => FilmIdCounter(f.id.value, f.counter))
    ProjectionPlan(films, draft.retired, draft.additions ++ freshEntries, draft.regroupings, draft.canary)
  }

  type VenueRows = VenueSlots.VenueRows
  def rowsByKey(cinema: Cinema, fetched: Seq[CinemaMovie]): VenueRows = VenueSlots.rowsByKey(cinema, fetched)

  /** How many venues' rows a projection reads at once to build their slots: a batch's rows are let go before the next. */
  private[identity] val RowBatch = 200

  /** A slot holds its showtimes elsewhere: only their digest and starts are on it. */
  private[identity] def isLean(slot: SourceData): Boolean = slot.showtimes.isEmpty && slot.showtimesDigest.isDefined

  /** One film's listings at one venue, and what its slots are built from. */
  private[identity] final case class VenueGroup(cinema: Cinema, rows: Seq[ProjectedListing], key: VenueSlotMemo.Key, counter: Long)

  /** A film [[draftOf]] plans: its venues' groups, each one kept from the last draft (`Left`) or worked out (`Right`, with
   *  the stored records of the films its priors are from). */
  private final case class Planned(counter: Long, members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                                   groups: Seq[(Cinema, Either[VenueShape, (VenueGroup, Seq[Option[String]], Seq[Int])])],
                                   was: Option[FilmShape])

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
  def inputs(previous: Seq[Option[String]], priors: Seq[Int]): Long = ContentHash.of((previous, priors))
}

/** A drafted film: the listings it was drafted with (and in key order), how many carry each clean title (its anchor),
 *  and each venue. Its venue slots as one map are worked out again per draft, not kept beside the venues' own. */
private[identity] final case class FilmShape(members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                                             venues: Map[Cinema, VenueShape]) {
  /** This shape over `now`, its members as other key objects (listings read again): each key as `canonical` holds it, so
   *  a shape kept for the next projection holds no key object the index has let go. */
  def over(now: Set[ListingKey], canonical: ListingKey => ListingKey): FilmShape =
    if (now eq members) this
    else FilmShape(now, keys.map(canonical), titles, venues.map { case (cinema, venue) => cinema -> venue.copy(keys = venue.keys.map(canonical)) })
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

  // Each stored record's prior slots by venue, as the slot memo's key reads them: worked out once per record — a film
  // stored the same is read the same — not once per draft of every film it lends a listing to.
  private val priorsByRecord = new java.util.IdentityHashMap[MovieRecord, Map[Cinema, Int]]()
  private val priorsUsed     = new java.util.IdentityHashMap[MovieRecord, Map[Cinema, Int]]()

  private[identity] def priorsOf(record: MovieRecord): Map[Cinema, Int] = {
    val known = Option(priorsUsed.get(record)).orElse(Option(priorsByRecord.get(record))).getOrElse(
      record.data.toSeq.collect { case (cs: CinemaShowing, slot) => cs.cinema -> ((cs: Source) -> slot) }
        .groupMap(_._1)(_._2).map { case (cinema, slots) => cinema -> VenueSlotMemo.priorsAt(slots) })
    priorsUsed.put(record, known)
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
  private def endPriors(): Unit = { priorsByRecord.clear(); priorsByRecord.putAll(priorsUsed); priorsUsed.clear() }
}

object FilmShapes {
  def apply(): FilmShapes = new FilmShapes(keeping = true)
  /** Shapes of nothing: every film drafted whole. */
  def none: FilmShapes = new FilmShapes(keeping = false)
}

/**
 * The canary over every cluster of the index ([[IdentityProjectionPlan.canary]]), kept from one projection to the next:
 * a scoped projection works out again only the relations of its scope's clusters and of the last one's. A cluster's
 * relation reads its members, their previous films and those films' listings, and a scope is closed over every one of
 * them that moved ([[ProjectionScope.close]]); the last scope's clusters are read again because its writes — after its
 * canary — may have put their listings on other films. Every other cluster's relation is the one it had: on worker-us,
 * walking every cluster's members each tick was 6% of a light projection.
 */
final class CanaryTally {
  private final case class Kept(relation: Option[ShadowRelation], films: Seq[(String, Set[ListingKey])])
  private val relations = scala.collection.mutable.HashMap.empty[ClusterId, Kept]
  private var last      = Set.empty[ClusterId]

  /** The canary of `index`, a scoped projection of `scope` reading it. */
  def of(index: ProjectionIndex, scope: ProjectionScope): Map[ShadowRelation, Int] = {
    (scope.clusters.iterator ++ last.iterator).foreach(relations.remove)
    // A listing that left a film moved it, though the scope reaches the listing's new film only: the film's listings,
    // held as one set replaced on each change, are another object.
    relations.filterInPlace((id, kept) => index.clusters.contains(id) &&
      kept.films.forall { case (film, listings) => index.listingsOf.getOrElse(film, Set.empty) eq listings })
    index.clusters.foreach { case (id, cluster) =>
      if (!relations.contains(id)) relations(id) = { val (relation, films) = IdentityProjectionPlan.relationOf(index, cluster); Kept(relation, films) }
    }
    last = scope.clusters
    IdentityProjectionPlan.canaryCounts(relations.valuesIterator.map(_.relation))
  }

  /** A projection of the whole corpus reported its own canary, and wrote any cluster's films: the next works out all. */
  def reset(): Unit = { relations.clear(); last = Set.empty }
}
