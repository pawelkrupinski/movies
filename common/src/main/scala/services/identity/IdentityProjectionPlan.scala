package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import services.movies.{CacheKey, CinemaSlotBuilder, FilmId, ListingKey, MovieRecordMerge, ScrapeListing, ScreeningTokens,
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
      film -> film.record.data.collect {
        case (showing: CinemaShowing, slot) if IdentityProjectionPlan.isLean(slot) && !stored(film.id).flatMap(_.data.get(showing))
          .exists(held => IdentityProjectionPlan.isLean(held) && ShowtimesDigest.slotLeanEqual(slot, held)) => showing.cinema
      }.toSet
    }
    val built = scala.collection.mutable.HashMap.empty[(Long, Cinema), Seq[(Source, SourceData)]]
    // Each venue's films that differ there, so a batch of venues visits only those, each venue's rows grouped once.
    val filmsAt = wanted.flatMap { case (film, differing) => differing.map(_ -> film) }.groupMap(_._1)(_._2)
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

/**
 * The venue slots a projection built, kept from one tick to the next by what they were built FROM —
 * the venue, its rows on the film (every showtime included) and the prior slots their previous films
 * held there — so a film whose listings did not change is not rebuilt every five minutes: on the US
 * corpus that rebuild was ~60 s and ~2.4 GB a tick for the ~50 films that changed of ~2,200.
 *
 * Kept LEAN ([[ShowtimesDigest.stripSlot]]: the showtimes' digest and starts, not the showtimes), as
 * the cache keeps them, so the memo holds a fraction of the corpus; a film that turns out to differ
 * from what is stored is built in full before it is written ([[ProjectionDraft.complete]]).
 * Only what the last tick used is kept.
 *
 * Across a restart the memo is gone, but what it held is in the store: each kept slot set is also a
 * [[VenueSlotMemo.fingerprint]] — what it was built from, under which build of the slot code
 * (`environment`), and what it came to — which the projection persists ([[VenueSlotFingerprints]]) and
 * [[seed]]s a fresh memo with. Until its first projection closes, a memo that misses takes the slots the
 * listings' previous film holds at the venue when their fingerprint was recorded: they are what the
 * listings would build. A venue whose rows, prior slots or stored slots moved while the worker was down
 * fingerprints otherwise, and is built.
 */
class VenueSlotMemo(environment: Long = 0L) {
  private var previous = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, VenueSlotMemo.Entry]
  private var current  = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, VenueSlotMemo.Entry]
  private var recorded = Set.empty[Long]
  private var hits     = 0
  private var builds   = 0
  // Why a build was needed, for the projection's log: the same venue's same listings with other rows, with other
  // prior slots, or listings the last projection did not have at the venue at all.
  private var seenBefore = scala.collection.mutable.HashMap.empty[(String, Int), VenueSlotMemo.Key]
  private var seenNow    = scala.collection.mutable.HashMap.empty[(String, Int), VenueSlotMemo.Key]
  private var missRows, missPriors, missNew = 0

  /** The fingerprints a previous run of the worker kept, for this memo's first projection to reuse stored slots by —
   *  none, unless that run's slots were made under this memo's `environment` (its mark is among them): then no
   *  stored slot can match, and finding that out a venue at a time was most of a US first tick's allocation. */
  def seed(fingerprints: Set[Long]): Unit = synchronized {
    recorded = if (fingerprints(VenueSlotMemo.mark(environment))) fingerprints else Set.empty
  }

  /** The lean slots the last projection built for `key`, if any — or, before this memo's first projection
   *  closes, the slots stored at the venue (`stored`) if a previous run recorded them as built from `key`; kept
   *  for the next. */
  private[identity] def lookup(key: VenueSlotMemo.Key, stored: => Option[Seq[(Source, SourceData)]] = None): Option[Seq[(Source, SourceData)]] =
    synchronized {
      seenNow((key.venue, key.keys)) = key
      previous.get(key).orElse(Option.when(recorded.nonEmpty)(stored).flatten.map(_.map { case (source, slot) =>
        source -> ShowtimesDigest.stripSlot(slot) }).map(lean => VenueSlotMemo.Entry(lean, fingerprint(key, lean)))
        .filter(entry => recorded(entry.fingerprint))) match {
        case Some(entry) => current(key) = entry; hits += 1; Some(entry.lean)
        case None =>
          seenBefore.get((key.venue, key.keys)) match {
            case Some(before) if before.rows != key.rows => missRows += 1
            case Some(_)                                 => missPriors += 1
            case None                                    => missNew += 1
          }
          None
      }
    }

  /** `lean`, built this projection for `key`: kept for the next when `keep` (it was built from the rows `key` names) —
   *  under `key`, and under the key the same rows have once `lean` is written and is their prior slot
   *  ([[VenueSlotMemo.written]]), which the next projection would otherwise build again only to get `lean` back. */
  private[identity] def store(key: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)], keep: Boolean,
                              writtenAs: Option[Long] = None): Unit = synchronized {
    if (keep) {
      current(key) = VenueSlotMemo.Entry(lean, fingerprint(key, lean))
      val after = writtenAs.fold(key)(VenueSlotMemo.written(key, lean, _))
      if (after != key) current(after) = VenueSlotMemo.Entry(lean, fingerprint(after, lean))
    }
    builds += 1
  }

  private def fingerprint(key: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)]): Long = VenueSlotMemo.fingerprint(environment, key, lean)

  /** Close a projection, and say how many slots it took from the memo and built. A projection of the whole corpus keeps
   *  only the slots it used; one of a scope (`retainUnseen`) also keeps every other film's, which it did not look at,
   *  dropping only the slots of a venue's listings it built anew from other rows. */
  def endTick(retainUnseen: Boolean = false): (Int, Int) = synchronized {
    if (retainUnseen) {
      seenNow.foreach { case (listings, key) =>
        seenBefore.get(listings).filter(_ != key).foreach(previous.remove)
        seenBefore(listings) = key
      }
      previous ++= current
      current.clear()
      seenNow.clear()
    } else {
      val used = current
      current = previous; current.clear(); previous = used
      val seen = seenNow
      seenNow = seenBefore; seenNow.clear(); seenBefore = seen
    }
    recorded = Set.empty
    val counts = (hits, builds)
    hits = 0; builds = 0
    counts
  }

  /** The fingerprints of the slots the last projection kept, and this memo's environment's mark: what a restarted
   *  worker's memo is seeded with. */
  def fingerprints: Set[Long] = synchronized(previous.valuesIterator.map(_.fingerprint).toSet + VenueSlotMemo.mark(environment))

  /** Why the last projection's builds were needed: (rows moved, prior slots moved, listings new to the venue). */
  def lastMisses(): (Int, Int, Int) = synchronized {
    val misses = (missRows, missPriors, missNew)
    missRows = 0; missPriors = 0; missNew = 0
    misses
  }
}

object VenueSlotMemo {
  /** What one venue's slots on a film are built from: the venue, its rows (content and listing keys),
   *  each row's previous film and the detail fields of the slots those films held at the venue — every input of
   *  `ScrapeListing.prepare` and `CinemaSlotBuilder.build` that is not fixed for the worker. */
  final case class Key(venue: String, rows: Int, keys: Int, priors: Int, size: Int)

  /** Slots the memo holds, and their fingerprint. */
  private final case class Entry(lean: Seq[(Source, SourceData)], fingerprint: Long)

  /** A memo of nothing: every film's slots built. */
  def none: VenueSlotMemo = new VenueSlotMemo {
    override private[identity] def lookup(key: Key, stored: => Option[Seq[(Source, SourceData)]]): Option[Seq[(Source, SourceData)]] = None
  }

  /** The build of the code a slot is made by, as a version: the build's digest of this file and of what the slot
   *  builder and the landing's fold reach, the resolver left out (`venue-slot-version.txt`, generated by `build.sbt` as
   *  `IdentityRules.codeVersion` is, from `IdentityRulesSources.VenueSlotRoots`). A deploy that changes none of them
   *  keeps every recorded fingerprint. */
  lazy val codeVersion: String =
    Option(getClass.getResourceAsStream("/venue-slot-version.txt")).fold("unknown") { stream =>
      try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8).trim finally stream.close()
    }

  /** What a worker's slots are made under besides their inputs: the slot code's build and the title rules. */
  def environment(normalizer: TitleNormalizer): Long = ContentHash.of((codeVersion, normalizer.rules.toString))

  /** Slots `lean` were built from `key` under `environment`, as one number the same in every JVM: every field of
   *  every slot but the showtimes, which their digest stands for. */
  private[identity] def fingerprint(environment: Long, key: Key, lean: Seq[(Source, SourceData)]): Long =
    ContentHash.of((environment, key, lean.map { case (source, slot) =>
      (source match { case showing: CinemaShowing => s"${showing.cinema.displayName}|${showing.titleKey}"; case other => other.toString },
        slot.copy(showtimes = Nil, showtimesDigest = None, showtimeStartMinutes = None), ShowtimesDigest.slotDigest(slot))
    }.sortBy(_._1)))

  /** The fields `CinemaSlotBuilder.build` carries forward from a prior slot. */
  private[identity] def carried(slot: SourceData): Int =
    (slot.originalTitle, slot.synopsis, slot.cast, slot.director, slot.runtimeMinutes, slot.releaseYear, slot.countries,
      slot.genres, slot.posterUrl, slot.trailerUrl, slot.ageRating).##

  /** One film's slots at one venue as a key's `priors` reads them: what each carries forward, by its title. */
  private[identity] def priorsAt(slots: Iterable[(Source, SourceData)]): Int =
    slots.iterator.collect { case (showing: CinemaShowing, slot) => (showing.titleKey, carried(slot)) }.toSeq.sorted.##

  /** `key` once `lean`, built from it, is written as film `counter`: its rows unchanged, each now on that film, the prior
   *  slots they read now its `lean`. Built over those, each slot comes out as `lean` holds it — `CinemaSlotBuilder.build` carries a prior
   *  field forward only where the row has none, so a slot carries forward exactly what it already holds. */
  private[identity] def written(key: Key, lean: Seq[(Source, SourceData)], counter: Long): Key =
    key.copy(priors = (Seq(priorsAt(lean)), Seq.fill(key.size)(Some(counter))).##)

  /** What a memo under `environment` records beside its fingerprints, so a memo seeded with them knows whether any
   *  can match its own. */
  private[identity] def mark(environment: Long): Long = ContentHash.of(("venue-slot-environment", environment))
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

  def slotsOf(r: StoredMovieRecord): Seq[Slot] = r.record.data.toSeq.map { case (source, sd) =>
    Slot(source match { case cs: CinemaShowing => Some(cs.cinema.displayName -> cs.titleKey); case _ => None }, sd, ListingKey.ofSource(source, sd))
  }

  /** The film a listing is the slot of, of the films whose slot it is: the smallest id. */
  def ownerOf(refs: Set[PipelineFilmRef]): Option[PipelineFilmRef] = refs.minByOption(_.id)

  /** The cluster a decision is part of: its TMDB film's, or for no film its own. */
  def clusterIdOf(d: ResolverDecision): ClusterId = d.film.fold[ClusterId](ClusterId.Unmatched(d.members.min))(ClusterId.Matched(_))

  /** Cluster `id` of `decisions`, over the listings `published`: none when it publishes none. */
  def clusterOf(id: ClusterId, decisions: Seq[ResolverDecision], published: ListingKey => Boolean): Option[Cluster] = {
    val members = decisions.iterator.flatMap(_.members).filter(published).toSet
    Option.when(members.nonEmpty)(Cluster(members, id match { case ClusterId.Matched(film) => Some(film); case _ => None }))
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
    val clusters      = (if (scope.whole) index.clusters.values.toSeq else scope.clusters.toSeq.map(index.clusters))
      .map(c => c.members -> c.film)
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
    def previousAt(ofVenue: Seq[ProjectedListing]): Seq[Option[String]] = ofVenue.map(r => index.previousOf.get(r.listing.key).map(_.id))
    def priorsAt(cinema: Cinema, previous: Seq[Option[String]]): Seq[Int] =
      previous.flatten.distinct.sorted.map(id => priorsOf(id).getOrElse(cinema, 0))
    def groupOf(cinema: Cinema, ofVenue: Seq[ProjectedListing], counter: Long): (VenueGroup, Seq[Option[String]], Seq[Int]) = {
      // The prior slots any of these rows' previous films held at the venue: a superset of the one each built
      // slot carries forward, so a change to any of them is a change to the key — and which film each row is on,
      // since a slot carries forward its own listing's film's slot: two rows trading films trade the slot each
      // carries, the set of priors unchanged. Left out, a slot built over one film's prior was taken from the memo
      // for the other's (a venue's "Lalka" and "Lalka 2D" trading films kept the director only one of them had).
      val previous = previousAt(ofVenue)
      val priors   = priorsAt(cinema, previous)
      // Each row's film by its counter, which a film keeps for good and has from its first draft: what the key of a
      // slot once written is worked out from ([[VenueSlotMemo.written]]), a film new to the store included.
      (VenueGroup(cinema, ofVenue, VenueSlotMemo.Key(cinema.displayName, ofVenue.map(r => (r.row, r.showtimes)).##,
        ofVenue.map(_.listing.key).##, (priors, previous.map(_.flatMap(covered.counterOf))).##, ofVenue.size), counter), previous, priors)
    }
    // A film drafted again with the listings it was last drafted with is drafted again only where it moved: a venue one of
    // whose listings, or whose listings' previous films' stored slots, moved since ([[FilmShapes]]).
    val reuse   = !scope.whole
    lazy val placedAt: Map[ListingKey, Long] = assigned.idOfListing
    val dirtyAt = if (!reuse) Map.empty[Long, Set[ListingKey]]
                  else changed.iterator.filter(byKey.contains).flatMap(k => placedAt.get(k).map(_ -> k)).toSeq.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val planned = assigned.ids.map { case (counter, members) =>
      shapes.get(counter).filter(sh => reuse && (sh.members eq members)) match {
        case Some(was) =>
          // The same listings: the same keys in the same order, the same titles (a key holds its listing's title).
          val dirtyVenues = dirtyAt.getOrElse(counter, Set.empty).map(byKey(_).listing.cinema)
          val groups = was.venues.toSeq.sortBy(_._1.displayName).map { case (cinema, venue) =>
            // Moved: one of its listings did, or which film one is on, or the slots those films hold at the venue.
            lazy val previous = previousAt(venue.group.rows)
            val moved = dirtyVenues(cinema) || previous != venue.previous || priorsAt(cinema, previous) != venue.priors
            if (!moved) cinema -> Left(venue)
            else cinema -> Right(groupOf(cinema, venue.group.rows.map(r => byKey(r.listing.key)), counter))
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
      (cinema, keys, rows) => buildVenue(cinema, keys, rows, index.previousOf, storedById, normalizer, slots, tokens)
    val venueSlots = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, Seq[(Source, SourceData)]]
    // The slots the stored film of a group's listings holds at its venue: what a restarted worker's memo reuses
    // when they are recorded as built from the group's key.
    def storedAt(group: VenueGroup): Option[Seq[(Source, SourceData)]] =
      group.rows.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct match {
        case Seq(id) => storedById.get(id).map(_ => slotsByVenueOf(id).getOrElse(group.cinema, Nil))
        case _ => None
      }
    planned.foreach(_.groups.foreach { case (_, Left(venue)) => venueSlots(venue.group.key) = venue.lean; case _ => () })
    val pending    = planned.flatMap(_.groups.collect { case (_, Right((group, _, _))) => group }).filter(group =>
      memo.lookup(group.key, storedAt(group)).fold(true) { lean => venueSlots(group.key) = lean; false })
    pending.groupBy(_.cinema).toSeq.sortBy(_._1.displayName).grouped(RowBatch).foreach { batch =>
      val fetched = rowsOf(batch.map(_._1).toSet)
      batch.foreach { case (cinema, groups) =>
        val rows = rowsByKey(cinema, fetched.getOrElse(cinema, Nil))
        groups.foreach { group =>
          val lean = build(cinema, group.rows.map(_.listing.key), rows).map { case (source, slot) => source -> ShowtimesDigest.stripSlot(slot) }
          venueSlots(group.key) = lean
          memo.store(group.key, lean, keep = readAsListed(cinema, group.rows, rows, normalizer), writtenAs = Some(group.counter))
        }
      }
    }
    val drafts = planned.map { plan =>
      import plan.{counter, members, keys}
      val previous = previousIdOf(counter).flatMap(storedById.get)
      val film     = filmOf(members)
      val anchor   = plan.titles.toSeq.sortBy { case (t, n) => (-n, t) }.headOption.map(_._1).getOrElse("")
      // Each venue's slots, and the film's as a whole: of a film drafted again, the venues it rebuilt replace theirs.
      val venueShapes = plan.groups.iterator.map {
        case (cinema, Left(venue))            => cinema -> venue
        case (cinema, Right((group, previous, priors))) => cinema -> VenueShape(group, venueSlots(group.key), previous, priors)
      }.toMap
      val venueData = plan.was.fold(venueShapes.valuesIterator.flatMap(_.lean).toMap) { was =>
        plan.groups.foldLeft(was.venueData) {
          case (data, (cinema, Right(_))) => data -- was.venues(cinema).lean.map(_._1) ++ venueShapes(cinema).lean
          case (data, _)                  => data
        }
      }
      shapes.put(counter, FilmShape(members, keys, plan.titles, venueShapes, venueData))
      val sameFilm = previous.exists(_.record.tmdbId == film)
      val base = previous.filter(_ => sameFilm).map(_.record).getOrElse(
        MovieRecord(retainedSynopses = previous.map(_.record.retainedSynopses).getOrElse(Map.empty)))
      val record = base.copy(
        tmdbId        = film,
        tmdbAttempt   = if (film.isDefined) None else base.tmdbAttempt.orElse(Some(TmdbAttempt(ResolverVerdict, at))),
        detailPending = false,
        searchTitle   = base.searchTitle.orElse(Some(normalizer.apiQuery(normalizer.recase(anchor)))),
        // A chain's network detail slot is venue source data no listing is published at: kept from the
        // stored film whatever it is matched to, as its venue slots are rebuilt from theirs.
        // The venue slots first, and the rest over them: no venue slot is a non-venue source or a chain's network one.
        data          = venueData ++ base.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty } ++
                          previous.fold(Map.empty[Source, SourceData])(_.record.data.filter { case (source, _) => Cinema.Networks.contains(source) }))
      FilmDraft(counter, previous.map(_.id), keys, record, anchor)
    }
    val venues = planned.map(plan => plan.counter -> plan.groups.map {
      case (cinema, Left(venue))           => cinema -> venue.group.rows.map(_.listing.key)
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
  def canary(index: ProjectionIndex): Map[ShadowRelation, Int] = canaryOf(index.clusters.values.toSeq.map(c => c.members -> c.film), index.previousOf)

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
      ProjectedFilm(id, d.counter, title, year, key, d.record, d.members)
    }
    val freshEntries = films.filter(f => draft.counters.filmIdOf(f.counter).isEmpty).map(f => FilmIdCounter(f.id.value, f.counter))
    ProjectionPlan(films, draft.retired, draft.additions ++ freshEntries, draft.regroupings, draft.canary)
  }

  /** A venue's rows now, by the listing key each is published under: grouped once per venue a projection reads, for
   *  every film it builds there — grouped once per film, a venue with many changed films regrouped its whole programme
   *  for each. */
  type VenueRows = Map[ListingKey, Seq[CinemaMovie]]
  def rowsByKey(cinema: Cinema, fetched: Seq[CinemaMovie]): VenueRows = fetched.groupBy(cm => ListingKey.of(cinema, cm))

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

  /** The rows `cinema` publishes under `key`, as one: the smallest by the listings' total order, carrying every row's
   *  showtimes — so a venue printing one listing twice loses none. */
  private def merged(cinema: Cinema, rows: Seq[CinemaMovie], normalizer: TitleNormalizer): CinemaMovie =
    if (rows.sizeIs == 1) rows.head
    else rows.minBy(cm => Listing.of(cinema, cm, normalizer)).copy(showtimes = MovieRecordMerge.dedupShowtimes(rows.flatMap(_.showtimes)))

  /** Whether `byKey` holds exactly the rows `listed` were read from (no re-scrape landed between the two reads), so
   *  what is built from it may be kept under their key. */
  private def readAsListed(cinema: Cinema, listed: Seq[ProjectedListing], byKey: VenueRows, normalizer: TitleNormalizer): Boolean =
    listed.forall { l =>
      byKey.get(l.listing.key).exists { rows =>
        val first = if (rows.sizeIs == 1) rows.head else rows.minBy(cm => Listing.of(cinema, cm, normalizer))
        ProjectedListing.rowDigest(first) == l.row &&
          (if (rows.sizeIs == 1) rows.head.showtimes.## else rows.map(_.showtimes.##).sorted.##) == l.showtimes
      }
    }

  /** `cinema`'s slots on a film from the rows it publishes under `keys` (of `byKey`, its rows now): per slot,
   *  the venue's rows unioned as the landing's fold unions them (every showtime kept), built over the slot the
   *  representative listing's previous film held there. */
  private def buildVenue(cinema: Cinema, keys: Seq[ListingKey], byKey: VenueRows, previousOf: Map[ListingKey, PipelineFilmRef],
                         storedById: Map[String, StoredMovieRecord], normalizer: TitleNormalizer, slots: CinemaSlotBuilder,
                         tokens: ScreeningTokens): Seq[(Source, SourceData)] = {
    val rows     = keys.flatMap(key => byKey.get(key).map(merged(cinema, _, normalizer)))
    val prepared = ScrapeListing.prepare(cinema, rows, normalizer, tokens)
    prepared.movies.groupBy(cm => CinemaShowing.keyFor(cinema, prepared.cleaned(cm), normalizer)).toSeq
      .sortBy(_._1.titleKey).map { case (source, group) =>
        val representative =
          if (group.sizeIs == 1) group.head
          else MovieRecordMerge.slotRepresentative(group).copy(showtimes = MovieRecordMerge.dedupShowtimes(group.flatMap(_.showtimes)))
        val prior = previousOf.get(ListingKey.of(cinema, representative)).flatMap(ref => storedById.get(ref.id))
          .flatMap(_.record.data.get(source))
        (source: Source) -> slots.build(representative, prepared.cleaned(representative), prior)
      }
  }
}

/** One venue of a drafted film: its group, the slots built for it, the film each of its listings was on, and what those
 *  films' slots at the venue were (their priors) — the venue is reused while each listing is on the same film and those
 *  slots are unmoved. */
private[identity] final case class VenueShape(group: IdentityProjectionPlan.VenueGroup, lean: Seq[(Source, SourceData)],
                                              previous: Seq[Option[String]], priors: Seq[Int])

/** A drafted film: the listings it was drafted with (and in key order), how many carry each clean title (its anchor),
 *  each venue, and its venue slots as one map. */
private[identity] final case class FilmShape(members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                                             venues: Map[Cinema, VenueShape], venueData: Map[Source, SourceData])

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
