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
                                 venues: Map[Long, Seq[(Cinema, Seq[ListingKey])]] = Map.empty,
                                 build: (Cinema, Seq[ListingKey], Seq[CinemaMovie]) => Seq[(Source, SourceData)] = (_, _, _) => Nil,
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
    wanted.flatMap(_._2).distinct.sortBy(_.displayName).grouped(IdentityProjectionPlan.RowBatch).foreach { batch =>
      val rows = rowsOf(batch.toSet)
      wanted.foreach { case (film, differing) =>
        batch.filter(differing).foreach { cinema =>
          val keys = venues.getOrElse(film.counter, Nil).collectFirst { case (`cinema`, ks) => ks }.getOrElse(Nil)
          built((film.counter, cinema)) = build(cinema, keys, rows.getOrElse(cinema, Nil))
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
  private var previous = Map.empty[VenueSlotMemo.Key, VenueSlotMemo.Entry]
  private val current  = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, VenueSlotMemo.Entry]
  private var recorded = Set.empty[Long]
  private var hits     = 0
  private var builds   = 0
  // Why a build was needed, for the projection's log: the same venue's same listings with other rows, with other
  // prior slots, or listings the last projection did not have at the venue at all.
  private var seenBefore = Map.empty[(String, Int), VenueSlotMemo.Key]
  private val seenNow    = scala.collection.mutable.HashMap.empty[(String, Int), VenueSlotMemo.Key]
  private var missRows, missPriors, missNew = 0

  /** The fingerprints a previous run of the worker kept, for this memo's first projection to reuse stored slots by. */
  def seed(fingerprints: Set[Long]): Unit = synchronized { recorded = fingerprints }

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

  /** `lean`, built this projection for `key`: kept for the next when `keep` (it was built from the rows `key` names). */
  private[identity] def store(key: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)], keep: Boolean): Unit = synchronized {
    if (keep) current(key) = VenueSlotMemo.Entry(lean, fingerprint(key, lean))
    builds += 1
  }

  private def fingerprint(key: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)]): Long = VenueSlotMemo.fingerprint(environment, key, lean)

  /** Close a projection: keep only the slots it used, and say how many it took from the memo and built. */
  def endTick(): (Int, Int) = synchronized {
    previous = current.toMap
    current.clear()
    recorded = Set.empty
    seenBefore = seenNow.toMap
    seenNow.clear()
    val counts = (hits, builds)
    hits = 0; builds = 0
    counts
  }

  /** The fingerprints of the slots the last projection kept: what a restarted worker's memo is seeded with. */
  def fingerprints: Set[Long] = synchronized(previous.valuesIterator.map(_.fingerprint).toSet)

  /** Why the last projection's builds were needed: (rows moved, prior slots moved, listings new to the venue). */
  def lastMisses(): (Int, Int, Int) = synchronized {
    val misses = (missRows, missPriors, missNew)
    missRows = 0; missPriors = 0; missNew = 0
    misses
  }
}

object VenueSlotMemo {
  /** What one venue's slots on a film are built from: the venue, its rows (content and listing keys)
   *  and the detail fields of the slots the rows' previous films held at the venue — every input of
   *  `ScrapeListing.prepare` and `CinemaSlotBuilder.build` that is not fixed for the worker. */
  final case class Key(venue: String, rows: Int, keys: Int, priors: Int, size: Int)

  /** Slots the memo holds, and their fingerprint. */
  private final case class Entry(lean: Seq[(Source, SourceData)], fingerprint: Long)

  /** A memo of nothing: every film's slots built. */
  def none: VenueSlotMemo = new VenueSlotMemo {
    override private[identity] def lookup(key: Key, stored: => Option[Seq[(Source, SourceData)]]): Option[Seq[(Source, SourceData)]] = None
  }

  /** The build of the code a slot is made by, as a version: the build's digest of the sources the compiler records
   *  this file as reaching (`venue-slot-version.txt`, generated by `build.sbt` as `IdentityRules.codeVersion` is). A
   *  deploy that changes none of them keeps every recorded fingerprint. */
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
    // One listing per key — the smallest by the total order, as the resolver takes it — standing for every row
    // published under that key (its slot is built from all of them: `merged`), so a venue printing one listing
    // twice loses no showtime.
    val byKey: Map[ListingKey, ProjectedListing] = listings.groupBy(_.listing.key).map { case (key, rows) =>
      key -> (if (rows.sizeIs == 1) rows.head else rows.minBy(_.listing).copy(showtimes = rows.map(_.showtimes).sorted.##))
    }
    val storedById = stored.map(r => r.id.value -> r).toMap

    // 1. The stored films as listing sets, numbered.
    // A listing is on the stored film whose slot it IS (the slot's own listing key); a listing no slot
    // names — one a venue's same-title fold hid behind another's slot — on the film holding its slot.
    val slotOwner: Map[ListingKey, PipelineFilmRef] = stored.flatMap { r =>
      r.record.data.flatMap { case (source, slot) => ListingKey.ofSource(source, slot).map(_ -> PipelineFilmRef(r.id.value, r.record.tmdbId)) }
    }.groupMapReduce(_._1)(_._2)((a, b) => if (a.id <= b.id) a else b)
    val previousOf: Map[ListingKey, PipelineFilmRef] =
      PipelineFilms.of(byKey.values.map(_.listing).toSeq, stored, normalizer) ++ slotOwner.filter { case (k, _) => byKey.contains(k) }
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

    // 3. Each film's record. Its venue slots are LEAN, each venue's from the memo when nothing it is built from
    // moved, else built from the venue's rows, read a batch of venues at a time and let go.
    // Each stored film's prior slots by venue, as the slot memo's key reads them: worked out once per film, not
    // once per venue of every film it lends a listing to — a US film showing at hundreds of venues was scanned
    // whole for each of them.
    val priorsByFilm = scala.collection.mutable.HashMap.empty[String, Map[Cinema, Int]]
    def priorsOf(id: String): Map[Cinema, Int] = priorsByFilm.getOrElseUpdate(id, storedById.get(id).fold(Map.empty[Cinema, Int])(r =>
      r.record.data.toSeq.collect { case (cs: CinemaShowing, slot) => cs.cinema -> (cs.titleKey, VenueSlotMemo.carried(slot)) }
        .groupMap(_._1)(_._2).map { case (cinema, slots) => cinema -> slots.sorted.## }))
    val planned = assigned.ids.map { case (counter, members) =>
      val rows   = members.toSeq.sorted.map(byKey)
      val groups = rows.groupBy(_.listing.cinema).toSeq.sortBy(_._1.displayName).map { case (cinema, ofVenue) =>
        // The prior slots any of these rows' previous films held at the venue: a superset of the one each built
        // slot carries forward, so a change to any of them is a change to the key.
        val priors = ofVenue.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct.sorted.map(id => priorsOf(id).getOrElse(cinema, 0))
        VenueGroup(cinema, ofVenue, VenueSlotMemo.Key(cinema.displayName, ofVenue.map(r => (r.row, r.showtimes)).##,
          ofVenue.map(_.listing.key).##, priors.##, ofVenue.size))
      }
      (counter, members, rows, groups)
    }
    val build: (Cinema, Seq[ListingKey], Seq[CinemaMovie]) => Seq[(Source, SourceData)] =
      (cinema, keys, fetched) => buildVenue(cinema, keys, fetched, previousOf, storedById, normalizer, slots, tokens)
    val venueSlots = scala.collection.mutable.HashMap.empty[VenueSlotMemo.Key, Seq[(Source, SourceData)]]
    // The slots the stored film of a group's listings holds at its venue: what a restarted worker's memo reuses
    // when they are recorded as built from the group's key.
    def storedAt(group: VenueGroup): Option[Seq[(Source, SourceData)]] =
      group.rows.flatMap(r => previousOf.get(r.listing.key)).map(_.id).distinct match {
        case Seq(id) => storedById.get(id).map(_.record.data.toSeq.collect {
          case (showing: CinemaShowing, slot) if showing.cinema == group.cinema => (showing: Source) -> slot
        })
        case _ => None
      }
    val pending    = planned.flatMap(_._4).filter(group =>
      memo.lookup(group.key, storedAt(group)).fold(true) { lean => venueSlots(group.key) = lean; false })
    pending.groupBy(_.cinema).toSeq.sortBy(_._1.displayName).grouped(RowBatch).foreach { batch =>
      val fetched = rowsOf(batch.map(_._1).toSet)
      batch.foreach { case (cinema, groups) =>
        val rows = fetched.getOrElse(cinema, Nil)
        groups.foreach { group =>
          val lean = build(cinema, group.rows.map(_.listing.key), rows).map { case (source, slot) => source -> ShowtimesDigest.stripSlot(slot) }
          venueSlots(group.key) = lean
          memo.store(group.key, lean, keep = readAsListed(cinema, group.rows, rows, normalizer))
        }
      }
    }
    val drafts = planned.map { case (counter, members, rows, groups) =>
      val previous = previousIdOf(counter).flatMap(storedById.get)
      val film     = filmOf(members)
      val anchor   = rows.map(r => r.listing.cleanTitle).groupMapReduce(identity)(_ => 1)(_ + _).toSeq
        .sortBy { case (t, n) => (-n, t) }.headOption.map(_._1).getOrElse("")
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
        data          = base.data.filter { case (source, _) => Source.cinemaOf(source).isEmpty } ++
                          previous.fold(Map.empty[Source, SourceData])(_.record.data.filter { case (source, _) => Cinema.Networks.contains(source) }) ++
                          groups.flatMap(group => venueSlots(group.key)))
      FilmDraft(counter, previous.map(_.id), rows.map(_.listing.key), record, anchor)
    }
    val venues = planned.map { case (counter, _, _, groups) => counter -> groups.map(g => g.cinema -> g.rows.map(_.listing.key)) }.toMap

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
    val placed = previousFilms.map(_.id).toSet
    // The canary compares the films as STORED — one per TMDB film — with the films before.
    val asStored = resolution.copy(decisions = clusters.map { case (members, film) =>
      ResolverDecision(members.toSeq.sorted, film, 1.0, ResolverDecision.Basis.OwnMatch, Nil)()
    })
    ProjectionDraft(drafts, retired, retired.filterNot(id => placed(id.value)), covered, additions, regroupings,
      ShadowDiff.counts(ShadowDiff.of(asStored, previousOf)._1), venues, build, rowsOf)
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

  /** How many venues' rows a projection reads at once to build their slots: a batch's rows are let go before the next. */
  private[identity] val RowBatch = 200

  /** A slot holds its showtimes elsewhere: only their digest and starts are on it. */
  private[identity] def isLean(slot: SourceData): Boolean = slot.showtimes.isEmpty && slot.showtimesDigest.isDefined

  /** One film's listings at one venue, and what its slots are built from. */
  private final case class VenueGroup(cinema: Cinema, rows: Seq[ProjectedListing], key: VenueSlotMemo.Key)

  /** The rows `cinema` publishes under `key`, as one: the smallest by the listings' total order, carrying every row's
   *  showtimes — so a venue printing one listing twice loses none. */
  private def merged(cinema: Cinema, rows: Seq[CinemaMovie], normalizer: TitleNormalizer): CinemaMovie =
    if (rows.sizeIs == 1) rows.head
    else rows.minBy(cm => Listing.of(cinema, cm, normalizer)).copy(showtimes = MovieRecordMerge.dedupShowtimes(rows.flatMap(_.showtimes)))

  /** Whether `fetched` holds exactly the rows `listed` were read from (no re-scrape landed between the two reads), so
   *  what is built from it may be kept under their key. */
  private def readAsListed(cinema: Cinema, listed: Seq[ProjectedListing], fetched: Seq[CinemaMovie], normalizer: TitleNormalizer): Boolean = {
    val byKey = fetched.groupBy(cm => ListingKey.of(cinema, cm))
    listed.forall { l =>
      byKey.get(l.listing.key).exists { rows =>
        val first = if (rows.sizeIs == 1) rows.head else rows.minBy(cm => Listing.of(cinema, cm, normalizer))
        ProjectedListing.rowDigest(first) == l.row &&
          (if (rows.sizeIs == 1) rows.head.showtimes.## else rows.map(_.showtimes.##).sorted.##) == l.showtimes
      }
    }
  }

  /** `cinema`'s slots on a film from the rows it publishes under `keys` (of `fetched`, its rows now): per slot,
   *  the venue's rows unioned as the landing's fold unions them (every showtime kept), built over the slot the
   *  representative listing's previous film held there. */
  private def buildVenue(cinema: Cinema, keys: Seq[ListingKey], fetched: Seq[CinemaMovie], previousOf: Map[ListingKey, PipelineFilmRef],
                         storedById: Map[String, StoredMovieRecord], normalizer: TitleNormalizer, slots: CinemaSlotBuilder,
                         tokens: ScreeningTokens): Seq[(Source, SourceData)] = {
    val byKey    = fetched.groupBy(cm => ListingKey.of(cinema, cm))
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
