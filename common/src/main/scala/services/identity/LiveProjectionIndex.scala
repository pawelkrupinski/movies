package services.identity

import models.{Source, SourceData}
import services.movies.{LeanRecords, ListingKey, StoredMovieRecord, TitleNormalizer}

import scala.collection.mutable

/**
 * The [[ProjectionIndex]] kept from one projection to the next and moved only where its inputs moved, so a projection
 * pays for what changed rather than for the corpus: a venue whose listing is another object than last time (the intake
 * holds a venue's listing until it is scraped again), a listing the model took up or let go, a decision that is another
 * object (the model keeps an unmoved family's), a stored film another writer changed. [[update]] returns exactly what
 * it moved, as [[ProjectionScope.Changes]] — the tick's changes, with nothing diffed whole.
 *
 * The index it keeps is the one [[IdentityProjectionPlan.index]] builds from the same inputs, entry for entry; the
 * rules — one listing per key, a listing's previous film, the clusters — are that builder's own, called from here
 * ([[IdentityProjectionPlan.oneByKey]], [[IdentityProjectionPlan.ownerOf]], [[PipelineFilms.pick]],
 * [[IdentityProjectionPlan.clusterOf]]). `ScopedProjectionEquivalenceSpec` checks the two equal after every tick.
 *
 * What it knows per listing key — the listing published under it, its previous film, its cluster, the decision naming
 * it, the stored films whose slot it is — is ONE [[LiveProjectionIndex.Entry]] in one map: a map per field kept a hash
 * node (and, for the immutable ones, a share of a trie) per key in each, ~11 MB of worker-us's ~90k listings. The
 * index's per-key maps are views of it, as of the last [[update]] or [[written]].
 *
 * Single-threaded: the projection's tick holds it.
 */
final class LiveProjectionIndex(normalizer: TitleNormalizer) {
  import IdentityProjectionPlan.{clusterOf => clusterFor, oneByKey, ownerOf}

  // Listings: each venue's listing as last read (the object, to tell it unmoved) — one listing per key at it is worked
  // out again only for a venue an update reads by key (a key's venue is its own, `ListingKey.venue`).
  private val venueListings = mutable.HashMap.empty[String, Seq[ProjectedListing]]
  // Per listing key, everything the index knows of it (an entry goes once it holds nothing), under the published
  // listing's own key object when there is one.
  private val entries       = mutable.HashMap.empty[ListingKey, LiveProjectionIndex.Entry]
  private var published, placed, clustered = 0   // how many entries hold a listing, a previous film, a cluster
  private var heldMark = 0                         // the updates so far: an entry's `heldAt` is the last that held it

  // Each venue slot (a listing's own is `PipelineFilms.slotOf` it): the published listings at it — whose previous film a
  // slot moves — and the stored films with a slot there, by where it is and its source only: a slot's data is read off
  // the stored record as it is now, so a record read again is not kept.
  private val slots         = mutable.HashMap.empty[(String, String), LiveProjectionIndex.Slot]

  private var storedById    = Map.empty[String, StoredMovieRecord]
  private var listingsOf    = Map.empty[String, Set[ListingKey]]

  // Decisions, by identity: the model hands back an unmoved family's own objects.
  private val decisions     = new java.util.IdentityHashMap[ResolverDecision, ClusterId]()
  private val decisionsOf   = mutable.HashMap.empty[ClusterId, List[ResolverDecision]]
  private var clusters      = Map.empty[ClusterId, Cluster]

  import LiveProjectionIndex.{Entry, Slot}

  private def listingAt(k: ListingKey): Option[ProjectedListing] = entries.get(k).flatMap(e => Option(e.listing))
  private def isPublished(k: ListingKey): Boolean = entries.get(k).exists(_.listing != null)
  private def previousAt(k: ListingKey): Option[PipelineFilmRef] = entries.get(k).flatMap(e => Option(e.previous))
  private def entry(k: ListingKey): Entry = entries.getOrElseUpdate(k, new Entry(k))
  private def tidy(e: Entry): Unit = if (e.holdsNothing) entries.remove(e.key)

  /** Publish `l` under its key, the entry re-filed under the listing's own key object. */
  private def publish(l: ProjectedListing): Unit = {
    val k = l.listing.key
    val e = entries.get(k) match {
      case Some(e) if e.key ne k => entries.remove(k); e.key = k; entries(k) = e; e
      case Some(e)               => e
      case None                  => val e = new Entry(k); entries(k) = e; e
    }
    if (e.listing == null) published += 1
    e.listing = l
  }
  private def unpublish(k: ListingKey): Unit = entries.get(k).filter(_.listing != null).foreach { e =>
    e.listing = null; published -= 1; tidy(e)
  }
  private def place(k: ListingKey, ref: PipelineFilmRef): Unit = { val e = entry(k); if (e.previous == null) placed += 1; e.previous = ref }
  private def unplace(k: ListingKey): Unit = entries.get(k).filter(_.previous != null).foreach { e =>
    e.previous = null; placed -= 1; tidy(e)
  }
  private def cluster(k: ListingKey, id: ClusterId): Unit = { val e = entry(k); if (e.cluster == null) clustered += 1; e.cluster = id }
  private def uncluster(k: ListingKey): Unit = entries.get(k).filter(_.cluster != null).foreach { e =>
    e.cluster = null; clustered -= 1; tidy(e)
  }
  private def slotAt(at: (String, String)): Slot = slots.getOrElseUpdate(at, new Slot)
  private def tidySlot(at: (String, String)): Unit = slots.get(at).filter(_.holdsNothing).foreach(_ => slots.remove(at))

  /** One field of every entry as a map, as the entries stand. */
  private final class FieldView[V <: AnyRef](field: Entry => V, count: () => Int) extends scala.collection.immutable.AbstractMap[ListingKey, V] {
    def get(key: ListingKey): Option[V] = entries.get(key).flatMap(e => Option(field(e)))
    override def contains(key: ListingKey): Boolean = entries.get(key).exists(field(_) != null)
    def iterator: Iterator[(ListingKey, V)] = entries.valuesIterator.filter(field(_) != null).map(e => e.key -> field(e))
    override def size: Int = count()
    override def knownSize: Int = count()
    override def isEmpty: Boolean = count() == 0
    def removed(key: ListingKey): Map[ListingKey, V] = Map.from(iterator).removed(key)
    def updated[V1 >: V](key: ListingKey, value: V1): Map[ListingKey, V1] = Map.from(iterator).updated(key, value)
  }
  private val byKeyView      = new FieldView(_.listing, () => published)
  private val previousOfView = new FieldView(_.previous, () => placed)
  private val clusterOfView  = new FieldView(_.cluster, () => clustered)

  /** The index as it stands, over `counters`: its listings, previous films and listings' clusters are views of this
   *  index, which read what the next [[update]] or [[written]] moves — a projection's, read before the two. */
  def index(counters: FilmIdCounters): ProjectionIndex = {
    val unmapped  = listingsOf.iterator.collect { case (id, ls) if counters.counterOf(id).isEmpty => IdSeeding.Film(id, ls) }.toSeq
    val covered   = if (unmapped.isEmpty) counters else counters.covering(unmapped)
    ProjectionIndex(byKeyView, storedById, previousOfView, listingsOf, covered, covered.entries.filterNot(e => counters.counterOf(e.filmId).isDefined),
      clusters, clusterOfView)
  }

  /** Move the index to what these inputs say, and say what moved. `held`: the keys of the listings the model holds. */
  def update(venues: Seq[(String, Seq[ProjectedListing])], held: Iterable[ListingKey], resolved: Seq[ResolverDecision],
             stored: Seq[StoredMovieRecord]): ProjectionScope.Changes = {
    val keys = mutable.HashSet.empty[ListingKey]   // whose published listing (or whether it is published) moved
    // 0. Held: a listing the model took up that is not published, or let go that is — published, held and indexed must
    // agree. Each entry marks the update it was last held at, so neither side is walked against the other as a set.
    heldMark += 1
    val heldUnindexed = mutable.HashSet.empty[ListingKey]
    held.foreach { k =>
      entries.get(k) match {
        case Some(e) => e.heldAt = heldMark; if (e.listing == null) keys += k
        case None    => heldUnindexed += k; keys += k
      }
    }
    entries.valuesIterator.foreach(e => if (e.listing != null && e.heldAt != heldMark) keys += e.key)
    def isHeld(k: ListingKey): Boolean = entries.get(k).exists(_.heldAt == heldMark) || heldUnindexed(k)
    val byKeyAt = mutable.HashMap.empty[String, Map[ListingKey, ProjectedListing]]
    def at(venue: String): Map[ListingKey, ProjectedListing] = byKeyAt.getOrElseUpdate(venue, oneByKey(venueListings.getOrElse(venue, Nil)))
    // 1. Listings: a venue whose listing is another object, or gone.
    val live = venues.iterator.map(_._1).toSet
    venues.foreach { case (venue, listings) =>
      if (!venueListings.get(venue).exists(_ eq listings)) {
        val was = at(venue)
        val now = oneByKey(listings)
        was.foreach { case (k, l) => if (!now.get(k).exists(n => (n eq l) || n == l)) keys += k }
        now.keysIterator.foreach(k => if (!was.contains(k)) keys += k)   // new to its venue
        // An unmoved listing read again is a new object of the same value: index that one, so the old read is not kept.
        now.foreach { case (k, n) => listingAt(k).foreach(l => if ((l ne n) && l == n) rekey(l, n)) }
        venueListings(venue) = listings
        byKeyAt(venue) = now
      }
    }
    venueListings.keys.filterNot(live).toSeq.foreach { venue =>
      keys ++= at(venue).keys
      venueListings.remove(venue)
      byKeyAt.remove(venue)
    }
    val republished = mutable.HashSet.empty[ListingKey]   // whose published listing moved
    keys.foreach { k =>
      val listing = (if (venueListings.contains(k.venue)) at(k.venue).get(k) else None).filter(_ => isHeld(k))
      val was = listingAt(k)
      if (listing != was) {
        republished += k
        was.foreach { old =>
          val at = PipelineFilms.slotOf(old.listing, normalizer)
          slots.get(at).foreach { slot => slot.keys -= k; tidySlot(at) }
        }
        listing match {
          case Some(l) =>
            publish(l)
            entries(k).heldAt = heldMark
            val slot = slotAt(PipelineFilms.slotOf(l.listing, normalizer))
            slot.keys += k
          case None => unpublish(k)
        }
      }
    }

    // 2. Stored films: another object is another film unless its content is the same (the cache re-reads a film it
    // wrote; a film this index was told was written is held as written). Its slots move the listings at them.
    val films   = mutable.HashSet.empty[String]          // changed or gone, as the changes name them
    val freed   = mutable.HashSet.empty[String]
    val reslot  = mutable.HashSet.empty[ListingKey]      // whose previous film may have moved
    val now     = stored.iterator.map(r => r.id.value -> r).toMap
    storedById.foreach { case (id, was) =>
      now.get(id) match {
        case Some(is) if is eq was => ()
        // The cache wraps the record it holds anew on every snapshot: the same record, the film held as it was.
        case Some(is) if (is.record eq was.record) && sameFilm(is, was) => ()
        case Some(is) if sameFilm(is, was) => storedById = storedById.updated(id, is)
        case other =>
          films += id
          freed += ProjectionScope.plainKey(was.key(normalizer))
          other.fold(unstore(id, reslot))(is => restore(is, reslot))
      }
    }
    now.foreach { case (id, is) => if (!storedById.contains(id)) restore(is, reslot) }

    // 3. Previous films: of every listing whose publication, slot or film moved.
    val moved = repoint(republished.iterator ++ reslot.iterator)

    // 4. Clusters: of every decision that is another object, and of every listing whose publication moved.
    val current = new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean](resolved.size * 2)
    resolved.foreach(d => current.put(d, java.lang.Boolean.TRUE))
    val touched = mutable.HashSet.empty[ClusterId]
    val gone    = mutable.ArrayBuffer.empty[ResolverDecision]
    decisions.forEach((d, id) => if (!current.containsKey(d)) gone += d)
    gone.foreach { d =>
      val id = decisions.remove(d)
      touched += id
      decisionsOf(id) = decisionsOf.getOrElse(id, Nil).filterNot(_ eq d)
      d.members.foreach(k => entries.get(k).filter(_.decision eq d).foreach { e => e.decision = null; tidy(e) })
    }
    resolved.foreach { d =>
      if (!decisions.containsKey(d)) {
        val id = IdentityProjectionPlan.clusterIdOf(d)
        decisions.put(d, id)
        touched += id
        decisionsOf(id) = d :: decisionsOf.getOrElse(id, Nil)
        d.members.foreach(k => entry(canonical(k)).decision = d)
      }
    }
    republished.foreach(k => entries.get(k).flatMap(e => Option(e.decision)).foreach(d => touched += decisions.get(d)))
    val regrouped = mutable.HashSet.empty[ListingKey]
    touched.foreach { id =>
      val was  = clusters.get(id)
      val next = clusterFor(id, decisionsOf.getOrElse(id, Nil), isPublished).map(c => c.copy(members = c.members.map(canonical)))
      if (next != was) {
        // A listing that left the cluster is gone, or in a cluster that moved too: the clusters partition.
        was.foreach(_.members.foreach(k => if (entries.get(k).exists(_.cluster == id)) uncluster(k)))
        next match {
          case Some(c) => clusters = clusters.updated(id, c); regrouped ++= c.members; c.members.foreach(cluster(_, id))
          case None    => clusters = clusters - id
        }
      }
      if (decisionsOf.get(id).exists(_.isEmpty)) decisionsOf.remove(id)
    }

    val changed = ProjectionScope.Changes((republished ++ moved ++ regrouped ++ afterWrites).toSet, films.toSet, freed.toSet)
    afterWrites.clear()
    changed
  }

  /** A projection wrote `films` and retired `retired`: the stored films are those now — the next update counts a film it
   *  wrote as moved only if another writer has moved it since. */
  def written(films: Seq[ProjectedFilm], retired: Seq[services.movies.FilmId]): Unit = {
    val reslot = mutable.HashSet.empty[ListingKey]
    retired.foreach(id => unstore(id.value, reslot))
    films.foreach(f => restore(StoredMovieRecord(f.title, f.year, f.record, f.id, Some(f.key)), reslot, f.touched))
    // A listing the store, as written, puts on the film it was written into is where the writes meant it to be. Any other
    // listing they moved, and any written into a film the store does not put it on, is moved for the next projection: a
    // written slot is not always the slot of the listing it was built from (a director carried forward from its prior is
    // part of the slot's own listing key), and a whole projection reading the store afresh finds the listing on another
    // film — and drafts its cluster again.
    val writtenInto = films.iterator.flatMap(f => f.members.iterator.map(_ -> f.id.value)).toMap
    val moved       = repoint(reslot.iterator)
    def placedAsWritten(k: ListingKey): Boolean = writtenInto.get(k).exists(id => previousAt(k).exists(_.id == id))
    afterWrites ++= moved.filterNot(placedAsWritten)
    afterWrites ++= writtenInto.keysIterator.filterNot(placedAsWritten)
    ()
  }

  /** `k` as the listing published under it holds it: one key object per listing, not one per map that names it — a stored
   *  slot's and a decision's are read anew from the store (worker-us held ~4.8 per listing). */
  private def canonical(k: ListingKey): ListingKey = listingAt(k).fold(k)(_.listing.key)

  /** Index `now` — an equal listing, another object — in place of `was`, under ITS key: the entry and every set holding
   *  the key hold the new object (a map or set keeps the key object it was first given), so a listing read again, or
   *  taken as the identity model's object, leaves no older key behind it (worker-us grew to ~2.6 key objects per listing). */
  private def rekey(was: ProjectedListing, now: ProjectedListing): Unit = {
    val k = now.listing.key
    publish(now)
    if (k ne was.listing.key) {
      previousAt(k).foreach(ref => listingsOf = listingsOf.updatedWith(ref.id)(_.map(_ - k + k)))
      slots.get(PipelineFilms.slotOf(now.listing, normalizer)).filter(_.keys.contains(k)).foreach(slot => slot.keys = slot.keys - k + k)
      entries.get(k).flatMap(e => Option(e.cluster)).foreach { id =>
        clusters.get(id).foreach(c => clusters = clusters.updated(id, c.copy(members = c.members - k + k)))
      }
    }
  }

  /** Listings the last writes moved to another film: changes for the next [[update]]. */
  private val afterWrites = mutable.HashSet.empty[ListingKey]

  /** Work out `keys`' previous films again: the film whose slot each is, else the one whose slot it folds into — the
   *  builder's rule ([[IdentityProjectionPlan.index]]). The ones that moved. */
  private def repoint(keys: Iterator[ListingKey]): mutable.HashSet[ListingKey] = {
    val moved = mutable.HashSet.empty[ListingKey]
    keys.foreach { k =>
      val next = entries.get(k).filter(_.listing != null).flatMap { e =>
        val l = e.listing
        Option.when(e.owners.nonEmpty)(e.owners).flatMap(ownerOf).orElse(
          slots.get(PipelineFilms.slotOf(l.listing, normalizer)).filter(_.films.nonEmpty).flatMap(at => PipelineFilms.pick(l.listing,
            at.films.values.toSeq.flatMap { case (ref, source) => storedById.get(ref.id).flatMap(_.record.data.get(source)).map(ref -> _) }, normalizer)))
      }
      val was = previousAt(k)
      if (next != was) {
        moved += k
        was.foreach(r => listingsOf = listingsOf.updatedWith(r.id)(_.map(_ - k).filter(_.nonEmpty)))
        next match {
          case Some(r) => place(k, r); listingsOf = listingsOf.updated(r.id, listingsOf.getOrElse(r.id, Set.empty) + k)
          case None    => unplace(k)
        }
      }
    }
    moved
  }

  /** Whether every venue slot of `is` reads to a listing's pick ([[PipelineFilms.pick]]: the slot's year, as its
   *  titles may carry it, and its directors) as the same slot of `was` does. */
  private def pickedAlike(was: StoredMovieRecord, is: StoredMovieRecord, at: Option[Set[Source]]): Boolean =
    (was.record eq is.record) || sourcesOf(is, at).forall {
      case (source: models.CinemaShowing, sd) => was.record.data.get(source).exists(w =>
        (w eq sd) || (w.releaseYear == sd.releaseYear && w.rawTitle == sd.rawTitle && w.title == sd.title && w.director == sd.director))
      case _ => true
    }

  /** Content alike: the same identity fields and the same record, showtimes by digest. */
  private def sameFilm(is: StoredMovieRecord, was: StoredMovieRecord): Boolean =
    is.storedKey == was.storedKey && is.title == was.title && is.year == was.year &&
      LeanRecords.equal(is.record, was.record)

  private def unstore(id: String, reslot: mutable.HashSet[ListingKey]): Unit = {
    storedById.get(id).foreach(r => reslot ++= listingsOf.getOrElse(r.id.value, Set.empty))
    // Each slot of the film as stored: where it is and the listing it is — worked out from the record, not kept beside it.
    storedById.get(id).map(layoutOf(_)).foreach(_.foreach { case (at, key) =>
      // What it moves by leaving is a listing it was on (above): another film's listing at its slot was not picked
      // over it, and stays picked without it.
      at.foreach(at => slots.get(at).foreach { slot => slot.films -= id; tidySlot(at) })
      key.foreach(k => entries.get(k).foreach { e => e.owners = e.owners.filterNot(_.id == id); tidy(e) })
    })
    storedById = storedById - id
  }

  private def layoutOf(r: StoredMovieRecord, at: Option[Set[Source]] = None): Seq[(Option[(String, String)], Option[ListingKey])] =
    sourcesOf(r, at).toSeq.map { case (source, sd) => val slot = IdentityProjectionPlan.slotOf(source, sd); slot.at -> slot.key }

  /** `r`'s slots, or only those at `at`'s sources. */
  private def sourcesOf(r: StoredMovieRecord, at: Option[Set[Source]]): Iterator[(Source, SourceData)] =
    at.fold(r.record.data.iterator)(_.iterator.flatMap(s => r.record.data.get(s).map(s -> _)))

  private def restore(r: StoredMovieRecord, reslot: mutable.HashSet[ListingKey], touched: Option[Set[Source]] = None): Unit = {
    val id     = r.id.value
    // The same film at the same slots, each read the same by a listing's pick: no listing's previous film can move, so
    // only the record is replaced — most films written are written for their showtimes, and the wide ones (thousands of
    // slots) re-pointed every listing at every slot of theirs for nothing. Of a film written as patched from its last
    // draft, only the `touched` sources can differ from the record held (the written one, as nothing else wrote it since:
    // [[ProjectedFilm.touched]]), and only they are compared — the same on them, the same everywhere.
    if (storedById.get(id).exists(was => was.record.tmdbId == r.record.tmdbId && pickedAlike(was, r, touched)) &&
        storedById.get(id).exists(was => layoutOf(was, touched).toSet == layoutOf(r, touched).toSet)) {
      storedById = storedById.updated(id, r)
      return
    }
    val slots  = r.record.data.toSeq.map { case (source, sd) => source -> IdentityProjectionPlan.slotOf(source, sd) }
    unstore(id, reslot)
    storedById = storedById.updated(id, r)
    val ref   = PipelineFilmRef(id, r.record.tmdbId)
    slots.foreach { case (source, slot) =>
      slot.at.foreach { at =>
        val slot = slotAt(at)
        slot.films = slot.films.updated(id, ref -> source)
        reslot ++= slot.keys
      }
      // The listing the slot is — at the slot's own title almost always (re-pointed above), but a slot folded under
      // another title than its listing's is reached only here.
      slot.key.map(canonical).foreach { k =>
        val e = entry(k)
        e.owners = e.owners.filterNot(_.id == id) + ref
        reslot += k
      }
    }
  }
}

private object LiveProjectionIndex {
  /** What the index knows of one listing key: the listing published under it, its previous film, its cluster and the
   *  decision naming it (each null for none), and the stored films whose slot it is. */
  private final class Entry(var key: ListingKey) {
    var listing: ProjectedListing   = null
    var previous: PipelineFilmRef   = null
    var cluster: ClusterId          = null
    var decision: ResolverDecision  = null
    var owners: Set[PipelineFilmRef] = Set.empty
    var heldAt: Int                  = -1   // the last update whose model held the key: a mark, not something it holds
    def holdsNothing: Boolean = listing == null && previous == null && cluster == null && decision == null && owners.isEmpty
  }

  /** One venue slot: the published listings at it, and the stored films with a slot there (by id: the film and its source). */
  private final class Slot {
    var keys: Set[ListingKey]                         = Set.empty
    var films: Map[String, (PipelineFilmRef, Source)] = Map.empty
    def holdsNothing: Boolean = keys.isEmpty && films.isEmpty
  }
}
