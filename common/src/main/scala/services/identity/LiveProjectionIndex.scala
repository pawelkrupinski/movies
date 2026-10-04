package services.identity

import models.Source
import services.movies.{ListingKey, ShowtimesDigest, StoredMovieRecord, TitleNormalizer}

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
 * Single-threaded: the projection's tick holds it.
 */
final class LiveProjectionIndex(normalizer: TitleNormalizer) {
  import IdentityProjectionPlan.{clusterOf => clusterFor, oneByKey, ownerOf}

  // Listings: each venue's listing as last read (the object, to tell it unmoved) — one listing per key at it is worked
  // out again only for a venue an update reads by key (a key's venue is its own, `ListingKey.venue`).
  private val venueListings = mutable.HashMap.empty[String, Seq[ProjectedListing]]
  private var byKey         = Map.empty[ListingKey, ProjectedListing]
  // The published listings at each venue slot (a listing's own is `PipelineFilms.slotOf` it): whose previous film a
  // slot moves.
  private val keysAtSlot    = mutable.HashMap.empty[(String, String), Set[ListingKey]]

  // Stored films: each one's venue slots and the listing keys its slots are, indexed both ways — by where each slot is
  // and its source only: a slot's data is read off the stored record as it is now, so a record read again is not kept.
  private var storedById    = Map.empty[String, StoredMovieRecord]
  private val slotsOfFilm   = mutable.HashMap.empty[String, Seq[(Option[(String, String)], Option[ListingKey])]]
  private val filmsAtSlot   = mutable.HashMap.empty[(String, String), Map[String, (PipelineFilmRef, Source)]]
  private val ownersOfKey   = mutable.HashMap.empty[ListingKey, Set[PipelineFilmRef]]

  private var previousOf    = Map.empty[ListingKey, PipelineFilmRef]
  private var listingsOf    = Map.empty[String, Set[ListingKey]]

  // Decisions, by identity: the model hands back an unmoved family's own objects.
  private val decisions     = new java.util.IdentityHashMap[ResolverDecision, ClusterId]()
  private val decisionsOf   = mutable.HashMap.empty[ClusterId, List[ResolverDecision]]
  private val decisionOf    = mutable.HashMap.empty[ListingKey, ResolverDecision]
  private var clusters      = Map.empty[ClusterId, Cluster]
  private var clusterOf     = Map.empty[ListingKey, ClusterId]

  /** The index as it stands, over `counters`. */
  def index(counters: FilmIdCounters): ProjectionIndex = {
    val unmapped  = listingsOf.iterator.collect { case (id, ls) if counters.counterOf(id).isEmpty => IdSeeding.Film(id, ls) }.toSeq
    val covered   = if (unmapped.isEmpty) counters else counters.covering(unmapped)
    ProjectionIndex(byKey, storedById, previousOf, listingsOf, covered, covered.entries.filterNot(e => counters.counterOf(e.filmId).isDefined),
      clusters, clusterOf)
  }

  /** Move the index to what these inputs say, and say what moved. */
  def update(venues: Seq[(String, Seq[ProjectedListing])], held: ListingKey => Boolean, resolved: Seq[ResolverDecision],
             stored: Seq[StoredMovieRecord]): ProjectionScope.Changes = {
    val keys = mutable.HashSet.empty[ListingKey]   // whose published listing (or whether it is published) moved
    val byKeyAt = mutable.HashMap.empty[String, Map[ListingKey, ProjectedListing]]
    def at(venue: String): Map[ListingKey, ProjectedListing] = byKeyAt.getOrElseUpdate(venue, oneByKey(venueListings.getOrElse(venue, Nil)))
    // 1. Listings: a venue whose listing is another object, or gone.
    val live = venues.iterator.map(_._1).toSet
    venues.foreach { case (venue, listings) =>
      if (!venueListings.get(venue).exists(_ eq listings)) {
        val was = at(venue)
        val now = oneByKey(listings)
        was.foreach { case (k, l) => if (!now.get(k).exists(n => (n eq l) || n == l)) keys += k }
        // An unmoved listing read again is a new object of the same value: index that one, so the old read is not kept.
        now.foreach { case (k, n) => if (byKey.get(k).exists(l => (l ne n) && l == n)) byKey = byKey.updated(k, n) }
        venueListings(venue) = listings
        byKeyAt(venue) = now
      }
    }
    venueListings.keys.filterNot(live).toSeq.foreach { venue =>
      keys ++= at(venue).keys
      venueListings.remove(venue)
      byKeyAt.remove(venue)
    }
    // …and a listing new to its venue, or one the model took up or let go: published, held and indexed must agree.
    venueListings.valuesIterator.foreach(_.foreach { l => val k = l.listing.key; if (held(k) != byKey.contains(k)) keys += k })
    val published = mutable.HashSet.empty[ListingKey]   // whose entry in `byKey` moved
    keys.foreach { k =>
      val listing = (if (venueListings.contains(k.venue)) at(k.venue).get(k) else None).filter(_ => held(k))
      if (listing != byKey.get(k)) {
        published += k
        byKey.get(k).foreach { was =>
          val slot = PipelineFilms.slotOf(was.listing, normalizer)
          keysAtSlot.updateWith(slot)(_.map(_ - k).filter(_.nonEmpty))
        }
        listing match {
          case Some(l) =>
            byKey = byKey.updated(k, l)
            val slot = PipelineFilms.slotOf(l.listing, normalizer)
            keysAtSlot(slot) = keysAtSlot.getOrElse(slot, Set.empty) + k
          case None => byKey = byKey - k
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
        case Some(is) if sameFilm(is, was) => storedById = storedById.updated(id, is)
        case other =>
          films += id
          freed += ProjectionScope.plainKey(was.key(normalizer))
          other.fold(unstore(id, reslot))(is => restore(is, reslot))
      }
    }
    now.foreach { case (id, is) => if (!storedById.contains(id)) restore(is, reslot) }

    // 3. Previous films: of every listing whose publication, slot or film moved.
    val moved = repoint(published.iterator ++ reslot.iterator)

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
      d.members.foreach(k => if (decisionOf.get(k).exists(_ eq d)) decisionOf.remove(k))
    }
    resolved.foreach { d =>
      if (!decisions.containsKey(d)) {
        val id = IdentityProjectionPlan.clusterIdOf(d)
        decisions.put(d, id)
        touched += id
        decisionsOf(id) = d :: decisionsOf.getOrElse(id, Nil)
        d.members.foreach(k => decisionOf(canonical(k)) = d)
      }
    }
    published.foreach(k => decisionOf.get(k).foreach(d => touched += decisions.get(d)))
    val regrouped = mutable.HashSet.empty[ListingKey]
    touched.foreach { id =>
      val was  = clusters.get(id)
      val next = clusterFor(id, decisionsOf.getOrElse(id, Nil), byKey.contains).map(c => c.copy(members = c.members.map(canonical)))
      if (next != was) {
        // A listing that left the cluster is gone, or in a cluster that moved too: the clusters partition.
        was.foreach(_.members.foreach(k => if (clusterOf.get(k).contains(id)) clusterOf = clusterOf - k))
        next match {
          case Some(c) => clusters = clusters.updated(id, c); regrouped ++= c.members; c.members.foreach(k => clusterOf = clusterOf.updated(k, id))
          case None    => clusters = clusters - id
        }
      }
      if (decisionsOf.get(id).exists(_.isEmpty)) decisionsOf.remove(id)
    }

    val changed = ProjectionScope.Changes((published ++ moved ++ regrouped ++ afterWrites).toSet, films.toSet, freed.toSet)
    afterWrites.clear()
    changed
  }

  /** A projection wrote `films` and retired `retired`: the stored films are those now — the next update counts a film it
   *  wrote as moved only if another writer has moved it since. */
  def written(films: Seq[ProjectedFilm], retired: Seq[services.movies.FilmId]): Unit = {
    val reslot = mutable.HashSet.empty[ListingKey]
    retired.foreach(id => unstore(id.value, reslot))
    films.foreach(f => restore(StoredMovieRecord(f.title, f.year, f.record, f.id, Some(f.key)), reslot))
    // A listing the store, as written, puts on the film it was written into is where the writes meant it to be. Any other
    // listing they moved, and any written into a film the store does not put it on, is moved for the next projection: a
    // written slot is not always the slot of the listing it was built from (a director carried forward from its prior is
    // part of the slot's own listing key), and a whole projection reading the store afresh finds the listing on another
    // film — and drafts its cluster again.
    val writtenInto = films.iterator.flatMap(f => f.members.iterator.map(_ -> f.id.value)).toMap
    val moved       = repoint(reslot.iterator)
    def placedAsWritten(k: ListingKey): Boolean = writtenInto.get(k).exists(id => previousOf.get(k).exists(_.id == id))
    afterWrites ++= moved.filterNot(placedAsWritten)
    afterWrites ++= writtenInto.keysIterator.filterNot(placedAsWritten)
    ()
  }

  /** `k` as the listing published under it holds it: one key object per listing, not one per map that names it — a stored
   *  slot's and a decision's are read anew from the store (worker-us held ~4.8 per listing). */
  private def canonical(k: ListingKey): ListingKey = byKey.get(k).fold(k)(_.listing.key)

  /** Listings the last writes moved to another film: changes for the next [[update]]. */
  private val afterWrites = mutable.HashSet.empty[ListingKey]

  /** Work out `keys`' previous films again: the film whose slot each is, else the one whose slot it folds into — the
   *  builder's rule ([[IdentityProjectionPlan.index]]). The ones that moved. */
  private def repoint(keys: Iterator[ListingKey]): mutable.HashSet[ListingKey] = {
    val moved = mutable.HashSet.empty[ListingKey]
    keys.foreach { k =>
      val next = byKey.get(k).flatMap { l =>
        ownersOfKey.get(k).flatMap(ownerOf).orElse(
          filmsAtSlot.get(PipelineFilms.slotOf(l.listing, normalizer)).flatMap(at => PipelineFilms.pick(l.listing, at.values.toSeq.flatMap { case (ref, source) =>
            storedById.get(ref.id).flatMap(_.record.data.get(source)).map(ref -> _) }, normalizer)))
      }
      val was = previousOf.get(k)
      if (next != was) {
        moved += k
        was.foreach(r => listingsOf = listingsOf.updatedWith(r.id)(_.map(_ - k).filter(_.nonEmpty)))
        next match {
          case Some(r) => previousOf = previousOf.updated(k, r); listingsOf = listingsOf.updated(r.id, listingsOf.getOrElse(r.id, Set.empty) + k)
          case None    => previousOf = previousOf - k
        }
      }
    }
    moved
  }

  /** Whether every venue slot of `is` reads to a listing's pick ([[PipelineFilms.pick]]: the slot's year, as its
   *  titles may carry it, and its directors) as the same slot of `was` does. */
  private def pickedAlike(was: StoredMovieRecord, is: StoredMovieRecord): Boolean =
    (was.record eq is.record) || is.record.data.forall {
      case (source: models.CinemaShowing, sd) => was.record.data.get(source).exists(w =>
        (w eq sd) || (w.releaseYear == sd.releaseYear && w.rawTitle == sd.rawTitle && w.title == sd.title && w.director == sd.director))
      case _ => true
    }

  /** Content alike: the same identity fields and the same record, showtimes by digest. */
  private def sameFilm(is: StoredMovieRecord, was: StoredMovieRecord): Boolean =
    is.storedKey == was.storedKey && is.title == was.title && is.year == was.year &&
      ((is.record eq was.record) || ShowtimesDigest.leanEqual(is.record, was.record))

  private def unstore(id: String, reslot: mutable.HashSet[ListingKey]): Unit = {
    storedById.get(id).foreach(r => reslot ++= listingsOf.getOrElse(r.id.value, Set.empty))
    storedById = storedById - id
    slotsOfFilm.remove(id).foreach(_.foreach { case (at, key) =>
      // What it moves by leaving is a listing it was on (above): another film's listing at its slot was not picked
      // over it, and stays picked without it.
      at.foreach(at => filmsAtSlot.updateWith(at)(_.map(_ - id).filter(_.nonEmpty)))
      key.foreach(k => ownersOfKey.updateWith(k)(_.map(_.filterNot(_.id == id)).filter(_.nonEmpty)))
    })
  }

  private def restore(r: StoredMovieRecord, reslot: mutable.HashSet[ListingKey]): Unit = {
    val id     = r.id.value
    val slots  = r.record.data.toSeq.map { case (source, sd) => source -> IdentityProjectionPlan.slotOf(source, sd) }
    val layout = slots.map { case (_, slot) => slot.at -> slot.key.map(canonical) }
    // The same film at the same slots, each read the same by a listing's pick: no listing's previous film can move, so
    // only the record is replaced — most films written are written for their showtimes, and the wide ones (thousands of
    // slots) re-pointed every listing at every slot of theirs for nothing.
    if (storedById.get(id).exists(was => was.record.tmdbId == r.record.tmdbId && pickedAlike(was, r)) &&
        slotsOfFilm.get(id).exists(_.toSet == layout.toSet)) {
      storedById = storedById.updated(id, r)
      return
    }
    unstore(id, reslot)
    storedById = storedById.updated(id, r)
    val ref   = PipelineFilmRef(id, r.record.tmdbId)
    slotsOfFilm(id) = layout
    slots.foreach { case (source, slot) =>
      slot.at.foreach { at =>
        filmsAtSlot(at) = filmsAtSlot.getOrElse(at, Map.empty).updated(id, ref -> source)
        reslot ++= keysAtSlot.getOrElse(at, Set.empty)
      }
      // The listing the slot is — at the slot's own title almost always (re-pointed above), but a slot folded under
      // another title than its listing's is reached only here.
      slot.key.map(canonical).foreach { k =>
        ownersOfKey(k) = ownersOfKey.getOrElse(k, Set.empty).filterNot(_.id == id) + ref
        reslot += k
      }
    }
  }
}
