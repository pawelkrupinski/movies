package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Source, SourceData}
import services.movies.{CinemaSlotBuilder, ListingKey, MovieRecordMerge, ScrapeListing, ScreeningTokens, ShowtimesDigest, StoredMovieRecord,
  TitleNormalizer}

// Everything a venue slot is built and fingerprinted by, and nothing else: this file is what the venue slot code version
// digests (`IdentityRulesSources.VenueSlotRoots`), so a change to the rest of the projection keeps every recorded slot.

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
  // Kept between projections by each key's 64 bits ([[VenueSlotMemo.Key.id]]), not the key, in sorted arrays: a key
  // object, a hash node and a share of the table per entry were ~10 MB of worker-us's ~110k, and the key's own fields
  // are 32-bit hashes of what they name. What a projection uses is gathered beside them, and merged in once it closes.
  private var previous = SortedLongMap.empty[VenueSlotMemo.Entry]
  private var current  = scala.collection.mutable.LongMap.empty[VenueSlotMemo.Entry]
  private var recorded = Set.empty[Long]
  private var hits     = 0
  private var builds   = 0
  // Why a build was needed, for the projection's log: the same venue's same listings with other rows, with other
  // prior slots, or listings the last projection did not have at the venue at all — by the venue's listings ([[Key.listings]]).
  private var seenBefore = SortedLongMap.empty[VenueSlotMemo.Key]
  private var seenNow    = scala.collection.mutable.LongMap.empty[VenueSlotMemo.Key]
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
      seenNow(key.listings) = key
      previous.get(key.id).orElse(Option.when(recorded.nonEmpty)(stored).flatten.map(_.map { case (source, slot) =>
        source -> ShowtimesDigest.stripSlot(slot) }).map(lean => VenueSlotMemo.Entry(lean, fingerprint(key, lean)))
        .filter(entry => recorded(entry.fingerprint))) match {
        case Some(entry) => current(key.id) = entry; hits += 1; Some(entry.lean)
        case None =>
          seenBefore.get(key.listings) match {
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
      current(key.id) = VenueSlotMemo.Entry(lean, fingerprint(key, lean))
      val after = writtenAs.fold(key)(VenueSlotMemo.written(key, lean, _))
      if (after != key) current(after.id) = VenueSlotMemo.Entry(lean, fingerprint(after, lean))
    }
    builds += 1
  }

  private def fingerprint(key: VenueSlotMemo.Key, lean: Seq[(Source, SourceData)]): Long = VenueSlotMemo.fingerprint(environment, key, lean)

  /** Close a projection, and say how many slots it took from the memo and built. A projection of the whole corpus keeps
   *  only the slots it used; one of a scope (`retainUnseen`) also keeps every other film's, which it did not look at,
   *  dropping only the slots of a venue's listings it built anew from other rows. */
  def endTick(retainUnseen: Boolean = false): (Int, Int) = synchronized {
    if (retainUnseen) {
      val moved = scala.collection.mutable.HashSet.empty[Long]
      seenNow.foreach { case (listings, key) => seenBefore.get(listings).filter(_ != key).foreach(moved += _.id) }
      previous = previous.merged(current, moved)
      seenBefore = seenBefore.merged(seenNow)
    } else {
      previous = SortedLongMap.empty.merged(current)
      seenBefore = SortedLongMap.empty.merged(seenNow)
    }
    // Afresh, not cleared: a cleared map keeps its table, sized for what the projection used, idle until the next.
    current = scala.collection.mutable.LongMap.empty
    seenNow = scala.collection.mutable.LongMap.empty
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
  final case class Key(venue: String, rows: Int, keys: Int, priors: Int, size: Int) {
    /** The memo's 64 bits of this key: what it keeps a venue's slots under. */
    def id: Long = ContentHash.of(this)
    /** The memo's 64 bits of the venue's listings this key is of, whatever their rows and priors. */
    def listings: Long = ContentHash.of((venue, keys))
  }

  /** The key of one film's `rows` at `venue`: their content and listing keys, the prior slots their previous films hold
   *  there (`priors`), and each row's film by its counter (`films`) — a film keeps its counter for good and has it from
   *  its first draft, so the key a slot has once written is worked out from it ([[written]]). */
  def keyOf(venue: String, rows: Seq[ProjectedListing], priors: Seq[Int], films: Seq[Option[Long]]): Key =
    Key(venue, rows.map(r => (r.row, r.showtimes)).##, rows.map(_.listing.key).##, (priors, films).##, rows.size)

  /** Slots the memo holds, and their fingerprint. */
  private final case class Entry(lean: Seq[(Source, SourceData)], fingerprint: Long)

  /** A memo of nothing: every film's slots built. */
  def none: VenueSlotMemo = new VenueSlotMemo {
    override private[identity] def lookup(key: Key, stored: => Option[Seq[(Source, SourceData)]]): Option[Seq[(Source, SourceData)]] = None
  }

  /** The build of the code a slot is made by, as a version: the build's digest of this file and of what the slot
   *  builder and the landing's fold reach — not of the projection plan, whose every other change kept no slot from
   *  being reused after a restart, the resolver left out (`venue-slot-version.txt`, generated by `build.sbt` as
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


/** How one film's slots at one venue are built from the venue's rows ([[IdentityProjectionPlan.draftOf]]). */
private[identity] object VenueSlots {
  /** A venue's rows now, by the listing key each is published under: grouped once per venue a projection reads, for
   *  every film it builds there — grouped once per film, a venue with many changed films regrouped its whole programme
   *  for each. */
  type VenueRows = Map[ListingKey, Seq[CinemaMovie]]
  def rowsByKey(cinema: Cinema, fetched: Seq[CinemaMovie]): VenueRows = fetched.groupBy(cm => ListingKey.of(cinema, cm))

  /** The rows `cinema` publishes under `key`, as one: the smallest by the listings' total order, carrying every row's
   *  showtimes — so a venue printing one listing twice loses none. */
  private[identity] def merged(cinema: Cinema, rows: Seq[CinemaMovie], normalizer: TitleNormalizer): CinemaMovie =
    if (rows.sizeIs == 1) rows.head
    else rows.minBy(cm => Listing.of(cinema, cm, normalizer)).copy(showtimes = MovieRecordMerge.dedupShowtimes(rows.flatMap(_.showtimes)))

  /** Whether `byKey` holds exactly the rows `listed` were read from (no re-scrape landed between the two reads), so
   *  what is built from it may be kept under their key. */
  private[identity] def readAsListed(cinema: Cinema, listed: Seq[ProjectedListing], byKey: VenueRows, normalizer: TitleNormalizer): Boolean =
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
  private[identity] def buildVenue(cinema: Cinema, keys: Seq[ListingKey], byKey: VenueRows, previousOf: Map[ListingKey, PipelineFilmRef],
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
