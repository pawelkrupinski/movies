package services.movies

import models.{Cinema, MovieRecord, Source, SourceData}

import scala.collection.mutable

/**
 * The four whole-corpus derivations `recordCinemaScrape` asks about, maintained AS
 * ROWS ARE WRITTEN instead of rebuilt from scratch on every venue.
 *
 * WHY THIS EXISTS. Each of those derivations — the sanitized-title set, the
 * concluded-row alias set, the (cinema, slot title) → row map, the rows-by-title
 * grouping — walked the whole `positive` map, and two more walks sat either side of
 * them (the guards' `heldSlotsOf`, the prune's stale-slot sweep). Six-plus full
 * passes, per venue, in a loop over every venue: O(venues x corpus). The corpus grows
 * as the venues land, so the cost of a FIXED-size chunk of venues grows linearly
 * through a tick — which is exactly what the United States convergence leg measured
 * on 2026-08-31, over 4205 venues in chunks of 420:
 *
 *     scraped  420/4205 in   530.9s      delta   531s
 *     scraped 1260/4205 in  3050.8s      delta  1402s
 *     scraped 3360/4205 in 17072.1s      delta  4281s
 *
 * The same 420 venues cost 8x more at venue 3000 than at venue 400. The leg never
 * finished a single tick: 3360 of 4205 venues after five hours and fifteen minutes,
 * with `scrapeTick` still running when the job timed out. Poland and Germany hide it
 * because their corpora are small enough for a linear-in-corpus constant to look
 * flat; the UK, at 92% of its CI budget, was next.
 *
 * The sibling path had already been through this. `StagingRepository.findByAnchor`
 * carries the same story in its doc comment ("during an ingest that grows the backlog
 * that is quadratic in it... 28 venues per 37s to 28 per 243s") and was fixed the same
 * way. This is that fix for the movies side.
 *
 * WHY AN INDEX AND NOT A MEMO. The obvious cheap fix — derive once per tick and reuse
 * across the venues — is WRONG: each venue's scrape writes rows, so the next venue
 * must see them. A stale snapshot would re-divert films that had just landed. The
 * index has to move with the writes, which is what makes maintaining it incrementally
 * the only correct shape.
 *
 * WHY IT CAN BE TRUSTED TO STAY IN SYNC. `positive` is an UNBOUNDED Caffeine cache
 * (`Caffeine.newBuilder().build()` — no `maximumSize`, no `expireAfter`), so nothing
 * leaves it except through an explicit write, and every one of those funnels through
 * `MovieCache.store` / `evict` / the `putIfPresent` compute. There is no eviction
 * callback to miss. [[CorpusIndexConsistencySpec]] pins that by replaying a realistic
 * scrape/fold/prune sequence and asserting the incremental index equals one rebuilt
 * from the rows.
 *
 * EVERY read here is a POINT query, which is the reason the swap is a clean one: the
 * call sites only ever asked `contains` / `get` / "this cinema's slots" of the maps
 * they were building in full.
 *
 * Not thread-safe on its own — every method synchronises on this instance. The
 * critical sections are a handful of map operations against per-key and per-cinema
 * buckets, against the full-corpus walks they replace.
 *
 * @param normalizer the country's rules; `sanitize` here must agree with the sanitize
 *                   the call sites use to ask, or a lookup silently misses.
 * @param isConcludedBareRow the alias-set predicate, passed in rather than imported so
 *                   this class holds no opinion about what makes a row eligible — it
 *                   is [[FilmCanonicalizer.isBareFilmTitle]] plus `tmdbConcluded`, and
 *                   it must stay the ONE definition the divert gate uses.
 */
private[movies] final class CorpusIndex(normalizer: TitleNormalizer,
                                        isConcludedBareRow: (CacheKey, MovieRecord) => Boolean) {

  /** `key.normalized` → the rows living under it. The old `rowsFor` grouping. */
  private val rowsByNormalized = mutable.Map.empty[String, mutable.Map[CacheKey, MovieRecord]]

  /** (cinema, sanitized slot title) → the keys holding that slot. The old
   *  `rowByCinemaSlot` / `knownByCinemaSlot` pair; the caller picks the canonical
   *  key off the (tiny) set, so the ranking rule stays in one place. */
  private val keysByCinemaSlot = mutable.Map.empty[(Cinema, String), mutable.Set[CacheKey]]

  /** cinema → its slots, with the row each belongs to. The old `heldSlotsOf` scan,
   *  and the prune's stale-slot sweep. */
  private val slotsByCinema = mutable.Map.empty[Cinema, mutable.Map[(CacheKey, Source), SourceData]]

  /** sanitized alias → the concluded bare rows carrying it.
   *
   *  A SET OF KEYS, not the refcount this used to be. The count answered `holdsAlias`
   *  and nothing else, so the divert gate's "is this an alias of a concluded row" was
   *  all it could serve; `concludedKeyFor` needed the KEYS and so kept walking the whole
   *  corpus for them. Keying by `CacheKey` gives the same "two rows offer it, dropping
   *  one must not un-know it" property the refcount was there for — set removal is
   *  idempotent per row — and answers the harder question too. */
  private val keysByAlias = mutable.Map.empty[String, mutable.Set[CacheKey]]

  /** Index `record` under `key`, replacing whatever that key contributed before. */
  def put(key: CacheKey, record: MovieRecord): Unit = synchronized {
    forget(key)
    rowsByNormalized.getOrElseUpdate(key.normalized, mutable.Map.empty).update(key, record)
    record.cinemaShowings.foreach { case (cinema, sd) =>
      sd.title.foreach { t =>
        keysByCinemaSlot.getOrElseUpdate((cinema, normalizer.sanitize(t)), mutable.Set.empty) += key
      }
    }
    record.data.foreach { case (source, sd) =>
      Source.cinemaOf(source).foreach { cinema =>
        slotsByCinema.getOrElseUpdate(cinema, mutable.Map.empty).update((key, source), sd)
      }
    }
    if (isConcludedBareRow(key, record))
      record.tmdbTitleAliases.foreach { alias =>
        keysByAlias.getOrElseUpdate(normalizer.sanitize(alias), mutable.Set.empty) += key
      }
  }

  /** Drop everything `key` contributes. */
  def remove(key: CacheKey): Unit = synchronized(forget(key))

  /** Is any row keyed under this sanitized title? (the old `knownSanitized`) */
  def holdsTitle(normalized: String): Boolean =
    synchronized(rowsByNormalized.get(normalized).exists(_.nonEmpty))

  /** Is this sanitized title an alias of a concluded bare row? (`knownAliases`) */
  def holdsAlias(alias: String): Boolean = synchronized(keysByAlias.get(alias).exists(_.nonEmpty))

  /** The concluded bare rows carrying this sanitized alias.
   *
   *  The alias arm of [[MovieCache.concludedKeyFor]], which walked the whole corpus
   *  running `isBareFilmTitle` and a `sanitize` per alias per row — for every landed
   *  listing. */
  def keysForAlias(alias: String): Set[CacheKey] =
    synchronized(keysByAlias.get(alias).map(_.toSet).getOrElse(Set.empty))

  /** Every row under this sanitized title, WITH its key. */
  def entriesFor(normalized: String): Seq[(CacheKey, MovieRecord)] =
    synchronized(rowsByNormalized.get(normalized).map(_.toVector).getOrElse(Vector.empty))

  /** Every row under this sanitized title. (`rowsFor`) */
  def rowsFor(normalized: String): Seq[MovieRecord] = entriesFor(normalized).map(_._2)

  /** Does any row already hold this cinema's slot? (`knownByCinemaSlot`) */
  def holdsCinemaSlot(cinema: Cinema, normalized: String): Boolean =
    synchronized(keysByCinemaSlot.get((cinema, normalized)).exists(_.nonEmpty))

  /** The keys holding it, for the caller to rank. (`rowByCinemaSlot`) */
  def keysForCinemaSlot(cinema: Cinema, normalized: String): Set[CacheKey] =
    synchronized(keysByCinemaSlot.get((cinema, normalized)).map(_.toSet).getOrElse(Set.empty))

  /** This cinema's slots, with the row each sits on. (`heldSlotsOf`, and the prune) */
  def slotsOf(cinema: Cinema): Seq[(CacheKey, Source, SourceData)] =
    synchronized(slotsByCinema.get(cinema)
      .map(_.iterator.map { case ((k, s), sd) => (k, s, sd) }.toVector)
      .getOrElse(Vector.empty))

  /**
   * Everything this index holds, flattened — the whole of its state, so a comparison
   * against a freshly-rebuilt index can find drift ANYWHERE rather than in the one place
   * a spec thought to look.
   *
   * An index that shadows the rows is only as good as the funnels that feed it, and a
   * missed funnel is silent: the wrong answer looks like an ordinary divert. This is how
   * [[CorpusIndexConsistencySpec]] catches one.
   */
  private[movies] def snapshot: CorpusIndex.Snapshot = synchronized {
    CorpusIndex.Snapshot(
      rowsByNormalized = rowsByNormalized.map { case (n, rows) => n -> rows.keySet.toSet }.toMap,
      keysByCinemaSlot = keysByCinemaSlot.map { case (slot, keys) => slot -> keys.toSet }.toMap,
      slotsByCinema    = slotsByCinema.map { case (c, slots) => c -> slots.keySet.toSet }.toMap,
      keysByAlias      = keysByAlias.map { case (a, keys) => a -> keys.toSet }.toMap)
  }

  private def forget(key: CacheKey): Unit = {
    val prior = rowsByNormalized.get(key.normalized).flatMap(_.get(key))
    prior.foreach { record =>
      record.cinemaShowings.foreach { case (cinema, sd) =>
        sd.title.foreach { t =>
          val slot = (cinema, normalizer.sanitize(t))
          keysByCinemaSlot.get(slot).foreach { keys =>
            keys -= key
            if (keys.isEmpty) keysByCinemaSlot -= slot
          }
        }
      }
      record.data.foreach { case (source, _) =>
        Source.cinemaOf(source).foreach { cinema =>
          slotsByCinema.get(cinema).foreach { slots =>
            slots -= ((key, source))
            if (slots.isEmpty) slotsByCinema -= cinema
          }
        }
      }
      if (isConcludedBareRow(key, record))
        record.tmdbTitleAliases.foreach { alias =>
          val a = normalizer.sanitize(alias)
          keysByAlias.get(a).foreach { keys =>
            keys -= key
            if (keys.isEmpty) keysByAlias -= a
          }
        }
    }
    rowsByNormalized.get(key.normalized).foreach { rows =>
      rows -= key
      if (rows.isEmpty) rowsByNormalized -= key.normalized
    }
  }
}

private[movies] object CorpusIndex {
  /** @see [[CorpusIndex.snapshot]] */
  final case class Snapshot(rowsByNormalized: Map[String, Set[CacheKey]],
                            keysByCinemaSlot: Map[(Cinema, String), Set[CacheKey]],
                            slotsByCinema: Map[Cinema, Set[(CacheKey, Source)]],
                            keysByAlias: Map[String, Set[CacheKey]])
}
