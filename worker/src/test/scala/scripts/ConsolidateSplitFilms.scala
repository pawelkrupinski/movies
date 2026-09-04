package scripts

import models.{Source, SourceData}
import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, RuntimeCorroboration,
  SingleCountryNormalizer, StoredMovieRecord, TitleNormalizer}
import tools.Env

/**
 * Re-file the cinema listings of a film that ended up stored as TWO films, and drop the
 * row left holding none.
 *
 * The state this repairs, measured on prod PL 2026-08-14: `tylkojednanoc|1961` (tmdbId
 * 41050, Antonioni's "La notte", 121 min) and `tylkojednanoc|2026` (tmdbId 1433367, 102
 * min) both held the SAME venues' listings — 67 slot keys under both ids, each with its
 * own `screenings` document — so `/poznan` rendered the film twice under one slug with
 * the same booking links on both cards. `MovieCache.chooseConcluded` now routes a
 * listing to the film the venue's own published runtime points at, so each venue moves
 * itself on its next scrape; this closes out the rows already split rather than a leak
 * that is still running. Run it AFTER that fix is deployed — against the old code the
 * next scrape simply re-files everything back onto the older row.
 *
 * The decision is the SAME rule the scrape path uses ([[RuntimeCorroboration]]), asked of
 * stored rows instead of cache state: a venue belongs to whichever candidate film its
 * published runtime sits strictly nearest, and — for a venue that published no runtime at
 * all — to the film more venues are screening.
 *
 * SAFETY. This only ever MOVES a venue between two rows of a group it has proven is a
 * split, and only deletes a row it has just emptied of cinemas:
 *   - a group is a candidate only if two of its rows carry DIFFERENT tmdbIds AND share at
 *     least one venue slot; two same-titled films that share no venue are left alone;
 *   - a venue with no evidence either way is left where it is, never guessed at;
 *   - a row is deleted only when it ends up with zero cinema slots, via
 *     `MovieRepository.delete`, which takes its `movie_slots` / `screenings` rows with it;
 *   - `--max` caps how many rows can be deleted in one run.
 *
 * Dry run by DEFAULT — pass `--apply` to write.
 *
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.ConsolidateSplitFilms"
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.ConsolidateSplitFilms --apply"
 */
object ConsolidateSplitFilms {

  /** One film's row as the evidence this decision needs. `slots` is keyed by the SLOT
   *  key the row actually stores — a `CinemaShowing(cinema, titleKey)`, not the bare
   *  `Cinema`. Keying this on the cinema is the trap: `MovieRecord.cinemaShowings` hands
   *  back the venue, so a removal expressed as `data -- cinemas` matches NOTHING and the
   *  repair silently writes nothing at all. */
  case class Candidate(key: String, tmdbId: Option[Int], ownRuntime: Option[Int], slots: Map[Source, SourceData])

  /** Where one venue's listing belongs, and where it is currently duplicated. */
  case class Move(slot: Source, winner: String, losers: Seq[String])

  /** Read the evidence off a stored row. `ownRuntime` comes from the row's `Tmdb` slot —
   *  the runtime of the film TMDB matched it to — never the merged `runtimeMinutes`,
   *  which on a row that has been absorbing another film's listings reports THEIR
   *  minutes and so would corroborate itself. */
  def candidateOf(row: StoredMovieRecord, normalizer: TitleNormalizer): Candidate =
    Candidate(
      key        = StoredMovieRecord.idOf(row, normalizer),
      tmdbId     = row.record.tmdbId,
      ownRuntime = row.record.data.get(models.Tmdb).flatMap(_.runtimeMinutes),
      slots      = row.record.cinemaSlots.toMap)

  /** The venues to re-file within ONE `sanitize(title)` group, and where each belongs.
   *
   *  Pure, so the spec pins the whole decision without Mongo — which matters more here
   *  than anywhere: this is the function that decides which of two rows a cinema's
   *  screenings end up under. Empty when the group is not a split (fewer than two
   *  distinct tmdbIds, or no venue held by more than one row). */
  def movesFor(group: Seq[Candidate]): Seq[Move] = {
    val resolved = group.filter(_.tmdbId.isDefined)
    val shared   = group.flatMap(_.slots.keys).groupBy(identity).collect { case (slot, held) if held.sizeIs > 1 => slot }
    if (resolved.map(_.tmdbId).distinct.sizeIs < 2 || shared.isEmpty) Nil
    else {
      // The film more venues are screening — the fallback for a slot that published no
      // runtime, and the same reading the scrape path takes: a title shared by a current
      // release and an old picture is overwhelmingly the release when nothing else speaks.
      val byVenueCount = resolved.groupBy(_.slots.size).maxByOption(_._1).map(_._2).getOrElse(Nil)
      val busiest      = if (byVenueCount.map(_.tmdbId).distinct.sizeIs == 1) byVenueCount.map(_.key).minOption else None
      shared.toSeq.sortBy(_.displayName).flatMap { slot =>
        val holders   = group.filter(_.slots.contains(slot)).map(_.key)
        val published = group.flatMap(_.slots.get(slot)).flatMap(_.runtimeMinutes).distinct
        RuntimeCorroboration.strictNearest(published, resolved.map(c => c.key -> c.ownRuntime))
          .orElse(busiest)
          .filter(holders.contains)
          .map(w => Move(slot, w, holders.filterNot(_ == w)))
          .filter(_.losers.nonEmpty)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val apply      = args.contains("--apply")
    val max        = args.toSeq.sliding(2).collectFirst { case Seq("--max", n) => n.toInt }.getOrElse(20)
    val normalizer = SingleCountryNormalizer.titleNormalizer
    val uri        = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName     = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    // The cinemas live in the side collections, so the read has to STITCH them: a bare
    // `MongoMovieRepository` returns every row with an empty `sourceData` and this script
    // would then see a corpus where no film has a venue — no split to find, and every row
    // one step from looking deletable. Wire them as the serving path does.
    val repository = new MongoMovieRepository(Some(db), fallbackToOwnInit = false,
      screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots      = Some(new MongoSlotsRepository(Some(db))),
      normalizer = normalizer)
    if (!repository.enabled) { println(s"movies repository not enabled for $dbName."); sys.exit(1) }
    println(s"ConsolidateSplitFilms — ${if (apply) "APPLY" else "DRY RUN"} (db=$dbName, max=$max)")

    val rows = repository.findAll()
    // A short read makes a split look like a single row and vice versa; refuse rather
    // than move screenings on a partial view of the corpus.
    if (rows.isEmpty) { println("movies read returned NOTHING — refusing to act on an empty corpus."); sys.exit(1) }
    val withVenues = rows.count(_.record.cinemaSlots.nonEmpty)
    if (withVenues == 0) {
      println(s"read ${rows.size} rows and NOT ONE has a cinema — that is a mis-wired or failed " +
        "side-collection read, not a corpus without venues. Refusing to act on it.")
      sys.exit(1)
    }
    println(s"read ${rows.size} movies rows ($withVenues with cinemas)")

    val groups = rows.groupBy(r => normalizer.sanitize(r.title)).toSeq.sortBy(_._1)
    // Decide EVERYTHING first, print it, and only then write: a dry run and an apply run
    // must report the same plan, and the `--max` cap has to bound the whole run rather
    // than however far one group happened to get.
    val planned = groups.flatMap { case (title, groupRows) =>
      val candidates = groupRows.map(candidateOf(_, normalizer))
      val moves      = movesFor(candidates)
      if (moves.isEmpty) None else Some((title, groupRows, candidates, moves))
    }
    if (planned.isEmpty) println("no split films found — nothing to do.")

    var deleted = 0
    planned.foreach { case (title, groupRows, candidates, moves) =>
      println()
      println(s"== '$title' — ${candidates.size} rows: " +
        candidates.map(c => s"${c.key}(tmdb=${c.tmdbId.getOrElse("—")}, ${c.ownRuntime.getOrElse("—")}min, ${c.slots.size} slots)").mkString(", "))
      moves.foreach(m => println(s"   ${m.slot.displayName}: -> ${m.winner}   (drop from ${m.losers.mkString(", ")})"))

      val byKey = groupRows.map(r => StoredMovieRecord.idOf(r, normalizer) -> r).toMap
      // Drop each loser's copy of the venue; the winner already holds its own.
      moves.flatMap(m => m.losers.map(_ -> m.slot)).groupBy(_._1).toSeq.sortBy(_._1).foreach { case (loserKey, pairs) =>
        val slots = pairs.map(_._2).toSet
        byKey.get(loserKey).foreach { loser =>
          val kept       = loser.record.data -- slots
          val noCinemas  = kept.keys.forall(source => models.Source.cinemaOf(source).isEmpty)
          if (noCinemas && deleted < max) {
            println(s"   ROW EMPTY -> delete $loserKey")
            if (apply) { repository.delete(loser.title, loser.year); deleted += 1 }
          } else if (noCinemas) {
            println(s"   ROW EMPTY -> $loserKey NOT deleted (--max $max reached)")
          } else if (apply) {
            repository.updateIfPresent(loser.title, loser.year, loser.record, loser.record.copy(data = kept))
          }
        }
      }
    }
    println()
    println(if (apply) s"done — $deleted row(s) deleted." else "dry run — nothing written. Re-run with --apply.")
    repository.close()
    client.close()
  }
}
