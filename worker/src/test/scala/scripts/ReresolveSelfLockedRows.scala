package scripts

import services.MongoConnection
import tools.Env
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, StoredMovieRecord}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.tasks.MongoTaskQueue

/**
 * One-off: unlock the PL rows that are keyed to a film they are not, and let today's
 * resolver decide again.
 *
 * Each row below matched a WORD OUT OF ITS BANNER under an older, looser matcher, and
 * has been stuck there ever since: `MovieService.needsTmdbResolution` skips any row that
 * already holds a `tmdbId`, so nothing ever re-asks. The bad match also poisoned the
 * row's KEY — the id carries the wrong film's year (`…standup|2009` is *Up*'s year) —
 * which is why a plain re-scrape cannot shake it loose either.
 *
 * Confirmed by hand on 2026-08-29 against prod PL, each by looking the stored `tmdbId`
 * up on TMDB and reading its title back against what the cinemas actually list:
 *
 *   - a stand-up night matched Pixar's *Up*; a ladies'-programme banner matched the
 *     1976 film *Klaps* (the CINEMA's own brand); "…10. rocznica" matched *Rocznica*;
 *     "…by Noël Coward" matched *Tchórz / Les Braves (Coward)*; "Andre Rieu. Letni
 *     koncert" matched *Koncert* (1954); "Miłość i polityka" matched *Miłość* (2013);
 *     "BAJKI RETRO: Bolek i Lolek" matched a 1936 drama, not the cartoon; and four
 *     opera/ballet broadcasts matched the same-named FILM (Clouzot's *Manon* 1949,
 *     Visconti's *La caduta degli dei* 1969, *Silent Night* 2021, *Tosca* 1941).
 *
 * There is nothing to re-point them AT: TMDB has no entry for a Royal Ballet broadcast
 * or a stand-up night, so the right outcome for most of these is UNRESOLVED. That is not
 * a loss — an unresolved row still shows its screenings; a wrongly-resolved one shows
 * another film's poster, synopsis and ratings. Two of the twelve DO have a right answer
 * and should land on it now that `4fa760959` strips their banners: the Kino Kobiet row
 * (→ *Jak żyć, żeby nie zwariować*) and, via the seed anniversary strip, "Twoje imię".
 *
 * This is the same forced-resolve the `/debug` re-enrich button fires, applied to a
 * named list instead of one row — see [[ForceResolveEnqueue]] for why `force` is what
 * gets a self-locked row moving.
 *
 * Dry run by DEFAULT — pass `--apply` to enqueue.
 *
 *   ssh -N -L 27017:127.0.0.1:27017 root@2.28.56.140            # separate shell
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.ReresolveSelfLockedRows"
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.ReresolveSelfLockedRows --apply"
 */
object ReresolveSelfLockedRows {

  /** Row `_id` → the WRONG `tmdbId` it is locked to. The id is carried so a re-run
   *  cannot clobber a row that has since moved on; see [[stillLocked]]. */
  val lockedTo: Map[String, Int] = Map(
    "grzegorzdolniakmoglobycgorzejstandup|2009"              -> 14160,   // Pixar's Up
    "klapskinokobietjakzyczebyniezwariowac|1976"             -> 49100,   // Klaps (1976)
    "twojeimie10rocznica|2025"                               -> 1126336, // Rocznica / Anniversary
    "fallenangelsbynoelcoward|2026"                          -> 1437981, // Tchórz / Les Braves (Coward)
    "andrerieuletnikoncert|1954"                             -> 283866,  // Koncert (1954)
    "miloscipolityka|2013"                                   -> 598535,  // Miłość (2013)
    "bajkiretrobolekilolek|1936"                             -> 1167324, // Bolek i Lolek (1936 drama)
    "royalballetandoperasezonkinowy202627manon|1949"         -> 132332,  // Clouzot's Manon
    "royalballetandoperasezonkinowy202627zmierzchbogow|1969" -> 41876,   // La caduta degli dei
    "opera20262027silentnight|2021"                          -> 664574,  // Silent Night (2021)
    "opera20262027toscaretransmisja|1941"                    -> 444999,  // Tosca (1941)
    "opera20262027otello|1986"                               -> 198469   // Otello (1986)
  )

  /** The rows still locked to the id this script recorded.
   *
   *  A one-off script outlives the state it was written against: by the time it runs the
   *  worker may have re-keyed a row, an operator may have fixed it, or the film may have
   *  stopped screening. Forcing a re-resolve on a row that has already moved would strip
   *  a good row back to scraped data and re-run the search for nothing — and a forced
   *  reset drops the row's ratings until they are re-fetched. So act only where the
   *  evidence still holds: the row exists, still carries the exact wrong id, AND still
   *  has a title some cinema actually published.
   *
   *  That last condition is not hypothetical. `klapskinokobietjakzyczebyniezwariowac|1976`
   *  is STRANDED on prod — 0 screenings, and its one cinema slot is an empty Kino Sokolnia
   *  leftover carrying no title (so "has a slot" is NOT the test; "a cinema named it" is). `resetToScrapedData` would find nothing to reset it to,
   *  and the payload would carry the id-derived display title
   *  ("Klapskinokobietjakzyczebyniezwariowac") straight into a TMDB search. A row with no
   *  cinema left is `UnscreenedCleanup`'s to remove, not this script's to re-resolve. */
  /** Did any CINEMA actually publish a title for this row?
   *
   *  Not merely "has a slot": the stranded row below holds four (TMDB, IMDB, Filmweb and
   *  an empty cinema leftover) and every one of them is title-less, which is exactly why
   *  its display title falls back to the id. The payload this script enqueues carries
   *  that title, so a row no cinema names is a row this script must not touch. */
  private def publishedATitle(r: StoredMovieRecord): Boolean =
    r.record.cinemaSlots.exists { case (_, sd) => sd.title.exists(_.trim.nonEmpty) }

  def stillLocked(found: Seq[(String, Option[StoredMovieRecord])], expected: Map[String, Int])
      : (Seq[StoredMovieRecord], Seq[String]) = {
    val (locked, skipped) = found.partition { case (id, row) =>
      row.exists(r => expected.get(id).exists(wrong => r.record.tmdbId.contains(wrong)) &&
                      publishedATitle(r))
    }
    (locked.flatMap(_._2), skipped.map { case (id, row) =>
      val why = row.fold("row is gone") { r =>
        if (!r.record.tmdbId.contains(expected(id)))
          s"now tmdbId=${r.record.tmdbId.getOrElse("\u2014")}, expected ${expected(id)}"
        else "no cinema publishes a title for it \u2014 stranded, UnscreenedCleanup's to remove"
      }
      s"$id ($why)"
    })
  }

  def main(args: Array[String]): Unit = {
    val apply = args.contains("--apply")
    val conn  = MongoConnection.fromEnvForDb(Env.get("MONGODB_DB").getOrElse("kinowo"), required = true)
    val db = conn.database.getOrElse {
      println("Could not open the database — is the Mongo tunnel up and MONGODB_URI set?")
      sys.exit(1)
    }
    // WIRE THE SIDE COLLECTIONS. Under the read/write split a migrated film's `movies`
    // document carries no `sourceData` and no `title` at all — both live in `movie_slots`
    // — so an unwired read hands back a row whose `title` is derived from its `_id`
    // ("Grzegorzdolniakmoglobycgorzejstandup"). Enqueuing THAT as the resolve payload
    // would key the reset on a title no cinema ever published and search TMDB for a
    // concatenated string. Same trap as the unstitched read that wiped live screenings on
    // 2026-08-10; the fix is the same one MergeKinotekaPoisonedRows already applies.
    val repo: MovieRepository = new MongoMovieRepository(
      sharedDb = Some(db), fallbackToOwnInit = false,
      screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots      = Some(new MongoSlotsRepository(Some(db))),
      normalizer = titleNormalizer)

    val found = lockedTo.keys.toSeq.sorted.map(id => id -> repo.findById(id))
    val (locked, skipped) = stillLocked(found, lockedTo)

    println(s"${locked.size} of ${lockedTo.size} row(s) still locked to the wrong film:")
    locked.foreach(r => println(f"  ${r.record.tmdbId.getOrElse(0)}%-9s '${r.title}' (${r.year.getOrElse("?")})"))
    if (skipped.nonEmpty) println(s"skipping ${skipped.size}:\n${skipped.map("  " + _).mkString("\n")}")

    if (!apply) println("dry run — nothing enqueued. Re-run with --apply.")
    else {
      val counts = ForceResolveEnqueue.all(new MongoTaskQueue(Some(db)), locked)
      println(s"done: ${counts.describe}. The worker will drain them and re-resolve off scraped data.")
    }
    conn.close()
    sys.exit(0)
  }
}
