package scripts

import models.{Country, MovieRecord, ResolvedMovie, SourceData}
import services.MongoConnection
import services.movies.{MongoMovieRepository, MongoSlotsRepository, StoredMovieRecord, TitleNormalizer}
import services.readmodel.MongoReadModelRepository
import tools.PersonName

/**
 * One-shot data repair: bring EXISTING cast lists up to what the write boundary
 * now produces.
 *
 * WHY: cast names arrive from a dozen sources at once, and one of them —
 * [[services.cinemas.common.FlicksClient]], the UK/US listings aggregator —
 * lifts `content_cast` out of a session button's `data-eventjson` blob, which
 * Flicks emits ENTIRELY LOWERCASE ("christoph waltz, jeff bridges"). Those names
 * were stored and rendered verbatim while every other source (TMDB, IMDb, the
 * Polish scrapers, the UK chain clients) supplied properly-cased ones, so the
 * corpus is MIXED. [[tools.PersonName]] now capitalises them at both the Flicks
 * parse boundary and `MovieCache.buildCinemaSlot`; this script applies the same
 * function to what is already persisted.
 *
 * Because the repair runs over a mostly-correct corpus, EVERY write is
 * conditional: `PersonName.capitalized` returns an already-cased name
 * byte-identical, and a row is only written when at least one of its names
 * actually changes.
 *
 * ==What it rewrites==
 *
 * Cast lives in three places per country database, and all three are visited:
 *
 *  - `movie_slots.slot.cast` — the live per-cinema slots (the read model's
 *    `MovieRecord.cast` is the longest of these), written through
 *    [[services.movies.SlotsRepository.upsertSlot]].
 *  - `movies.sourceData.<slot>.cast` — the LEGACY embedded copy, still present
 *    on films whose slots have not been retired by `scripts.RetireEmbeddedSlots`.
 *    Written through `updateIfPresent`, whose field-level patch touches nothing
 *    else.
 *  - `web_movies.cast` — the read model the site actually serves, written
 *    through [[services.readmodel.ReadModelWriter.upsertMovie]] so the change is
 *    visible without waiting for a re-projection.
 *
 * `screenings` carries no cast, and `cinema_scrapes` (the raw scrape archive) is
 * deliberately left alone: it is a record of what a site SAID, not something the
 * site serves.
 *
 * Only `cast` is touched. `director` shares the same casing rule going forward
 * (both go through `MovieCache.displayNames`), but no source has been observed
 * shipping a lowercase director, so there is nothing to repair.
 *
 * ==Running it==
 *
 * Each country is its OWN database on the shared cluster, taken from
 * `Country.mongoDb` and NOT from `Country.dbNameFor` — the latter lets
 * `MONGODB_DB` win, and `.env.local` pins that to prod `kinowo`, which would
 * silently point every country's pass at Poland.
 *
 * DRY RUN BY DEFAULT: it reports per-country counts and writes nothing. Pass
 * `--apply` to write. Optional country codes narrow the run; with none it
 * visits `Country.all`.
 *
 * {{{
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   sbt "worker/Test/runMain scripts.CastNameCaseBackfill"                # dry run, all countries
 *   sbt "worker/Test/runMain scripts.CastNameCaseBackfill uk"             # dry run, UK only
 *   sbt "worker/Test/runMain scripts.CastNameCaseBackfill --apply"        # WRITE, all countries
 *   sbt "worker/Test/runMain scripts.CastNameCaseBackfill --apply uk de"  # WRITE, UK + Germany
 * }}}
 *
 * The work is one Mongo round-trip per CHANGED row against a scan that is
 * otherwise pure reads, so it runs serially — there is no external service to
 * pipeline against and no rate limit to respect. Throughput is printed per
 * country and for the whole run.
 */
object CastNameCaseBackfill {

  /** The per-country tallies the dry run reports and the apply run confirms. */
  case class Counts(
    slotRows:      Int = 0,   // `movie_slots` rows visited
    slotRewrites:  Int = 0,   // …of which carried a name that changed
    movieRows:     Int = 0,   // `movies` rows visited
    movieRewrites: Int = 0,   // …of which carried a legacy embedded name that changed
    movieMisses:   Int = 0,   // …of which the conditional write could not match (non-canonical `_id`)
    webRows:       Int = 0,   // `web_movies` rows visited
    webRewrites:   Int = 0,   // …of which carried a name that changed
    names:         Int = 0    // individual names recased, across all three
  ) {
    def +(o: Counts): Counts = Counts(
      slotRows + o.slotRows, slotRewrites + o.slotRewrites,
      movieRows + o.movieRows, movieRewrites + o.movieRewrites, movieMisses + o.movieMisses,
      webRows + o.webRows, webRewrites + o.webRewrites, names + o.names)

    def rows: Int = slotRows + movieRows + webRows

    def describe: String =
      s"movie_slots $slotRewrites/$slotRows · movies(embedded) $movieRewrites/$movieRows" +
        (if (movieMisses > 0) s" ($movieMisses unmatched)" else "") +
        s" · web_movies $webRewrites/$webRows · $names name(s) recased"
  }

  /** The slot with its cast recased, or `None` when nothing changed. The gate on
   *  every `movie_slots` / `movies` write. */
  def recased(slot: SourceData): Option[SourceData] = {
    val cast = PersonName.capitalizedAll(slot.cast)
    Option.when(cast != slot.cast)(slot.copy(cast = cast))
  }

  /** The read-model document with its cast recased, or `None` when nothing
   *  changed. The gate on every `web_movies` write. */
  def recased(movie: ResolvedMovie): Option[ResolvedMovie] = {
    val cast = PersonName.capitalizedAll(movie.cast)
    Option.when(cast != movie.cast)(movie.copy(cast = cast))
  }

  /** The record with every source's cast recased, or `None` when no source
   *  changed — so an untouched film costs no write at all. */
  def recased(record: MovieRecord): Option[MovieRecord] = {
    val data = record.data.map { case (source, slot) => source -> recased(slot).getOrElse(slot) }
    Option.when(data != record.data)(record.copy(data = data))
  }

  /** How many individual names differ between two equal-length lists. */
  private def changedNames(before: Seq[String], after: Seq[String]): Int =
    before.zip(after).count { case (b, a) => b != a }

  def main(args: Array[String]): Unit = {
    val apply     = args.contains("--apply")
    val requested = args.filterNot(_.startsWith("--")).toSeq
    val countries =
      if (requested.isEmpty) Country.all
      else requested.flatMap(code => Country.byCode(code).orElse {
        println(s"Unknown country code '$code' — expected one of ${Country.all.map(_.code).mkString(", ")}.")
        sys.exit(1)
      })

    println(if (apply) "APPLY — rows whose cast casing changes will be WRITTEN."
            else "DRY RUN — nothing is written. Pass --apply to write.")
    println(s"Countries: ${countries.map(c => s"${c.displayName} (${c.mongoDb})").mkString(", ")}\n")

    val startedAtMs = System.currentTimeMillis()
    val total = countries.foldLeft(Counts()) { (accumulated, country) =>
      accumulated + backfill(country, apply)
    }

    val seconds = (System.currentTimeMillis() - startedAtMs) / 1000.0
    println(f"\nTOTAL  ${total.describe}")
    println(f"Done in $seconds%.1fs · ${total.rows} row(s) scanned · ${total.rows / math.max(seconds, 0.001)}%.0f rows/s")
    sys.exit(0)
  }

  private def backfill(country: Country, apply: Boolean): Counts = {
    // `country.mongoDb`, never `Country.dbNameFor` — see the class comment.
    val connection = MongoConnection.fromEnvForDb(country.mongoDb, required = true)
    val database = connection.database.getOrElse {
      println(s"${country.displayName}: could not open ${country.mongoDb} — is the Mongo tunnel up " +
        "(scripts/local-mirror/prod-tunnel.sh) and MONGODB_URI set?")
      sys.exit(1)
    }
    val startedAtMs = System.currentTimeMillis()
    val normalizer  = TitleNormalizer.forCountry(country)

    val counts =
      backfillSlots(new MongoSlotsRepository(Some(database)), apply) +
        backfillEmbedded(new MongoMovieRepository(
          sharedDb = Some(database), fallbackToOwnInit = false, normalizer = normalizer), apply) +
        backfillReadModel(new MongoReadModelRepository(Some(database)), apply)

    val seconds = (System.currentTimeMillis() - startedAtMs) / 1000.0
    println(f"${country.displayName}%-15s ${counts.describe} · $seconds%.1fs " +
      f"(${counts.rows / math.max(seconds, 0.001)}%.0f rows/s)")
    connection.close()
    counts
  }

  /** `movie_slots.slot.cast` — the live per-cinema slots. */
  private def backfillSlots(slots: MongoSlotsRepository, apply: Boolean): Counts = {
    val (byFilm, complete) = slots.findAllChecked()
    if (!complete)
      println("  ! movie_slots scan did not complete — the counts below are a PARTIAL view of that collection.")
    val counts = byFilm.foldLeft(Counts()) { case (accumulated, (filmId, filmSlots)) =>
      filmSlots.foldLeft(accumulated) { case (soFar, (slotKey, slot)) =>
        recased(slot) match {
          case None => soFar.copy(slotRows = soFar.slotRows + 1)
          case Some(fixed) =>
            if (apply) slots.upsertSlot(filmId, slotKey, fixed)
            soFar.copy(slotRows = soFar.slotRows + 1, slotRewrites = soFar.slotRewrites + 1,
              names = soFar.names + changedNames(slot.cast, fixed.cast))
        }
      }
    }
    slots.close()
    counts
  }

  /** `movies.sourceData.<slot>.cast` — the legacy embedded copy on films whose
   *  slots have not been retired yet.
   *
   *  The repository is deliberately built WITHOUT `slots`/`screenings`: this pass
   *  targets the embedded map specifically, and a wired `slots` makes
   *  `updateIfPresent` drop `patch.data` on purpose (so a patch can't resurrect
   *  the map the split exists to remove). The scan is likewise the UNSTITCHED
   *  `foreachRecordWithoutShowtimes`, so `record.data` is exactly what the
   *  `movies` document itself carries — a film already retired to `movie_slots`
   *  reads as having no sources here and costs nothing. */
  private def backfillEmbedded(movies: MongoMovieRepository, apply: Boolean): Counts = {
    var counts = Counts()
    val complete = movies.foreachRecordWithoutShowtimes { (row: StoredMovieRecord) =>
      recased(row.record) match {
        case None => counts = counts.copy(movieRows = counts.movieRows + 1)
        case Some(fixed) =>
          val changed = row.record.data.foldLeft(0) { case (n, (source, slot)) =>
            n + changedNames(slot.cast, fixed.data.get(source).map(_.cast).getOrElse(slot.cast))
          }
          val landed = !apply || movies.updateIfPresent(row.title, row.year, row.record, fixed)
          counts = counts.copy(
            movieRows     = counts.movieRows + 1,
            movieRewrites = counts.movieRewrites + 1,
            movieMisses   = counts.movieMisses + (if (landed) 0 else 1),
            names         = counts.names + changed)
      }
    }
    if (!complete)
      println("  ! movies scan did not complete — the counts above are a PARTIAL view of that collection.")
    movies.close()
    counts
  }

  /** `web_movies.cast` — the read model the site serves. */
  private def backfillReadModel(readModel: MongoReadModelRepository, apply: Boolean): Counts = {
    val counts = readModel.findAllMovies().foldLeft(Counts()) { (accumulated, movie) =>
      recased(movie) match {
        case None => accumulated.copy(webRows = accumulated.webRows + 1)
        case Some(fixed) =>
          if (apply) readModel.upsertMovie(fixed)
          accumulated.copy(webRows = accumulated.webRows + 1, webRewrites = accumulated.webRewrites + 1,
            names = accumulated.names + changedNames(movie.cast, fixed.cast))
      }
    }
    readModel.close()
    counts
  }
}
