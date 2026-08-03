package clients.tools

import models.{MovieRecord, Source}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, ObservableFuture}
import services.movies.{MovieCodecs, StoredMovieDto, StoredMovieRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.given

/**
 * Localises the `detail-reaper` thread's CPU against a REAL corpus.
 *
 * Measured on `kinowo-worker-uk` 2026-07-29: that one thread burned 7.93cc of the
 * worker's 12.46cc total — 64% — while `DetailReaper` produced only 0.30
 * `EnrichDetails`/min. Per-thread `/proc` sampling at 2s showed the shape: bursts
 * of ~5 CPU-SECONDS every ~65s (the 1min self-rescheduling tick plus its own
 * duration), running at 50-98cc during the burst, with 5-28 voluntary context
 * switches per 2s — CPU-bound compute, not a spin and not I/O.
 *
 * ~5s over a ~1,600-row corpus is ~3ms PER ROW, which is wildly too much for the
 * in-memory walk `DetailReaper.tick` is supposed to be. Reading the code did not
 * settle it — `cinemaSlots` is an O(n) filter, `cinemaData` an O(n log n) sort,
 * `Source.priority` is a `val` Map and `cinemaOf` a cheap match, and
 * `enqueueIfDue` short-circuits on the in-memory `DueWindow` before touching
 * Mongo. So measure it instead of arguing about it.
 *
 * This times each candidate over the real rows and prints the slot-count
 * distribution, since a mean hides the wide-release films that carry a slot per
 * venue across an 843-venue roster.
 *
 * Run (UK corpus, over the prod tunnel):
 *   flyctl proxy 27017 -a kinowo-mongo &
 *   MONGODB_DB=kinowo_uk sbt 'worker/Test/runMain clients.tools.ProfileDetailReaper'
 *
 * `def main`, not `extends App` — an `App` body runs inside `<clinit>`, so a
 * connect timeout surfaces as `ExceptionInInitializerError` with the real cause
 * buried (and fires a fatal Sentry event through Play's logback appender).
 */
object ProfileDetailReaper {

  private def ms(nanos: Long): String = f"${nanos / 1e6}%9.2f ms"

  private def time[A](label: String, reps: Int)(body: => A): A = {
    val t0 = System.nanoTime()
    var i = 0
    var last: A = null.asInstanceOf[A]
    while (i < reps) { last = body; i += 1 }
    val elapsed = System.nanoTime() - t0
    println(f"  $label%-52s ${ms(elapsed / reps)} / pass")
    last
  }

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      System.err.println("MONGODB_URI not set — abort."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    println(s"\nProfileDetailReaper → $dbName\n")

    val client = MongoClient(uri)
    try {
      val db: MongoDatabase = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
      val movies: MongoCollection[StoredMovieDto] = db.getCollection[StoredMovieDto]("movies")
      val raw: Seq[StoredMovieRecord] =
        Await.result(movies.find().toFuture(), 120.seconds).map(StoredMovieDto.toDomain)
      println(s"  raw `movies` decode: ${raw.size} records, ${raw.map(_.record.cinemaSlots.size).sum} cinema slots")

      // The cache holds STITCHED rows — slots unioned from the `movie_slots` side
      // collection — so the raw decode above is NOT the shape DetailReaper walks.
      // Go through the repository, which is what MovieCache hydrates from.
      val repository = new services.movies.MongoMovieRepository()
      val rows: Seq[StoredMovieRecord] = repository.findAll()
      val records: Seq[MovieRecord] = rows.map(_.record)
      println(s"  STITCHED repository.findAll(): ${records.size} records\n")

      // Slot-count distribution. The reaper walks every row every tick, so the TOTAL
      // slot count is what the per-row work integrates over — and the tail matters
      // more than the mean when one blockbuster carries a slot per venue.
      val slotCounts = records.map(_.cinemaSlots.size).sorted
      val total      = slotCounts.sum
      def pct(p: Int) = slotCounts(math.min(slotCounts.size - 1, slotCounts.size * p / 100))
      println(f"  cinema slots: total=$total  mean=${total.toDouble / records.size}%.1f  " +
              f"p50=${pct(50)}  p90=${pct(90)}  p99=${pct(99)}  max=${slotCounts.last}")
      println(f"  data-map entries: total=${records.map(_.data.size).sum}\n")

      // The candidates, each over the WHOLE corpus — one "pass" == one reaper tick.
      println("  --- per full-corpus pass (one tick's worth) ---")
      time("records.map(_.cinemaSlots)", 5) { records.foreach(r => r.cinemaSlots); () }
      time("records.map(_.cinemaData)", 5) { records.foreach(r => r.cinemaData); () }
      time("records.map(_.cinemaShowings)", 5) { records.foreach(r => r.cinemaShowings); () }
      time("records.map(_.detailPending)  [reapStuckPending]", 5) { records.foreach(r => r.detailPending); () }

      // What the reaper actually does per row for its ONE UK enricher: resolve the
      // representative slot for a single cinema and read its filmUrl.
      val someCinema = records.iterator.flatMap(_.cinemaSlots).flatMap { case (s, _) => Source.cinemaOf(s) }
        .toSeq.headOption
      someCinema.foreach { cinema =>
        println(s"\n  --- nativeDetailRef-equivalent for ONE cinema ($cinema) ---")
        time("records.map(_.cinemaData.get(cinema).flatMap(_.filmUrl))", 5) {
          records.foreach(r => r.cinemaData.get(cinema).flatMap(_.filmUrl)); ()
        }
      }

      // THE REAL LOOP. `detailEnrichers` collects INSTANCES — one CineworldClient per
      // venue — so the inner loop runs once per venue per row, and `nativeDetailRef`
      // recomputes `cinemaData` (a sort + Map build) every single time.
      val catalog   = new services.cinemas.CinemaScraperCatalog(new tools.RealHttpFetch(), java.time.LocalDate.now())
      val enrichers = catalog.all.collect { case de: services.cinemas.common.DetailEnricher => de }
      println(s"\n  --- the ACTUAL nested loop: ${records.size} records x ${enrichers.size} enricher INSTANCES ---")
      time(s"tick() inner loop as written (recompute per enricher)", 3) {
        records.foreach(r => enrichers.foreach(e => e.nativeDetailRef(r))); ()
      }
      time(s"tick() inner loop with cinemaData HOISTED per row", 3) {
        records.foreach { r =>
          val cd = r.cinemaData
          enrichers.foreach(e => cd.get(e.cinema).flatMap(_.filmUrl))
        }; ()
      }
      // What the reaper does now: index enrichers by venue and drive off the row's own
      // slots, so a row asks about the venues that carry it rather than all 185.
      val byCinema = enrichers.groupBy(_.cinema)
      time(s"tick() inner loop INVERTED (row's own venues only)", 3) {
        records.foreach { r =>
          val cd = r.cinemaData
          cd.keysIterator.foreach(v => byCinema.getOrElse(v, Nil).foreach(e => e.nativeDetailRefIn(cd)))
        }; ()
      }
      println()
    } catch {
      case e: Throwable => System.err.println(s"FAILED: ${e.getClass.getSimpleName}: ${e.getMessage}")
    } finally { client.close(); sys.exit(0) }
  }
}
