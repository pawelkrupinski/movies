package clients.tools

import com.github.benmanes.caffeine.cache.Caffeine
import models.MovieRecord
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.movies.{MovieCodecs, StoredMovieDto, StoredMovieRecord}
import tools.Env

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._

/**
 *  Measures every phase of the boot hydrate path against the live Mongo
 *  cluster so we can see exactly where the seconds go. Reports each phase
 *  in ms; the sum equals what production sees when `Wiring.start()` runs.
 *
 *  Phases:
 *
 *    1. MongoClient construction — driver init, no I/O.
 *    2. First Mongo round-trip — `countDocuments` on `movies`. This is the
 *       startup probe `MongoConnection.init` does. Captures DNS + TLS +
 *       replica-set handshake + one round-trip.
 *    3. `find().toFuture()` wall time — full cursor drain of `movies`
 *       collection, including driver-side BSON decoding into the typed DTO.
 *    4. Per-document wall time — same query via the streaming Observable,
 *       so we get "time from cursor open until N-th document arrives". The
 *       gap between successive docs tells us if it's network-bound (steady)
 *       or batched (stepped at 101/201/…). Reported as first / median /
 *       last document arrival time relative to cursor open.
 *    5. `StoredMovieDto.toDomain` mapping — pure-Scala fold over the
 *       decoded DTOs. Builds `Map[Source, SourceData]` per row.
 *    6. Caffeine populate — `put` every row into a fresh cache.
 *    7. Total wall time end-to-end.
 *
 *  Run:
 *    set -a; . ./.env.local; set +a
 *    sbt 'Test/runMain clients.tools.MeasureStartup'
 */
// `def main` rather than `extends App` deliberately — `App` runs the whole
// body inside the class's `<clinit>`, so a Mongo connect timeout surfaces
// as `ExceptionInInitializerError` (real cause buried) AND fires a
// `[fatal]` event into Sentry via the logback appender Play wires up.
// Running in a real `main` keeps the failure a normal `TimeoutException`
// and the try/catch below exits the JVM cleanly instead of throwing
// across the JVM boundary.
object MeasureStartup {

  private def ms(nanos: Long): String = f"${nanos / 1e6}%9.2f ms"
  private def fmt(label: String, nanos: Long): Unit =
    println(f"  ${label}%-50s ${ms(nanos)}")

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      System.err.println("MONGODB_URI not set — abort.")
      sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

    println(s"\nMeasureStartup → $dbName\n")

    val t0     = System.nanoTime()
    val client = MongoClient(uri)
    try {
      val db: MongoDatabase = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
      val t1 = System.nanoTime()
      fmt("1. MongoClient construction (no I/O)", t1 - t0)

      // Phase 2: First round-trip — countDocuments.
      val movies: MongoCollection[StoredMovieDto] = db.getCollection[StoredMovieDto]("movies")
      val count = Await.result(movies.countDocuments().toFuture(), 30.seconds)
      val t2 = System.nanoTime()
      fmt(s"2. countDocuments() — first RTT, $count docs", t2 - t1)

      // Phase 3: find().toFuture() wall time.
      val rows: Seq[StoredMovieDto] = Await.result(movies.find().toFuture(), 60.seconds)
      val t3 = System.nanoTime()
      fmt(s"3. find().toFuture() — full cursor drain (${rows.size} docs)", t3 - t2)

      // Phase 4: Per-document timing via streaming Observable.
      val cursorOpen = System.nanoTime()
      val timings    = collection.mutable.ArrayBuffer.empty[Long]
      val streamDone = Promise[Unit]()
      movies.find().subscribe(
        (_: StoredMovieDto) => timings += (System.nanoTime() - cursorOpen),
        (ex: Throwable)     => { System.err.println(s"   stream error: ${ex.getMessage}"); streamDone.tryFailure(ex) },
        ()                  => streamDone.trySuccess(())
      )
      Await.result(streamDone.future, 60.seconds)
      val t4     = System.nanoTime()
      val first  = timings.head
      val median = timings.sorted.apply(timings.size / 2)
      val last   = timings.last
      fmt(s"4. streamed find() — first doc arrives at",   first)
      fmt(s"   streamed find() — median doc arrives at",  median)
      fmt(s"   streamed find() — last doc arrives at",    last)
      fmt(s"   streamed find() — total wall time",        t4 - t3)

      // Phase 5: StoredMovieDto → StoredMovieRecord conversion.
      val converted: Seq[StoredMovieRecord] = rows.map(StoredMovieDto.toDomain)
      val t5 = System.nanoTime()
      fmt(s"5. StoredMovieDto.toDomain × ${rows.size}", t5 - t4)

      // Phase 6: Caffeine populate. String key matches Caffeine's HashMap-
      // based put cost; the real CacheKey is a private case class in
      // services.movies and isn't worth exposing just for the probe.
      // Allocations dominate either way.
      val cache = Caffeine.newBuilder().build[String, MovieRecord]()
      converted.foreach(r => cache.put(r.title + " " + r.year.getOrElse(""), r.record))
      val t6 = System.nanoTime()
      fmt(s"6. Caffeine populate × ${converted.size}", t6 - t5)

      println()
      fmt("TOTAL boot hydrate path", t6 - t0)

      println()
      println(f"   per-row find().toFuture():   ${(t3 - t2).toDouble / rows.size / 1e6}%6.3f ms")
      println(f"   per-row stream + decode:      ${(t4 - t3).toDouble / rows.size / 1e6}%6.3f ms")
      println(f"   per-row toDomain mapping:     ${(t5 - t4).toDouble / rows.size / 1e6}%6.3f ms")
      println(f"   per-row Caffeine populate:    ${(t6 - t5).toDouble / rows.size / 1e6}%6.3f ms")
    } catch {
      case ex: Throwable =>
        System.err.println(s"MeasureStartup aborted: ${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}")
        sys.exit(2)
    } finally {
      client.close()
    }
  }
}
