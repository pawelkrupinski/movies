package services.movies

import models.{Cinema, CinemaShowing, MovieRecord, Showtime, SourceData}

import java.time.LocalDateTime
import org.mongodb.scala.ObservableFuture

/**
 * What the movie cache keeps anew when the change stream brings back the identity projection's own writes: a light
 * projection on worker-us writes ~30 wide films (~35k slots) a tick, each moved at a few venues, and the stream reads
 * each film back — its moved venues alone, or the whole film — after a 30 s–2 min debounce. Whatever the cache stores
 * from that read lives until the film's next write: minutes, so a young collection promotes it.
 *
 *   sbt "worker/Test/runMain services.movies.CacheEchoBench [films] [venues]"   (a Mongo on :28017)
 *
 * Per echo kind, the bytes the cache holds that the record before the echo did not (heap after a full GC with both
 * the old and the new record reachable, less with the old alone). Not a spec — a measuring tool.
 */
object CacheEchoBench {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val start      = LocalDateTime.of(2036, 10, 5, 12, 0)

  def main(args: Array[String]): Unit = {
    val films  = args.headOption.map(_.toInt).getOrElse(30)
    val venues = args.lift(1).map(_.toInt).getOrElse(1100)
    // Rostered venues: a read back names its venues by the roster.
    val cinemas: IndexedSeq[Cinema] = Cinema.byDisplayName.values.toIndexedSeq.sortBy(_.displayName).take(venues)
    def slot(film: Int, cinema: Cinema, shift: Int) =
      (CinemaShowing.keyFor(cinema, s"Film Number $film", normalizer): models.Source) -> SourceData(title = Some(s"Film Number $film"),
        rawTitle = Some(s"Film Number $film"), releaseYear = Some(2026), cast = Seq("Ann Lee", "Bo Chan"), director = Seq("Cy Dee"),
        filmUrl = Some(s"https://v/${cinema.pillName}/$film"), showtimes = (0 until 14).map(h => Showtime(start.plusHours((h * 11 + shift).toLong), None)))
    // Over Mongo — the local :28017 server, in a database of the bench's own, dropped after — a read back is decoded into
    // new objects, as production's is; the in-memory store hands back the objects it was given.
    val connection = new services.MongoConnection(Some(settings.MongoUri("mongodb://127.0.0.1:28017/?directConnection=true")),
      settings.MongoDatabaseName(s"kinowo_cache_echo_bench_${ProcessHandle.current.pid}"), services.MongoRequirement.Required)
    val db         = connection.database
    val repository = new MongoMovieRepository(db, java.time.Clock.systemUTC(), screenings = Some(new MongoScreeningsRepository(db)),
      slots = Some(new MongoSlotsRepository(db)), normalizer = normalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = normalizer, clock = _root_.tools.SpecClock.Pinned)
    val ids   = (0 until films).map(f => (FilmId(s"f$f"), CacheKey.stored(s"Film Number $f", s"film number $f|2026")))
    ids.zipWithIndex.foreach { case ((id, key), f) =>
      cache.writeProjected(id, key, MovieRecord(data = cinemas.map(slot(f, _, 0)).toMap)) shouldBe WriteOutcome.Written
    }
    def live(): Long = { (1 to 3).foreach { _ => System.gc(); Thread.sleep(100) }; val r = Runtime.getRuntime; r.totalMemory - r.freeMemory }
    def held(): Seq[MovieRecord] = ids.map { case (_, key) => cache.get(key).get }
    // One tick's writes: each film moved at 3 venues, patched as the projection patches it.
    def tick(shift: Int): Unit = ids.zipWithIndex.foreach { case ((id, key), f) =>
      val before = cache.get(key).get
      val moved  = (0 until 3).map(i => slot(f, cinemas((shift * 3 + i) % venues), shift))
      cache.patchProjected(id, key, before, before.copy(data = before.data ++ moved), Some(moved.map(_._1).toSet)) shouldBe WriteOutcome.Written
    }
    def measure(name: String)(echo: (FilmId, CacheKey) => Unit): Unit = {
      val before = held()
      val base   = live()
      ids.foreach(echo.tupled)
      val after  = live()
      val now    = held()
      val same   = now.zip(before).count { case (a, b) => a eq b }
      val slots  = now.zip(before).map { case (a, b) => a.data.count { case (s, sd) => !b.data.get(s).exists(_ eq sd) } }.sum
      println(f"$name: the cache keeps ${(after - base) / 1e6}%.2f MB anew (live ${base / 1e6}%.0f -> ${after / 1e6}%.0f MB) over $films films x " +
        f"$venues venues: $same of $films records the same object, $slots slots new")
      before.size shouldBe films
    }
    tick(1)
    measure("whole-film echo") { (id, _) => cache.applyUpsert(repository.findByIdChecked(id).answered.get, FilmWriteFence.Unfenced) }
    tick(2)
    measure("venue echo") { (id, _) =>
      val stored = repository.findByIdChecked(id).answered.get.record
      val moved  = (0 until 3).map(i => cinemas((2 * 3 + i) % venues)).toSet
      cache.applyVenueSlots(VenueSlots(id, moved.map(c => c -> stored.data.toSeq.collect { case (s @ CinemaShowing(`c`, _), sd) => s -> sd }).toMap),
        FilmWriteFence.Unfenced)
    }
    db.foreach(d => scala.concurrent.Await.result(d.drop().toFuture(), scala.concurrent.duration.Duration(30, "s")))
    connection.close()
    sys.exit(0)
  }

  extension [A](a: A) private infix def shouldBe(b: A): Unit = require(a == b, s"$a != $b")
}
