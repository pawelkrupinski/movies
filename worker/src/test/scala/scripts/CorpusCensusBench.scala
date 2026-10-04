package scripts

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.metrics.{CorpusCensus, ReferenceCensus, WorkerCorpusMetrics, WorkerCorpusScan, WorkerShowtimesMetrics, WorkerSlotFanoutMetrics, WorkerSourceFilmsMetrics}
import services.movies.{CaffeineMovieCache, TitleNormalizer}
import services.{MongoConnection, MongoRequirement}
import settings.{MongoDatabaseName, MongoUri}

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._

/**
 * What the corpus census costs, against a country's LOCAL prod mirror (scripts/local-mirror,
 * `<db>_prod_mirror` on :28017): the census over a cache hydrated from the real corpus — its first
 * count of every film, then `ticks` publishes — beside one pass of the full scan it replaced, with
 * process CPU, allocation and GC for each. Read-only; the mirror is never written.
 *
 *   sbt "worker/Test/runMain scripts.CorpusCensusBench us 5"
 *
 * Add `-XX:StartFlightRecording=filename=…` to `.jvmopts` for a profile of the same work.
 */
object CorpusCensusBench {
  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.UnitedStates)
    val ticks   = args.lift(1).map(_.toInt).getOrElse(5)
    val conn    = new MongoConnection(Some(MongoUri("mongodb://127.0.0.1:28017/?directConnection=true")),
      MongoDatabaseName(s"${country.mongoDb}_prod_mirror"), MongoRequirement.Required)
    val normalizer = TitleNormalizer.forCountry(country)
    val clock      = java.time.Clock.systemUTC()
    // Wired as CorpusWiring wires prod: showtimes and slots in their own collections, stitched per page.
    val repo     = AmbientMovieRepository.over(conn.database, normalizer, clock)
    val os       = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
    val threads  = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    def gcMillis = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getCollectionTime).sum
    def measured(label: String)(work: => Unit): Unit = {
      val (cpu0, alloc0, gc0, t0) = (os.getProcessCpuTime, threads.getTotalThreadAllocatedBytes, gcMillis, System.nanoTime)
      work
      val (cpu1, alloc1, gc1, t1) = (os.getProcessCpuTime, threads.getTotalThreadAllocatedBytes, gcMillis, System.nanoTime)
      println(f"$label: wall ${(t1 - t0) / 1e9}%.2fs, cpu ${(cpu1 - cpu0) / 1e9}%.2f core-s, alloc ${(alloc1 - alloc0) / 1e9}%.3f GB, gc ${(gc1 - gc0) / 1e3}%.1fs")
    }

    val cache    = new CaffeineMovieCache(repo, normalizer = normalizer, clock = clock)
    val registry = new PrometheusRegistry()
    val census   = new CorpusCensus(cache, WorkerCorpusMetrics.gauge(registry), WorkerSourceFilmsMetrics.gauge(registry),
      WorkerShowtimesMetrics.gauge(registry), WorkerSlotFanoutMetrics.gauge(registry), country.code, country.cities, clock)
    measured("census, first count of every film")(census.seed())
    (1 to ticks).foreach(n => measured(s"census tick $n")(census.publish()))
    measured("reference scan, one pass")(WorkerCorpusScan.over(repo,
      ReferenceCensus.collectors(new PrometheusRegistry(), country.code, country.cities, clock, normalizer)))
    census.stop()
    conn.close()
    sys.exit(0)
  }
}
