package scripts

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.metrics.{WorkerCorpusMetrics, WorkerCorpusScan, WorkerShowtimesMetrics, WorkerSlotFanoutMetrics, WorkerSourceFilmsMetrics}
import services.movies.TitleNormalizer
import services.{MongoConnection, MongoRequirement}
import settings.{MongoDatabaseName, MongoUri}

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters._

/**
 * What one corpus-census pass costs, against a country's LOCAL prod mirror (scripts/local-mirror,
 * `<db>_prod_mirror` on :28017): the production scan and collectors exactly as `MetricsWiring`
 * builds them, over the real corpus, with process CPU, allocation and GC per pass — the numbers a
 * census change must move. Read-only; the mirror is never written.
 *
 *   sbt "worker/Test/runMain scripts.CorpusCensusBench us 5"
 *
 * Add `-XX:StartFlightRecording=filename=…` to `.jvmopts` for a profile of the same passes.
 */
object CorpusCensusBench {
  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(Country.byCode).getOrElse(Country.UnitedStates)
    val passes  = args.lift(1).map(_.toInt).getOrElse(5)
    val conn    = new MongoConnection(Some(MongoUri("mongodb://127.0.0.1:28017/?directConnection=true")),
      MongoDatabaseName(s"${country.mongoDb}_prod_mirror"), MongoRequirement.Required)
    val normalizer = TitleNormalizer.forCountry(country)
    // Wired as CorpusWiring wires prod: showtimes and slots in their own collections, stitched per page.
    val repo       = AmbientMovieRepository.over(conn.database, normalizer)
    val registry   = new PrometheusRegistry()
    val scan = new WorkerCorpusScan(repo, Seq(
      new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), country.code),
      new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), country.code, cities = country.cities, normalizer = normalizer),
      new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), country.code, cities = country.cities, normalizer = normalizer),
      new WorkerSlotFanoutMetrics(WorkerSlotFanoutMetrics.gauge(registry), country.code)))
    val os      = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
    val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    def gcMillis = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getCollectionTime).sum
    (1 to passes).foreach { n =>
      val (cpu0, alloc0, gc0, t0) = (os.getProcessCpuTime, threads.getTotalThreadAllocatedBytes, gcMillis, System.nanoTime)
      val pass = scan.sample()
      val (cpu1, alloc1, gc1, t1) = (os.getProcessCpuTime, threads.getTotalThreadAllocatedBytes, gcMillis, System.nanoTime)
      println(f"pass $n: wall ${(t1 - t0) / 1e9}%.1fs, cpu ${(cpu1 - cpu0) / 1e9}%.1f core-s, alloc ${(alloc1 - alloc0) / 1e9}%.2f GB, gc ${(gc1 - gc0) / 1e3}%.1fs — " +
        pass.byCollector.map { case (name, time) => s"$name ${time.toMillis}ms" }.mkString(", "))
    }
    conn.close()
    sys.exit(0)
  }
}
