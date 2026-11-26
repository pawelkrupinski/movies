package services.metrics.worker

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.movies.{CaffeineMovieCache, CorpusMirror, InMemoryMovieRepository, SingleCountryNormalizer}
import services.tasks.{InMemoryTaskQueue, WorkerHeartbeat}
import tools.{RecordingScheduler, SpecThreadPools}

/** The loops that ran on the scheduler's fixed RATE before they rode `PeriodicLoop` still do: each arms exactly one fixed-rate
 *  task with its old initial delay and period, and schedules nothing one run at a time. (The sweeper's two are pinned in
 *  `ReadModelProjectorSpec`.) */
class PeriodicLoopSitesSpec extends AnyFlatSpec with Matchers {

  "SampledCensus" should "run at a fixed rate, first after its first-sample delay" in {
    val scheduler = new RecordingScheduler
    new SampledCensus {
      protected def censusName: String = "pinned-census"
      protected def threadPools: tools.ThreadPools = SpecThreadPools.scheduling(scheduler)
      protected def sampleInterval: FiniteDuration = 30.seconds
      def sample(): Unit = ()
    }.start()
    scheduler.fixedRates.toList shouldBe List(30000L -> 30000L)
    scheduler.oneShots shouldBe empty
  }

  "WorkerHeartbeat" should "beat at a fixed rate, first after one interval" in {
    val scheduler = new RecordingScheduler
    new WorkerHeartbeat(new InMemoryTaskQueue, intervalSeconds = 60, now = () => 0L, threadPools = SpecThreadPools.scheduling(scheduler)).start()
    scheduler.fixedRates.toList shouldBe List(60000L -> 60000L)
    scheduler.oneShots shouldBe empty
  }

  "CorpusCensus" should "publish at a fixed rate, first after one interval" in {
    val scheduler = new RecordingScheduler
    val cache  = CorpusMetricsFixtures.cacheOver(CorpusMetricsFixtures.repositoryOf())
    val census = CorpusMetricsFixtures.censusOver(cache, new PrometheusRegistry, threadPools = SpecThreadPools.scheduling(scheduler))
    census.start()
    scheduler.fixedRates.toList shouldBe List(300000L -> 300000L)
  }

  "CorpusMirror" should "retry the cold read and rehydrate at fixed rates, each first after one interval" in {
    val scheduler = new RecordingScheduler
    val mirror    = new CorpusMirror(threadPools = SpecThreadPools.scheduling(scheduler), sleep = tools.Sleep.Blocking)
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = SingleCountryNormalizer.titleNormalizer),
      normalizer = SingleCountryNormalizer.titleNormalizer, clock = tools.SpecClock.Pinned, mirror = mirror)
    cache.start()
    scheduler.fixedRates.toList shouldBe List(30000L -> 30000L, 21600000L -> 21600000L)
    scheduler.oneShots shouldBe empty
    cache.stop()
  }
}
