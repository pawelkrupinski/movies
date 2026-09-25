package services.contracts

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, ResolveDispatcher, ResolveDuplicateMetrics}
import services.staging.InMemoryStagingRepository
import services.tasks.{EnrichTaskKeys, MongoTaskQueue, ResolveMode, TaskQueue, TaskType}
import tools.contracts.Implementations
import tools.{Env, IsolatedMongoDatabase}

import java.lang.reflect.{ParameterizedType, Type}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * ONE behaviour suite for [[ResolveDispatcher]], run against every implementation found on
 * the class path: production's queue dispatcher (over the real Mongo queue) and the inline
 * dispatcher the fixture harness and every unit spec run.
 *
 * THE DRIFT THIS PINS. A re-try (RetryMiss/Force) that finds its film's resolve still
 * WAITING must raise that resolve's mode — the queue dispatcher amends the waiting task. The
 * inline dispatcher dropped it (fixed in 141b08d6a), so the harness ran a plain resolve where
 * production ran the re-try, and nothing compared the two.
 *
 * Each dispatcher is built by reflection from what it asks for: an executor that holds its
 * work until the test lets it run, a resolve callback that records the mode it ran with, a
 * queue the test claims from. "Waiting" and "running" therefore mean the same thing to both:
 * not yet started, and started with a mode that can no longer change.
 */
class ResolveDispatcherContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "resolve-dispatcher-contract")

  private lazy val database = isolatedDatabase.database
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private val collections = new AtomicInteger

  /** One dispatcher and everything it was built with. */
  private final class Harness(cls: Class[? <: ResolveDispatcher]) {
    val ran        = new ConcurrentLinkedQueue[ResolveMode]()
    val duplicates = new ConcurrentLinkedQueue[(ResolveMode, Boolean)]()
    private val gate    = new CountDownLatch(1)
    private val hold    = new CountDownLatch(1)
    private val entered = new CountDownLatch(1)
    private var executor: Option[java.util.concurrent.ExecutorService] = None
    private var queue:    Option[TaskQueue] = None

    private def raw(t: Type): String = t match {
      case p: ParameterizedType => p.getRawType.getTypeName
      case other                => other.getTypeName
    }

    val dispatcher: ResolveDispatcher = Implementations.construct(cls, t => raw(t) match {
      case "scala.concurrent.ExecutionContextExecutorService" =>
        val pool = Executors.newSingleThreadExecutor()
        pool.execute(() => gate.await())   // everything submitted after this WAITS until `start`
        executor = Some(pool)
        Some(ExecutionContext.fromExecutorService(pool))
      case "scala.Function2" =>
        Some((title: String, year: Option[Int]) => CacheKey(title, year, services.movies.SingleCountryNormalizer.titleNormalizer))
      case "scala.Function5" =>
        Some((_: String, _: Option[Int], _: Option[String], _: Option[String], mode: ResolveMode) => {
          ran.add(mode); entered.countDown(); hold.await(); ()
        })
      case "services.tasks.TaskQueue" =>
        val q = new MongoTaskQueue(Some(database), s"tasks_${collections.incrementAndGet()}")
        queue = Some(q)
        Some(q)
      case "services.movies.ResolveDuplicateMetrics" =>
        Some(new ResolveDuplicateMetrics {
          def recordDuplicate(mode: ResolveMode, upgraded: Boolean): Unit = { duplicates.add(mode -> upgraded); () }
        })
      case _ => None
    }).fold(missing => fail(missing), identity)

    def dispatch(mode: ResolveMode): Unit = dispatcher.dispatch("Tosca", Some(2026), None, None, mode)

    /** The pending resolve STARTS, with whatever mode it has by now. */
    def start(): Unit = {
      executor.foreach { _ => gate.countDown(); entered.await(10, TimeUnit.SECONDS) shouldBe true }
      queue.foreach(claimOne)
    }

    /** Everything still pending runs to the end. */
    def finish(): Seq[ResolveMode] = {
      gate.countDown(); hold.countDown()
      dispatcher.drain()
      queue.foreach(q => while (claimOne(q)) ())
      executor.foreach(_.shutdownNow())
      ran.asScala.toSeq
    }

    private def claimOne(q: TaskQueue): Boolean =
      q.claim("worker", 1.minute, Instant.now()).exists { task =>
        task.taskType shouldBe TaskType.ResolveTmdb
        ran.add(EnrichTaskKeys.modeOf(task.payload)); true
      }
  }

  private val implementations =
    Implementations.of(classOf[ResolveDispatcher], classOf[ResolveDispatcher], classOf[TaskQueue], classOf[InMemoryStagingRepository])

  "the ResolveDispatcher implementations" should "include the queue and the inline dispatcher" in {
    implementations.map(_.getSimpleName) should contain allOf ("QueueResolveDispatcher", "InlineResolveDispatcher")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] raise a WAITING resolve to a re-try's mode, and count it upgraded" in {
      val h = new Harness(cls)
      h.dispatch(ResolveMode.Normal)
      h.dispatch(ResolveMode.RetryMiss)
      h.finish() shouldBe Seq(ResolveMode.RetryMiss)
      h.duplicates.asScala.toSeq shouldBe Seq(ResolveMode.RetryMiss -> true)
    }

    it should s"[$name] never lower a waiting resolve's mode, nor count a plain duplicate" in {
      val h = new Harness(cls)
      h.dispatch(ResolveMode.RetryMiss)
      h.dispatch(ResolveMode.Normal)
      h.finish() shouldBe Seq(ResolveMode.RetryMiss)
      h.duplicates.asScala.toSeq shouldBe empty
    }

    it should s"[$name] leave a RUNNING resolve's mode alone, and count the re-try not upgraded" in {
      val h = new Harness(cls)
      h.dispatch(ResolveMode.Normal)
      h.start()
      h.dispatch(ResolveMode.Force)
      h.finish() shouldBe Seq(ResolveMode.Normal)
      h.duplicates.asScala.toSeq shouldBe Seq(ResolveMode.Force -> false)
    }
  }
}
