package services.movies

import modules.WorkerWiring
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
 * A suite that boots a pipeline stops it once its tests are done.
 *
 * A booted wiring starts schedulers and pools ("identity-model-pl", "identity-suspect-takes-pl", …) that only its
 * `stop()` ends. e2e runs unforked, inside the kept sbt server (the write-failure tripwire needs it), so a wiring no
 * one stopped kept its threads alive there — and through them the run's test classloader and everything the boot
 * built. Measured 2026-10-19: every `e2eShared` left eight wirings' threads behind, ~155 MB of live heap and ~145 MB
 * of metaspace that no GC could take back, run after run, in the server every later `testUnit` ran in.
 */
trait StopsItsWirings extends BeforeAndAfterAll { this: Suite =>

  private val booted = new ConcurrentLinkedQueue[WorkerWiring]()

  /** `wiring`, stopped after the suite's last test. */
  protected def stoppedAfterAll[W <: WorkerWiring](wiring: W): W = { booted.add(wiring); wiring }

  override protected def afterAll(): Unit =
    try booted.asScala.foreach(_.stop())
    finally { booted.clear(); super.afterAll() }
}
