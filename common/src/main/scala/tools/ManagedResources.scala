package tools

import play.api.Logging

import java.util.concurrent.{ConcurrentLinkedDeque, ExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * What a composition root built that must be shut: every executor, scheduler and closeable the wiring
 * creates registers here as it is created, and the root's `stop()` closes them all, in reverse order of
 * creation, before its Mongo connection closes.
 *
 * The class of bug this closes: a pool the wiring built and `stop()` never named — the identity
 * model's scheduler and prefetch pool, a trace store per rebuild, the debug dashboard's children, the
 * scrape's adaptive-timeout pool — each found and fixed (or not) one at a time, by adding a line to a
 * hand-kept list. Registering at the creation site makes forgetting impossible to spell, and a lazy
 * member never created is never forced into existence just to be shut. `NoUnmanagedWiringExecutorSpec`
 * fails the build on a wiring executor created without registering.
 */
final class ManagedResources(grace: FiniteDuration = ManagedResources.Grace, stopwatch: Stopwatch = Stopwatch.System) extends Logging {
  // `close` is handed what is left of the stop's grace.
  private final case class Held(name: String, resource: AnyRef, close: FiniteDuration => Unit, terminated: () => Boolean)

  private val held     = new ConcurrentLinkedDeque[Held]()
  private val shut     = new ConcurrentLinkedDeque[Held]()
  @volatile private var closing = false

  /** `executor`, shut (interrupting what runs, then waiting for what is left of the grace) by [[closeAll]]. */
  def executor[E <: ExecutorService](name: String)(executor: E): E =
    hold(Held(name, executor, left => { executor.shutdownNow(); executor.awaitTermination(left.toMillis, TimeUnit.MILLISECONDS); () },
      () => executor.isTerminated), executor)

  /** `service`, stopped by [[closeAll]] — a reaper, census or cache that owns its own scheduler. */
  def stopping[A <: services.Stoppable](service: A): A =
    register(service.getClass.getSimpleName, service)(_.stop())

  /** Each of `services` (an `Option` of one, a `Seq` of several), stopped by [[closeAll]]. */
  def stoppingEach[C <: IterableOnce[services.Stoppable]](all: C): C = {
    all.iterator.foreach(stopping); all
  }

  /** Whether `resource` (by identity) is registered — for the specs. */
  def holds(resource: AnyRef): Boolean = (held.toArray(Array.empty[Held]) ++ shut.toArray(Array.empty[Held])).exists(_.resource eq resource)

  /** `resource`, closed by `close` in [[closeAll]]. */
  def register[A](name: String, resource: A)(close: A => Unit): A =
    hold(Held(name, resource.asInstanceOf[AnyRef], _ => close(resource), () => true), resource)

  private def hold[A](entry: Held, resource: A): A = {
    held.push(entry)
    // Created while — or after — the root stops: shut at once rather than left running.
    if (closing) closeAll()
    resource
  }

  /** Close everything registered, newest first; one that fails is logged and the rest still close.
   *  The grace is ONE budget for the whole stop, not one per executor: the pod's stop window is fixed
   *  (web: 30 s less a 15 s preStop), and N executors whose tasks ignore their interrupt held it
   *  N x grace, past the SIGKILL and before the root ever reached its Mongo close. */
  def closeAll(): Unit = {
    closing = true
    val started = stopwatch.start()
    Iterator.continually(held.poll()).takeWhile(_ != null).foreach { entry =>
      try entry.close((grace - started.elapsed).max(Duration.Zero))
      catch { case NonFatal(e) => logger.warn(s"closing ${entry.name} failed: $e", e) }
      shut.push(entry)
    }
  }

  /** The registered resources not yet shut down — for the specs. */
  def unterminated: Seq[String] =
    (held.toArray(Array.empty[Held]) ++ shut.toArray(Array.empty[Held])).toSeq.filterNot(_.terminated()).map(_.name)

  /** How many are registered and not yet closed — for the specs. */
  def open: Int = held.size
}

object ManagedResources {
  /** How long a closing executor's tasks get to answer their interrupt before the stop moves on. */
  val Grace: FiniteDuration = 5.seconds
}
