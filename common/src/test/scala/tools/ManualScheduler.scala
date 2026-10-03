package tools

import java.time.Duration
import java.util.concurrent.{AbstractExecutorService, Callable, Delayed, RejectedExecutionException, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.collection.mutable

/** A [[ScheduledExecutorService]] whose delayed tasks run only when the spec moves `clock` past
 *  them with [[advance]], on the spec's own thread — so a timer boundary is stepped across, never
 *  slept across. Only `schedule(Runnable, …)` is supported; after `shutdown` it refuses, as a real
 *  scheduler does. */
final class ManualScheduler(clock: MutableClock) extends AbstractExecutorService with ScheduledExecutorService {
  private final class Task(val dueMillis: Long, val sequence: Long, run: Runnable) extends ScheduledFuture[Unit] {
    @volatile private var progress = 0 // 0 waiting, 1 done, 2 cancelled
    def fire(): Unit = if (progress == 0) { progress = 1; run.run() }
    def getDelay(unit: TimeUnit): Long = unit.convert(dueMillis - clock.millis(), TimeUnit.MILLISECONDS)
    def compareTo(other: Delayed): Int = java.lang.Long.compare(getDelay(TimeUnit.MILLISECONDS), other.getDelay(TimeUnit.MILLISECONDS))
    def cancel(mayInterrupt: Boolean): Boolean = if (progress == 0) { progress = 2; true } else false
    def isCancelled: Boolean = progress == 2
    def isDone: Boolean      = progress != 0
    def get(): Unit = ()
    def get(timeout: Long, unit: TimeUnit): Unit = ()
  }

  private val tasks    = mutable.ArrayBuffer.empty[Task]
  private var sequence = 0L
  @volatile private var stopped = false

  /** Move the clock by `by`, running every task that falls due on the way, in due order. */
  def advance(by: Duration): Unit = {
    val until = clock.millis() + by.toMillis
    Iterator.continually(nextDue(until)).takeWhile(_.isDefined).flatten.foreach { task =>
      if (task.dueMillis > clock.millis()) clock.advance(Duration.ofMillis(task.dueMillis - clock.millis()))
      task.fire()
    }
    if (until > clock.millis()) clock.advance(Duration.ofMillis(until - clock.millis()))
  }

  private def nextDue(until: Long): Option[Task] = synchronized {
    val due = tasks.filter(t => !t.isDone && t.dueMillis <= until).minByOption(t => (t.dueMillis, t.sequence))
    due.foreach(tasks -= _)
    due
  }

  override def schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[?] = synchronized {
    if (stopped) throw new RejectedExecutionException("shut down")
    sequence += 1
    val task = new Task(clock.millis() + unit.toMillis(delay), sequence, command)
    tasks += task
    task
  }

  override def schedule[V](callable: Callable[V], delay: Long, unit: TimeUnit): ScheduledFuture[V] = unsupported
  override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[?] = unsupported
  override def scheduleWithFixedDelay(command: Runnable, initialDelay: Long, delay: Long, unit: TimeUnit): ScheduledFuture[?] = unsupported
  override def execute(command: Runnable): Unit = { schedule(command, 0L, TimeUnit.MILLISECONDS); () }
  override def shutdown(): Unit = stopped = true
  override def shutdownNow(): java.util.List[Runnable] = { stopped = true; java.util.List.of() }
  override def isShutdown: Boolean   = stopped
  override def isTerminated: Boolean = stopped
  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true

  private def unsupported: Nothing = throw new UnsupportedOperationException("ManualScheduler runs one-shot Runnables only")
}
