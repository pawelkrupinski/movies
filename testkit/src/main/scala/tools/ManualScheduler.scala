package tools

import java.time.Duration
import java.util.concurrent.{AbstractExecutorService, Callable, Delayed, RejectedExecutionException, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.collection.mutable

/** A [[ScheduledExecutorService]] whose delayed tasks run only when the spec moves `clock` past
 *  them with [[advance]], on the spec's own thread — so a timer boundary is stepped across, never
 *  slept across. One-shot `schedule(Runnable, …)` and the two periodic forms are supported (a
 *  period re-arms from the moment the run is due, which a stepped clock makes the same for both);
 *  after `shutdown` it refuses, as a real scheduler does. */
final class ManualScheduler(clock: MutableClock) extends AbstractExecutorService with ScheduledExecutorService {
  private final class Task(@volatile var dueMillis: Long, val sequence: Long, periodMillis: Option[Long], run: Runnable)
      extends ScheduledFuture[Unit] {
    @volatile private var progress = 0 // 0 waiting, 1 done, 2 cancelled
    /** Runs the task; true when it is periodic and must be re-armed. */
    def fire(): Boolean = progress == 0 && {
      run.run()
      periodMillis match {
        case Some(period) if progress == 0 => dueMillis += period; true
        case _                             => if (progress == 0) progress = 1; false
      }
    }
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
      if (task.fire()) rearm(task)
    }
    if (until > clock.millis()) clock.advance(Duration.ofMillis(until - clock.millis()))
  }

  /** Run what is due now without moving the clock — the tasks `execute` handed over. */
  def runDue(): Unit = advance(Duration.ZERO)

  /** Run due tasks one at a time, without moving the clock, until `done` holds or none is left —
   *  as an `ExecutionContext.fromExecutor(this)`, this stops a chain of callbacks at an exact step. */
  def runUntil(done: => Boolean): Unit =
    Iterator.continually(if (done) None else nextDue(clock.millis())).takeWhile(_.isDefined).flatten.foreach { task =>
      if (task.fire()) rearm(task)
    }

  private def nextDue(until: Long): Option[Task] = synchronized {
    val due = tasks.filter(t => !t.isDone && t.dueMillis <= until).minByOption(t => (t.dueMillis, t.sequence))
    due.foreach(tasks -= _)
    due
  }

  private def rearm(task: Task): Unit = synchronized { if (!stopped) tasks += task }

  private def add(delayMillis: Long, period: Option[Long], command: Runnable): ScheduledFuture[?] = synchronized {
    if (stopped) throw new RejectedExecutionException("shut down")
    sequence += 1
    val task = new Task(clock.millis() + delayMillis, sequence, period, command)
    tasks += task
    task
  }

  override def schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[?] =
    add(unit.toMillis(delay), None, command)
  override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[?] =
    add(unit.toMillis(initialDelay), Some(unit.toMillis(period)), command)
  override def scheduleWithFixedDelay(command: Runnable, initialDelay: Long, delay: Long, unit: TimeUnit): ScheduledFuture[?] =
    add(unit.toMillis(initialDelay), Some(unit.toMillis(delay)), command)

  override def schedule[V](callable: Callable[V], delay: Long, unit: TimeUnit): ScheduledFuture[V] =
    throw new UnsupportedOperationException("ManualScheduler runs Runnables only")
  override def execute(command: Runnable): Unit = { schedule(command, 0L, TimeUnit.MILLISECONDS); () }
  override def shutdown(): Unit = stopped = true
  override def shutdownNow(): java.util.List[Runnable] = { stopped = true; java.util.List.of() }
  override def isShutdown: Boolean   = stopped
  override def isTerminated: Boolean = stopped
  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true
}
