package tools

import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.util.control.NonFatal

/**
 * The one shape of a test database name that belongs to ONE run: `…_pid<pid>…`, the owning JVM's
 * pid as a marker — and the sweep that reclaims such a database once its run is gone.
 *
 * Unique-per-run names keep two runs on the one local `:28017` server from dropping each other's
 * databases, but they trade that for a leak: a run that is killed (an sbt timeout, Ctrl-C, an OOM)
 * never reaches its `finally`, and no later run drops a name it never generates. 351 such
 * databases had piled up on the local server by 2026-10-04. The marker makes the owner readable
 * back from the name, so any later run can tell an orphan (its pid no longer alive) from a
 * neighbour's live database, and drop only the former.
 *
 * A pid that was reused by an unrelated live process only DEFERS a drop; it never drops a live
 * run's database. Every run-scoped name goes through here — `IntegrationDatabaseIsolationSpec`
 * fails an it spec that builds one by hand.
 */
object RunScopedDatabaseName {

  private val Pid: Long = ProcessHandle.current().pid()

  /** `_pid<pid>`: what marks a database as this run's. */
  private val Marker: String = s"_pid$Pid"

  /** `<base>_pid<pid>` — the same name for every call in this JVM. */
  def forThisRun(base: String): String = base + Marker

  /** `<base>_pid<pid>_<nanos>` — a different name on every call. */
  def fresh(base: String): String = s"$base${Marker}_${System.nanoTime()}"

  /** The suffix [[fresh]] appends, for a caller that must fit `base` into Mongo's 63 characters. */
  def freshSuffix(): String = s"${Marker}_${System.nanoTime()}"

  private val Owned = """_pid(\d+)(?=_|$)""".r
  /** The run-scoped names `IsolatedMongoDatabase` gave out before the marker:
   *  `kinowo_isolated_<purpose>_<pid>_<nanos>`. */
  private val LegacyIsolated = """^kinowo_isolated_.+_(\d+)_\d+$""".r

  /** The pid of the run that owns `name`, when it is a run-scoped name. */
  def owner(name: String): Option[Long] =
    Owned.findFirstMatchIn(name).orElse(LegacyIsolated.findFirstMatchIn(name)).map(_.group(1).toLong)

  /** Of `names`, those whose owning run is no longer `alive`. */
  def orphans(names: Seq[String], alive: Long => Boolean): Seq[String] =
    names.filter(name => owner(name).exists(pid => !alive(pid)))

  private def isAlive(pid: Long): Boolean = ProcessHandle.of(pid).isPresent

  /** Drop every database on `client`'s server whose owning run has ended; the names dropped. A
   *  failed sweep is not this run's failure: it costs a leaked database, never a test. */
  def sweepOrphans(client: MongoClient): Seq[String] =
    try {
      val dead = orphans(Await.result(client.listDatabaseNames().toFuture(), SpecTimeouts.Io), isAlive)
      dead.foreach(name => Await.result(client.getDatabase(name).drop().toFuture(), SpecTimeouts.Io))
      dead
    } catch { case NonFatal(_) => Nil }
}
