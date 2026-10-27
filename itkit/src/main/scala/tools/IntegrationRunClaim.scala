package tools

import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.{Filters, ReplaceOptions}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await

/**
 * One `MONGODB_DB` prefix, one run: the claim that makes a second run on the same prefix FAIL,
 * by name, instead of flaking.
 *
 * Every it/ database is the prefix itself (the rows the light specs share by sentinel id) or a
 * name derived from it, and the bare prefix is shared by every spec of the run — so two runs on
 * one prefix (two agents' itAll, an itAll and a dump beside it) delete each other's sentinels
 * and count each other's rows. That failed suites at random and passed them alone
 * (`MongoStampedRowsIntegrationSpec`, 2026-10-04), and nothing said why: the documented rule —
 * a prefix of your own per concurrent run — was only ever documented.
 *
 * So the first suite of a run records the run (its pid and that process's start) under the
 * prefix in [[Database]], and refuses to start while another LIVE run holds it. Live means the
 * pid is running AND is the same process that claimed (a reused pid is not); a process that is
 * this one's ancestor or descendant is the same run (a forked test JVM and the sbt that forked
 * it). A dead run's claim is cleared as it is found. A pid is only meaningful on this machine,
 * so — like [[RunScopedDatabaseName.sweepOrphans]] — the claim is taken only on a loopback
 * server; CI's one run per runner never meets another.
 */
object IntegrationRunClaim {

  /** The database the claims live in — its own, so no spec's view of its corpus includes them. */
  val Database: String = "kinowo_it_run_claims"
  private val Collection = "runs"

  /** A run as a claim names it: the process, and when it started (what tells a reused pid apart). */
  final case class RunProcess(pid: Long, startedAt: Option[Instant])

  /** Of the runs holding `prefix`, those that are a DIFFERENT live run from `self`. Pure. */
  def rivals(self: RunProcess, holders: Seq[RunProcess], live: RunProcess => Boolean,
             sameRun: Long => Boolean): Seq[RunProcess] =
    holders.filter(holder => holder.pid != self.pid && live(holder) && !sameRun(holder.pid))

  private def processOf(handle: ProcessHandle): RunProcess = {
    val started = handle.info().startInstant()
    RunProcess(handle.pid(), if (started.isPresent) Some(started.get) else None)
  }

  private val Self: RunProcess = processOf(ProcessHandle.current())

  /** Running, and the very process that claimed — not a later one given its pid. */
  private def live(run: RunProcess): Boolean = {
    val handle = ProcessHandle.of(run.pid)
    handle.isPresent && handle.get.isAlive && {
      // Compared at the millisecond the claim stores it to.
      val now = processOf(handle.get).startedAt.map(_.toEpochMilli)
      run.startedAt.isEmpty || now.isEmpty || now == run.startedAt.map(_.toEpochMilli)
    }
  }

  /** Whether `pid` is this process's ancestor or descendant: a fork of the same run. */
  private def sameRun(pid: Long): Boolean = {
    val me = ProcessHandle.current()
    Iterator.iterate(me.parent())(_.flatMap(_.parent())).takeWhile(_.isPresent).exists(_.get.pid == pid) ||
      me.descendants().anyMatch(_.pid == pid)
  }

  private val claimed = ConcurrentHashMap.newKeySet[String]()

  /** Claim `target`'s prefix for this run, once per JVM; throw when another live run holds it. */
  def claim(target: IntegrationMongoTarget): Unit = {
    val key = s"${target.uri.value}|${target.databasePrefix.value}"
    if (RunScopedDatabaseName.isLoopbackOnly(target.uri.value) && !claimed.contains(key)) synchronized {
      if (!claimed.contains(key)) {
        claimOn(target.uri.value, target.databasePrefix.value, Self, live, sameRun)
        claimed.add(key)
      }
    }
  }

  /** The claim itself, against the server — every judgement passed in, so a spec can stand in for the
   *  process table. */
  private[tools] def claimOn(uri: String, prefix: String, self: RunProcess, live: RunProcess => Boolean,
                             sameRun: Long => Boolean): Unit = {
    val client = MongoClient(uri)
    try {
      val runs = client.getDatabase(Database).getCollection(Collection)
      def idOf(run: RunProcess) = s"$prefix|${run.pid}"
      val mine = Document("_id" -> idOf(self), "prefix" -> prefix, "pid" -> self.pid) ++
        self.startedAt.fold(Document())(at => Document("startedAt" -> at.toEpochMilli))
      Await.result(runs.replaceOne(Filters.eq("_id", idOf(self)), mine, ReplaceOptions().upsert(true)).toFuture(), SpecTimeouts.Io)
      val holders = Await.result(runs.find(Filters.eq("prefix", prefix)).toFuture(), SpecTimeouts.Io).map { d =>
        RunProcess(d("pid").asInt64.getValue, d.get("startedAt").map(v => Instant.ofEpochMilli(v.asInt64.getValue)))
      }
      holders.filterNot(run => run.pid == self.pid || live(run)).foreach { dead =>
        Await.result(runs.deleteOne(Filters.eq("_id", idOf(dead))).toFuture(), SpecTimeouts.Io)
      }
      val others = rivals(self, holders, live, sameRun)
      if (others.nonEmpty) {
        Await.result(runs.deleteOne(Filters.eq("_id", idOf(self))).toFuture(), SpecTimeouts.Io)
        throw new IllegalStateException(
          s"""MONGODB_DB=$prefix is in use by another live it/ run (pid ${others.map(_.pid).mkString(", ")}) on ${IntegrationMongo.redact(uri)}.
             |
             |Two runs on one prefix drop and count each other's rows: their suites fail at random and pass alone.
             |Give this run a prefix of its own (MONGODB_DB=kinowo_it_<name>1, …2), or a server of its own:
             |  scripts/it-local.sh""".stripMargin)
      }
    } finally client.close()
  }
}
