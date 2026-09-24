package tools

import com.mongodb.event.{CommandFailedEvent, CommandListener, CommandStartedEvent, CommandSucceededEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, CyclicBarrier, TimeUnit}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * Two (or N) instances of one tier against ONE Mongo database, doing the same work at the same
 * moment — what a rolling deploy does for a few seconds on every release: the old pod and the new
 * one both serve, both write, both boot their repositories.
 *
 * Every race this repository has shipped a fix for was an interleaving nobody's single-process
 * test could reach: duplicate `userStates` rows from two first writes (live for months), a
 * per-pod cache writing back a row another pod had changed, two folds for one tmdbId minting two
 * films, a reconnect publishing a database it never claimed. The specs built on this pin the
 * invariants those fixes restored, with the instances genuinely overlapping:
 *
 *  - an [[Instance]] is one pod: its OWN `MongoClient` (own pool, own sessions, own change-stream
 *    cursors — what a second process has), recording every command it sends, so a spec can ask
 *    which pod mutated an index;
 *  - [[race]] starts operations on separate threads behind one barrier, each after a SEEDED jitter,
 *    so they overlap rather than queue and each [[Round]] covers a different order;
 *  - [[rounds]] repeats a scenario K times and names the seed of a failing round, so the
 *    interleaving that broke can be replayed (`KINOWO_RACE_SEED`);
 *  - [[matchesSomeSerialOrder]] is the correctness bar for concurrent writes: whatever order the
 *    writes landed in, the final state must be one that SOME serial order of them produces.
 *
 * Extracted from `ConcurrentFoldRaceHarness` (the fold-race specs' seed/barrier/thread/join/collect
 * plumbing), which now delegates its start here.
 */
object ConcurrentInstances {

  /** One simulated pod. `database` is its view of the shared database, through its own client. */
  final class Instance(val name: String, uri: String, dbName: String) extends AutoCloseable {
    private val sent    = new ConcurrentLinkedQueue[SentCommand]()
    // requestId -> the collection the command named. Read AT START: the command document is a view
    // over the connection's buffer, released once the command completes.
    private val started = new ConcurrentHashMap[Int, String]()

    private val listener = new CommandListener {
      override def commandStarted(event: CommandStartedEvent): Unit = {
        Option(event.getCommand.get(event.getCommandName)).collect { case name: org.bson.BsonString => name.getValue }
          .foreach(started.put(event.getRequestId, _))
      }
      override def commandSucceeded(event: CommandSucceededEvent): Unit = finish(event.getRequestId, event.getCommandName, None)
      override def commandFailed(event: CommandFailedEvent): Unit =
        finish(event.getRequestId, event.getCommandName, Some(Option(event.getThrowable.getMessage).getOrElse(event.getThrowable.toString)))
    }

    private def finish(requestId: Int, command: String, failure: Option[String]): Unit = {
      sent.add(SentCommand(command, Option(started.remove(requestId)), failure))
      ()
    }

    val client: MongoClient = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(uri)).addCommandListener(listener)
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)   // what `MongoClient(uri)` gives every other spec
      .build())
    val database: MongoDatabase = client.getDatabase(dbName)

    /** Every command this pod has sent so far, in completion order. */
    def commands: Seq[SentCommand] = sent.asScala.toSeq

    /** The index DDL this pod ran (`createIndexes` / `dropIndexes`) against `collection`,
     *  succeeded or not. A `createIndexes` for an index that already exists with the same options
     *  is a no-op on the server, so a spec about "who mutated the index" counts the drops. */
    def indexCommands(collection: String): Seq[SentCommand] =
      commands.filter(c => IndexCommands(c.name) && c.collection.contains(collection))

    def close(): Unit = client.close()
  }

  /** A command one pod sent: its name, the collection it named, and the server's error if it failed. */
  final case class SentCommand(name: String, collection: Option[String], failure: Option[String]) {
    def succeeded: Boolean = failure.isEmpty
  }

  private val IndexCommands = Set("createIndexes", "dropIndexes")

  /** `count` pods over a database of `suite`'s own (see [[IntegrationCorpusDatabase]]), dropped
   *  afterwards, with every pod's client closed first. */
  def withInstances[A](suite: String, count: Int = 2)(body: Seq[Instance] => A): A = {
    val uri = Env.get("MONGODB_URI").getOrElse(throw new IllegalStateException("MONGODB_URI not set"))
    IntegrationCorpusDatabase.withDatabase(uri, suite) { db =>
      val instances = (1 to count).map(i => new Instance(s"pod-$i", uri, db.name))
      try body(instances) finally instances.foreach(_.close())
    }
  }

  /** One repetition of a scenario, with the seed its interleaving is drawn from. */
  final case class Round(number: Int, seed: Long) {
    val random = new scala.util.Random(seed)
    override def toString = s"round $number (KINOWO_RACE_SEED=$seed)"
  }

  /** The seed round 1 is drawn from — fixed, so a run is reproducible, and overridable to replay
   *  (or to explore) other interleavings. */
  def baseSeed: Long = Env.get("KINOWO_RACE_SEED").flatMap(_.toLongOption).getOrElse(20260924L)

  /** Run `body` for `count` rounds, each with its own seed; a failure names the round and seed. */
  def rounds(count: Int, seed: Long = baseSeed)(body: Round => Unit): Unit =
    (0 until count).foreach { i =>
      val round = Round(i + 1, seed + i)
      org.scalatest.Assertions.withClue(s"$round: ")(body(round))
    }

  /** Start every op together: each on its own thread, all released by one barrier, and each then
   *  waiting its own seeded jitter (up to `maxJitter`, drawn from `round`) before it runs — without
   *  it every round replays the same photo-finish; with it, K rounds cover different orders. Returns
   *  each op's outcome in `ops` order; an op still running at `joinTimeout` is a `Left`. */
  def race[A](ops: Seq[() => A], round: Option[Round] = None, maxJitter: FiniteDuration = 2.millis,
              joinTimeout: FiniteDuration = 30.seconds): Seq[Either[Throwable, A]] = {
    val delays   = ops.map(_ => round.fold(0L)(r => (r.random.nextDouble() * maxJitter.toNanos).toLong))
    val barrier  = new CyclicBarrier(ops.size)
    val outcomes = Array.fill[Either[Throwable, A]](ops.size)(Left(new IllegalStateException(s"did not finish within $joinTimeout")))
    val threads = ops.zip(delays).zipWithIndex.map { case ((op, delay), i) =>
      val thread = new Thread(() => {
        barrier.await()
        if (delay > 0) LockSupport.parkNanos(delay)
        outcomes(i) = try Right(op()) catch { case e: Throwable => Left(e) }
      }, s"race-${round.fold(0)(_.number)}-$i")
      thread.setDaemon(true)
      thread.start()
      thread
    }
    val deadline = System.nanoTime() + joinTimeout.toNanos
    threads.foreach(t => t.join(math.max(1L, TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime()))))
    outcomes.toSeq
  }

  /** The values of every successful outcome, or a failure naming every op that threw. */
  def successes[A](outcomes: Seq[Either[Throwable, A]]): Seq[A] = {
    val failures = outcomes.collect { case Left(e) => e }
    if (failures.nonEmpty)
      throw new AssertionError(s"${failures.size} of ${outcomes.size} concurrent ops failed: " +
        failures.map(e => s"${e.getClass.getSimpleName}: ${e.getMessage}").mkString("; "), failures.head)
    outcomes.collect { case Right(a) => a }
  }

  /** Is `actual` what SOME serial order of `ops` leaves behind, starting from `initial`? The bar for
   *  concurrent read-modify-writes: a lost update is a state no order produces. Tries every
   *  permutation, so keep `ops` to a handful (8 is 40,320 orders). */
  def matchesSomeSerialOrder[S, O](initial: S, ops: Seq[O], actual: S)(apply: (S, O) => S): Boolean =
    ops.permutations.exists(_.foldLeft(initial)(apply) == actual)
}
