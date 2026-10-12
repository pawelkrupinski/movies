import tools.{IntegrationMongoTarget, IntegrationRunClaim, RunScopedDatabaseName, SpecTimeouts}
import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.MongoDatabaseName

import scala.concurrent.Await
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/**
 * A second it/ run on a `MONGODB_DB` prefix another live run holds must FAIL BY NAME, not flake.
 *
 * Two runs on one prefix drop and count each other's rows, so their suites failed at random and
 * passed alone — and nothing said a second run was there. The rival here is a real live process
 * that is no part of this run (neither this JVM's ancestor nor its descendant), claimed onto a
 * prefix of this spec's own, exactly as a neighbouring agent's sbt would hold it.
 */
class IntegrationRunClaimIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, SpecTimeouts.Io)

  /** A live process unrelated to this run and its start (epoch ms) — a neighbour's sbt, as the claim
   *  sees one. The OLDEST such process: a short-lived one can exit between this pick and the claim,
   *  and an exited neighbour is, rightly, no rival. */
  private lazy val neighbour: (Long, Long) = {
    val me        = ProcessHandle.current()
    val ancestors = Iterator.iterate(me.parent())(_.flatMap(_.parent())).takeWhile(_.isPresent).map(_.get.pid).toSet
    ProcessHandle.allProcesses().iterator().asScala
      .filter(p => p.pid != me.pid && !ancestors.contains(p.pid) && !me.descendants().anyMatch(_.pid == p.pid))
      .flatMap(p => p.info().startInstant().toScala.map(start => p.pid -> start.toEpochMilli))
      .minByOption(_._2)
      .getOrElse(cancel("no unrelated process visible to stand in for a neighbouring run"))
  }

  /** Record `pid` as holding `prefix`, the way its own first suite would have. */
  private def heldBy(client: MongoClient, prefix: String, pid: Long, startedAtMillis: Long): Unit =
    await(client.getDatabase(IntegrationRunClaim.Database).getCollection("runs").insertOne(Document(
      "_id" -> s"$prefix|$pid", "prefix" -> prefix, "pid" -> pid, "startedAt" -> startedAtMillis)).toFuture())

  private def holders(client: MongoClient, prefix: String): Seq[Long] =
    await(client.getDatabase(IntegrationRunClaim.Database).getCollection("runs").find(Filters.eq("prefix", prefix)).toFuture())
      .map(_("pid").asInt64.getValue)

  private def withPrefix[A](body: (MongoClient, IntegrationMongoTarget) => A): A = {
    val prefix = RunScopedDatabaseName.fresh("kinowo_it_claim")
    val client = MongoClient(mongoTarget.uri.value)
    try body(client, mongoTarget.copy(databasePrefix = MongoDatabaseName(prefix)))
    finally {
      await(client.getDatabase(IntegrationRunClaim.Database).getCollection("runs").deleteMany(Filters.eq("prefix", prefix)).toFuture())
      client.close()
    }
  }

  "a run starting on a prefix another live run holds" should "be refused, naming that run" in withPrefix { (client, target) =>
    val (pid, start) = neighbour
    heldBy(client, target.databasePrefix.value, pid, start)
    val refused = the[IllegalStateException] thrownBy target.requireThrowaway()
    refused.getMessage should include(s"pid $pid")
    refused.getMessage should include(target.databasePrefix.value)
    withClue("the refused run must not leave a claim behind: ")(holders(client, target.databasePrefix.value) shouldBe Seq(pid))
  }

  "a claim whose process has ended" should "be cleared, not block the run" in withPrefix { (client, target) =>
    // The neighbour's pid, but a start it never had: that process is gone and the pid reused.
    val (pid, start) = neighbour
    heldBy(client, target.databasePrefix.value, pid, start - 60_000)
    noException should be thrownBy target.requireThrowaway()
    holders(client, target.databasePrefix.value) shouldBe Seq(ProcessHandle.current().pid)
  }
}
