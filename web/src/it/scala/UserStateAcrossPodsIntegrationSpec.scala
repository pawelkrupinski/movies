package integration

import tools.SpecTimeouts


import org.mongodb.scala.model.{Filters, IndexOptions, Indexes}
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture, ToSingleObservableUnit}
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import services.users.{InMemoryUserRepository, UserStateRepository}
import tools.ConcurrentInstances
import tools.ConcurrentInstances.{matchesSomeSerialOrder, race, rounds, successes}

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Two web pods writing ONE user's state at the same moment — the old and the new pod of a
 * rolling deploy, a phone and a laptop landing on different pods, a double-tap split by the load
 * balancer. Each pod is its own Mongo client over the shared database (`ConcurrentInstances`).
 *
 * What went wrong here before: two first-ever writes for one user each upserted their own row
 * under a plain `userId` index (caaae16bb — four duplicates lived from 2026-05-26 until someone
 * noticed in September), and a per-pod copy of the row wrote a stale version back over another
 * pod's hide (df4146340).
 */
class UserStateAcrossPodsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val clock = Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), ZoneOffset.UTC)

  /** The writes a round races, each on the pod it names. */
  private sealed trait Write { def pod: Int }
  private final case class Hide(pod: Int, title: String)     extends Write
  private final case class Unhide(pod: Int, title: String)   extends Write
  private final case class Language(pod: Int, code: String)  extends Write

  /** What a user's row means to them: the Polish hidden list and the language. */
  private final case class Seen(hidden: Set[String], language: Option[String])

  private def applySerially(state: Seen, write: Write): Seen = write match {
    case Hide(_, title)     => state.copy(hidden = state.hidden + title)
    case Unhide(_, title)   => state.copy(hidden = state.hidden - title)
    case Language(_, code)  => state.copy(language = Some(code))
  }

  /** Wait for the spec's OWN Mongo calls — arranging a collection, reading it back. None of them
   *  is under test, so none may fail on latency: in itAll's parallel run on 2026-10-03 the server
   *  spent 19.3 s on the boot case's setup `createIndex` (7.2 s creating the collection, 11.9 s
   *  waiting for majority write concern), and the 10 s each of these used to allow failed the case
   *  before a pod had booted. The code under test keeps its own budget (`MongoIndex`'s). */
  private def await[A](operation: scala.concurrent.Future[A]): A = Await.result(operation, SpecTimeouts.Io)

  private def rowsFor(db: MongoDatabase, userId: String): Long =
    await(db.getCollection(UserStateRepository.Collection).countDocuments(Filters.eq("userId", userId)).toFuture())

  "two web pods writing one user's state at once" should
    "leave exactly one row, and one that some serial order of the writes produces" in
    ConcurrentInstances.withInstances(mongoTarget, "userstate-two-pods") { instances =>
      val users = new InMemoryUserRepository
      val pods  = instances.map(instance => new UserStatePod(instance.database, users, clock))
      try rounds(12, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        // A user with NO row yet: the first writes from both pods are the ones that each upsert.
        val userId = UserStatePod.signIn(users, s"two-pods-${round.number}")
        val writes = Seq(Hide(0, "A1"), Hide(1, "B1"), Unhide(0, "B1"), Hide(1, "A2"), Language(0, "en"), Language(1, "de"))
        val statuses = successes(race(writes.map(write => () => {
          val pod = pods(write.pod)
          status(write match {
            case Hide(_, title)    => pod.hide(userId, title)
            case Unhide(_, title)  => pod.unhide(userId, title)
            case Language(_, code) => pod.putLanguage(userId, code)
          })
        }), Some(round)))

        statuses.distinct shouldBe Seq(OK)
        withClue("two pods' first writes must converge on ONE row, not upsert one each: ") {
          rowsFor(instances.head.database, userId) shouldBe 1L
        }
        val stored = pods(1).states.find(userId).value
        val seen   = Seen(stored.hiddenFilmsByCountry.getOrElse("pl", Set.empty), stored.language)
        withClue(s"$seen is no serial order of $writes — a write was lost: ") {
          matchesSomeSerialOrder(Seen(Set.empty, None), writes, seen)(applySerially) shouldBe true
        }
      } finally pods.foreach(_.close())
    }

  /** Boot a pod's store on every instance at once, each reporting its index health, and
   *  return what each reported and how many indexes each DROPPED on `userStates` while booting. */
  private def bootTogether(instances: Seq[ConcurrentInstances.Instance], round: ConcurrentInstances.Round): (Seq[List[Boolean]], Seq[Int]) = {
    import scala.jdk.CollectionConverters._
    def drops(instance: ConcurrentInstances.Instance) =
      instance.indexCommands(UserStateRepository.Collection).count(_.name == "dropIndexes")
    val before   = instances.map(drops)
    val reported = instances.map(_ => new java.util.concurrent.ConcurrentLinkedQueue[Boolean]())
    val pods = instances.zip(reported).map { case (instance, health) =>
      new UserStatePod(instance.database, new InMemoryUserRepository, clock, indexHealth = (present: Boolean) => { health.add(present); () })
    }
    try {
      // Joined past `MongoIndex`'s own 30 s, so a slow boot reports what IT decided rather than
      // the race giving up on it at the same instant.
      successes(race(pods.map(pod => () => pod.states.enabled), Some(round), joinTimeout = SpecTimeouts.Run)).distinct shouldBe Seq(true)
      (reported.map(_.asScala.toList), instances.map(drops).zip(before).map { case (after, was) => after - was })
    } finally pods.foreach(_.close())
  }

  private def userIdIndex(db: MongoDatabase): Option[Document] =
    await(db.getCollection[Document](UserStateRepository.Collection).listIndexes().toFuture())
      .find(_.get("name").exists(_.asString().getValue == "userId_1"))

  /** Keep `userStates` locked from `instance` for `duration`, as a slow neighbour in a loaded run
   *  does: a transaction that has written to the collection holds it until it ends, and an index
   *  build waits for that. Closing the hold ends it early — it must end before the instance's
   *  client closes, or the server keeps the transaction (and the lock) for its whole lifetime. */
  private def holdCollection(instance: ConcurrentInstances.Instance, duration: FiniteDuration): AutoCloseable = {
    await(instance.database.createCollection(UserStateRepository.Collection).toFuture())
    val session = await(instance.client.startSession().toFuture())
    session.startTransaction()
    await(instance.database.getCollection[Document](UserStateRepository.Collection)
      .insertOne(session, Document("userId" -> "held-by-a-neighbour")).toFuture())
    val released = new java.util.concurrent.atomic.AtomicBoolean(false)
    val hold: AutoCloseable = () => if (released.compareAndSet(false, true)) {
      try await(ToSingleObservableUnit(session.abortTransaction()).toFuture()) finally session.close()
    }
    val timer = new Thread(() => { Thread.sleep(duration.toMillis); hold.close() }, "userstate-hold")
    timer.setDaemon(true)
    timer.start()
    hold
  }

  // Every web pod builds the unique `userId` index on boot, and a rolling deploy boots the new pod
  // while the old one serves. Finding the index already there, a boot must leave it alone: the boot
  // that dropped and rebuilt it every time (fixed in e098b3b62) left the collection with no
  // uniqueness at all between the drop and the create — exactly while the old pod was writing.
  "a web pod booting while another serves" should "find the unique userId index and drop nothing" in
    ConcurrentInstances.withInstances(mongoTarget, "userstate-two-pods-boot") { instances =>
      // The setup's index build, made to wait 12 s the way a loaded itAll mongod made it wait.
      val held = holdCollection(instances.last, 12.seconds)
      try await(instances.head.database.getCollection[Document](UserStateRepository.Collection)
        .createIndex(Indexes.ascending("userId"), IndexOptions().unique(true)).toFuture())
      finally held.close()
      rounds(3, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        val (reported, drops) = bootTogether(instances, round)
        reported shouldBe Seq(List(true), List(true))
        withClue(s"indexes dropped per pod: $drops — ") { drops shouldBe Seq(0, 0) }
        userIdIndex(instances.head.database).flatMap(_.get("unique")).map(_.asBoolean().getValue) shouldBe Some(true)
      }
    }

  // Over a collection still carrying the pre-caaae16bb PLAIN index, a boot that rebuilt it raced
  // the other pod's boot: both read "legacy" and both dropped by name — the second drop either
  // found nothing (that pod then reported the index MISSING although the first had built it,
  // paging `UserStateUniqueIndexMissing`) or removed the unique index the first pod had just
  // built. For a while after that a pod only REPORTED a plain index. It now converts it in place
  // (`MongoIndex`: `collMod` prepareUnique, then unique), which is idempotent — so two pods doing
  // it at once both end with the unique index, and neither drops anything.
  "two web pods booting at once over a plain userId index" should "both convert it in place, and neither drop it" in
    ConcurrentInstances.withInstances(mongoTarget, "userstate-two-pods-legacy") { instances =>
      val coll = instances.head.database.getCollection[Document](UserStateRepository.Collection)
      rounds(4, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        await(coll.drop().toFuture())
        await(coll.createIndex(Indexes.ascending("userId"), IndexOptions()).toFuture())
        val (reported, drops) = bootTogether(instances, round)
        withClue(s"indexes dropped per pod: $drops — ") { drops shouldBe Seq(0, 0) }
        withClue("both pods must find the index unique once they are done: ") { reported shouldBe Seq(List(true), List(true)) }
        userIdIndex(instances.head.database).flatMap(_.get("unique")).map(_.asBoolean().getValue) shouldBe Some(true)
      }
    }

  // Duplicate rows make the conversion impossible: the plain index stays, untouched, and both
  // pods report it missing for an operator to dedupe.
  "two web pods booting at once over a plain userId index with duplicate rows" should "both report it, and keep the plain index" in
    ConcurrentInstances.withInstances(mongoTarget, "userstate-two-pods-duplicates") { instances =>
      val coll = instances.head.database.getCollection[Document](UserStateRepository.Collection)
      rounds(2, tools.ConcurrentInstances.baseSeed(configuration)) { round =>
        await(coll.drop().toFuture())
        await(coll.insertMany(Seq(Document("userId" -> "twice"), Document("userId" -> "twice"))).toFuture())
        await(coll.createIndex(Indexes.ascending("userId"), IndexOptions()).toFuture())
        val (reported, drops) = bootTogether(instances, round)
        withClue(s"indexes dropped per pod: $drops — ") { drops shouldBe Seq(0, 0) }
        reported shouldBe Seq(List(false), List(false))
        val index = userIdIndex(instances.head.database)
        index.isDefined shouldBe true
        index.flatMap(_.get("unique")) shouldBe None
      }
    }
}
