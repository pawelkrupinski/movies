package services

import models.Country
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import tools.Env

import java.net.{InetSocketAddress, ServerSocket, Socket, URI}
import org.mongodb.scala.SingleObservableFuture
import scala.concurrent.duration._

/** A worker whose Mongo is UNREACHABLE at boot starts degraded and reconnects in the
 *  background. That reconnect must claim the database for the worker's country before it
 *  publishes the database to anything that writes: a German worker whose `MONGODB_DB`
 *  names Poland's database must stay degraded rather than start writing into it, and a
 *  worker on its own database must still recover (the positive control).
 *
 *  "Unreachable, then reachable" is a local port nothing listens on at boot, which a
 *  forwarder to the test Mongo starts answering on once the connection is degraded. */
class LateReconnectClaimIntegrationSpec extends AnyFlatSpec with Matchers with Eventually {
  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  private val uri      = Env.get("MONGODB_URI").get
  private val target   = URI.create(uri.replace("mongodb://", "http://"))
  private val patience = PatienceConfig(timeout = Span(30, Seconds), interval = Span(1, Seconds))

  "a reconnect after an unreachable boot" should "claim the database before publishing it, and refuse another country's" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "late-reconnect-claim") { db =>
      new DatabaseOwner(db).claim(Country.Poland)
      val port = freePort()
      val via  = s"mongodb://127.0.0.1:$port/?directConnection=true&connectTimeoutMS=300"
      def open(country: Country) = new MongoConnection(Some(via), db.name, required = true,
        probeTimeout = 2.seconds, serverSelectionTimeout = Some(500.millis),
        onConnected = MongoConnection.claimFor(country))
      val german = open(Country.Germany)
      val polish = open(Country.Poland)
      german.database shouldBe None
      polish.database shouldBe None
      val forwarder = forward(port, target.getHost, target.getPort)
      try {
        eventually(polish.database should not be empty)(using patience, implicitly)
        german.database shouldBe None
        new DatabaseOwner(db).owner() shouldBe Some(Country.Poland.code)
      } finally { german.close(); polish.close(); forwarder.close() }
    }

  // Giving up is for the ownership refusal alone. Every other non-transient failure — an auth
  // error mid role change, a command error — also ended the reconnect for good, leaving the
  // process degraded until something restarted it.
  "a reconnect that meets a failure other than another country's claim" should "keep retrying and recover" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "late-reconnect-retry") { db =>
      val attempts = new java.util.concurrent.atomic.AtomicInteger(0)
      val connection = new MongoConnection(Some(uri), db.name, required = true, probeTimeout = 2.seconds,
        onConnected = _ => attempts.incrementAndGet() match {
          case 1 => throw new com.mongodb.MongoTimeoutException("unreachable at boot")
          case 2 => throw new IllegalArgumentException("refused once, for a reason no claim gave")
          case _ => ()
        })
      try {
        connection.database shouldBe None
        eventually(connection.database should not be empty)(using PatienceConfig(Span(40, Seconds), Span(1, Seconds)), implicitly)
      } finally connection.close()
    }

  // A connection bound to a SHARED client does not own it (the worker closes it once, after
  // every borrowing connection). A close landing while the reconnect's probe was in flight
  // made the reconnect close "its" client — the shared one, under every other country.
  "a reconnect that finds its connection closed" should "leave a shared client it does not own open" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "late-reconnect-shared") { db =>
      val shared   = org.mongodb.scala.MongoClient(uri)
      val attempts = new java.util.concurrent.atomic.AtomicInteger(0)
      val probed   = new java.util.concurrent.CountDownLatch(1)
      @volatile var connection: MongoConnection = null
      try {
        connection = new MongoConnection(Some(uri), db.name, required = true, probeTimeout = 2.seconds,
          sharedClient = Some(shared),
          onConnected = _ =>
            if (attempts.incrementAndGet() == 1) throw new com.mongodb.MongoTimeoutException("unreachable at boot")
            else { connection.close(); probed.countDown() })   // the owner closes it mid-probe
        connection.database shouldBe None
        probed.await(30, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
        Thread.sleep(500)                                     // let the reconnect finish its step
        connection.database shouldBe None
        noException should be thrownBy
          scala.concurrent.Await.result(shared.getDatabase(db.name).getCollection("movies").countDocuments().toFuture(), 5.seconds)
      } finally shared.close()
    }

  private def freePort(): Int = { val s = new ServerSocket(0); try s.getLocalPort finally s.close() }

  /** A byte-pipe from `port` to `host:targetPort`, one thread pair per accepted socket. */
  private def forward(port: Int, host: String, targetPort: Int): AutoCloseable = {
    val server = new ServerSocket()
    server.bind(new InetSocketAddress("127.0.0.1", port))
    def pipe(from: Socket, to: Socket): Unit = daemon {
      try from.getInputStream.transferTo(to.getOutputStream) catch { case _: java.io.IOException => () }
      finally { from.close(); to.close() }
    }
    daemon {
      try while (true) {
        val client   = server.accept()
        val upstream = new Socket(host, targetPort)
        pipe(client, upstream); pipe(upstream, client)
      } catch { case _: java.io.IOException => () }
    }
    () => server.close()
  }

  private def daemon(body: => Unit): Unit = {
    val t = new Thread(() => body); t.setDaemon(true); t.start()
  }
}
