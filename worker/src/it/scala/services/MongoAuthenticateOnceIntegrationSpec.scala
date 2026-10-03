package services

import com.mongodb.event.{CommandListener, CommandSucceededEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A boot's first reads open the whole pool at once, and every connection that finds the client's
 * credential cache empty derives the password key itself (25 SCRAM derivations, ~4 s of CPU, in a
 * us worker's first minute). [[MongoConnection.authenticateOnce]] completes one authenticated round
 * trip first, so the cache is filled before the pool opens — and never stalls a boot past its bound.
 */
class MongoAuthenticateOnceIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "authenticateOnce" should "complete a round trip on the fresh client before it returns" in {
    val succeeded = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandSucceeded(event: CommandSucceededEvent): Unit = succeeded.add(event.getCommandName)
      })
      .build())
    try {
      MongoConnection.authenticateOnce(client)
      succeeded.asScala.toSeq should contain ("ping")
    } finally client.close()
  }

  it should "give up within its bound when the server is out of reach, and not throw" in {
    val client  = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=60000")
    val started = System.nanoTime()
    try MongoConnection.authenticateOnce(client, 2.seconds)
    finally client.close()
    (System.nanoTime() - started).nanos should be < 10.seconds
  }
}
