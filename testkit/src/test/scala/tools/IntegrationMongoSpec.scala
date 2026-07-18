package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The guard that stops `sbt itAll` writing sentinel rows into prod. The cases
 *  below are the four URIs this project actually runs against — the two that must
 *  keep working matter as much as the two that must be refused, because a guard
 *  that also blocks CI just gets deleted. */
class IntegrationMongoSpec extends AnyFlatSpec with Matchers {

  private val ciDocker  = "mongodb://127.0.0.1:27017/kinowo?directConnection=true"
  private val localRs   = "mongodb://127.0.0.1:28017/?directConnection=true"
  private val prodTunnel = "mongodb://kinowo_app:s3cr3t@127.0.0.1:27017/kinowo?authSource=kinowo"
  private val atlas     = "mongodb+srv://user:s3cr3t@cluster0.abcde.mongodb.net/kinowo"

  "the throwaway-Mongo guard" should "allow CI's docker Mongo, despite it using the prod DB NAME on the prod PORT" in {
    // The reason the db name and host can't be the discriminator.
    IntegrationMongo.isProtectedCluster(ciDocker) shouldBe false
    noException should be thrownBy IntegrationMongo.requireThrowaway(ciDocker, overrideSet = false)
  }

  it should "allow the local replica set the docs tell you to use" in {
    IntegrationMongo.isProtectedCluster(localRs) shouldBe false
  }

  it should "refuse the prod tunnel — same host and port as CI, but credentialed" in {
    IntegrationMongo.isProtectedCluster(prodTunnel) shouldBe true
    val thrown = the [IllegalStateException] thrownBy
      IntegrationMongo.requireThrowaway(prodTunnel, overrideSet = false)
    thrown.getMessage should include ("Refusing to run integration tests")
    thrown.getMessage should include ("start-local-mongo.sh")   // says how to fix it
  }

  it should "refuse an Atlas SRV cluster" in {
    IntegrationMongo.isProtectedCluster(atlas) shouldBe true
  }

  it should "yield to an explicit override" in {
    noException should be thrownBy IntegrationMongo.requireThrowaway(prodTunnel, overrideSet = true)
  }

  "the refusal message" should "not leak the password into a CI transcript" in {
    val thrown = the [IllegalStateException] thrownBy
      IntegrationMongo.requireThrowaway(prodTunnel, overrideSet = false)
    thrown.getMessage should not include "s3cr3t"
    thrown.getMessage should include ("kinowo_app:***@")
  }

  it should "redact an SRV password too" in {
    IntegrationMongo.redact(atlas) should not include "s3cr3t"
  }
}
