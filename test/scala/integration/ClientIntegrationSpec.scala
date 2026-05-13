package integration

import clients._
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza}
import modules.CacheModule
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceApplicationBuilder

class ClientIntegrationSpec
  extends AnyFlatSpec
    with Matchers
    with ParallelTestExecution
    with GuiceOneAppPerSuite {

  override def fakeApplication() =
    new GuiceApplicationBuilder()
      .disable[CacheModule]
      .build()

  "MultikinoClient" should "fetch films" in {
    MultikinoClient.fetch() should not be empty
  }
  "CharlieMonroeClient" should "fetch films" in {
    CharlieMonroeClient.fetch() should not be empty
  }
  "KinoPalacoweClient" should "fetch films" in {
    KinoPalacoweClient.fetch() should not be empty
  }
  "HeliosClient" should "fetch films" in {
    val heliosClient = app.injector.instanceOf[HeliosClient]
    val result = heliosClient.fetch()
    result should not be empty
    result.find(_.movie.runtimeMinutes.isEmpty) shouldBe empty
    result.flatMap(_.showtimes) should not be empty
  }
  "CinemaCityClient Kinepolis" should "fetch films" in {
    CinemaCityClient.fetch("1081", CinemaCityKinepolis) should not be empty
  }
  "CinemaCityClient Plaza" should "fetch films" in {
    CinemaCityClient.fetch("1078", CinemaCityPoznanPlaza) should not be empty
  }
  "KinoMuzaClient" should "fetch films" in {
    KinoMuzaClient.fetch() should not be empty
  }
  "KinoBulgarskaClient" should "fetch films" in {
    KinoBulgarskaClient.fetch() should not be empty
  }
  "RialtoClient" should "fetch films" in {
    RialtoClient.fetch() should not be empty
  }
}
