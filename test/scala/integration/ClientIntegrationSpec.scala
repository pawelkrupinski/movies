package integration

import clients._
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie}
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
    assertAllHaveRuntime(MultikinoClient.fetch())
  }
  "CharlieMonroeClient" should "fetch films" in {
    assertAllHaveRuntime(CharlieMonroeClient.fetch())
  }
  "KinoPalacoweClient" should "fetch films" in {
    assertAllHaveRuntime(KinoPalacoweClient.fetch())
  }
  "HeliosClient" should "fetch films" in {
    val heliosClient = app.injector.instanceOf[HeliosClient]
    assertAllHaveRuntime(heliosClient.fetch())
  }

  // Cinema City lists upcoming films before their duration is published — e.g.
  // "Sabotażysta 5" returns "length":null until the runtime is confirmed and
  // the public film page literally shows "Czas niepotwierdzony". We keep these
  // entries (they still have valid screenings and posters) and require only
  // two-thirds of the catalogue to have a known runtime.
  "CinemaCityClient Kinepolis" should "fetch films" in {
    assertMostHaveRuntime(CinemaCityClient.fetch("1081", CinemaCityKinepolis))
  }

  "CinemaCityClient Plaza" should "fetch films" in {
    assertMostHaveRuntime(CinemaCityClient.fetch("1078", CinemaCityPoznanPlaza))
  }

  "KinoMuzaClient" should "fetch films" in {
    assertAllHaveRuntime(KinoMuzaClient.fetch())
  }

  "KinoBulgarskaClient" should "fetch films" in {
    assertAllHaveRuntime(KinoBulgarskaClient.fetch())
  }

  "RialtoClient" should "fetch films" in {
    assertAllHaveRuntime(RialtoClient.fetch())
  }

  private def assertAllHaveRuntime(result: Seq[CinemaMovie]) = {
    assertBaseShape(result)
    result.find(_.movie.runtimeMinutes.isEmpty) shouldBe empty
  }

  private def assertMostHaveRuntime(result: Seq[CinemaMovie]) = {
    assertBaseShape(result)
    val withRuntime = result.count(_.movie.runtimeMinutes.nonEmpty)
    withClue(s"only $withRuntime of ${result.size} movies have a runtime: ") {
      (withRuntime * 3) should be >= (result.size * 2)
    }
  }

  private def assertBaseShape(result: Seq[CinemaMovie]) = {
    result                      should not be empty
    result.flatMap(_.showtimes) should not be empty
  }
}
