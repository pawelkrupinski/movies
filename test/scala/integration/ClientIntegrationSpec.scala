package integration

import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie}
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CharlieMonroeClient, CinemaCityClient, HeliosClient, KinoBulgarskaClient, KinoMuzaClient, KinoPalacoweClient, MultikinoClient, RialtoClient}

class ClientIntegrationSpec
  extends AnyFlatSpec
    with Matchers
    with ParallelTestExecution {

  "MultikinoClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new MultikinoClient().fetch()) }
  }

  "CharlieMonroeClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new CharlieMonroeClient().fetch()) }
  }

  "KinoPalacoweClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new KinoPalacoweClient().fetch()) }
  }

  "HeliosClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new HeliosClient().fetch()) }
  }

  // Cinema City lists upcoming films before their duration is published — e.g.
  // "Sabotażysta 5" returns "length":null until the runtime is confirmed and
  // the public film page literally shows "Czas niepotwierdzony". We keep these
  // entries (they still have valid screenings and posters) and require only
  // two-thirds of the catalogue to have a known runtime.
  "CinemaCityClient Kinepolis" should "fetch films" in {
    RetryWithBackoff() { assertMostHaveRuntime(new CinemaCityClient().fetch("1081", CinemaCityKinepolis)) }
  }

  "CinemaCityClient Plaza" should "fetch films" in {
    RetryWithBackoff() { assertMostHaveRuntime(new CinemaCityClient().fetch("1078", CinemaCityPoznanPlaza)) }
  }

  "KinoMuzaClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new KinoMuzaClient().fetch()) }
  }

  "KinoBulgarskaClient" should "fetch films" in {
    // Most films have a runtime, but the cinema occasionally lists
    // preview-screening events ("pokazy przedpremierowe") that don't include
    // one. Don't drop them — they're still real screenings.
    RetryWithBackoff() { assertMostHaveRuntime(new KinoBulgarskaClient().fetch()) }
  }

  "RialtoClient" should "fetch films" in {
    RetryWithBackoff() { assertAllHaveRuntime(new RialtoClient().fetch()) }
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
