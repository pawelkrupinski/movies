package integration

import models.{Helios, KinoMuza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{IdentityMeasures, Listing}
import services.movies.ListingKey

import java.nio.file.Files

/** The booted-pipeline cache the shadow run measures resolver variants against: what one boot
 *  wrote is what every later run reads, listing by listing. */
class IdentityPipelineCacheSpec extends AnyFlatSpec with Matchers {

  import IdentityShadow.{BootedPipeline, PipelineFilm}

  private def listing(venue: models.Cinema, title: String, year: Option[Int], director: Option[String]): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director.toSeq), title, title, title, year,
      director.toSeq, None, None, None)

  private val lalka  = listing(Helios, "Lalka", Some(2025), Some("Maciej Wojtyszko"))
  private val bare   = listing(KinoMuza, "Lalka", None, None)
  private val staged = listing(KinoMuza, "Nowy film", None, None)

  "a booted pipeline" should "read back as it was written: its films, each listing's film and the boot's cost" in {
    val booted = BootedPipeline(
      Seq(PipelineFilm("f1", Some(1276100), Some(IdentityMeasures.Film("Lalka", Some("Lalka"), Nil, Some(2025), Some(98),
          Some(Seq("Maciej Wojtyszko")), None)), Nil),
        PipelineFilm("f2", None, None, Nil)),
      Map(lalka.key -> 0, bare.key -> 0), seconds = 182.5, requests = 54143L, unanswerable = 2681L)
    val path = Files.createTempDirectory("pipeline-cache").resolve("full-pl.json.gz")
    BootedPipeline.write(path, booted)
    val read = BootedPipeline.read(path, Seq(lalka, bare, staged))
    read shouldBe booted
    read.filmOf.get(staged.key) shouldBe None
  }

  it should "refuse a cache naming a listing the corpus does not list: it was booted over another corpus" in {
    val path = Files.createTempDirectory("pipeline-cache").resolve("full-pl.json.gz")
    BootedPipeline.write(path, BootedPipeline(Seq(PipelineFilm("f1", None, None, Nil)), Map(lalka.key -> 0), 1.0, 1L, 0L))
    an[IllegalStateException] should be thrownBy BootedPipeline.read(path, Seq(bare))
  }
}
