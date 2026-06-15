package clients.helios

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosNuxt

import java.io.File
import scala.io.{Codec, Source}

/** The Helios NUXT repertoire blob carries `titleOriginal` next to each film's
 *  `title`/`slug`. `buildMovies` lifts it onto `Movie.originalTitle` as a TMDB
 *  resolution hint, RAW (the "(re-release)" decoration is stripped downstream,
 *  not here). Fixture recorded from helios.pl/poznan/kino-helios/repertuar. */
class HeliosNuxtOriginalTitleSpec extends AnyFlatSpec with Matchers {

  private val html: String = {
    val source = Source.fromFile(
      new File("test/resources/fixtures/08-06-2026/helios.pl/poznan/kino-helios/repertuar"))(using Codec.UTF8)
    try source.mkString finally source.close()
  }

  private val byTitle =
    HeliosNuxt.buildMovies(html, HeliosNuxt.Poznan).map(cm => cm.movie.title -> cm).toMap

  "HeliosNuxt.buildMovies" should "set originalTitle from the NUXT titleOriginal field, kept raw" in {
    byTitle.get("Pianista").flatMap(_.movie.originalTitle) shouldBe Some("The Pianist (re-release)")
  }
}
