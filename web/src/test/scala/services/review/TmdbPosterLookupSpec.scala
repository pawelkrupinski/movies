package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.PosterAnswers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

/** A review card's TMDB poster looked up live (dev only), over TMDB's recorded answer for "Howl's Moving Castle" (4935). */
class TmdbPosterLookupSpec extends AnyFlatSpec with Matchers {
  import TmdbPosterLookupSpec._

  "a film's TMDB poster" should "be its poster_path at the size the review cards show, asked of TMDB once" in {
    val fetch  = http
    val lookup = new TmdbPosterLookup(fetch, settings.TmdbApiKey("k"))
    lookup.poster(4935, Locale.forLanguageTag("pl-PL")) shouldBe Some(HowlsPoster)
    lookup.poster(4935, Locale.forLanguageTag("pl-PL"))
    fetch.calls.map(_._2) shouldBe Seq("https://api.themoviedb.org/3/movie/4935?language=pl-PL&api_key=k")
  }

  it should "be none for a film TMDB no longer has" in {
    lookup.poster(1575247, Locale.UK) shouldBe None
  }
}

object TmdbPosterLookupSpec {
  /** TMDB's recorded `/movie/4935` answer ("Howl's Moving Castle"). */
  val HowlsAnswer: String =
    new String(Files.readAllBytes(Paths.get(getClass.getResource("/review/tmdb-movie-4935.json").toURI)), StandardCharsets.UTF_8)
  /** TMDB answering `/movie/4935` as recorded, every other film 404. */
  def http: tools.RoutingHttpFetch =
    new tools.RoutingHttpFetch(Seq("/3/movie/4935?" -> HowlsAnswer), getOnly = true, unroutedIsNotFound = true)
  def lookup: TmdbPosterLookup = new TmdbPosterLookup(http, settings.TmdbApiKey("k"))
  val HowlsPoster = s"${PosterAnswers.FilmPosterBase}/13kOl2v0nD2OLbVSHnHk8GUFEhO.jpg"
}
