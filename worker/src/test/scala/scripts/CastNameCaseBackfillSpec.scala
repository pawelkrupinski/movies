package scripts

import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The decision half of [[CastNameCaseBackfill]] — which persisted rows the
 * backfill rewrites and which it leaves alone. The Mongo half is thin plumbing
 * over `SlotsRepository` / `MovieRepository` / `ReadModelRepository`, each
 * already covered by its own specs; what this script contributes is the
 * "rewrite ONLY when the value actually changes" rule, which is the difference
 * between a targeted repair and a churn of the whole corpus through the change
 * stream.
 */
class CastNameCaseBackfillSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def resolved(cast: Seq[String]): ResolvedMovie = ResolvedMovie(
    _id = "minions 3|2026", title = "Minions 3", originalTitle = None, posterUrl = None,
    fallbackPosterUrls = Seq.empty, runtimeMinutes = None, releaseYear = Some(2026),
    genres = Seq.empty, countries = Seq.empty, directors = Seq.empty, cast = cast,
    synopsis = None, trailerUrls = Seq.empty,
    ratings = ResolvedRatings(imdb = None, imdbUrl = None, metascore = None, metacriticUrl = "",
      rottenTomatoes = None, rottenTomatoesUrl = "", filmweb = None, filmwebUrl = ""),
    weightedRating = 0.0)

  behavior of "CastNameCaseBackfill.recased (a cinema slot)"

  it should "capitalise a lowercase cast list" in {
    val fixed = CastNameCaseBackfill.recased(SourceData(cast = Seq("christoph waltz", "jeff bridges")))
    fixed.map(_.cast) shouldBe Some(Seq("Christoph Waltz", "Jeff Bridges"))
  }

  it should "report NO change for an already-cased list, so the row is never written" in {
    CastNameCaseBackfill.recased(SourceData(cast = Seq("Christoph Waltz", "Ludwig van Beethoven"))) shouldBe None
    CastNameCaseBackfill.recased(SourceData(cast = Seq.empty)) shouldBe None
  }

  it should "rewrite only the lowercase members of a mixed list" in {
    val before = SourceData(cast = Seq("Sandra Bullock", "christoph waltz", "KARL URBAN"))
    // KARL URBAN is left as-is: PersonName deliberately doesn't touch ALL CAPS.
    CastNameCaseBackfill.recased(before).map(_.cast) shouldBe
      Some(Seq("Sandra Bullock", "Christoph Waltz", "KARL URBAN"))
  }

  it should "leave every other field of the slot untouched" in {
    val before = SourceData(title = Some("Minions 3"), synopsis = Some("Blurb"),
      director = Seq("pierre coffin"), cast = Seq("christoph waltz"), releaseYear = Some(2026))
    val after  = CastNameCaseBackfill.recased(before).value
    after shouldBe before.copy(cast = Seq("Christoph Waltz"))
    // director is out of scope — no source ships a lowercase one (see the script doc).
    after.director shouldBe Seq("pierre coffin")
  }

  behavior of "CastNameCaseBackfill.recased (a movies record)"

  it should "recase every source's slot in one write, and skip a record with nothing to fix" in {
    // One source shipped the names lowercase (the Flicks shape), another cased.
    val record = MovieRecord(data = Map[Source, SourceData](
      Imdb -> SourceData(cast = Seq("christoph waltz")),
      Tmdb -> SourceData(cast = Seq("Christoph Waltz"))))

    val fixed = CastNameCaseBackfill.recased(record).value
    fixed.data(Imdb).cast shouldBe Seq("Christoph Waltz")
    fixed.data(Tmdb).cast shouldBe Seq("Christoph Waltz")

    CastNameCaseBackfill.recased(fixed) shouldBe None
  }

  behavior of "CastNameCaseBackfill.recased (a read-model document)"

  it should "capitalise a lowercase web_movies cast" in {
    CastNameCaseBackfill.recased(resolved(Seq("christoph waltz"))).map(_.cast) shouldBe Some(Seq("Christoph Waltz"))
  }

  it should "report NO change for an already-cased web_movies cast" in {
    CastNameCaseBackfill.recased(resolved(Seq("Christoph Waltz", "Andie MacDowell"))) shouldBe None
  }

  it should "be idempotent — a second pass finds nothing to do" in {
    val once = CastNameCaseBackfill.recased(resolved(Seq("christoph waltz", "peter o'toole"))).value
    CastNameCaseBackfill.recased(once) shouldBe None
  }
}
