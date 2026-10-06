package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityMeasures.{Film, Listing, Number}

/** TMDB keeps one record per film at its theatrical runtime; cinemas bill a cut — the extended "Return of the King",
 *  "Apocalypse Now" Final Cut or Redux — at the cut's own runtime. A cut the table names ([[FilmCuts]]) is the film. */
class FilmCutsSpec extends AnyFlatSpec with Matchers {

  private def delta(stated: Int, f: Film) = IdentityMeasures.listingFilm(Listing(f.title, runtime = Some(stated)), f, None, 0, 0)("runtime.delta")

  private val returnOfTheKing = Film("The Lord of the Rings: The Return of the King", year = Some(2003), runtime = Some(201), imdbNumber = 167260)
  private val apocalypseNow   = Film("Apocalypse Now", year = Some(1979), runtime = Some(147), imdbNumber = 78788)
  private val bladeRunner     = Film("Blade Runner", year = Some(1982), runtime = Some(117), imdbNumber = 83658)

  "a listing billing a known cut's runtime" should "be no runtime gap from the film" in {
    delta(263, returnOfTheKing) shouldBe Number(0)
    delta(183, apocalypseNow) shouldBe Number(0)
    delta(202, apocalypseNow) shouldBe Number(0)
    delta(116, bladeRunner) shouldBe Number(0)
    delta(117, bladeRunner) shouldBe Number(0)
  }

  "a runtime no cut runs" should "still be the gap to the nearest of the film's runtimes" in {
    delta(150, returnOfTheKing) shouldBe Number(51)
    delta(240, returnOfTheKing) shouldBe Number(23)
    delta(95, apocalypseNow) shouldBe Number(52)
    // a film the table names no cut of keeps its one runtime
    delta(263, Film("Some Other Film", runtime = Some(201), imdbNumber = 1)) shouldBe Number(62)
  }

  "the checked-in table" should "parse every row, each with a source to check it by" in {
    FilmCuts.table.size should be >= 5
    FilmCuts.table.values.flatten.foreach { cut =>
      withClue(cut)(cut.source should startWith("https://"))
      withClue(cut)(services.movies.FilmRuntime.plausible(cut.runtime) shouldBe true)
    }
  }
}
