package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The records a listing's own title searches found — what a record's local title may be learned from
 *  (`CorpusContext.titlesByFacts`), never a director's filmography alone. */
class SearchFoundSpec extends AnyFlatSpec with Matchers {

  "a listing's own searches" should "count IMDb's first suggestion for its title as found, and only its first" in {
    // PL "Camino dla opornych" {Yann Samuell} [2026]: TMDB holds Compostelle under no Polish title, so its own title
    // search finds nothing; IMDb's first suggestion for the title is Compostelle. Without it the local title was never
    // learned and 14 listings the old pipeline had right went unmatched.
    val camino  = IdentityMeasures.Listing("Camino dla opornych", year = Some(2026), directors = Seq("Yann Samuell"))
    val answers = (query: CandidateQuery) => query match {
      case CandidateQuery.Imdb("Camino dla opornych") => Some(Seq(1404604, 777))
      case _                                          => Some(Nil)
    }
    CorpusContext.searchFound(camino, answers) shouldBe Seq(1404604)
    // a director's filmography is still no title's search
    CorpusContext.searchFound(camino, {
      case CandidateQuery.Director(_) => Some(Seq(1404604))
      case _                          => Some(Nil)
    }) shouldBe empty
  }
}
