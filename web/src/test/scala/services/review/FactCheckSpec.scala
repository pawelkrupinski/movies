package services.review

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The venues' facts against the film a card puts forward: one line per distinct disagreement, and no "conflict"
 *  where the film credits a company, or the venues bill a stage relay and credit its stage director. */
class FactCheckSpec extends AnyFlatSpec with Matchers {

  private val title = "Kandydaci śmierci"
  private val venues = (1 to 46).map(i => ReviewMember(s"Cinema $i", title, Some(s"https://cinema$i/kandydaci")))

  "the disagreements" should "be one line for every venue saying the same thing, its venues listed apart" in {
    val members = venues.map(_.copy(directors = Seq("Richard Jones"))) :+ ReviewMember("Kino X", title, None, year = Some(2019))
    val film    = FilmFacts(FilmRef.tmdb(1703629), Some("Kandydaci"), Some(2027), Seq("Maciej Kozłowski"))
    val found   = FactCheck.disagreements(members, film)
    found.map(_.render) shouldBe Seq(
      "46 cinemas credit Richard Jones; Kandydaci (2027) is directed by Maciej Kozłowski",
      s"Kino X states $title is from 2019; Kandydaci (2027) is from 2027")
    found.head.venues shouldBe venues.map(_.venue)
    FactCheck.warnings(members, film) shouldBe found.map(_.render)
  }

  "a film directed by a company" should "raise no director conflict" in {
    val members = Seq(ReviewMember("Kino X", "Kandydaci śmierci", None, directors = Seq("Richard Jones")))
    for (house <- Seq("The Metropolitan Opera", "National Theatre Live", "Royal Opera House", "Berliner Philharmoniker"))
      withClue(house)(FactCheck.warnings(members, FilmFacts(FilmRef.tmdb(1), Some("Fanciulla"), None, Seq(house))) shouldBe empty)
    FactCheck.warnings(members, FilmFacts(FilmRef.tmdb(1), Some("Fanciulla"), None, Seq("Gary Halvorson"))) should not be empty
  }

  "a listing billing a stage relay" should "raise no director conflict: its venue credits the stage director" in {
    val members = Seq(ReviewMember("Kino X", "Royal Ballet and Opera 2025/26: Tosca", None, directors = Seq("Jonathan Kent")))
    FactCheck.warnings(members, FilmFacts(FilmRef.tmdb(1), Some("Tosca"), None, Seq("Rhodri Huw"))) shouldBe empty
  }
}
