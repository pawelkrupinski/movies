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

  "a director spelt in another language or alphabet" should "be the same person, not a conflict" in {
    def warned(stated: String, credited: String) =
      FactCheck.warnings(Seq(ReviewMember("Kino X", "Dom durniv", None, directors = Seq(stated))),
        FilmFacts(FilmRef.tmdb(1), Some("Dom durniv"), None, Seq(credited)))
    for ((stated, credited) <- Seq(
           "Andrei Konchalovsky" -> "Andrei Kontschalowski", "Andriej Konczałowski" -> "Andrei Kontschalowski",
           "Konchalovsky" -> "Kontschalowski", "Kontschalowski" -> "Andrei Konchalovsky",
           "Bolshynska" -> "Большинська", "Iryna Bolshynska" -> "Ірина Большинська", "Большинська" -> "Bolshynska"))
      withClue(s"$stated / $credited")(warned(stated, credited) shouldBe empty)
    warned("Andrei Konchalovsky", "Andrzej Wajda") should not be empty
    warned("Bolshynska", "Сергій Лозниця") should not be empty
  }

  "a re-release" should "raise no year conflict: the venue states its re-release year, the film its original" in {
    val film = FilmFacts(FilmRef.tmdb(770), Some("Gone with the Wind"), Some(1939), Seq("Victor Fleming"))
    def warned(raw: String, year: Int) = FactCheck.warnings(Seq(ReviewMember("Kino X", raw, None, year = Some(year))), film)
    warned("Przeminęło z wiatrem (re-release)", 2026) shouldBe empty
    warned("Przeminęło z wiatrem – wersja odrestaurowana 4K", 2026) shouldBe empty
    warned("Przeminęło z wiatrem (1939)", 2026) shouldBe empty
    warned("Przeminęło z wiatrem", 2026) should not be empty
    // A venue stating a year BEFORE the film's own is no re-release.
    warned("Przeminęło z wiatrem (re-release)", 1925) should not be empty
  }

  "an NT Live or opera relay" should "raise no director conflict: the venue credits the stage director, the film the screen one" in {
    def warned(raw: String, stated: String, screen: String) =
      FactCheck.warnings(Seq(ReviewMember("Kino X", raw, None, directors = Seq(stated))), FilmFacts(FilmRef.tmdb(1), Some(raw), None, Seq(screen)))
    warned("NT Live: Hamlet", "Robert Hastie", "Tim van Someren") shouldBe empty
    warned("National Theatre Live: The Importance of Being Earnest", "Max Webster", "Tim van Someren") shouldBe empty
    warned("Met Opera: Aida", "Michael Mayer", "Gary Halvorson") shouldBe empty
    warned("Royal Opera House: La traviata", "Richard Eyre", "Peter Jones") shouldBe empty
    warned("Aftersun", "Robert Hastie", "Tim van Someren") should not be empty
  }
}
