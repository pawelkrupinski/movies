package services.tasks

import models.{KinoApollo, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The layer that stops a NAME disagreement being acted on as a wrong film.
 *
 * Comparing the strings can only ever approximate "same person", and six rounds of
 * folding, prefixes, edit distances and an alias list still left the two biggest
 * classes standing: a venue crediting the film's OTHER director (Ethan for a film
 * TMDB credits to Joel Coen; Karl Freund on the 1931 "Dracula") and a pseudonym
 * sharing no letters with the name behind it (Loriot / Vicco von Bülow). TMDB knows
 * both — the first is in the film's crew, the second is one person id — so the
 * confirmation asks TMDB rather than the strings.
 *
 * It matters because acting is not free: force-re-resolving a correct row can LOSE
 * its resolution, since a director walk on a name TMDB does not credit finds
 * nothing.
 */
class CrewConfirmationSpec extends AnyFlatSpec with Matchers {

  private def row(filmDirectors: Seq[String], cinemaDirectors: Seq[String]): MovieRecord =
    MovieRecord(tmdbId = Some(7),
      data = Map[Source, SourceData](
        Tmdb       -> SourceData(title = Some("F"), runtimeMinutes = Some(100), director = filmDirectors),
        KinoApollo -> SourceData(title = Some("F"), runtimeMinutes = Some(100), director = cinemaDirectors)))

  /** TMDB stand-in: which person ids a name can mean, and who is on the film's crew. */
  private def credits(peopleByName: Map[String, Seq[Int]], crew: Set[Int]) =
    new CrewConfirmation.Credits {
      def personIds(name: String): Seq[Int] = peopleByName.getOrElse(name, Seq.empty)
      def crewIds(tmdbId: Int): Set[Int]    = crew
    }

  "a director disagreement" should "be dismissed when the venue names someone on the film's crew" in {
    // "O Brother, Where Art Thou?" — TMDB credits Joel Coen, the venue Ethan, and
    // both are on the crew.
    val confirm = new CrewConfirmation(credits(Map("Ethan Coen" -> Seq(1224)), crew = Set(1223, 1224)))
    confirm.confirmed(row(Seq("Joel Coen"), Seq("Ethan Coen"))) shouldBe false
  }

  it should "be dismissed when the venue names the person behind a pseudonym" in {
    // TMDB credits "Loriot"; the venue names Vicco von Bülow. One person, one id.
    val confirm = new CrewConfirmation(credits(Map("Vicco von Bülow" -> Seq(4242)), crew = Set(4242)))
    confirm.confirmed(row(Seq("Loriot"), Seq("Vicco von Bülow"))) shouldBe false
  }

  it should "stand when the venue names nobody who worked on the film" in {
    // Prod's "birdman|2014": the venues screen Iñárritu's, the row resolved to a
    // Romanian film of the same name, and TMDB puts him nowhere on its crew.
    val confirm = new CrewConfirmation(
      credits(Map("Alejandro González Iñárritu" -> Seq(223)), crew = Set(1, 2, 3)))
    confirm.confirmed(row(Seq("Alexandru Mavrodineanu"), Seq("Alejandro González Iñárritu"))) shouldBe true
  }

  it should "look past the middle names TMDB does not carry" in {
    // German venues published "David Kerrick Hand" for Disney's "Snow White"; TMDB
    // knows him only as "David Hand" and answers the fuller name with NOTHING, so
    // the row sat on a 1939 German film of the same title untouched.
    val confirm = new CrewConfirmation(credits(Map("David Hand" -> Seq(5446)), crew = Set(1, 2)))
    confirm.confirmed(row(Seq("Carl Heinz Wolff"), Seq("David Kerrick Hand"))) shouldBe true
  }

  it should "not shorten across a nobiliary particle" in {
    // "Lars von Trier" is not "Lars Trier". Dropping the middle token is only safe when
    // it is a middle NAME; across a particle the shortened form names a DIFFERENT
    // person, and TMDB answering for them would confirm a contradiction that isn't
    // there — the row would be force-re-resolved off a stranger who never existed.
    val confirm = new CrewConfirmation(credits(Map("Lars Trier" -> Seq(9999)), crew = Set(1, 2)))
    confirm.confirmed(row(Seq("Someone Else"), Seq("Lars von Trier"))) shouldBe false
  }

  it should "still take TMDB's answer to the full name when it has one" in {
    // The shortening is a fallback, never an override: a name TMDB does answer is
    // answered once, by the name the venue actually published.
    val confirm = new CrewConfirmation(
      credits(Map("Anthony M. Dawson" -> Seq(7), "Anthony Dawson" -> Seq(9999)), crew = Set(7)))
    confirm.confirmed(row(Seq("Antonio Margheriti"), Seq("Anthony M. Dawson"))) shouldBe false
  }

  // Abstention again: an answer TMDB cannot give is not evidence of a wrong film.
  it should "not confirm when TMDB knows no such person" in {
    val confirm = new CrewConfirmation(credits(Map.empty, crew = Set(1, 2)))
    confirm.confirmed(row(Seq("Brian De Palma"), Seq("Some Unknown"))) shouldBe false
  }

  it should "not confirm when the film's crew cannot be read" in {
    val confirm = new CrewConfirmation(credits(Map("Howard Hawks" -> Seq(9999)), crew = Set.empty))
    confirm.confirmed(row(Seq("Brian De Palma"), Seq("Howard Hawks"))) shouldBe false
  }

  it should "leave a RUNTIME contradiction alone — it needs no confirming" in {
    val short = MovieRecord(tmdbId = Some(7),
      data = Map[Source, SourceData](
        Tmdb       -> SourceData(title = Some("F"), runtimeMinutes = Some(15)),
        KinoApollo -> SourceData(title = Some("F"), runtimeMinutes = Some(180))))
    new CrewConfirmation(credits(Map.empty, Set.empty)).confirmed(short) shouldBe true
  }
}
