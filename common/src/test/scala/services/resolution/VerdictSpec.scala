package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerdictSpec extends AnyFlatSpec with Matchers {

  private def evidence(directors: Seq[String] = Nil, runtimes: Seq[Int] = Nil): FilmEvidence =
    FilmEvidence.empty.copy(directors = directors, runtimes = runtimes)

  "Verdict" should "let an agreeing credit settle a runtime mismatch — a short film in a longer slot is still the film" in {
    // Almodóvar's 30-minute "The Human Voice" advertised at 90 with a Q&A.
    Verdict.of(evidence(Seq("Pedro Almodovar"), Seq(90)), Candidate(1, runtime = Some(30), crew = Seq("Pedro Almodóvar"))) shouldBe
      Verdict.Accept(Support.Crew)
  }

  it should "accept a venue that credits the writer, since cinemas print either" in {
    Verdict.of(evidence(Seq("Simon Farnaby")), Candidate(1, crew = Seq("Ben Gregor", "Simon Farnaby"))) shouldBe
      Verdict.Accept(Support.Crew)
  }

  it should "deny a candidate a category shorter than every venue's minutes" in {
    // "Vivaldi i ja": an 18-minute concert short against 46 venues advertising 110.
    Verdict.of(evidence(runtimes = Seq(110, 112)), Candidate(1, runtime = Some(18))) shouldBe
      Verdict.Reject(Contradiction.Runtime)
  }

  it should "let the minutes deny before a name does, when the names disagree" in {
    Verdict.of(evidence(Seq("Someone Else"), Seq(180)), Candidate(1, runtime = Some(15), crew = Seq("Another Person"))) shouldBe
      Verdict.Reject(Contradiction.Runtime)
  }

  it should "deny on a credited name that matches nobody on the crew" in {
    Verdict.of(evidence(Seq("Michel Franco"), Seq(98)), Candidate(1, runtime = Some(100), crew = Seq("Dag Johan Haugerud"))) shouldBe
      Verdict.Reject(Contradiction.Director)
  }

  it should "accept on compatible minutes when nobody is credited on either side" in {
    Verdict.of(evidence(runtimes = Seq(105)), Candidate(1, runtime = Some(102))) shouldBe Verdict.Accept(Support.Runtime)
  }

  it should "abstain when the venues published nothing comparable" in {
    Verdict.of(evidence(), Candidate(1, runtime = Some(102), crew = Seq("Someone"))) shouldBe Verdict.Insufficient
    Verdict.of(evidence(Seq("Someone"), Seq(100)), Candidate(1)) shouldBe Verdict.Insufficient
  }

  it should "compare a CJK credit as nothing rather than as a stranger" in {
    // "王家衛" and "Wong Kar Wai" are the same person; nothing here can know it,
    // so the runtime decides and the name does not deny.
    Verdict.of(evidence(Seq("王家衛"), Seq(100)), Candidate(1, runtime = Some(100), crew = Seq("Wong Kar Wai"))) shouldBe
      Verdict.Accept(Support.Runtime)
  }

  it should "read a row's own Tmdb slot as the candidate it is resolved to" in {
    val slot = models.SourceData(title = Some("Dreams"), originalTitle = Some("Drømmer"), englishTitle = Some("Dreams"),
      director = Seq("Dag Johan Haugerud"), runtimeMinutes = Some(110), releaseYear = Some(2024), cast = Seq("Ella Øverbye"))
    Candidate.fromSlot(7, slot) shouldBe Candidate(7, Set("Dreams", "Drømmer"), Some(2024), Some(110), Seq("Dag Johan Haugerud"), Seq("Ella Øverbye"))
  }
}
