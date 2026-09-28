package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Evidence, IdentityMeasures}
import IdentityReferee.Verdict

/** The benchmark's absolute referee on the cases 2026-09-28's comparison turned on: old-pipeline
 *  matches the listings' own facts contradict, and resolver matches that only LOOKED contradicted. */
class IdentityRefereeSpec extends AnyFlatSpec with Matchers {

  private def ev(title: String, year: Option[Int] = None, directors: Seq[String] = Nil, runtime: Option[Int] = None,
                 original: Option[String] = None) = Evidence(title, title, title, year, directors, runtime, original)
  private def film(title: String, year: Int, directors: Seq[String] = Nil, runtime: Option[Int] = None, original: Option[String] = None) =
    IdentityMeasures.Film(title, original, Nil, Some(year), runtime, Some(directors), None, None)

  "the referee" should "judge a season broadcast filed under an old film of its work wrong" in {
    IdentityReferee.judge(ev("RBO Cinema Season 2026-27: The Nutcracker", runtime = Some(140)),
      film("The Nutcracker", 1985, runtime = Some(99)))._1 shouldBe Verdict.Wrong
  }

  it should "judge another house's production wrong, even of the same year" in {
    IdentityReferee.judge(ev("MetOpera: Carmen (2009)"), film("CARMEN (2009) Opéra Comique", 2009))._1 shouldBe Verdict.Wrong
    IdentityReferee.judge(ev("MET Opera Live im Kino: Macbeth"), film("National Theatre Live: Macbeth", 2024))._1 shouldBe Verdict.Wrong
  }

  it should "judge a film another director made, far from the listing's runtime, wrong" in {
    IdentityReferee.judge(ev("Fallen Angels by Noël Coward", directors = Seq("Sean Foley"), runtime = Some(150)),
      film("Fallen Angels", 1995, Seq("Wong Kar-wai"), Some(99)))._1 shouldBe Verdict.Wrong
  }

  it should "not judge wrong a director written family name first, or in another script" in {
    IdentityReferee.judge(ev("Kura", Some(2026), Seq("György Pálfi"), Some(96)), film("Kura", 2026, Seq("Pálfi György"), Some(96)))._1 should not be Verdict.Wrong
    IdentityReferee.judge(ev("Resurrection", Some(2025), Seq("Bi Gan"), Some(160)), film("Resurrection", 2025, Seq("毕赣"), Some(160)))._1 should not be Verdict.Wrong
  }

  it should "not judge wrong an old film a festival lists under its own year, with an English original title" in {
    IdentityReferee.judge(ev("Labirynt fauna | Splat!FilmFest", Some(2026), Seq("Guillermo del Toro"), Some(118), Some("Pan's Labyrinth")),
      film("Labirynt fauna", 2006, Seq("Guillermo del Toro"), Some(118), Some("El laberinto del fauno")))._1 should not be Verdict.Wrong
  }

  it should "read a year the original title writes, and not call the film of another year right" in {
    val (v, denials) = IdentityReferee.judge(ev("The Royal Ballet: The Nutcracker", original = Some("The Royal Ballet: The Nutcracker (2024)")),
      film("The Royal Ballet: The Nutcracker", 2015))
    denials should contain("originalTitleYear")
    v should not be Verdict.Right
  }

  it should "judge a match its facts back right" in {
    IdentityReferee.judge(ev("Lalka", Some(2026), Seq("Maciej Kawalski")), film("Lalka", 2026, Seq("Maciej Kawalski"), Some(162)))._1 shouldBe Verdict.Right
  }
}
