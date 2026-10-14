package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityMeasures, ScreeningDays}

import java.time.LocalDate

/** The film a cluster's venues list on Filmweb: each listing named by the one film of its venue's programme that bears
 *  its title on its days, half the listings at least by the same one and none by another (PL cases of 2026-10-05,
 *  Filmweb's programmes then). */
class VenueListingsSpec extends AnyFlatSpec with Matchers {
  private def days(ds: String*) = ScreeningDays.of(ds.map(LocalDate.parse))
  private def screening(venue: models.Cinema, title: String, on: String*) = FilmTable.listing(venue, title).copy(screenings = days(on*))
  private def film(title: String, original: Option[String], year: Int, director: String) =
    SourceRecord(IdentityMeasures.Film(title, original, Nil, Some(year), None, Some(Seq(director))), Map("filmweb" -> "_"))

  private val brokenVoices = film("Dyrygent", Some("Sbormistr"), 2025, "Ondřej Provazník")
  private val lalka        = film("Lalka", None, 2026, "Maciej Kawalski")
  private val hasLalka     = film("Lalka", None, 1968, "Wojciech Has")
  private val dolly        = film("Lalka", Some("Dolly"), 2025, "Rod Blackhurst")
  private val records      = Map("10085635" -> brokenVoices, "10057628" -> lalka, "1174" -> hasLalka, "10094291" -> dolly)
  private def filmweb(programmes: (String, Seq[Showing])*) = new HeldFamilyAnswers(VoterFamily.Filmweb, records, programmes = programmes.toMap)
  private def named(id: String) = Answer.Known(Some(records(id).copy(crossIds = Map("filmweb" -> id))))

  "A cluster's listings" should "name the one film their venue's programme bears under their title, on their days" in {
    // PL Kozienicki Dom Kultury's bare "Dyrygent" on 7 October: Filmweb's Kozienice programme screens Broken Voices that day
    val kozienice = filmweb(KinoMuza.displayName -> Seq(Showing("10085635", days("2026-10-07")), Showing("10057628", days("2026-10-05", "2026-10-07"))))
    VenueListings.listed(Seq(screening(KinoMuza, "Dyrygent", "2026-10-07")), kozienice) shouldBe named("10085635")
    // on a day the programme does not screen it, nothing
    VenueListings.listed(Seq(screening(KinoMuza, "Dyrygent", "2026-10-08")), kozienice) shouldBe Answer.Known(None)
  }

  it should "tell two namesakes on the same days apart only by a title one of them alone bears" in {
    // PL Kino Echo screens both Kawalski's "Lalka" and Blackhurst's (Filmweb's "Lalka", originally "Dolly")
    val echo = filmweb(KinoMuza.displayName -> Seq(Showing("10057628", days("2026-10-05")), Showing("10094291", days("2026-10-05"))))
    VenueListings.listed(Seq(screening(KinoMuza, "Lalka", "2026-10-05")), echo) shouldBe Answer.Known(None)
    VenueListings.listed(Seq(screening(KinoMuza, "Lalka (Dolly)", "2026-10-05")), echo) shouldBe named("10094291")
    // Kino za Rogiem's "Lalka" on 6 October is the one film of that name its programme screens then: Has's
    val zaRogiem = filmweb(KinoMuza.displayName -> Seq(Showing("1174", days("2026-10-06")), Showing("10057628", days("2026-10-08"))))
    VenueListings.listed(Seq(screening(KinoMuza, "Lalka", "2026-10-06")), zaRogiem) shouldBe named("1174")
  }

  it should "name the film half of them at least are named by, the rest named by none, and none where two are named" in {
    // PL Kino Seniora's "Ktoś całkiem obcy" at three venues the 2024 film, pooled with Kino Kryterium's past screening
    val seniora = filmweb(KinoMuza.displayName -> Seq(Showing("1174", days("2026-10-06"))), Multikino.displayName -> Nil)
    val pooled  = Seq(screening(KinoMuza, "Lalka", "2026-10-06"), screening(Multikino, "Lalka", "2026-10-02"))
    VenueListings.listed(pooled, seniora) shouldBe named("1174")
    // one venue's programme speaks for no cluster of many
    VenueListings.listed(pooled :+ screening(Multikino, "Lalka", "2026-10-03"), seniora) shouldBe Answer.Known(None)
    // PL "Dyrygent": Kino Marzenie's (Wajda's) pooled with Kozienice's Broken Voices, each named by its own
    val two = filmweb(KinoMuza.displayName -> Seq(Showing("10085635", days("2026-10-07"))), Multikino.displayName -> Seq(Showing("1174", days("2026-10-08"))))
    VenueListings.listed(Seq(screening(KinoMuza, "Dyrygent", "2026-10-07"), screening(Multikino, "Lalka", "2026-10-08")), two) shouldBe Answer.Known(None)
    VenueListings.listed(Nil, two) shouldBe Answer.Known(None)
  }

  it should "be unknown while a venue's programme or the record of a film on its days is not answered yet" in {
    VenueListings.listed(Seq(screening(KinoMuza, "Dyrygent", "2026-10-07")), filmweb()) shouldBe Answer.Unknown
    val noRecords = new HeldFamilyAnswers(VoterFamily.Filmweb, Map.empty, programmes = Map(KinoMuza.displayName -> Seq(Showing("10085635", days("2026-10-07")))))
    val unrecorded = new FamilyAnswers {
      val family: VoterFamily = VoterFamily.Filmweb
      def titled(text: String)     = noRecords.titled(text)
      def directedBy(name: String) = noRecords.directedBy(name)
      def record(id: String): Answer[Option[SourceRecord]] = Answer.Unknown
      override def showing(venue: String) = noRecords.showing(venue)
    }
    VenueListings.listed(Seq(screening(KinoMuza, "Dyrygent", "2026-10-07")), unrecorded) shouldBe Answer.Unknown
  }
}
