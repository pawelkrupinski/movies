package services.identity.agreement

import models.{KinoMuza, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, Listing, Resolution, ResolverDecision, ScreeningDays}
import services.movies.SingleCountryNormalizer

import java.time.LocalDate

/** The broadcast date join on the agreement's way to the projection: a cluster billing a stage work that screens on the
 *  day one record of it was broadcast takes that record; an encore, only when its title bills the house or season. */
class AgreementBroadcastSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer

  private val metSamson = FilmTable.F(1703624, "The Metropolitan Opera 2026/27: Samson et Dalila", 2026, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2026, 12, 5)))
  private val metMacbeth = FilmTable.F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2026, 10, 17)))
  private val metFanciulla = FilmTable.F(1703629, "The Metropolitan Opera 2026/27: La Fanciulla del West", 2027, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2027, 1, 23)))
  private val rboSamson = FilmTable.F(1800001, "Royal Ballet & Opera 2026/27: Samson et Dalila", 2027, "", 0, popularity = 1.0,
    released = Some(LocalDate.of(2027, 3, 2)))
  private val film1949 = FilmTable.F(29993, "Samson and Delilah", 1949, "Cecil B. DeMille", 131, alternatives = Seq("Samson i Dalila"),
    released = Some(LocalDate.of(1949, 12, 21)))
  private val table = new FilmTable(Seq(metSamson, metMacbeth, metFanciulla, rboSamson, film1949), normalizer)

  private def silentFamilies = VoterFamily.values.map(family => family -> new HeldFamilyAnswers(family, Map.empty)).toMap[VoterFamily, FamilyAnswers]
  private def screening(title: String, days: String*): Listing =
    FilmTable.listing(Multikino, title).copy(screenings = ScreeningDays.of(days.map(LocalDate.parse)))

  private def decided(listing: Listing): ResolverDecision = {
    val resolution = Resolution(Seq(ResolverDecision(Seq(listing.key), None, 0.1, ResolverDecision.Basis.BelowThreshold, Nil)()),
      1, Map(listing.key -> 0), Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    new AgreementStage(silentFamilies, table, normalizer, IdentityCalibration.resolver, tmdbOf = _ => Answer.Known(None), new InMemoryAgreementVerdicts,
      clock = _root_.tools.SpecClock.Pinned, tmdb = Some(table)).apply(resolution, Map(listing.key -> listing).get, version = 1).decisions.head
  }

  "a stage work screening on a production's broadcast day" should "take that production, whatever its title leaves out" in {
    // PL Kino Amok's "Samson i Dalila" on 5 December 2026: the Met's live broadcast, not DeMille's 1949 film nor RBO's staging
    val taken = decided(screening("Samson i Dalila", "2026-12-05"))
    (taken.film, taken.basis) shouldBe ((Some(metSamson.id), ResolverDecision.Basis.Broadcast))
    taken.explanation.last should include ("2026-12-05")
    // Kino Powiśle bills it run into the house's word
    decided(screening("Opera-samson i dalila", "2026-12-05")).film shouldBe Some(metSamson.id)
  }

  it should "take none on another day, unless the title bills the production's house or season" in {
    // a bare title days after the broadcast says nothing of which staging it is
    decided(screening("Samson i Dalila", "2026-12-26")).film shouldBe None
    // PL Kino Kijów's retransmission three weeks on, its title naming the season
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2026-12-26")).film shouldBe Some(metSamson.id)
    // …but none long after the broadcast, nor before it
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2027-06-26")).film shouldBe None
    decided(screening("OPERA 2026/2027 - SAMSON I DALILA- RETRANSMISJA", "2026-12-01")).film shouldBe None
  }

  it should "take none for a title billing another house, though the work and day are the record's" in {
    // US Regal's "Opéra National de Paris: La fanciulla del West" on the Met's broadcast day shares only "opera" with it
    decided(screening("Opéra National de Paris: La fanciulla del West", "2027-01-23")).film shouldBe None
    decided(screening("The Metropolitan Opera: La Fanciulla del West", "2027-01-23")).film shouldBe Some(metFanciulla.id)
  }

  it should "take none against a fact the listing states, nor for a title billing no stage work" in {
    val dated = FilmTable.listing(KinoMuza, "Samson i Dalila", year = Some(1949)).copy(screenings = ScreeningDays.of(Seq(LocalDate.of(2026, 12, 5))))
    decided(dated).basis should not be ResolverDecision.Basis.Broadcast
    decided(screening("Lalka", "2026-12-05")).film shouldBe None
  }
}
