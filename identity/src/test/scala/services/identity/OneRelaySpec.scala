package services.identity

import java.time.LocalDate

import models.{Helios, KinoApollo, KinoMuza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

import FilmTable.{F, listing}

/**
 * One stage house's relay of one work in one season is ONE film however each venue spells the house's banner beside the
 * work ([[RelayStage.oneRelay]], the `same-relay` must-link): the 2026-11-13 corpus audit found the Paris Opera's
 * "La Bayadère" as ten UK films ("Opéra de Paris:", "Ballet -", "The Opéra National de Paris:" …), "Notre-Dame de Paris"
 * as six, PL "Jezioro łabędzie" and "Romeo i Julia" as four each — TMDB holds no record of them, so no film joined their
 * spellings, and their titles share no form. Two houses' relays of one work, or two seasons' of one house's, stay apart.
 */
class OneRelaySpec extends AnyFlatSpec with Matchers {

  private def on(l: Listing, day: LocalDate): Listing = l.copy(screenings = ScreeningDays.of(Seq(day)))
  private val ParisDay = LocalDate.of(2027, 1, 31)
  private val RoyalDay = LocalDate.of(2027, 2, 9)
  private def measured(l: Listing) = Evidence.of(l, None, TitleDecorations.resolver).measured

  "One house's relay of one work in one season" should "be one relay whatever banner each venue spells the house by" in {
    val spellings = Seq("Opéra De Paris Ballet - La Bayadère", "La Bayadère (Opéra de Paris)", "Opéra de Paris: La Bayadère",
      "The Opéra National de Paris: La Bayadère", "Opera National De Paris Ballet: La Bayadere", "La Bayadère - Opéra De Paris Ballet Season")
      .map(title => measured(on(listing(KinoMuza, title), ParisDay)))
    for (a <- spellings; b <- spellings) withClue(s"${a.title} | ${b.title}")(RelayStage.oneRelay(a, b) shouldBe true)
  }

  it should "be one relay in Polish, its house translated" in {
    val day = LocalDate.of(2026, 12, 12)
    val spellings = Seq("Balet z Opery Paryskiej: Jezioro łabędzie", "Jezioro łabędzie z opéra national de paris",
      "Jezioro łabędzie | Paris Opera Ballet In Cinemas").map(title => measured(on(listing(KinoMuza, title), day)))
    for (a <- spellings; b <- spellings) withClue(s"${a.title} | ${b.title}")(RelayStage.oneRelay(a, b) shouldBe true)
  }

  it should "be one relay in German, the work in German" in {
    val day = LocalDate.of(2027, 3, 14)
    val spellings = Seq("Paris Opera: Der Glöckner von Notre-Dame", "Der Glöckner von Notre-Dame (Opera national de Paris)",
      "Ballett aus der Oper Paris: Der Glöckner von Notre-Dame (2027)", "Paris Opera Ballett: Der Glöckner von Notre Dame")
      .map(title => measured(on(listing(KinoMuza, title), day)))
    withClue(spellings.map(l => s"${l.title}: ${RelayStage.relayKeys(l)} ${l.relay}").mkString("\n"))(
      for (a <- spellings; b <- spellings) withClue(s"${a.title} | ${b.title}")(RelayStage.oneRelay(a, b) shouldBe true))
  }

  "Two houses' relays of one work in one season" should "be two" in {
    val paris = measured(on(listing(KinoMuza, "Opéra de Paris: La Bayadère"), ParisDay))
    val royal = measured(on(listing(Helios, "Royal Ballet: La Bayadère", runtime = Some(170)), RoyalDay))
    RelayStage.oneRelay(paris, royal) shouldBe false
  }

  "One house's relays of one work in two seasons" should "be two" in {
    val season = measured(on(listing(KinoMuza, "RBO 2026/27: The Nutcracker"), LocalDate.of(2026, 12, 1)))
    val dated  = measured(on(listing(Helios, "Royal Ballet: The Nutcracker (2021)"), LocalDate.of(2026, 12, 5)))
    val last   = measured(on(listing(Kinoteka, "Royal Ballet & Opera 2024-25: The Nutcracker"), LocalDate.of(2026, 12, 5)))
    RelayStage.oneRelay(season, dated) shouldBe false
    RelayStage.oneRelay(season, last) shouldBe false
  }

  "A billing naming two houses" should "be neither's relay" in {
    // UK recording 573: The Flavel's "RBO/MET presents MACBETH", Northallerton's "RBO: MACBETH (MET)" and Cardigan's
    // "Met Opera in Association with RBO: Macbeth" screen the Met's 2026/27 Macbeth; keyed as the RBO's they formed a
    // relay of their own, apart from the Met's record, and four right takes beside both were left alone
    val day = LocalDate.of(2026, 11, 7)
    for (title <- Seq("RBO/MET presents MACBETH", "RBO: MACBETH (MET)", "Met Opera in Association with RBO: Macbeth (12A)"))
      withClue(title)(RelayStage.relayKeys(measured(on(listing(KinoMuza, title), day))) shouldBe empty)
    RelayStage.relayKeys(measured(on(listing(KinoMuza, "Met Opera 2026-27: Macbeth"), day))) should not be empty
    RelayStage.relayKeys(measured(on(listing(KinoMuza, "Royal Ballet & Opera 2026/27: The Nutcracker"), day))) should not be empty
  }

  "A billing naming no house" should "be no house's relay" in {
    val bare  = measured(on(listing(KinoMuza, "La Bayadère"), ParisDay))
    val paris = measured(on(listing(Helios, "Opéra de Paris: La Bayadère"), ParisDay))
    RelayStage.relayKeys(bare) shouldBe empty
    RelayStage.oneRelay(bare, paris) shouldBe false
  }

  private val normalizer = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
  // TMDB holds no record of the Paris Opera's 2026/27 relay: namesakes of the work, La Scala's and the Royal Ballet's records
  private val films = Seq(F(1300733, "La Bayadere", 2020, "", 120, 0.5), F(142479, "La Scala Ballet: La Bayadère", 2006, "", 130, 0.6),
    F(555004, "La Bayadère (Royal Ballet)", 2018, "", 128, 1.2))
  private def resolve(listings: Seq[Listing]) = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)

  "The resolver" should "join one relay's spellings into one film, and keep another house's relay of the work apart" in {
    val paris = Seq(KinoMuza -> "Opéra De Paris Ballet - La Bayadère", Helios -> "La Bayadère (Opéra de Paris)",
      KinoPalacowe -> "Opéra de Paris: La Bayadère", Kinoteka -> "The Opéra National de Paris: La Bayadère")
      .map { case (venue, title) => on(listing(venue, title, runtime = Some(125)), ParisDay) }
    val royal = on(listing(Multikino, "Royal Ballet: La Bayadère", runtime = Some(170)), RoyalDay)
    val other = on(listing(KinoApollo, "Royal Ballet: La Bayadère", runtime = Some(170)), RoyalDay)
    val r = resolve(paris :+ royal :+ other)
    withClue((paris :+ royal).map(l => s"${l.title}: ${r.decisionOf(l.key).render}").mkString("\n")) {
      paris.map(l => r.decisionOf(l.key)).distinct should have size 1
      (r.decisionOf(royal.key) eq r.decisionOf(paris.head.key)) shouldBe false
      paris.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
    }
  }

  it should "join one relay's spellings before the bare work's title links reach them, the bare spellings kept apart" in {
    // UK, recording 43331601375: the Paris Opera's "Notre-Dame de Paris" of 14 March 2027 stayed six films. Each spelling's
    // title-segment link reached the bare "Notre-Dame De Paris" ×4 (105, 125 min), which the learned cut keeps apart from
    // the relay's 88-90 minute billings: at the segment's tier the solver left every spelling alone.
    val day = LocalDate.of(2027, 3, 14)
    val films = Seq(F(11594, "Notre-Dame de Paris", 1956, "Jean Delannoy", 115, 3.0), F(790330, "La Scala Ballet: Notre-Dame de Paris", 2013, "", 105, 0.5),
      F(973305, "Notre-Dame de Paris (Opéra Bastille)", 2021, "", 0, 0.4))
    val paris = Seq[(models.Cinema, (String, Option[Int]))](KinoMuza -> ("Opéra De Paris Ballet - Notre-Dame de Paris", Some(90)), Helios -> ("Opéra De Paris Ballet - Notre-Dame de Paris", Some(90)),
      KinoPalacowe -> ("Opéra de Paris: Notre-Dame De Paris", Some(90)), Kinoteka -> ("The Opéra National de Paris: Notre-Dame De Paris", None),
      Multikino -> ("Notre-Dame De Paris - Opera de Paris", None))
      .map { case (venue, (title, runtime)) => on(listing(venue, title, runtime = runtime), day) }
    val bare = Seq(on(listing(KinoApollo, "Notre-Dame De Paris", runtime = Some(105)), day),
      on(listing(models.KinoBulgarska, "Notre-Dame De Paris (Cert. TBC)", runtime = Some(125)), day))
    val r = IdentityResolver.resolve(paris ++ bare, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
    withClue((paris ++ bare).map(l => s"${l.title}: ${r.decisionOf(l.key).render}").mkString("\n")) {
      paris.map(l => r.decisionOf(l.key)).distinct should have size 1
      paris.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
    }
  }
}
