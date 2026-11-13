package services.identity

import java.time.LocalDate

import models.{Helios, KinoApollo, KinoMuza, KinoPalacowe, Kinoteka, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

import FilmTable.{F, listing}

/**
 * Billings of one film-less event ([[agreement.NonFilmEvents.facts]]) are one web film whatever running time each venue
 * books it for: the 2026-11-13 corpus audit found UK "League of Legends Worlds 26 | Finals in Cinema" as three films
 * (Odeon's and Vue's 420 minutes, Cineworld's and Showcase's 300) and "The CBeebies Panto 2026: Treasure Island" (75 min)
 * apart from the 313 venues' "CBeebies Panto 2026: Treasure Island", each kept apart by the learned listing-listing cut
 * on the minutes alone. Another edition, other episodes or a session of another length stay apart.
 */
class FilmlessEventGroupingSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
  private val day        = LocalDate.of(2026, 11, 15)
  private def on(l: Listing): Listing = l.copy(screenings = ScreeningDays.of(Seq(day)))
  // the records a search finds: last year's final, another game's, and the panto's namesakes (all judged wrong)
  private val films = Seq(F(1552059, "League of Legends Worlds25 - Finals in Cinema", 2025, "", 300, 0.6),
    F(1555166, "2025 VALORANT Champions Tour | Finals in Cinema", 2025, "", 420, 0.6),
    F(6646, "Treasure Island", 1950, "Byron Haskin", 96, 8.0), F(1710881, "Treasure Island", 2027, "Ridley Scott", 0, 20.0))
  private def resolve(listings: Seq[Listing]) = IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
  private def rendered(r: Resolution, ls: Seq[Listing]) = ls.map(l => s"${l.title}: ${r.decisionOf(l.key).render}").mkString("\n")

  "One esports final's billings" should "be one film-less film whatever minutes each venue books it for" in {
    val event = Seq(KinoMuza -> ("League of Legends Worlds 26 | Finals in Cinema", 420), Helios -> ("League of Legends Worlds 26 | Finals in Cinema", 420),
      KinoPalacowe -> ("League Of Legends Worlds 26 - Finals In Cinema", 300), Kinoteka -> ("League Of Legends Worlds 26 - Finals In Cinema", 300))
      .map { case (venue, (title, runtime)) => on(listing(venue, title, runtime = Some(runtime))) }
    val r = resolve(event)
    withClue(rendered(r, event)) {
      event.map(l => r.decisionOf(l.key)).distinct should have size 1
      event.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
    }
  }

  "One panto's billings" should "be one film-less film, its article and minutes aside" in {
    val event = Seq(KinoMuza -> ("CBeebies Panto 2026: Treasure Island", 60), Helios -> ("CBeebies Panto 2026: Treasure Island", 60),
      KinoPalacowe -> ("The CBeebies Panto 2026: Treasure Island", 75), Kinoteka -> ("The CBeebies Panto 2026: Treasure Island", 75))
      .map { case (venue, (title, runtime)) => on(listing(venue, title, runtime = Some(runtime))) }
    val r = resolve(event)
    withClue(rendered(r, event)) {
      event.map(l => r.decisionOf(l.key)).distinct should have size 1
      event.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
    }
  }

  "One panto's billings spelt otherwise beside its name" should "be one film-less film, a sensory-friendly screening apart" in {
    // UK, recording 43331601375: no title form joins them, and the learned cut kept them apart besides
    val event = Seq[(models.Cinema, (String, Option[Int]))](KinoMuza -> ("CBeebies Panto 2026: Treasure Island", Some(60)), Helios -> ("Cbeebies Panto: Treasure Island", Some(75)),
      KinoPalacowe -> ("The CBeebies Panto 2026: Treasure Island", Some(75)), Kinoteka -> ("Cbeebies Panto : Treasure Island", None))
      .map { case (venue, (title, runtime)) => on(listing(venue, title, runtime = runtime)) }
    val sensory = on(listing(KinoApollo, "CBeebies Panto Treasure Island – Sensory Friendly Screening", runtime = Some(60)))
    val r = resolve(event :+ sensory)
    withClue(rendered(r, event :+ sensory)) {
      event.map(l => r.decisionOf(l.key)).distinct should have size 1
      event.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
      (r.decisionOf(sensory.key) eq r.decisionOf(event.head.key)) shouldBe false
    }
  }

  "One panto's billings beside the panto's own TMDB record" should "take none of it alone, and be one film-less film" in {
    // UK, prod 2026-11-13: Lonsdale Alhambra Penrith's, BEAM Hertford's and Hebden Bridge's "Cbeebies Panto: Treasure Island"
    // each took the staged show's own record (1785679, judged wrong 10-21) alone by favoured-calibrated, which the
    // billed-as-one-piece veto withdrew only after the assignment: holding it, they never joined the event's other
    // spellings by the same-event must-link (the 313 venues' "CBeebies Panto 2026: …" hold none by the cast veto, which
    // this table's listings carry no cast for: the spelling stating the edition is left out)
    val own   = F(1785679, "Cbeebies Panto: Treasure Island", 2026, "", 75, 0.4, original = Some("Cbeebies Panto: Treasure Island"))
    val event = Seq[(models.Cinema, (String, Option[Int]))](Helios -> ("Cbeebies Panto: Treasure Island", Some(75)),
      KinoPalacowe -> ("The CBeebies Panto 2026: Treasure Island", Some(75)), Kinoteka -> ("Cbeebies Panto : Treasure Island", None))
      .map { case (venue, (title, runtime)) => on(listing(venue, title, runtime = runtime)) }
    val r = IdentityResolver.resolve(event, new FilmTable(films :+ own, normalizer), normalizer, ResolveInputs.Unpinned)
    withClue(rendered(r, event)) {
      event.map(l => r.decisionOf(l.key).film).distinct shouldBe Seq(None)
      event.map(l => r.decisionOf(l.key)).distinct should have size 1
    }
  }

  "Two editions of one event" should "stay two" in {
    val now  = on(listing(KinoMuza, "League of Legends Worlds 26 | Finals in Cinema", runtime = Some(420)))
    val last = on(listing(Multikino, "League of Legends Worlds 2025 | Finals in Cinema", runtime = Some(300)))
    val r = resolve(Seq(now, last))
    withClue(rendered(r, Seq(now, last)))((r.decisionOf(now.key) eq r.decisionOf(last.key)) shouldBe false)
  }

  "Two gaming sessions of other lengths" should "stay two" in {
    val three = on(listing(KinoMuza, "Gaming im Kino - 3 Stunden", runtime = Some(180)))
    val four  = on(listing(KinoApollo, "Gaming im Kino - 4 Stunden", runtime = Some(240)))
    val r = resolve(Seq(three, four))
    withClue(rendered(r, Seq(three, four)))((r.decisionOf(three.key) eq r.decisionOf(four.key)) shouldBe false)
  }
}
