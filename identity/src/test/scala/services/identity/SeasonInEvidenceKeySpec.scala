package services.identity

import models.{KinoMuza, KinoPalacowe}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer

import FilmTable.listing

/** The season a stage work's screenings fall in is read by its season rules ([[StageRelays.stageWorkQueries]]'s closing
 *  year, [[RelayStage.of]], [[StageRelays.bareWorksSeasonRecord]]), so it is part of the evidence a node is keyed by
 *  ([[Evidence.key]]): two listings billing the same work alike but screening in different seasons are two questions,
 *  each searched with its own season. UK Riverside Theatre Woodbridge's "Romeo and Juliet (2027)", screening on 25 May
 *  2027 (season 2026/27), searches "Romeo and Juliet 2026" for the RBO 2026/27 relay; a billing alike screening in
 *  October 2027 (season 2027/28) must neither lose that search to it nor be given it. */
class SeasonInEvidenceKeySpec extends AnyFlatSpec with Matchers {
  private val normalizer = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
  private def screening(day: java.time.LocalDate) = ScreeningDays.of(Seq(day))
  private val may     = listing(KinoMuza, "Romeo and Juliet (2027)").copy(screenings = screening(java.time.LocalDate.of(2027, 5, 25)))
  private val october = listing(KinoPalacowe, "Romeo and Juliet (2027)").copy(screenings = screening(java.time.LocalDate.of(2027, 10, 12)))

  private def generation(listings: Seq[Listing]) = new IdentityResolver.Stages(listings, new FilmTable(Nil, normalizer), normalizer,
    IdentityCalibration.resolver, IdentityResolver.Mutation.None, PinConstraints(Nil), TitleDecorations.None).generation

  private def searchesOf(listings: Seq[Listing], of: Listing): Seq[String] = {
    val g = generation(listings)
    g.queriesOf(g.nodes.find(_.listings.exists(_.key == of.key)).get.id).collect { case CandidateQuery.Title(text) => text }
  }

  "two listings billing a stage work alike in different seasons" should "be two evidence nodes" in {
    generation(Seq(may, october)).nodes.map(_.listings.map(_.key)) should have size 2
  }

  they should "each be searched with its own season, whichever arrives first" in {
    Seq(Seq(may, october), Seq(october, may)).foreach { order =>
      searchesOf(order, may) should contain ("Romeo and Juliet 2026")
      searchesOf(order, october) should not contain "Romeo and Juliet 2026"
    }
  }

  "a film's listings screening in different seasons" should "stay one node" in {
    // a season read by no rule of its billing splits nothing: a film's run past July re-resolves nothing
    val films = Seq(listing(KinoMuza, "Paddington (2027)").copy(screenings = may.screenings),
      listing(KinoPalacowe, "Paddington (2027)").copy(screenings = october.screenings))
    generation(films).nodes should have size 1
  }
}
