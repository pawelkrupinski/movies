package services.identity

import services.evidence.ListingConstraints
import services.identity.measures.{BannerHouses, Film, IdentityMeasures, Measure, MeasuredListing, RelayStage, StageRelays, StageSeasonRecords}
import services.identity.vocabulary.Denial

/** What rules a film out on the listing's own evidence ([[evidenceDenial]]): the season its title names, its stage house's
 *  billing as `houses` reads each banner ([[BannerHouses]]), its title's shape ([[TitleShapeRefusals]]), then a learned
 *  cannot-link under `calibration`, its facts weighed by `weights`. */
private[identity] final class ListingDenials(houses: BannerHouses, calibration: IdentityCalibration, weights: EvidenceWeights) {
  /** Does the listing name its season's production of its work by its own banner's house — no other house's
   *  ([[BannerHouses.other]])? */
  def namesItsSeasonProduction(listing: MeasuredListing, film: Film): Boolean =
    StageRelays.namesSeasonProduction(listing, film) && !StageRelays.billing(listing, film).exists(houses.other)

  /** Does the listing's own evidence rule the film out: a learned cannot-link, its facts'
   *  probability below the certified cut, a season its title names that the film is not of (but its house's recording
   *  of it, [[StageRelays.itsHousesRecording]], or the relay its screenings air, [[RelayStage.airsItsRelay]]),
   *  its season's production of its work by ANOTHER house than its banner's (`houses`), a screen
   *  namesake of the work its stage house's banner bills ([[StageRelays.screenNamesake]]), a record of
   *  another work than that banner's relay names ([[StageRelays.namesAnotherWork]]), a house's record of another
   *  production of its work than the season its screenings fall in ([[StageRelays.anotherProduction]]), or one its
   *  title's shape refuses ([[TitleShapeRefusals]]). A learned cannot-link never denies a relay its own house's record of
   *  its work and season ([[RelayStage.particularRecordOf]]). */
  def evidenceDenies(listing: MeasuredListing, film: Film, measures: Map[String, Measure], country: Option[String]): Boolean =
    evidenceDenial(listing, film, measures, country).isDefined

  /** [[evidenceDenies]], saying which of its reasons rules the film out: the season its title names first, then the first
   *  of [[evidenceRefusals]] that holds, then a learned cannot-link. `country`: the venue's (ISO-3166-1). */
  def evidenceDenial(listing: MeasuredListing, film: Film, measures: Map[String, Measure], country: Option[String]): Option[Denial] =
    ListingConstraints.seasonsApart(listing.seasonYear, film.season, film.year)
      .filterNot(_ => StageRelays.itsHousesRecording(listing, film) || RelayStage.airsItsRelay(listing, film))
      .map(link => Denial(link.toString))
      .orElse(evidenceRefusals.collectFirst { case (denial, holds) if holds(listing, film, measures, country) => denial })
      // a relay's record of its house, work and season ([[RelayStage]]) files a broadcast's running time and its house's
      // banner on the work: the learned runtime-gap and title-overlap vetoes say nothing of it. Nor of the record of the
      // very season a relay's screenings broadcast its work in ([[StageSeasonRecords.screenedSeasonRecord]]), under a banner naming
      // no house in particular: ES Ocine's "Carmen (OPERA LIVE)", screening the day the RBO's 2026/27 Carmen airs
      .orElse(Option.unless(RelayStage.particularRecordOf(listing, film) || StageSeasonRecords.screenedSeasonRecord(listing, film))(LearnedCannotLinks.listingFilmDenial(calibration, measures, weights.factsProbability(measures))).flatten)

  /** The reasons of the listing's own billing to rule a film out ([[evidenceDenial]]), in the order they are asked: each the
   *  denial it gives — its text what the trace names it by — and the question of the listing, the film, their measures
   *  and the venue's country that gives it; its stage house's billing first, then its title's shape. */
  private val evidenceRefusals: List[(Denial, ListingDenials.Question)] = List[(Denial, ListingDenials.Question)](
    (Denial(ListingDenials.AnotherHousesSeason, Denial.Kind.AnotherHousesSeason),
      (l, f, _, _) => StageRelays.namesSeasonProduction(l, f) && !namesItsSeasonProduction(l, f)),
    // a credit the venue publishes names its staging's record whatever TMDB titles it
    (Denial("a screen namesake of the work its stage house's billing names", Denial.Kind.NotTheWork(creditOverrules = true)),
      (l, f, measures, _) => StageRelays.screenNamesake(l, f) && !IdentityMeasures.sameDirector(measures)),
    // a credit or a running time never holds a record of another work: a screen director films many
    (Denial("a record of another work than its stage house's billing names"), (l, f, _, _) => StageRelays.namesAnotherWork(l, f)),
    (Denial("another production of its work than the season its screenings fall in"),
      (l, f, _, country) => StageRelays.anotherProduction(l, f, country))) ++
    TitleShapeRefusals.refusals.map { case (denial, holds) => (denial, (l: MeasuredListing, f: Film, _: Map[String, Measure], _: Option[String]) => holds(l, f)) }
}

private[identity] object ListingDenials {
  /** A question [[ListingDenials.evidenceDenial]] asks of a listing and a film, beside their measures and the venue's
   *  country. */
  type Question = (MeasuredListing, Film, Map[String, Measure], Option[String]) => Boolean
  /** How [[ListingDenials.evidenceDenial]] names a season production its listing's banner is not the house of: a denial
   *  about the banner, which says nothing of another listing billing the record's own house ([[Families.deniedBySibling]]). */
  val AnotherHousesSeason = "another house's season production"
}
