package services.identity

import services.movies.ListingConstraints.CannotLink

/** The identity resolver's LEARNED cannot-links: the rules of the calibration artefact (`identity-weights.json`) and its
 *  certified probability cuts, beside the hand-written ones of [[services.movies.ListingConstraints]]. */
object LearnedCannotLinks {

  /** Are these two pieces of evidence — measured by `IdentityMeasures` for `scope` ("listing-film"
   *  or "listing-listing") and scored `probability` by the calibration — NOT one film? A learned
   *  rule of `identity-weights.json` holds, or the probability is below the scope's certified
   *  cannot-link cut. Evaluated generically: no predicate here names a film property, the
   *  artefact's rules do. */
  def learned(calibration: IdentityCalibration, scope: String,
              measures: Map[String, IdentityMeasures.Measure], probability: Double): Option[CannotLink] =
    calibration.cannotLink(scope, measures).map(r => CannotLink.Learned(r.name))
      .orElse(Option.when(calibration.forbidsLink(scope, probability))(CannotLink.Learned(s"$scope probability below the cannot-link cut")))

  /** [[learned]] for a LISTING against a FILM (the "listing-film" scope, `probability` its own
   *  facts' calibrated probability): the listing's evidence denies the film only when it compares
   *  at least one published fact with it. A listing that publishes nothing but a title — a
   *  programme banner, an anniversary or format suffix, a local-language title of a foreign
   *  film — is scored by how its title relates to the film's, and that relation alone never
   *  vetoes: a decorated or translated spelling is otherwise denied the very film its plain,
   *  credited siblings matched, and the denial outranks the title must-link that would join them.
   *  Nor does the probability cut when the facts it does compare agree, together, with the film: its
   *  low score is then the title's (PL Kino Łuków's "Vincent. Legenda oceanu" at 88 minutes against
   *  "The Last Whale Singer" at 91, TMDB carrying no Polish title). Nor does it when only the RUNTIME pulls it down: the
   *  minutes a venue bills are its screening's — a lecture, a Halloween night, a pre-premiere's round estimate around the
   *  film (Kino Spektrum's "Akademia Polskiego Filmu: Znachor (1937)" at 150 against 98, CK Lublin's "Martwe zło 2" at 121
   *  against 84) — not a fact of the film, and the learned rules certified on the runtime's size (`runtime.gap`) are what
   *  a runtime vetoes through. A learned rule still vetoes. */
  def learnedListingFilm(calibration: IdentityCalibration,
                         measures: Map[String, IdentityMeasures.Measure],
                         probability: Double): Option[CannotLink] = {
    import IdentityMeasures.{ListingFilm, comparedFacts, comparesAFact}
    // The cut on a probability the film's own facts do not pull down is the title's, or the screening's length's, alone.
    lazy val factsAgree = calibration.contributionSum(ListingFilm, comparedFacts(ListingFilm, measures) - "runtime.delta") >= 0
    if (!comparesAFact(ListingFilm, measures)) None
    else calibration.cannotLink(ListingFilm, measures).map(r => CannotLink.Learned(r.name))
      .orElse(Option.when(!factsAgree && calibration.forbidsLink(ListingFilm, probability))(CannotLink.Learned(s"$ListingFilm probability below the cannot-link cut")))
  }

  /** [[learned]] for two LISTINGS (the "listing-listing" scope): the pair is kept apart only by
   *  the facts both published. Two listings that publish nothing comparable — "Tony" beside "Kino
   *  bez barier: Tony (AD + CC)", or beside a title stating its year in a bracket where the other
   *  states it in a field — are scored by their titles (and venues) alone, and that never vetoes:
   *  the veto otherwise outranks the title must-link that joins a decorated spelling to its plain
   *  sibling's film. Nor does it when one title carries the other's whole ([[IdentityMeasures.ContainingRelations]])
   *  and the facts they compare do not, together, weigh against one film: one venue's "Lalka (2026)"
   *  beside its "Lalka (2026) | seans DKF Projekcja", the same year in each, is one film however
   *  heavily one venue's two spellings weigh against it. A fact that does weigh against — Sheri
   *  Hagen's 2025 "Billie" beside James Erskine's 2020 "Billie – Legende des Jazz" — still vetoes. */
  def learnedListingListing(calibration: IdentityCalibration,
                            measures: Map[String, IdentityMeasures.Measure]): Option[CannotLink] = {
    import IdentityMeasures.{Category, ContainingRelations, ListingListing, comparedFacts}
    val onlyNamesDiffer = measures.get("title").exists { case Category(c) => ContainingRelations(c); case _ => false } &&
      calibration.contributionSum(ListingListing, comparedFacts(ListingListing, measures)) >= 0
    if (onlyNamesDiffer) None
    else learnedOnFacts(calibration, ListingListing, measures, calibration.probability(ListingListing, measures))
  }

  private def learnedOnFacts(calibration: IdentityCalibration, scope: String,
                             measures: Map[String, IdentityMeasures.Measure],
                             probability: => Double): Option[CannotLink] =
    if (!IdentityMeasures.comparesAFact(scope, measures)) None
    else learned(calibration, scope, measures, probability)
}
