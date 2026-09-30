package services.movies

import models.{MovieRecord, SourceData, Tmdb}

/**
 * THE constraint model: every rule that says two pieces of listing evidence cannot be one film
 * (a CANNOT-LINK) or must stay together (a MUST-LINK), in one place, each with its reason.
 *
 * Before this object the same rules were asked in five shapes at five call sites, each wording
 * its own composition of `MixedFilmDetector` predicates: the TMDB candidate a venue denies
 * (`MovieService.lookupTmdb`), the decoration landing a listing's crew denies
 * (`ListingLanding.ask`), the containment fold the row's venue denies (`FilmCanonicalizer`, the
 * Faust refusal), the unresolved row the staging fold files apart (`StagingFold.planGroup`), the
 * convergence harness's wrong-merge check (`ServedCorpusInvariants.wrongMerges`) and the bare
 * listing's one home (`ScrapeLanding.concludedKeyFor`). They now all ask here, so the identity
 * resolver (docs/design/identity-resolver.md) draws its edges from the very rules the
 * incremental pipeline enforces, and a narrowed or added rule reaches every stage at once.
 *
 * The rule BODIES stay where they are (`MixedFilmDetector.deniesFilm`, `listingDeniesFilm`,
 * `wouldAddASecondFilm`, `describeDifferentFilms`): this object routes and names them. Keep it
 * that way — a predicate change belongs in the predicate, a new constraint belongs here.
 * `ListingConstraintsRoutingSpec` fails any production file that asks one of those predicates
 * directly.
 */
object ListingConstraints {

  /** Why two pieces of evidence cannot be one film. */
  enum CannotLink {
    /** A venue's own published year AND director both contradict the film TMDB named
     *  (`MixedFilmDetector.deniesFilm`): the Met's 2026 "Samson i Dalila" is not DeMille's
     *  1949 film; Kinoteka's Wong Kar Wai "Happy Together" is not Kim Jeong-hwan's 2018 one. */
    case VenueDeniesFilm
    /** A listing matched only by the SHAPE of its title names a director the film does not
     *  credit and a runtime or year it does not share (`MixedFilmDetector.listingDeniesFilm`):
     *  "It Ends with Us" is not "It Ends". */
    case ListingDeniesFilm
    /** The listing's own original title names another film than the row, uncorroborated by
     *  director and runtime (`MixedFilmDetector.wouldAddASecondFilm`). */
    case OriginalTitleNamesAnotherFilm
    /** Two rows' cinemas publish contradicting identities
     *  (`MixedFilmDetector.describeDifferentFilms`). */
    case CinemasDescribeDifferentFilms
    /** One venue lists two rows under one title whose directors share no person
     *  (`MixedFilmDetector.creditSamePerson`): Marion Theatre Ocala's "Planet of the Apes"
     *  (Schaffner) beside "Planet of the Apes (2001)" (Burton). */
    case VenueCreditsApart
    /** An admin pinned the listing as never this film (`services.identity.PinClaim.NeverFilm`):
     *  it cannot share a film with any listing that is that film. */
    case PinnedNotFilm
    /** A title naming a SEASON ("2026/27") lists that season's production: another season, or a
     *  film dated outside the season's two years, is another production ([[seasonsApart]]). */
    case SeasonsApart
    /** A rule LEARNED from corroborated films (`identity-weights.json`), by name. */
    case Learned(rule: String)
  }

  /** Why two pieces of evidence must be one film, where the rule is not a title or TMDB edge
   *  the resolver draws itself. */
  enum MustLink {
    /** An admin pinned the listings as one film (`PinClaim.SameFilm`), or as the same film
     *  (`PinClaim.IsFilm`). Hard: it overrides a derived cannot-link between them. */
    case Pinned
  }

  /** What one listing published that the landing constraints read. */
  final case class ListingEvidence(originalTitle: Option[String], runtime: Option[Int], year: Option[Int],
                                   director: Seq[String])

  // ── cannot-links ─────────────────────────────────────────────────────────────────────

  /** A venue's own `slot` against the `film` TMDB described (a `Tmdb` slot, or a candidate's
   *  year and director). */
  def slotDeniesFilm(slot: SourceData, film: SourceData, normalizer: TitleNormalizer): Option[CannotLink] =
    Option.when(MixedFilmDetector.deniesFilm(slot, film, normalizer))(CannotLink.VenueDeniesFilm)

  /** The first of `row`'s venue slots (in its cinema-data order) that denies one of `films`. */
  def denyingSlot(row: MovieRecord, films: Seq[SourceData], normalizer: TitleNormalizer): Option[SourceData] =
    row.cinemaData.values.find(slot => films.exists(slotDeniesFilm(slot, _, normalizer).isDefined))

  /** Does one of `row`'s venues deny one of `films`? A TMDB candidate so denied is not the row's
   *  film, however it was found; an unresolved row so denied is filed apart by the staging fold. */
  def rowDeniesFilms(row: MovieRecord, films: Seq[SourceData], normalizer: TitleNormalizer): Option[CannotLink] =
    denyingSlot(row, films, normalizer).map(_ => CannotLink.VenueDeniesFilm)

  /** Two rows' cinemas publish contradicting identities for their main film. STRICT (see
   *  `MixedFilmDetector.describeDifferentFilms`): the canonicaliser's pairwise veto, the
   *  imdbId sibling edge, and the rule-4 straggler's home check all read it. */
  def cinemasDescribeDifferentFilms(a: MovieRecord, b: MovieRecord, normalizer: TitleNormalizer): Option[CannotLink] =
    Option.when(MixedFilmDetector.describeDifferentFilms(a, b, normalizer))(CannotLink.CinemasDescribeDifferentFilms)

  /** [[cinemasDescribeDifferentFilms]] on identities already read by
   *  `MixedFilmDetector.publishedIdentity` — for a caller comparing many pairs. */
  def identitiesDescribeDifferentFilms(a: Option[MixedFilmDetector.Group], b: Option[MixedFilmDetector.Group]): Option[CannotLink] =
    Option.when(MixedFilmDetector.describeDifferentFilms(a, b))(CannotLink.CinemasDescribeDifferentFilms)

  /** May the containment fold adopt `row` onto the film the `onto` rows are? Refused when a row
   *  of `onto` and `row` publish different films, or one of `row`'s venues denies the film
   *  `onto` resolved to — Kulturfabrik Meda's "Zärtlich kreist die Faust" (1990) is not
   *  Murnau's "Faust" (1926), whose English title it ends with. Or when one of `row`'s venues
   *  publishes what [[landingRefused]] refuses a listing for — the fold must not adopt what the
   *  landing kept apart: Cinema City's "Lalka (ale to horror)" (Rod Blackhurst, 82 min) is not
   *  Kawalski's 162-minute "Lalka", whose title it contains. */
  def foldRefused(row: MovieRecord, onto: Seq[MovieRecord], normalizer: TitleNormalizer): Option[CannotLink] =
    onto.iterator.flatMap(cinemasDescribeDifferentFilms(_, row, normalizer)).nextOption()
      .orElse(rowDeniesFilms(row, onto.flatMap(_.data.get(Tmdb)), normalizer))
      .orElse(Option.when(onto.exists(film => row.cinemaData.values.exists(slot =>
        MixedFilmDetector.listingDeniesFilm(film, slot.runtimeMinutes, slot.releaseYear, slot.director, normalizer))))(
        CannotLink.ListingDeniesFilm))

  /** Does the listing's own original title make it a second film on `row`? The landing's
   *  "every same-titled row is a different film" divert reads this rule alone. */
  def originalTitleNamesAnotherFilm(listing: ListingEvidence, row: MovieRecord,
                                    normalizer: TitleNormalizer): Option[CannotLink] =
    Option.when(MixedFilmDetector.wouldAddASecondFilm(row, listing.originalTitle, listing.runtime, listing.year,
      listing.director, normalizer))(CannotLink.OriginalTitleNamesAnotherFilm)

  /** May a listing matched by the SHAPE of its title (a decoration, a shared search key) land
   *  on `row`? */
  def landingRefused(listing: ListingEvidence, row: MovieRecord, normalizer: TitleNormalizer): Option[CannotLink] =
    originalTitleNamesAnotherFilm(listing, row, normalizer).orElse(
      Option.when(MixedFilmDetector.listingDeniesFilm(row, listing.runtime, listing.year, listing.director, normalizer))(
        CannotLink.ListingDeniesFilm))

  /** May two rows ONE venue lists under one title be one film, by their directors? Refused when
   *  both credit someone and no person is credited by both. Within one venue this never split
   *  a film on the five full recorded corpora (every such pair also differs in year or
   *  runtime); across venues a director difference is no evidence (a venue prints the writer),
   *  so this is for the same-venue fold only. */
  def venueCreditsApart(a: Seq[String], b: Seq[String], normalizer: TitleNormalizer): Option[CannotLink] = {
    def credited(ds: Seq[String]) = ds.exists(_.trim.nonEmpty)
    Option.when(credited(a) && credited(b) && !MixedFilmDetector.creditSamePerson(a, b, normalizer))(CannotLink.VenueCreditsApart)
  }

  /** A listing whose title names a SEASON (`season`, its first year: "2026/27" → 2026) against
   *  another piece of evidence: a film, or another listing. The season is the listing's own
   *  statement of WHEN its production runs — a year and the next, which is what makes it a season
   *  — so the other side is another production when it names another season, or is dated outside
   *  those two years: the Met's 2026/27 "Silent Night" is not John Woo's 2023 film, the Royal
   *  Ballet's 2026/27 "The Nutcracker" not its 2024/25 one. `otherYear` is a film's year or a
   *  listing's PUBLISHED year, never a year its title brackets. The identity resolver's rule; the
   *  incremental pipeline does not read it. */
  def seasonsApart(season: Option[Int], otherSeason: Option[Int], otherYear: Option[Int]): Option[CannotLink] =
    season.flatMap(s => Option.when(otherSeason.exists(_ != s) || otherYear.exists(y => y < s || y > s + 1))(CannotLink.SeasonsApart))

  // ── learned cannot-links (the identity resolver's, from the calibration artefact) ──────

  /** Are these two pieces of evidence — measured by `IdentityMeasures` for `scope` ("listing-film"
   *  or "listing-listing") and scored `probability` by the calibration — NOT one film? A learned
   *  rule of `identity-weights.json` holds, or the probability is below the scope's certified
   *  cannot-link cut. Evaluated generically: no predicate here names a film property, the
   *  artefact's rules do. The incremental pipeline does not read these — it keeps the predicates
   *  above, so nothing it serves changes while the resolver runs in shadow. */
  def learned(calibration: services.identity.IdentityCalibration, scope: String,
              measures: Map[String, services.identity.IdentityMeasures.Measure], probability: Double): Option[CannotLink] =
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
   *  "The Last Whale Singer" at 91, TMDB carrying no Polish title). A learned rule still vetoes. */
  def learnedListingFilm(calibration: services.identity.IdentityCalibration,
                         measures: Map[String, services.identity.IdentityMeasures.Measure],
                         probability: Double): Option[CannotLink] = {
    import services.identity.IdentityMeasures.{ListingFilm, comparedFacts, comparesAFact}
    // The cut on a probability the compared facts do not pull down is the title's verdict alone.
    lazy val factsAgree = calibration.contributions(ListingFilm, comparedFacts(ListingFilm, measures)).map(_._2).sum >= 0
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
  def learnedListingListing(calibration: services.identity.IdentityCalibration,
                            measures: Map[String, services.identity.IdentityMeasures.Measure]): Option[CannotLink] = {
    import services.identity.IdentityMeasures.{Category, ContainingRelations, ListingListing, comparedFacts}
    val onlyNamesDiffer = measures.get("title").exists { case Category(c) => ContainingRelations(c); case _ => false } &&
      calibration.contributions(ListingListing, comparedFacts(ListingListing, measures)).map(_._2).sum >= 0
    if (onlyNamesDiffer) None
    else learnedOnFacts(calibration, ListingListing, measures, calibration.probability(ListingListing, measures))
  }

  private def learnedOnFacts(calibration: services.identity.IdentityCalibration, scope: String,
                             measures: Map[String, services.identity.IdentityMeasures.Measure],
                             probability: => Double): Option[CannotLink] =
    if (!services.identity.IdentityMeasures.comparesAFact(scope, measures)) None
    else learned(calibration, scope, measures, probability)

  // ── must-links ───────────────────────────────────────────────────────────────────────

  /** The admin pins as hard constraints — the one way curation reaches the resolver: its
   *  must-links, cannot-links, block keys, the per-listing film override, and which derived
   *  edges a pin overrides (see [[services.identity.PinConstraints]]). */
  def pinned(pins: Seq[services.identity.Pin]): services.identity.PinConstraints =
    services.identity.PinConstraints(pins)

  /** A BARE listing — no year, no runtime — names nothing that could put it on another film,
   *  so it stays on the resolved film its venue already holds it on (its incumbent home)
   *  rather than moving to a same-titled row the settle would move it back from. PL,
   *  2026-09-25: Kino Amok's bare "Samson i Dalila". */
  def keepsIncumbentHome(year: Option[Int], runtime: Option[Int]): Boolean =
    year.isEmpty && runtime.isEmpty
}
