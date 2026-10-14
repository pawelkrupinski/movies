package services.movies

import models.{MovieRecord, SourceData}

/**
 * THE constraint model: every rule that says two pieces of listing evidence cannot be one film
 * (a CANNOT-LINK) or must stay together (a MUST-LINK), in one place, each with its reason.
 *
 * Before this object the same rules were asked in five shapes at five call sites, each wording
 * its own composition of `MixedFilmDetector` predicates: the TMDB candidate a venue denies
 * (`MovieService.lookupTmdb`), the decoration landing a listing's crew denies
 * (the since-deleted `ListingLanding.ask`), the containment fold the row's venue denies (the
 * since-deleted `FilmCanonicalizer`, the Faust refusal), the unresolved row the staging fold files
 * apart (the since-deleted `StagingFold.planGroup`), the convergence harness's wrong-merge check
 * (`ServedCorpusInvariants.wrongMerges`) and the bare listing's one home (the since-deleted
 * `ScrapeLanding.concludedKeyFor`). They now all ask here, so the identity resolver
 * (docs/design/identity-resolver.md) draws its edges from the very rules those stages enforced,
 * and a narrowed or added rule reaches every stage at once.
 *
 * The rule BODIES stay where they are (`MixedFilmDetector.deniesFilm`,
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
    /** A rule LEARNED from corroborated films (`identity-weights.json`), by name
     *  (`services.identity.LearnedCannotLinks`). */
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
  def slotDeniesFilm(slot: SourceData, film: SourceData, normalizer: TitleNormalizer)(using LatestTitleYear): Option[CannotLink] =
    Option.when(MixedFilmDetector.deniesFilm(slot, film, normalizer))(CannotLink.VenueDeniesFilm)

  /** Two rows' cinemas publish contradicting identities for their main film. STRICT (see
   *  `MixedFilmDetector.describeDifferentFilms`): the canonicaliser's pairwise veto, the
   *  imdbId sibling edge, and the rule-4 straggler's home check all read it. */
  def cinemasDescribeDifferentFilms(a: MovieRecord, b: MovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): Option[CannotLink] =
    Option.when(MixedFilmDetector.describeDifferentFilms(a, b, normalizer))(CannotLink.CinemasDescribeDifferentFilms)

  /** Does the listing's own original title make it a second film on `row`? The landing's
   *  "every same-titled row is a different film" divert reads this rule alone. */
  def originalTitleNamesAnotherFilm(listing: ListingEvidence, row: MovieRecord,
                                    normalizer: TitleNormalizer)(using LatestTitleYear): Option[CannotLink] =
    Option.when(MixedFilmDetector.wouldAddASecondFilm(row, listing.originalTitle, listing.runtime, listing.year,
      listing.director, normalizer))(CannotLink.OriginalTitleNamesAnotherFilm)

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
}
