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
   *  Murnau's "Faust" (1926), whose English title it ends with. */
  def foldRefused(row: MovieRecord, onto: Seq[MovieRecord], normalizer: TitleNormalizer): Option[CannotLink] =
    onto.iterator.flatMap(cinemasDescribeDifferentFilms(_, row, normalizer)).nextOption()
      .orElse(rowDeniesFilms(row, onto.flatMap(_.data.get(Tmdb)), normalizer))

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

  // ── must-links ───────────────────────────────────────────────────────────────────────

  /** A BARE listing — no year, no runtime — names nothing that could put it on another film,
   *  so it stays on the resolved film its venue already holds it on (its incumbent home)
   *  rather than moving to a same-titled row the settle would move it back from. PL,
   *  2026-09-25: Kino Amok's bare "Samson i Dalila". */
  def keepsIncumbentHome(year: Option[Int], runtime: Option[Int]): Boolean =
    year.isEmpty && runtime.isEmpty
}
