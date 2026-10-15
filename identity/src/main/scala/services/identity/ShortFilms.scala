package services.identity

import services.movies.{TitleContainment, TitleWords}

/** A SHORT film — one TMDB states runs [[MaxMinutes]] or less in every language and cut it holds ([[FilmCuts]]), the
 *  Academy's line — and the screenings that may show one alone.
 *
 *  A cinema slot billing a title and nothing more screens a feature: a venue showing a short on its own says so, by the
 *  running time it states, the words of its billing ("Kurzfilm", "short film", "krótkometrażowy", "court métrage",
 *  "cortometraje" — [[TitleWords.Concept.ShortFilm]]), or a TMDB-keyed feed linking that very short. A short taken for
 *  any other listing is a namesake of the feature it shows: DE Central im Bürgerbräu's "GINA - mit der Domschule Würzburg"
 *  took the 7-minute Indonesian "Gina", Kommunales Kino Freiburg's "THE COMMUNITY" the 4-minute 2022 "The Community"
 *  (Cineamo links the 64-minute 2025 documentary), Kinocenter Giessen's "Imagine Peace (GLOBALE)" the 40-minute 2010
 *  short (Cineamo links Chiquet's 2026 documentary) — each pooled at under 42%, nothing the venue published for it.
 *  [[SuspectTakes]]' runtime signal reads the same line. */
private[identity] object ShortFilms {

  /** The longest running time of a short, in minutes: the Academy's "40 minutes or less". */
  val MaxMinutes = 40

  /** Every running time TMDB states for `film`, and those of the cuts [[FilmCuts]] names for it. */
  def runtimesOf(film: IdentityMeasures.Film): Seq[Int] = (film.runtimes ++ FilmCuts.of(film.imdbNumber).map(_.runtime)).distinct

  /** Does every running time stated for `film` — at least one — make it a short? */
  def isShort(film: IdentityMeasures.Film): Boolean = {
    val runs = runtimesOf(film)
    runs.nonEmpty && runs.max <= MaxMinutes
  }

  private val ShortWords = TitleWords.foldedPattern(TitleWords.Concept.ShortFilm).r

  /** Does the listing bill a short — a running time under a feature's ([[SuspectTakes.FeatureMinutes]]), or a word saying
   *  so in one of its titles? */
  def billsAShort(listing: IdentityMeasures.Listing): Boolean =
    listing.statedRuntime.exists(_ < SuspectTakes.FeatureMinutes) ||
      (listing.ownTitles ++ listing.originalTitle).exists(title => ShortWords.findFirstIn(TitleContainment.tokens(title).mkString(" ")).isDefined)

  /** Why `listing` may not take `film` (`tmdbId`): a short, for a listing billing none, its feed linking no such short
   *  (`linked`, the TMDB film a TMDB-keyed feed links it to). */
  def denial(listing: IdentityMeasures.Listing, tmdbId: Int, film: IdentityMeasures.Film, linked: Option[Int]): Option[String] =
    Option.when(isShort(film) && !linked.contains(tmdbId) && !billsAShort(listing))(
      s"a ${runtimesOf(film).max}-minute short, for a listing billing " +
        listing.statedRuntime.fold("no running time")(minutes => s"$minutes minutes") + " and no short")
}
