package services.movies

import models._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Corpus rows in the shapes `FilmCanonicalizer` decides over — a resolved row, an
 * unresolved year-bearing or yearless row, a resolved row whose cinema published an
 * original title and a runtime — and the one question the matching specs ask of a
 * corpus: after `groupByFilm` + `clusterByFilm`, do two keys share a film?
 *
 * Shared by `FilmCanonicalizerSpec` (one rule at a time), `MatchingCorporaSpec` (the
 * historical never-merge / always-merge pairs) and `MatchingPropertySpec`.
 */
object CanonicalizerRows {

  type Row = (CacheKey, MovieRecord)

  def cacheKey(title: String, year: Option[Int]): CacheKey = CacheKey(title, year, titleNormalizer)

  /** A resolved row: carries a tmdbId and a Tmdb slot whose releaseYear IS the
   *  cluster's authoritative tmdbYear. */
  def resolved(title: String, tmdbId: Int, tmdbYear: Int, cinema: Source): Row =
    cacheKey(title, Some(tmdbYear)) -> MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(releaseYear = Some(tmdbYear)),
        cinema -> SourceData(title = Some(title), releaseYear = Some(tmdbYear))
      )
    )

  /** An unresolved cinema row, keyed at `year` (yearless when None). */
  def unresolved(title: String, year: Option[Int], cinema: Source): Row =
    cacheKey(title, year) -> MovieRecord(
      data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), releaseYear = year))
    )

  /** A resolved row whose cinema PUBLISHED an original title and a runtime — the
   *  evidence `MixedFilmDetector` reads when the fold asks whether two rows describe
   *  different films. */
  def published(title: String, tmdbId: Int, tmdbYear: Int, cinema: Source,
                originalTitle: String, runtime: Int, imdbId: Option[String] = None): Row =
    cacheKey(title, Some(tmdbYear)) -> MovieRecord(
      tmdbId = Some(tmdbId), imdbId = imdbId,
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(releaseYear = Some(tmdbYear)),
        cinema -> SourceData(title = Some(title), releaseYear = Some(tmdbYear),
                             originalTitle = Some(originalTitle), runtimeMinutes = Some(runtime))))

  /** The film partition the settle reaches: identity components, each split per film. */
  def films(rows: Seq[Row], normalizer: TitleNormalizer = titleNormalizer): Seq[Seq[Row]] =
    FilmCanonicalizer.groupByFilm(rows, normalizer).flatMap(FilmCanonicalizer.clusterByFilm(_, normalizer))

  /** Do the rows keyed `a` and `b` land in one film? */
  def sameFilm(rows: Seq[Row], a: CacheKey, b: CacheKey, normalizer: TitleNormalizer = titleNormalizer): Boolean =
    films(rows, normalizer).exists { film => val keys = film.map(_._1).toSet; keys(a) && keys(b) }
}
