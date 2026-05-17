package services.movies

import models.{MovieRecord, Source, SourceData}

/**
 * Field-level diff between two `MovieRecord` snapshots — the minimal set of
 * per-field updates needed to turn `before` into `after`.
 *
 * Used by `MovieRepo.updateIfPresent` to drive a `$set`/`$unset` Mongo
 * update instead of a full-doc `replaceOne`. Out-of-band edits to fields
 * the in-memory cache didn't touch then survive a concurrent write — the
 * audit-vs-rating-tick race that produced the wrong Filmweb URLs on
 * production rows.
 *
 * `FieldUpdate` distinguishes:
 *   - `NoChange` — `before == after`; field is omitted from the wire update.
 *   - `Unset`    — `before` was `Some(x)`, `after` is `None`; emit `$unset`.
 *   - `SetTo(v)` — `after` is `Some(v)` and differs from `before`; emit
 *                  `$set` with the new value.
 *
 * The `data` map is diffed per-key; only the source slots that changed
 * land in the wire update.
 */
final case class MovieRecordPatch(
  imdbId:            FieldUpdate[String]                    = FieldUpdate.NoChange,
  imdbRating:        FieldUpdate[Double]                    = FieldUpdate.NoChange,
  metascore:         FieldUpdate[Int]                       = FieldUpdate.NoChange,
  filmwebUrl:        FieldUpdate[String]                    = FieldUpdate.NoChange,
  filmwebRating:     FieldUpdate[Double]                    = FieldUpdate.NoChange,
  rottenTomatoes:    FieldUpdate[Int]                       = FieldUpdate.NoChange,
  tmdbId:            FieldUpdate[Int]                       = FieldUpdate.NoChange,
  metacriticUrl:     FieldUpdate[String]                    = FieldUpdate.NoChange,
  rottenTomatoesUrl: FieldUpdate[String]                    = FieldUpdate.NoChange,
  data:              Map[Source, FieldUpdate[SourceData]]   = Map.empty
) {
  /** No effective change — caller can skip the write entirely. */
  def isEmpty: Boolean =
    imdbId == FieldUpdate.NoChange && imdbRating == FieldUpdate.NoChange &&
    metascore == FieldUpdate.NoChange && filmwebUrl == FieldUpdate.NoChange &&
    filmwebRating == FieldUpdate.NoChange && rottenTomatoes == FieldUpdate.NoChange &&
    tmdbId == FieldUpdate.NoChange && metacriticUrl == FieldUpdate.NoChange &&
    rottenTomatoesUrl == FieldUpdate.NoChange && data.isEmpty

  /** Apply the patch to `current`, returning a new `MovieRecord` with the
   *  changed fields overwritten and everything else preserved. Used by
   *  `InMemoryMovieRepo` to mirror Mongo's `$set`/`$unset` semantics.
   */
  def applyTo(current: MovieRecord): MovieRecord = {
    def merge[A](u: FieldUpdate[A], existing: Option[A]): Option[A] = u match {
      case FieldUpdate.NoChange  => existing
      case FieldUpdate.Unset     => None
      case FieldUpdate.SetTo(v)  => Some(v)
    }
    val mergedData = data.foldLeft(current.data) {
      case (acc, (source, FieldUpdate.SetTo(sd)))  => acc + (source -> sd)
      case (acc, (source, FieldUpdate.Unset))      => acc - source
      case (acc, (_,      FieldUpdate.NoChange))   => acc
    }
    current.copy(
      imdbId            = merge(imdbId,            current.imdbId),
      imdbRating        = merge(imdbRating,        current.imdbRating),
      metascore         = merge(metascore,         current.metascore),
      filmwebUrl        = merge(filmwebUrl,        current.filmwebUrl),
      filmwebRating     = merge(filmwebRating,     current.filmwebRating),
      rottenTomatoes    = merge(rottenTomatoes,    current.rottenTomatoes),
      tmdbId            = merge(tmdbId,            current.tmdbId),
      metacriticUrl     = merge(metacriticUrl,     current.metacriticUrl),
      rottenTomatoesUrl = merge(rottenTomatoesUrl, current.rottenTomatoesUrl),
      data              = mergedData
    )
  }
}

object MovieRecordPatch {

  /** Compute the minimal patch that turns `before` into `after`. */
  def diff(before: MovieRecord, after: MovieRecord): MovieRecordPatch =
    MovieRecordPatch(
      imdbId            = diffOpt(before.imdbId,            after.imdbId),
      imdbRating        = diffOpt(before.imdbRating,        after.imdbRating),
      metascore         = diffOpt(before.metascore,         after.metascore),
      filmwebUrl        = diffOpt(before.filmwebUrl,        after.filmwebUrl),
      filmwebRating     = diffOpt(before.filmwebRating,     after.filmwebRating),
      rottenTomatoes    = diffOpt(before.rottenTomatoes,    after.rottenTomatoes),
      tmdbId            = diffOpt(before.tmdbId,            after.tmdbId),
      metacriticUrl     = diffOpt(before.metacriticUrl,     after.metacriticUrl),
      rottenTomatoesUrl = diffOpt(before.rottenTomatoesUrl, after.rottenTomatoesUrl),
      data              = diffData(before.data, after.data)
    )

  private def diffOpt[A](before: Option[A], after: Option[A]): FieldUpdate[A] =
    if (before == after) FieldUpdate.NoChange
    else after match {
      case Some(v) => FieldUpdate.SetTo(v)
      case None    => FieldUpdate.Unset
    }

  private def diffData(
    before: Map[Source, SourceData],
    after:  Map[Source, SourceData]
  ): Map[Source, FieldUpdate[SourceData]] =
    (before.keySet ++ after.keySet).flatMap { source =>
      (before.get(source), after.get(source)) match {
        case (Some(b), Some(a)) if b == a => None
        case (Some(_), Some(a))           => Some(source -> FieldUpdate.SetTo(a))
        case (None,    Some(a))           => Some(source -> FieldUpdate.SetTo(a))
        case (Some(_), None)              => Some(source -> FieldUpdate.Unset)
        case (None,    None)              => None
      }
    }.toMap
}

enum FieldUpdate[+A]:
  case NoChange
  case Unset
  case SetTo(value: A)
