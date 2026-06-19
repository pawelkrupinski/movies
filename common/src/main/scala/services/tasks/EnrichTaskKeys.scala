package services.tasks

/**
 * Dedup keys + payload shapes for the operator-triggered enrichment tasks the
 * web `/tasks` and `/debug` pages enqueue and the worker handlers consume.
 *
 * Lives in `common` (not the worker's `services.tasks`) precisely because both
 * sides need it: the web enqueues with these exact keys, the worker reads the
 * same payload back — keeping the contract in one place stops the two ends
 * drifting.
 *
 * The bulk-run keys are CONSTANT (one per source): the queue's per-`dedupKey`
 * idempotency then makes a second button click while a run is active a no-op,
 * so the corpus-wide refresh is a singleton, never stacked N-deep.
 */
object EnrichTaskKeys {

  /** The constant dedup key for each corpus-wide refresh run. */
  def bulkDedup(taskType: TaskType): String = s"bulk|${taskType.name}"

  // Per-movie TMDB resolve. Distinct dedup key per (title, year) so two
  // different films resolve concurrently, but a repeat trigger for the same row
  // while one is queued collapses to one task. Enqueued both by the normal
  // enrichment flow (each scraped film whose tmdbId is still empty) and by the
  // operator `/debug` "re-enrich" button — the `force` flag separates them: the
  // button forces a re-resolve even of an already-resolved row, the normal flow
  // does not (re-resolving a resolved row can flip it to a more-popular
  // same-title hit).
  val TitleKey         = "title"
  val YearKey          = "year"
  val DirectorKey      = "director"
  val OriginalTitleKey = "originalTitle"
  val ForceKey         = "force"

  def resolveTmdbDedup(title: String, year: Option[Int]): String =
    s"resolve-tmdb|$title|${year.map(_.toString).getOrElse("")}"

  /** Payload for a `ResolveTmdb` task. The director + originalTitle hints let
   *  the worker's `directorWalk` / secondary-title search fire for films TMDB
   *  doesn't index under their Polish title — they're the same hints the inline
   *  resolution carried off the triggering `MovieDetailsComplete`. */
  def resolveTmdbPayload(
    title:         String,
    year:          Option[Int],
    director:      Option[String] = None,
    originalTitle: Option[String] = None,
    force:         Boolean        = false
  ): Map[String, String] =
    Map(TitleKey -> title, YearKey -> year.map(_.toString).getOrElse("")) ++
      director.filter(_.nonEmpty).map(DirectorKey -> _) ++
      originalTitle.filter(_.nonEmpty).map(OriginalTitleKey -> _) ++
      (if (force) Some(ForceKey -> "true") else None)

  def titleOf(payload: Map[String, String]): String = payload.getOrElse(TitleKey, "")
  def yearOf(payload: Map[String, String]): Option[Int] =
    payload.get(YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
  def directorOf(payload: Map[String, String]): Option[String] =
    payload.get(DirectorKey).filter(_.nonEmpty)
  def originalTitleOf(payload: Map[String, String]): Option[String] =
    payload.get(OriginalTitleKey).filter(_.nonEmpty)
  def forceOf(payload: Map[String, String]): Boolean =
    payload.get(ForceKey).contains("true")

  // Per-movie IMDb-id resolution (movies path). Distinct dedup key per (title,
  // year) so different films resolve concurrently while a repeat for the same row
  // collapses. Carries the search title the IMDb suggestion lookup queries with.
  val SearchTitleKey = "searchTitle"

  def resolveImdbIdDedup(title: String, year: Option[Int]): String =
    s"resolve-imdbid|$title|${year.map(_.toString).getOrElse("")}"

  def resolveImdbIdPayload(title: String, year: Option[Int], searchTitle: String): Map[String, String] =
    Map(TitleKey -> title, YearKey -> year.map(_.toString).getOrElse(""), SearchTitleKey -> searchTitle)

  def searchTitleOf(payload: Map[String, String]): Option[String] =
    payload.get(SearchTitleKey).filter(_.nonEmpty)
}
