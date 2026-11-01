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

  val TitleKey = "title"
  val YearKey  = "year"

  def titleOf(payload: Map[String, String]): String = payload.getOrElse(TitleKey, "")
  def yearOf(payload: Map[String, String]): Option[Int] =
    payload.get(YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)

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
