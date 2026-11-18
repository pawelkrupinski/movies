package services.tasks

/**
 * One stored external source an operator can have fetched again for given keys — TMDB's film records, a rating source's
 * rows — when what it answered is known to have changed (an edit made on TMDB today, a rating site's page fixed) and the
 * periodic paths would reach it only later. Fetching again is the source's own path (its client, its pace, its rate
 * limits: the external-api-rate-limits skill), and so is the downstream work what moved kicks: an identity re-resolve
 * through the model's store filings, a read-model projection through the film row's change.
 *
 * A new source is a new implementation and one entry in the worker's [[RefreshableSources]] (OperatorWiring): the task,
 * its handler and the operator's script stay as they are.
 */
trait RefreshableSource {
  /** The stable name the task's payload names the source by (`tmdb-films`, `imdb`, …). */
  def name: String
  /** What the source's keys are, for an operator: `TMDB film ids`. */
  def keys: String
  /** Fetch again what the source stores for `keys`, kicking what moved downstream; what it came to. */
  def refresh(keys: Seq[String]): BulkRefreshResult
}

/** The sources this worker can refresh, by name: only those it wires (Filmweb only where it is enabled). */
final class RefreshableSources(val all: Seq[RefreshableSource]) {
  require(all.map(_.name).distinct.size == all.size, s"two refreshable sources share a name: ${all.map(_.name).mkString(", ")}")
  def byName(name: String): Option[RefreshableSource] = all.find(_.name == name)
}

object RefreshableSources {
  /** The names of the sources a worker may wire: what the enqueuing script accepts. */
  val TmdbFilms  = "tmdb-films"
  val Imdb       = "imdb"
  val Filmweb    = "filmweb"
  val Metacritic = "metacritic"
  val Rt         = "rt"
  val Names: Seq[String] = Seq(TmdbFilms, Imdb, Filmweb, Metacritic, Rt)

  /** The most keys one task carries: a task runs on one worker thread within its lease, so a longer list is several. */
  val MaxKeys = 50

  private val SourceKey = "source"
  private val KeysKey   = "keys"

  /** A `RefreshSource` task's payload, and the dedup key a repeat of the same request collapses into. */
  def payload(source: String, keys: Seq[String]): Map[String, String] = Map(SourceKey -> source, KeysKey -> keys.distinct.sorted.mkString("\n"))
  def dedupKey(source: String, keys: Seq[String]): String = s"refresh-source|$source|${keys.distinct.sorted.mkString(",")}"

  /** The source a task names, and its keys. */
  def sourceOf(task: Task): String = task.payload.getOrElse(SourceKey, "")
  def keysOf(task: Task): Seq[String] = task.payload.get(KeysKey).toSeq.flatMap(_.split('\n')).map(_.trim).filter(_.nonEmpty).distinct
}
