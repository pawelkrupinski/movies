package tools

/**
 * WHO ASKED for an outbound request: the `path` label of the worker's `kinowo_worker_http_total`. The `phase` label
 * says which fetch chain a request went down (cinema sites or metadata APIs), `service` where it went; neither says
 * which part of the worker wanted it — the identity model's lookup fill, its live asks at take-up and the pipeline's own
 * enrichment all reach TMDB down the one `enrich` chain.
 *
 * The caller names itself, scoped to the work it does ([[within]]): the task worker by the task's type, the lookup fill
 * around its gaps and its refreshes, the changes sweep around a sweep, the venue page reader around a read, the model's
 * stored-first lookups around a live ask. The counter innermost in the chain ([[CountingHttpFetch]]) reads it on the
 * thread the request is made on, so no signature carries it. A pool the worker builds ([[DaemonExecutors]]) runs each
 * task under the path of the thread that handed it over ([[carried]]): a scrape's parallel detail fetches stay the
 * scrape's. A request no caller named counts as [[Other]].
 *
 * A closed set, so the label stays low-cardinality and the counter's seed grid agrees with the dashboards' filters.
 */
object HttpPath {
  /** The identity model's paced lookup fill asking its gaps (`LookupFill`). */
  val Fill = "fill"
  /** The identity model asking live, at take-up, what its store has no answer to (`StoredFirstLookups`). */
  val Live = "live"
  /** The identity agreement's questions (the `Agreement*` tasks: families, finds, records, posters, catalogue). */
  val Agreement = "agreement"
  /** TMDB's change lists swept into the model's store (`TmdbChangesSweep`). */
  val Sweep = "sweep"
  /** The model's aged questions, records and people asked again by the fill (`QuestionRenewals`). */
  val Refresh = "refresh"
  /** A venue detail page read into venue_pages (`VenuePageReader`, the `ReadVenuePage` task). */
  val VenuePage = "venue_page"
  /** The pipeline's own enrichment: ratings, IMDb-id resolution, a projected film's TMDB details. */
  val Enrich = "enrich"
  /** The scraping pipeline: listing, chunk and per-film detail scrapes. */
  val Scrape = "scrape"
  /** A request no caller named. */
  val Other = "other"

  /** Every path, for seeding the counter at 0. */
  val all: Seq[String] = Seq(Fill, Live, Agreement, Sweep, Refresh, VenuePage, Enrich, Scrape, Other)

  private val scoped = new ThreadLocal[String]

  /** The path this thread's requests are asked for: the innermost [[within]] around them, else [[Other]]. */
  def current: String = Option(scoped.get).getOrElse(Other)

  /** Run `body` with every request it makes on this thread — and on the pools it hands work to — asked for `path`. */
  def within[A](path: String)(body: => A): A = {
    val outer = scoped.get
    scoped.set(path)
    try body
    finally scoped.set(outer)
  }

  /** `task`, to run on another thread under the path of the thread handing it over — `task` itself when that thread
   *  names none, so an unscoped hand-over is exactly what it was. */
  def carried(task: Runnable): Runnable = Option(scoped.get).fold(task)(new Carried(task, _))

  /** A task run under the path it was handed over with. [[DaemonExecutors]] reaches through it to cancel a task its
   *  executor will never run. */
  final class Carried(val command: Runnable, path: String) extends Runnable {
    override def run(): Unit = within(path)(command.run())
  }
}
