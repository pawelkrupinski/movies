package tools

import scala.io.Source
import scala.util.Try

/**
 * Reads environment-style configuration with a layered fallback:
 *
 *   0. An admin override (installed at runtime via [[installOverrides]]) — the
 *      Mongo-backed `env_overrides` flips set on the `/admin/config` page. Wins
 *      over every static source so a flip takes effect even when a Fly env var
 *      is set; consulted live on every read, so a knob read per-use changes
 *      mid-flight (no restart) within the override cache's refresh interval.
 *   1. Process environment variable (System.getenv)
 *   2. JVM system property of the same name (-Dkey=value or sys.props)
 *   3. `.env.local` in the working directory — a gitignored file for local dev.
 *      Format: simple `KEY=VALUE` lines, `#` for comments, optional quoting.
 *
 * The `.env.local` route is meant for putting secrets like ZYTE_API_KEY in
 * a local file without polluting the shell or the repository.
 *
 * Every read also self-registers its key, type and default into an in-memory
 * [[registry]], so the admin page can enumerate all knobs without a hand-kept
 * manifest: adding `Env.positiveLong("KINOWO_NEW", x)` anywhere surfaces it on
 * the page on that process's next registry publish.
 */
object Env {

  /** The kind of a registered knob — drives parsing on the admin page and lets
   *  the page show only the flippable numeric/string knobs. */
  enum Kind { case Str, Int, Long }

  /** A self-registered config knob: its key, type, and default (None for the
   *  untyped [[get]], whose callers supply their own downstream default). */
  final case class Knob(key: String, kind: Kind, default: Option[String])

  // ── admin override source (installed by the composition root) ───────────────
  // A cheap, thread-safe lookup into the live override cache. Default: no
  // overrides, so Env behaves exactly as before until a process installs one.
  @volatile private var overrideSource: String => Option[String] = _ => None

  /** Install the live override lookup (the Mongo-backed override cache). The last
   *  install wins; called once per process at boot. */
  def installOverrides(source: String => Option[String]): Unit = overrideSource = source

  // ── auto-registry ───────────────────────────────────────────────────────────
  private val registry = new java.util.concurrent.ConcurrentHashMap[String, Knob]()
  private def register(knob: Knob): Unit = { registry.put(knob.key, knob); () }

  /** Every knob this process has read so far, sorted by key. */
  def knobs: Seq[Knob] = {
    import scala.jdk.CollectionConverters._
    registry.values().asScala.toVector.sortBy(_.key)
  }

  // ── resolution ──────────────────────────────────────────────────────────────
  /** The static value (env → property → file), ignoring any admin override. */
  private def staticValue(key: String): Option[String] =
    Option(System.getenv(key)).filter(_.nonEmpty)
      .orElse(Option(System.getProperty(key)).filter(_.nonEmpty))
      .orElse(fileVars.get(key))
      .filter(_.nonEmpty)

  /** Override (if any) wins over the static sources. */
  private def resolve(key: String): Option[String] =
    overrideSource(key).filter(_.nonEmpty).orElse(staticValue(key))

  /** The value this process is currently using for `key` (post-override) — what
   *  the admin page reports as "current". None when neither an override nor any
   *  static source supplies it (the consumer is on its compiled-in default). */
  def currentValue(key: String): Option[String] = resolve(key)

  def get(key: String): Option[String] = {
    register(Knob(key, Kind.Str, None))
    resolve(key)
  }

  /** A strictly-positive Int from `key`, or `default` when unset, unparseable,
   *  or ≤ 0. For numeric tuning knobs (concurrency budgets, …) where a bad value
   *  should fall back to the sane default rather than crash or disable the work. */
  def positiveInt(key: String, default: Int): Int = {
    register(Knob(key, Kind.Int, Some(default.toString)))
    resolve(key).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)
  }

  /** A strictly-positive Long from `key`, or `default` when unset, unparseable,
   *  or ≤ 0. For numeric tuning knobs (intervals in seconds, …). */
  def positiveLong(key: String, default: Long): Long = {
    register(Knob(key, Kind.Long, Some(default.toString)))
    resolve(key).flatMap(_.toLongOption).filter(_ > 0).getOrElse(default)
  }

  private lazy val fileVars: Map[String, String] =
    Try {
      val file = new java.io.File(".env.local")
      if (!file.exists()) Map.empty[String, String]
      else {
        val source = Source.fromFile(file, "UTF-8")
        try {
          source.getLines()
            .map(_.trim)
            .filterNot(line => line.isEmpty || line.startsWith("#"))
            .flatMap { line =>
              val index = line.indexOf('=')
              if (index <= 0) None
              else {
                val key = line.take(index).trim
                val value = line.drop(index + 1).trim
                  .stripPrefix("\"").stripSuffix("\"")
                  .stripPrefix("'").stripSuffix("'")
                Some(key -> value)
              }
            }.toMap
        } finally source.close()
      }
    }.getOrElse(Map.empty)
}
