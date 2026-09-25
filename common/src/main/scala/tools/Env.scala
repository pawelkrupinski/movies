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
 *   1. The static source this instance was built over — for [[Env.fromProcess]]:
 *      the process environment variable (System.getenv), then the JVM system
 *      property of the same name, then `.env.local` in the working directory (a
 *      gitignored `KEY=VALUE` file for local dev, `#` for comments, optional
 *      quoting — meant for secrets like ZYTE_API_KEY without polluting the shell).
 *
 * ONE instance per process, built at the composition root (`AppLoader` for web,
 * `WorkerMain` for the worker) and handed to whatever reads a knob. It holds the
 * process's mutable config state — the installed override source and the knob
 * registry — which is why it is an instance rather than an object: two instances
 * (two specs, two wirings under test) never see each other's overrides or knobs.
 *
 * Every read also self-registers its key, type and default into this instance's
 * registry, so the admin page can enumerate all knobs without a hand-kept
 * manifest: adding `env.positiveLong("KINOWO_NEW", x)` anywhere surfaces it on
 * the page on that process's next registry publish.
 */
final class Env(staticSource: String => Option[String]) {
  import Env.{Kind, Knob}

  // ── admin override source (installed by EnvConfigService) ───────────────────
  // A cheap, thread-safe lookup into the live override cache. Default: no
  // overrides, so the static sources decide until a process installs one.
  @volatile private var overrideSource: String => Option[String] = _ => None

  /** Install the live override lookup (the Mongo-backed override cache). The last
   *  install wins; called once per process at boot. */
  def installOverrides(source: String => Option[String]): Unit = overrideSource = source

  // ── auto-registry ───────────────────────────────────────────────────────────
  private val registry = new java.util.concurrent.ConcurrentHashMap[String, Knob]()
  private def register(knob: Knob): Unit = { registry.put(knob.key, knob); () }

  /** Every knob read through this instance so far, sorted by key. */
  def knobs: Seq[Knob] = {
    import scala.jdk.CollectionConverters._
    registry.values().asScala.toVector.sortBy(_.key)
  }

  // ── resolution ──────────────────────────────────────────────────────────────
  /** Override (if any) wins over the static sources. */
  private def resolve(key: String): Option[String] =
    overrideSource(key).filter(_.nonEmpty).orElse(staticSource(key).filter(_.nonEmpty))

  /** The value this process is currently using for `key` (post-override) — what
   *  the admin page reports as "current". None when neither an override nor any
   *  static source supplies it (the consumer is on its compiled-in default). */
  def currentValue(key: String): Option[String] = resolve(key)

  def get(key: String): Option[String] = {
    register(Knob(key, Kind.Str, None))
    resolve(key)
  }

  /** A boolean switch: true for `true` or `1`, false for anything else,
   *  including unset. Both spellings because both are what the deployment
   *  surfaces produce — a Fly secret and a Kubernetes env value are strings, and
   *  whoever sets one writes whichever of the two they think in. */
  def flag(key: String): Boolean = {
    register(Knob(key, Kind.Str, Some("false")))
    resolve(key).exists(v => v == "true" || v == "1")
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
}

object Env {

  /** The kind of a registered knob — drives parsing on the admin page and lets
   *  the page show only the flippable numeric/string knobs. */
  enum Kind { case Str, Int, Long }

  /** A self-registered config knob: its key, type, and default (None for the
   *  untyped [[Env.get]], whose callers supply their own downstream default). */
  final case class Knob(key: String, kind: Kind, default: Option[String])

  /** This process's configuration: env var → system property → `localFile`
   *  (`.env.local` in the working directory by default). The file is read at most
   *  once, and only when a key misses the first two. Built ONCE per process at the
   *  composition root; everything else is handed the instance. */
  def fromProcess(localFile: java.io.File = new java.io.File(".env.local")): Env = {
    lazy val fileVars = readVarsFile(localFile)
    new Env(key =>
      Option(System.getenv(key)).filter(_.nonEmpty)
        .orElse(Option(System.getProperty(key)).filter(_.nonEmpty))
        .orElse(fileVars.get(key)))
  }

  /** An instance over a fixed map — for a spec that needs a knob set without
   *  touching the process environment. */
  def of(vars: (String, String)*): Env = new Env(vars.toMap.get)

  /** Parse a `KEY=VALUE` file (`#` comments, optional single/double quoting).
   *  Empty when the file is absent or unreadable. */
  private[tools] def readVarsFile(file: java.io.File): Map[String, String] =
    Try {
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
