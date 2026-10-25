package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every Prometheus family is registered at ONE place in the main sources, and no two families share a help string.
 *
 * The class of failure: a new counter was registered under the name of an existing one,
 * `kinowo_worker_identity_take_changes_total`, with help of its own. Both compiled, every spec built its own registry, and
 * Prometheus's client refused the second registration at runtime ("Conflicting help strings"), in production. Two
 * declarations of one name are that failure waiting for the two classes to meet in a registry; one help string on two
 * families is the copy-paste that leads to it, and leaves a dashboard reader two series that claim to be the same thing.
 *
 * The scan reads every `.name("kinowo_…")` literal of a builder chain in common, identity, scrapers, worker and web, and
 * the `.help("…")` that follows it. A counter's `_total` is the same family as its base name, as the client registers
 * it. A family whose name is passed in (`ScheduledTickMetrics`, `MongoWriteErrors.Series`, one class per tier) is
 * declared through its builder once and is not a literal here, which is what keeps one help string over its two tiers.
 */
class MetricRegistrationsUniqueSpec extends AnyFlatSpec with Matchers {

  import MetricRegistrationsUniqueSpec._

  private lazy val registrations: Seq[Registration] = ScalaSourceScan.scalaFiles(ScalaSourceScan.MainRoots).flatMap { path =>
    val source = ScalaSourceScan.codeOf(path)
    Declaration.findAllMatchIn(source).map(m => Registration(path.toString, familyOf(m.group(1)), Option(m.group(2)), built = true)) ++
      Handed.findAllMatchIn(source).flatMap(m => Name.findAllMatchIn(m.group(1)).map(n => Registration(path.toString, familyOf(n.group(1)), None, built = false)))
  }

  "the metric registration scan" should "reach the metric classes of every module" in {
    val modules = registrations.map(_.path.takeWhile(_ != '/')).toSet
    modules should contain allOf ("common", "worker", "web")
    registrations.size should be > 100 // a broken pattern must not pass vacuously
  }

  "a metric family" should "be registered at one place only" in {
    val twice = registrations.groupBy(_.family).collect { case (family, sites) if sites.size > 1 =>
      s"$family: ${sites.map(_.path).mkString(", ")}"
    }
    withClue(s"families registered more than once — Prometheus refuses the second with \"Conflicting help strings\":\n${twice.toSeq.sorted.mkString("\n")}\n") {
      twice shouldBe empty
    }
  }

  "a help string" should "describe one family only" in {
    val shared = registrations.flatMap(r => r.help.map(_ -> r.family)).groupMap(_._1)(_._2).collect {
      case (help, families) if families.distinct.size > 1 => s"${families.distinct.sorted.mkString(", ")}: \"$help\""
    }
    withClue(s"families sharing one help string:\n${shared.toSeq.sorted.mkString("\n")}\n") {
      shared shouldBe empty
    }
  }

  it should "be read off every registration the scan finds" in {
    val helpless = registrations.filter(r => r.built && r.help.isEmpty).map(r => s"${r.family} (${r.path})")
    withClue(s"a `.name(\"kinowo_…\")` with no `.help(\"…\")` literal after it, which the uniqueness check cannot see:\n${helpless.mkString("\n")}\n") {
      helpless shouldBe empty
    }
  }

  "the declaration pattern" should "read a builder's name and help, across lines, and a counter's family without _total" in {
    val source = "Counter.builder()\n    .name(\"kinowo_worker_x_total\")\n    .help(\"Things, by \\\"kind\\\".\")\n    .labelNames(\"country\")"
    val m = Declaration.findFirstMatchIn(source).get
    familyOf(m.group(1)) shouldBe "kinowo_worker_x"
    m.group(2) shouldBe "Things, by \\\"kind\\\"."
  }
}

object MetricRegistrationsUniqueSpec {

  /** One family's declaration: `built` when it is a builder's `.name(…)` literal, else handed to a class that builds it. */
  final case class Registration(path: String, family: String, help: Option[String], built: Boolean)

  /** A builder's `.name("kinowo_…")` and, when the next call is one, its `.help("…")` (the first literal of it). */
  private val Declaration = """\.name\(\s*"(kinowo_[a-z0-9_]+)"\s*\)(?:\s*\.help\(\s*s?"((?:[^"\\]|\\.)*)")?""".r

  /** Names handed to a class that registers them: `ScheduledTickMetrics(registry, "kinowo_…", "kinowo_…")`,
   *  `CacheGauge.register(registry, "kinowo_…", …)`. */
  private val Handed = """\(\s*registry\s*,((?:\s*"kinowo_[a-z0-9_]+"\s*,?)+)""".r
  private val Name   = """"(kinowo_[a-z0-9_]+)"""".r

  /** A counter registers under its base name: `x_total` and `x` are one family. */
  def familyOf(name: String): String = name.stripSuffix("_total")
}
