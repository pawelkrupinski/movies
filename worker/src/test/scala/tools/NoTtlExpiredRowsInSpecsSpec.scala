package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * An `it/` spec must not write rows Mongo's TTL monitor may already delete.
 *
 * The class of failure: a class that owns a TTL index (`MongoTtlIndex.ensure/reconcile`, or its own
 * `expireAfter(...)`) is handed a pinned PAST instant — [[SpecClock.Pinned]] or an `Instant.parse`
 * literal — so every row it writes is expired on arrival, and the monitor, running about once a
 * minute on the server's own clock, may delete it before the spec reads it back. The auth exchange
 * code spec lost its code this way in a loaded `itAll`; the resolution-store and uptime round-trip
 * specs carried the same exposure. Pin to [[MongoTtlSpecClock.Pinned]] instead.
 *
 * The TTL-owning classes are discovered from the main sources, so a new one is covered the day it
 * gains its index.
 */
class NoTtlExpiredRowsInSpecsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{MainRoots, codeOf, read, scalaFiles}

  private val ItRoots      = Seq("web/src/it", "worker/src/it")
  private val TtlIndexCall = """MongoTtlIndex\.(?:ensure|reconcile)\(|IndexOptions\(\)\s*\.expireAfter\(""".r
  private val Declaration  = """\b(?:class|object)\s+([\w$]+)""".r
  private val PastPinned   = """\bSpecClock\.Pinned\b""".r
  private val ParsedDate   = """Instant\.parse\(\s*"([^"]+)"""".r
  private val Horizon      = MongoTtlSpecClock.Pinned.instant()

  /** The class or object enclosing each TTL-index call in `src`. */
  private[tools] def ttlOwners(src: String): Set[String] =
    TtlIndexCall.findAllMatchIn(src).flatMap(m => Declaration.findAllMatchIn(src.substring(0, m.start)).toSeq.lastOption.map(_.group(1))).toSet

  /** The 1-based lines of `src` pinning a time the TTL monitor may already have passed. */
  private[tools] def expiredPins(src: String): Seq[Int] = {
    def lineOf(at: Int) = src.substring(0, at).count(_ == '\n') + 1
    val pinned = PastPinned.findAllMatchIn(src).map(m => lineOf(m.start))
    val parsed = ParsedDate.findAllMatchIn(src).collect { case m if Instant.parse(m.group(1)).isBefore(Horizon) => lineOf(m.start) }
    (pinned ++ parsed).toSeq.distinct.sorted
  }

  /** it/ file → why it may pin a past time although it names a TTL-owning class. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/it/scala/TaskClaimsAcrossWorkersIntegrationSpec.scala" ->
      ("its pinned Now feeds only MongoTaskQueue (no TTL index); the MongoScheduledRunStore it also races stamps " +
        "claimedAt with the system clock, so its rows are never born expired")
  )

  // The helper that builds TTL indexes for its callers — the callers are the owners.
  private lazy val owners: Set[String] =
    scalaFiles(MainRoots).flatMap(p => ttlOwners(codeOf(p))).toSet - "MongoTtlIndex"

  "the matchers" should "find each TTL owner and each expired pin" in {
    ttlOwners("class A {}\nclass MongoStore(db: Db) {\n  MongoTtlIndex.reconcile(c, \"at\", 60L, \"x\", m)\n}") shouldBe Set("MongoStore")
    ttlOwners("class B { c.createIndex(i, new JIndexOptions().expireAfter(60L, SECONDS)) }") shouldBe Set("B")
    ttlOwners("class C { Caffeine.newBuilder().expireAfterWrite(1, SECONDS) }") shouldBe empty
    ttlOwners("object D { Caffeine.newBuilder().expireAfter(Expiry.creating((_, v) => ttl)) }") shouldBe empty
    expiredPins("new Store(clock = _root_.tools.SpecClock.Pinned)") shouldBe Seq(1)
    expiredPins("val now = Instant.parse(\"2026-09-23T12:00:00Z\")") shouldBe Seq(1)
    expiredPins("new Store(clock = _root_.tools.MongoTtlSpecClock.Pinned)") shouldBe empty
    expiredPins("val later = Instant.parse(\"2127-01-01T00:00:00Z\")") shouldBe empty
  }

  "the TTL-owning classes" should "be discovered from the main sources" in {
    owners should contain allOf ("MongoAuthExchangeCodeStore", "MongoResolutionStore", "UptimeMonitor", "MongoCachingDetailFetch")
  }

  "it specs naming a TTL-owning class" should "pin time past the TTL monitor's reach" in {
    val word  = owners.map(name => name -> s"\\b${java.util.regex.Pattern.quote(name)}\\b".r).toMap
    val found = scalaFiles(ItRoots).filterNot(path => Allowlist.contains(path.toString)).flatMap { path =>
      val src = codeOf(path)
      if (!word.values.exists(_.findFirstIn(src).isDefined)) Nil
      else {
        val raw = read(path).linesIterator.toIndexedSeq
        expiredPins(src).map(n => s"$path:$n: ${raw(n - 1).trim}")
      }
    }
    withClue("These specs hand a TTL-owning class a time already past, so Mongo's TTL monitor may delete the " +
      "rows mid-spec — use tools.MongoTtlSpecClock.Pinned:\n" + found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "keep every allowlist entry pointing at a file that still pins a past time" in {
    Allowlist.keys.foreach(file => withClue(s"$file: ")(expiredPins(codeOf(java.nio.file.Paths.get(file))) should not be empty))
  }
}
