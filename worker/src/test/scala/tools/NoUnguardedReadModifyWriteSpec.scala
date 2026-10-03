package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, codeOf, scalaFiles}

import scala.util.matching.Regex

/**
 * A repository method that reads a Mongo row and then writes it decides the write on what it read
 * — and, unguarded, writes that decision over whatever another writer landed in between: a LOST
 * UPDATE. Staging rows re-stamped over a resolve, a searched IMDb id over TMDB's, a blank scrape's
 * marker over a listing that had just landed: each was fixed by hand, each with its own
 * compare-and-set. The shape is now `tools.GuardedWrite` (read → decide → write only over what was
 * read, re-deciding on a mismatch) with `services.MongoGuard` building the Mongo guard.
 *
 * This flags, in main sources, a method that both reads (`find`, `first`, `headOption`) and writes
 * (`replaceOne`, `updateOne`, their bulk models, `findOneAndReplace`) a collection without going
 * through either. A write that is deliberately last-writer-wins goes in [[Allowlist]] with WHY.
 *
 * Its reach is one method: a read in one method and the write in another (a helper each) is not
 * seen — route both through `GuardedWrite`, which keeps them in one place by construction.
 */
class NoUnguardedReadModifyWriteSpec extends AnyFlatSpec with Matchers {

  private val Read: Regex   = """\.\s*(?:find\s*[\[(]|first\s*\(\s*\)|headOption\s*\(\s*\))""".r
  private val Write: Regex  = """\b(?:replaceOne|updateOne|ReplaceOneModel|UpdateOneModel|findOneAndReplace)\s*[\[(]""".r
  private val Guarded: Regex = """\b(?:GuardedWrite|MongoGuard)\b""".r
  private val Def: Regex    = """\bdef\s+([\w$]+)""".r

  /** (file, method) → why its read-then-write is right unguarded. */
  private val Allowlist: Map[(String, String), String] = Map(
    ("common/src/main/scala/services/movies/MovieRepository.scala", "upsert") -> (
      "last-writer-wins by design: the whole-record path writes MovieCache's copy of the film, and the cache is the one " +
        "writer of `movies` and serialises a film's writes in-process; the read decides only whether the stored document " +
        "already equals the write (skip) and whether its identity collides (the unique index refuses a racing sibling)"),
    ("common/src/main/scala/services/identity/IdentityTraceStore.scala", "write") -> (
      "one writer thread per store (see the class doc): the read is the stored digests, deciding only whether a trace " +
        "moved; a trace another process wrote meanwhile is rewritten whole by the next model update"),
    ("common/src/main/scala/services/config/EnvRegistryStore.scala", "publish") -> (
      "each app publishes only its own slice, from one process, every config tick: the read decides which rows differ, " +
        "and a racing publish of the same slice writes the same rows"),
    ("worker/src/main/scala/services/tasks/MongoChunkScrapeStore.scala", "startRun") -> (
      "the claim is itself conditional — an insert on the unique `_id`, else a findOneAndReplace filtered on the stale " +
        "`createdAt` it supersedes — so the filter is the guard"),
    ("common/src/main/scala/services/identity/IdentityModelStore.scala", "replace") -> (
      "one writer per country (the projection's model update): the read is the stored content digests, deciding only " +
        "which families moved; a family another process wrote meanwhile is rewritten whole by the next model update")
  )

  /** Every (method, body) of `src`: from each `def` to the next. */
  private def methods(src: String): Seq[(String, String)] = {
    val defs = Def.findAllMatchIn(src).toSeq
    defs.zipWithIndex.map { case (m, i) =>
      m.group(1) -> src.substring(m.start, if (i + 1 < defs.size) defs(i + 1).start else src.length)
    }
  }

  private def unguarded(src: String): Seq[String] =
    methods(src).collect { case (name, body) if Read.findFirstIn(body).isDefined && Write.findFirstIn(body).isDefined &&
      Guarded.findFirstIn(body).isEmpty => name }.distinct

  private lazy val found: Set[(String, String)] = scalaFiles(MainRoots).flatMap { p =>
    val src = codeOf(p)
    if (src.contains("MongoCollection")) unguarded(src).map(p.toString -> _) else Nil
  }.toSet

  "A repository method that reads and then writes a row" should "guard the write on what it read, or say why not" in {
    val offenders = (found -- Allowlist.keySet).toSeq.sorted
    withClue(s"${offenders.size} method(s) write over a row they read without a guard — route them through " +
      "tools.GuardedWrite + services.MongoGuard, or allowlist them with WHY:\n  " + offenders.mkString("\n  ") + "\n") {
      offenders shouldBe empty
    }
  }

  "The allowlist" should "name only methods that still read and write unguarded" in {
    val stale = (Allowlist.keySet -- found).toSeq.sorted
    withClue(s"stale entries (the method was guarded, renamed or removed — drop them): ${stale.mkString(", ")} — ")(stale shouldBe empty)
  }

  "The lint" should "see a read-then-write in one method, and not a guarded one or a write alone" in {
    val src =
      """class A(c: MongoCollection[Document]) {
        |  def lost(id: String): Unit = {
        |    val row = Await.result(c.find(Filters.eq("_id", id)).headOption(), t)
        |    Await.result(c.replaceOne(Filters.eq("_id", id), next(row)).toFuture(), t)
        |  }
        |  def guarded(id: String): GuardedWrite[Bson] = GuardedWrite(3)(() => read(id))(decide)((r, u) =>
        |    MongoGuard.updateIfUnchanged(c, MongoGuard.unchanged(id, r, Fields), u, t))
        |  def read(id: String) = Await.result(c.find(Filters.eq("_id", id)).first().toFuture(), t)
        |  def blind(id: String): Unit = Await.result(c.updateOne(Filters.eq("_id", id), u).toFuture(), t)
        |  def bulk(id: String): Unit = { val r = c.find(f).first(); c.bulkWrite(Seq(UpdateOneModel(f, u))) }
        |}""".stripMargin
    unguarded(src) shouldBe Seq("lost", "bulk")
  }
}
