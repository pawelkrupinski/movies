package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A value cached by its full dependency set: valid only while every dependency's digest is unchanged, the dirty index
 *  (checked at a version, nothing it read filed since) agreeing with full validation, and a disagreement counted. */
class MemoSpec extends AnyFlatSpec with Matchers {

  private val title  = Dependency.Answer(Question.Family(agreement.VoterFamily.Imdb, "title|Lalka"))
  private val record = Dependency.Lookup(Dependency.Via.Tmdb, Dependency.Aspect.Answer, Question.FilmRecord(913760))
  private final case class Cached(film: Int, reads: Map[Dependency, Long])

  private final class World {
    val answers = scala.collection.mutable.Map[Dependency, Long](title -> 1L, record -> 2L)
    val filings = scala.collection.mutable.ArrayBuffer.empty[(Long, String)]
    var version = 0L
    def file(read: Dependency, digest: Long): Unit = { answers(read) = digest; version += 1; filings += version -> read.filing }
    val filedSince: Long => Option[Set[String]] = since => Some(filings.iterator.collect { case (at, id) if at > since => id }.toSet)
    val now: Dependency => Option[Long] = answers.get
    val memo = new Memo[String, Cached](_.reads, _.readsTmdb)
  }

  "a memo entry" should "stand while nothing it read is filed, and stop at the first filing of what it read" in {
    val w = new World
    w.memo.put("k", Cached(1, Map(title -> 1L, record -> 2L))); w.memo.check("k", stamp = 7L, version = w.version)
    w.memo.clean("k", 7L, w.version, w.filedSince) shouldBe true
    w.file(Dependency.Answer(Question.Family(agreement.VoterFamily.Wiki, "title|Other")), 9L)   // read by no entry
    w.memo.newPass()
    w.memo.clean("k", 7L, w.version, w.filedSince) shouldBe true
    w.file(record, 3L)
    w.memo.newPass()
    w.memo.clean("k", 7L, w.version, w.filedSince) shouldBe false
    w.memo.stands(w.memo.get("k").get, w.now) shouldBe false
  }

  it should "stand under the stamp it was checked under alone" in {
    val w = new World
    w.memo.put("k", Cached(1, Map(title -> 1L))); w.memo.check("k", stamp = 7L, version = 0L)
    w.memo.clean("k", 8L, 0L, w.filedSince) shouldBe false
  }

  it should "stand by full validation exactly while every digest it recorded is unchanged" in {
    val w = new World
    val cached = Cached(1, Map(title -> 1L, record -> 2L))
    w.memo.stands(cached, w.now) shouldBe true
    w.memo.stands(cached, w.now.andThen(_.map(_ + 1))) shouldBe false
    w.memo.stands(cached.copy(reads = cached.reads + (Dependency.Unparsed("venues|detail|0") -> 0L)), w.now) shouldBe false
  }

  "full validation" should "count as drift an entry the index calls clean whose dependency moved unannounced, and uncheck it" in {
    val w = new World
    w.memo.put("k", Cached(1, Map(title -> 1L, record -> 2L))); w.memo.check("k", 7L, w.version)
    w.memo.verified("k", w.memo.get("k").get, w.now) shouldBe Memo.Validation(1, 0)
    w.answers(record) = 3L   // filed with no announcement: the index cannot know
    w.memo.clean("k", 7L, w.version, w.filedSince) shouldBe true
    w.memo.verified("k", w.memo.get("k").get, w.now) shouldBe Memo.Validation(1, 1)
    w.memo.isChecked("k") shouldBe false
  }

  "the dirty index" should "answer for the indexed filings of the entries held, and forget an entry's once it is replaced or removed" in {
    val w = new World
    w.memo.put("a", Cached(1, Map(title -> 1L, record -> 2L)))
    w.memo.put("b", Cached(2, Map(record -> 2L)))
    w.memo.reads(record.filing) shouldBe true
    w.memo.reads(title.filing) shouldBe false   // not indexed: no TMDB read
    w.memo.remove("a")
    w.memo.reads(record.filing) shouldBe true
    w.memo.put("b", Cached(2, Map(title -> 1L)))
    w.memo.reads(record.filing) shouldBe false
  }
}
