package services.movies

import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class AppliedPrefixSpec extends AnyFlatSpec with Matchers {

  private def token(n: Int) = new BsonDocument("_data", new BsonString(s"t$n"))

  private final class Positions {
    val moved = mutable.Buffer.empty[String]
    val prefix = new AppliedPrefix((t, _) => moved += t.getString("_data").getValue)
  }

  "AppliedPrefix" should "move the position only past a contiguous run of applied events" in {
    val p = new Positions
    val acks = (1 to 4).map(n => p.prefix.deliver(token(n), 0L))
    acks(1)(); acks(3)()
    p.moved shouldBe empty                      // event 1 still waits: nothing past it may be persisted
    p.prefix.waiting shouldBe 2
    acks(0)()
    p.moved shouldBe Seq("t2")                  // 1 and 2 applied; 3 still waits
    acks(2)()
    p.moved shouldBe Seq("t2", "t4")
    p.prefix.waiting shouldBe 0
  }

  it should "treat a second acknowledgement of the same event as a no-op" in {
    val p = new Positions
    val first = p.prefix.deliver(token(1), 0L)
    first(); first()
    p.moved shouldBe Seq("t1")
  }

  it should "never move the position backwards when acknowledgements race" in {
    val p = new Positions
    val acks = (1 to 2000).map(n => p.prefix.deliver(token(n), 0L))
    scala.util.Random.shuffle(acks).par4(_())
    p.moved.last shouldBe "t2000"
    p.moved.map(_.drop(1).toInt) shouldBe sorted
  }

  extension (fs: Seq[() => Unit]) private def par4(run: (() => Unit) => Unit): Unit = {
    val threads = fs.grouped(fs.size / 4).toSeq.map(g => new Thread(() => g.foreach(run)))
    threads.foreach(_.start()); threads.foreach(_.join())
  }
}
