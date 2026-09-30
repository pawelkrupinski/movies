package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class SlotKeyedRowsForFilmsSpec extends AnyFlatSpec with Matchers {

  private val films = (1 to 120).map(i => f"film$i%03d").toSet

  "SlotKeyed.rowsForFilmsChecked" should "ask for every piece of a page before any of them has answered" in {
    // Each piece is held open until every piece has been asked for: a read that waited on one
    // piece before asking for the next would never finish, and time out instead.
    val asked   = mutable.ArrayBuffer.empty[Seq[String]]
    val answers = mutable.ArrayBuffer.empty[(Promise[Seq[String]], Seq[String])]
    val expected = (films.size + SlotKeyed.FilmsPerRead - 1) / SlotKeyed.FilmsPerRead
    val (rows, complete) = SlotKeyed.rowsForFilmsChecked[String](films, "test", _ => ()) { ids =>
      asked.synchronized {
        asked += ids
        val promise = Promise[Seq[String]]()
        answers += promise -> ids
        if (asked.size == expected) answers.foreach { case (p, pieceIds) => p.success(pieceIds.map(_ + "|slot")) }
        promise.future
      }
    }
    complete shouldBe true
    asked.size shouldBe expected
    asked.forall(_.size <= SlotKeyed.FilmsPerRead) shouldBe true
    rows.toSet shouldBe films.map(_ + "|slot")
  }

  it should "report the whole read incomplete when any one piece fails" in {
    var warned = Option.empty[String]
    val (rows, complete) = SlotKeyed.rowsForFilmsChecked[String](films, "test", w => warned = Some(w)) { ids =>
      if (ids.contains("film060")) Future.failed(new RuntimeException("mongo down")) else Future.successful(ids)
    }
    complete shouldBe false
    rows shouldBe empty
    warned.exists(_.contains("mongo down")) shouldBe true
  }

  it should "not read at all for no films" in {
    SlotKeyed.rowsForFilmsChecked[String](Set.empty, "test", _ => ())(_ => fail("read for no films")) shouldBe (Seq.empty, true)
  }
}
