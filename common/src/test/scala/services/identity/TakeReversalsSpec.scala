package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

class TakeReversalsSpec extends AnyFlatSpec with Matchers {

  private def key(title: String): ListingKey = ListingKey.Published("Chichester Cinema at New Park", title, None, Nil)
  private def change(title: String, before: Option[Int], after: Option[Int]) = TakeChange(key(title), s"film-$title", before, after)
  private val ties = "The Ties That Bind Us"

  "take reversals" should "count a listing the next projection moves back to the film it held" in {
    // UK Chichester, 2026-10-14: the periodic projection's exact-top-hit own match takes the 2008 film (1460982), the
    // changed-scope projection's correction the 2025 one (1096642) back 17–20 s later
    val reversals = new TakeReversals
    reversals.note(Seq(change(ties, Some(1096642), Some(1460982)))) shouldBe empty
    reversals.note(Seq(change(ties, Some(1460982), Some(1096642)))).map(_.after) shouldBe Seq(Some(1096642))
    reversals.note(Seq(change(ties, Some(1096642), Some(1460982)))) should have size 1
  }

  it should "count a take lost and regained within the window, and not a move on to a third film" in {
    val reversals = new TakeReversals
    reversals.note(Seq(change("Kaidan", Some(30959), None), change("Mozu", Some(1), Some(2))))
    reversals.note(Seq(change("Kaidan", None, Some(30959)), change("Mozu", Some(2), Some(3)))).map(_.listing) shouldBe Seq(key("Kaidan"))
  }

  it should "not count a correction many projections after the change it undoes" in {
    // DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)": lost to a gap 10-13 14:54, regained by a fix 10-14 06:51
    val reversals = new TakeReversals(window = 3)
    reversals.note(Seq(change("Wolf", Some(581), None)))
    (1 to 3).foreach(_ => reversals.note(Nil))
    reversals.note(Seq(change("Wolf", None, Some(581)))) shouldBe empty
  }

  it should "forget the least recently changed listing past its capacity" in {
    val reversals = new TakeReversals(capacity = 1)
    reversals.note(Seq(change("A", Some(1), Some(2)), change("B", Some(1), Some(2))))
    reversals.note(Seq(change("A", Some(2), Some(1)))) shouldBe empty // A forgotten when B was noted
  }
}
