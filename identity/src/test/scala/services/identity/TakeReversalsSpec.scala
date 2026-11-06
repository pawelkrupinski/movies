package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ResolverDecision.Basis
import services.movies.ListingKey
import tools.MutableClock

import java.time.Instant

class TakeReversalsSpec extends AnyFlatSpec with Matchers {

  private def key(title: String): ListingKey = ListingKey.Published("Chichester Cinema at New Park", title, None, Nil)
  private def change(title: String, before: Option[Int], after: Option[Int], trigger: String = "scrape") =
    TakeChange(key(title), Some(s"film-$title"), before, after, trigger = Some(trigger))
  /** A decision of `basis` holding `members` (the listing alone by default), as the projection hands it over. */
  private def decided(basis: Basis, members: Seq[ListingKey] = Nil): ListingKey => Option[ResolverDecision] =
    k => Some(ResolverDecision(if (members.isEmpty) Seq(k) else members, None, 0.9, basis, Nil)())
  private val model = decided(Basis.OwnMatch)
  private val ties  = "The Ties That Bind Us"
  private def fresh() = { val clock = new MutableClock(Instant.parse("2026-11-05T10:00:00Z")); (clock, new TakeReversals(clock)) }

  "take reversals" should "count a listing the next projection moves back to the film it held, an oscillation and a flip" in {
    // UK Chichester, 2026-10-14: the periodic projection's exact-top-hit own match takes the 2008 film (1460982), the
    // changed-scope projection's correction the 2025 one (1096642) back 17–20 s later
    val (clock, reversals) = fresh()
    reversals.note(Seq(change(ties, Some(1096642), Some(1460982))), model) shouldBe empty
    clock.advanceSeconds(18)
    val back = reversals.note(Seq(change(ties, Some(1460982), Some(1096642))), decided(Basis.Corrected))
    back.map(r => (r.change.after, r.oscillation, r.flip, r.pattern, r.droppedBy, r.restoredBy, r.seconds, r.lasted)) shouldBe
      Seq((Some(1096642), true, true, "film-film-film", "model", "correction", 18.0, "1m"))
    reversals.note(Seq(change(ties, Some(1096642), Some(1460982))), model) should have size 1
  }

  it should "count a take lost and regained, and not a move on to a third film" in {
    // the backtest's commonest flip: a model-stage loss a later stage restores in the next projection, 5-7 s later
    val (_, reversals) = fresh()
    reversals.note(Seq(change("Kaidan", Some(30959), None), change("Mozu", Some(1), Some(2))), model)
    val back = reversals.note(Seq(change("Kaidan", None, Some(30959)), change("Mozu", Some(2), Some(3))), decided(Basis.Filled))
    back.map(r => (r.change.listing, r.pattern, r.droppedBy, r.restoredBy)) shouldBe Seq((key("Kaidan"), "film-none-film", "model", "fill"))
  }

  it should "name a take gained and lost again none-film-none, and a change no decision holds none" in {
    val (clock, reversals) = fresh()
    reversals.note(Seq(change("Lalka", None, Some(7))), decided(Basis.Agreed))
    clock.advanceSeconds(2 * 3600)
    reversals.note(Seq(change("Lalka", Some(7), None)), _ => None).map(r => (r.pattern, r.droppedBy, r.restoredBy, r.lasted)) shouldBe
      Seq(("none-film-none", "agreement", "none", "24h"))
  }

  it should "count a correction many projections later as a flip and no oscillation, and forget a change past the horizon" in {
    // DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)": lost to a gap 10-13 14:54, regained by a fix 10-14 06:51
    val (clock, reversals) = fresh()
    reversals.note(Seq(change("Wolf", Some(581), None)), model)
    (1 to 3).foreach(_ => reversals.note(Nil, model))
    clock.advanceSeconds(16 * 3600)
    reversals.note(Seq(change("Wolf", None, Some(581))), model).map(r => (r.oscillation, r.flip, r.lasted, r.projections)) shouldBe
      Seq((false, true, "24h", 4L))
    reversals.note(Seq(change("Wolf", Some(581), None)), model)
    clock.advanceSeconds(24 * 3600 + 1)
    reversals.note(Seq(change("Wolf", None, Some(581))), model) shouldBe empty
  }

  it should "bucket how long the undone take stood: under a minute, under an hour, under a day" in {
    val (clock, reversals) = fresh()
    def flipAfter(seconds: Long): String = {
      reversals.note(Seq(change("A", Some(1), Some(2))), model)
      clock.advanceSeconds(seconds)
      reversals.note(Seq(change("A", Some(2), Some(1))), model).map(_.lasted).mkString
    }
    Seq(flipAfter(59), flipAfter(60), flipAfter(3599), flipAfter(3600)) shouldBe Seq("1m", "1h", "1h", "24h")
  }

  it should "not call a reversal either side of which a deploy set off a flip, unless a pin decided it" in {
    // the backtest leaves out a reversal whose either change's trigger is `deploy`; a pin's line reports `review`
    val (_, reversals) = fresh()
    reversals.note(Seq(change("Boot", Some(1), None, trigger = "deploy"),
      change("Pin", Some(1), None, trigger = "deploy").copy(basisAfter = Some(Basis.Pinned))), model)
    val back = reversals.note(Seq(change("Boot", None, Some(1)), change("Pin", None, Some(1))), model)
    back.map(r => (r.change.listing.rawTitle, r.flip, r.oscillation, r.cause)) shouldBe
      Seq(("Boot", false, true, "DEPLOY/RESTART"), ("Pin", true, true, "RULE"))
  }

  it should "classify each by scripts/flip-timeline's classes: an answer's restore, the decision's members moved, else the rule" in {
    // DE Geister (2026-11-04): the scrape projection's model stage drops the take, the answer-triggered one 6 s later
    // restores it once the late ask is filed
    val (_, reversals) = fresh()
    val geister  = key("Geister")
    val siblings = Seq(geister, ListingKey.Published("Kino Babylon", "Geister", None, Nil))
    reversals.note(Seq(change("Geister", Some(9), None), change("Hay Fever", Some(4), None), change("Eternity", Some(7), Some(8))), model)
    val back = reversals.note(Seq(change("Geister", None, Some(9), trigger = "answer+scrape"), change("Hay Fever", None, Some(4)),
      change("Eternity", Some(8), Some(7))), model)
    back.map(r => (r.change.listing.rawTitle, r.cause)) shouldBe Seq(("Geister", "LATE-ANSWER"), ("Hay Fever", "RULE"), ("Eternity", "RULE"))
    reversals.note(Seq(change("Geister", Some(9), None)), model)
    reversals.note(Seq(change("Geister", None, Some(9))), decided(Basis.OwnMatch, siblings)).map(_.cause) shouldBe Seq("SIBLING/POPULATION")
  }

  it should "never read a new listing's first take, or a change keeping the film, as a reversal" in {
    val (_, reversals) = fresh()
    reversals.note(Seq(change("Diuna", Some(2), None)), model)
    reversals.note(Seq(TakeChange(key("Diuna"), None, None, Some(2))), model) shouldBe empty
    reversals.note(Seq(change("Diuna", None, None)), model) shouldBe empty
  }

  it should "forget the least recently changed listing past its capacity" in {
    val clock     = new MutableClock(Instant.parse("2026-11-05T10:00:00Z"))
    val reversals = new TakeReversals(clock, maxHeld = 1)
    reversals.note(Seq(change("A", Some(1), Some(2)), change("B", Some(1), Some(2))), model)
    reversals.note(Seq(change("A", Some(2), Some(1))), model) shouldBe empty // A forgotten when B was noted
  }
  it should "start from a seed of the latest changes within the horizon, at most its capacity, none an oscillation's" in {
    val clock = new MutableClock(Instant.parse("2026-11-05T10:00:00Z"))
    val at    = clock.millis()
    def held(seconds: Long) = TakeReversals.Held(Some(1), Some(2), Some("model"), deploy = false, Set.empty[ListingKey].hashCode, at - seconds * 1000, 0L)
    val reversals = new TakeReversals(clock, maxHeld = 2,
      seed = Seq(key("Old") -> held(25 * 3600), key("A") -> held(300), key("B") -> held(200), key("C") -> held(100)))
    // the next projection, the boot's first: neither an oscillation (projections apart unknown), both flips
    reversals.note(Seq("B", "C").map(change(_, Some(2), Some(1))), _ => None)
      .map(r => (r.change.listing.rawTitle, r.oscillation, r.flip)) shouldBe Seq(("B", false, true), ("C", false, true))
    // Old past the horizon, A the least recent past the capacity of two: neither seeded
    reversals.note(Seq("Old", "A").map(change(_, Some(2), Some(1))), _ => None) shouldBe empty
  }
}
