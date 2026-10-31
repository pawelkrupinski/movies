package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

/** The convergence leg's flip check: a listing's served take, recorded at every step, flips when it goes back to a film
 *  it left, changes more than once, or changes where none of its deciding inputs did — and only then. */
class TakeFlipsSpec extends AnyFlatSpec with Matchers {
  private val stranger = ListingKey.Published("Riverside Theatre Woodbridge", "The Stranger", None, Nil)
  private val other    = ListingKey.Published("Riverside Theatre Woodbridge", "Wicked", None, Nil)
  private def own(id: Int)       = TakeFlips.Served(s"tmdb:$id", "OwnMatch: own match")
  private def corrected(id: Int) = TakeFlips.Served(s"tmdb:$id", "Corrected: the posters name it")

  /** One replay: each step's takes, each listing's inputs moved where `moved` names it. */
  private def replay(steps: (String, Map[ListingKey, TakeFlips.Served], Set[ListingKey])*): TakeFlips = {
    val flips = new TakeFlips
    steps.foreach { case (step, takes, moved) => flips.record(step, takes, moved) }
    flips
  }

  "a take moved once by an input that changed" should "be no flip" in {
    replay(("1 boot", Map(stranger -> own(1429348)), Set.empty), ("2 answer", Map(stranger -> corrected(848791)), Set(stranger)),
      ("3 scrape", Map(stranger -> corrected(848791)), Set.empty)).flips shouldBe empty
  }

  "a take going back to a film it left" should "flip, naming every take with its step and path" in {
    val flips = replay(("1 boot", Map(stranger -> corrected(848791), other -> own(1)), Set.empty),
      ("2 scrape", Map(stranger -> own(1429348), other -> own(1)), Set(stranger, other)),
      ("3 answer", Map(stranger -> corrected(848791), other -> own(1)), Set(stranger, other))).flips
    flips.map(_.key) shouldBe Seq(stranger)
    flips.head.films shouldBe "tmdb:848791 -> tmdb:1429348 -> tmdb:848791"
    flips.head.reasons shouldBe Seq("back to a film it left")
    flips.head.line should include("[2 scrape: OwnMatch: own match]")
  }

  "a take changing twice" should "flip" in {
    replay(("1 boot", Map(stranger -> own(1)), Set.empty), ("2 answer", Map(stranger -> own(2)), Set(stranger)),
      ("3 answer", Map(stranger -> own(3)), Set(stranger))).flips.map(_.reasons) shouldBe Seq(Seq("moved between films 2 times"))
  }

  "a take lost while answers were missing and found again, or a first take of no film" should "be no flip" in {
    replay(("1 boot", Map(stranger -> own(1429348), other -> TakeFlips.Served("none", "BelowThreshold")), Set.empty),
      ("2 fill", Map(stranger -> TakeFlips.Served("none", "NoEvidence", pending = true), other -> own(1)), Set(stranger, other)),
      ("3 answer", Map(stranger -> own(1429348), other -> own(2)), Set(stranger, other))).flips shouldBe empty
  }

  // The review of 58ea51d22 (2026-10-30): a none counted alike whatever it was left the check blind to a take dropping out
  // and back at any step where some answer, anywhere, was filed.
  "a take dropped to no film with nothing pending and given back" should "flip: it left the film and went back" in {
    val flips = replay(("1 boot", Map(stranger -> own(1429348)), Set.empty),
      ("2 fill", Map(stranger -> TakeFlips.Served("none", "BelowThreshold")), Set(stranger)),
      ("3 answer", Map(stranger -> own(1429348)), Set(stranger))).flips
    flips.map(_.reasons) shouldBe Seq(Seq("back to a film it left"))
    flips.head.films shouldBe "tmdb:1429348 -> none -> tmdb:1429348"
  }

  "no film given up and found again, a film between" should "be no flip: none is no film it left" in {
    replay(("1 boot", Map(stranger -> TakeFlips.Served("none", "BelowThreshold")), Set.empty),
      ("2 answer", Map(stranger -> own(1)), Set(stranger)),
      ("3 gone", Map(stranger -> TakeFlips.Served("none", "BelowThreshold")), Set(stranger))).flips shouldBe empty
  }

  "a take changing where none of its inputs did" should "flip, even once: an interim take a pending correction overrides" in {
    val flips = replay(("1 boot", Map(stranger -> own(1429348)), Set.empty), ("2 re-apply", Map(stranger -> corrected(848791)), Set.empty)).flips
    flips.map(_.reasons) shouldBe Seq(Seq("changed with its inputs unchanged at 2 re-apply"))
    flips.head.line should include("inputs unchanged")
  }

  "a listing's first take, and a step it is not served at" should "be no change" in {
    replay(("1 boot", Map(other -> own(1)), Set.empty), ("2 scrape", Map(stranger -> own(2), other -> own(1)), Set.empty),
      ("3 gone", Map(other -> own(1)), Set.empty), ("4 re-apply", Map(stranger -> own(2), other -> own(1)), Set.empty)).flips shouldBe empty
  }

  "a take served Pending at a step none of its inputs moved" should "be no change: no film YET is no take" in {
    replay(("1 boot", Map(stranger -> own(1429348)), Set.empty),
      ("2 deploy", Map(stranger -> TakeFlips.Served("none", "BelowThreshold", pending = true)), Set.empty),
      ("3 re-apply", Map(stranger -> own(1429348)), Set(stranger))).flips shouldBe empty
  }

  // Step 6 (the owner, 2026-10-31): a Pending take is the decision on the inputs as they stand -- a film or none -- served
  // provisionally, never a previous take. Provisional, it is no take: UK flicks.co.uk "Rose (2023)" serves the parade's
  // hit while the parade's record is pending, and Oplev's film once the record denies the parade.
  private def provisional(id: Int) = TakeFlips.Served(s"tmdb:$id", "OwnMatch: own match", pending = true)

  "a provisional film served while an answer was pending, between two decided takes of one film" should "be no flip" in {
    replay(("1 boot", Map(stranger -> own(901251)), Set.empty),
      ("2 fill", Map(stranger -> provisional(1069112)), Set(stranger)),
      ("3 answer", Map(stranger -> own(901251)), Set(stranger))).flips shouldBe empty
  }

  "a provisional take decided as it stood, at a step none of its inputs moved" should "be no change" in {
    replay(("1 boot", Map(stranger -> provisional(1069112)), Set.empty),
      ("2 re-apply", Map(stranger -> own(1069112)), Set.empty)).flips shouldBe empty
  }

  "a take left for a provisional film that is then decided, and given back" should "flip: the decided film was left" in {
    val flips = replay(("1 boot", Map(stranger -> own(1)), Set.empty),
      ("2 fill", Map(stranger -> provisional(2)), Set(stranger)),
      ("3 answer", Map(stranger -> own(2)), Set(stranger)),
      ("4 scrape", Map(stranger -> own(1)), Set(stranger))).flips
    flips.map(_.reasons) shouldBe Seq(Seq("back to a film it left"))
    flips.head.films shouldBe "tmdb:1 -> tmdb:2 -> tmdb:2 -> tmdb:1"
  }
}
