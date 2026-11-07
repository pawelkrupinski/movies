package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.TakeFlips
import services.movies.ListingKey

/** THE SEEDED FLIP the convergence leg's flip check must catch (scripts.IdentityDrift, [[TakeFlips]]): "own match first,
 *  correction later", replayed step by step as the leg replays a corpus — a boot, a listing joining a take the evidence
 *  corrects, applies with nothing new, its answers filed, a re-scrape, a deploy — every listing's served take recorded at
 *  each apply. PL "Ktoś całkiem obcy" (2026-10-25): Bałtyk's "Klub Konesera" listing joined Na Starówce's corrected take
 *  and both were served the model's film until its answers came in, then the correction again (UK "The Stranger" at
 *  Riverside Woodbridge flips the same way, 1429348 <-> 848791). Replayed over a [[CorrectionWorld]]. */
class CorrectionFlipReplaySpec extends AnyFlatSpec with Matchers {
  import CorrectionWorld._
  import CorrectionFlipReplaySpec.Replay

  "the leg's flip check" should "find no flip in a replay of a listing joining a corrected take" in {
    val replay = Replay()
    replay.flips.flips.map(_.line) shouldBe empty
    replay.flips.recorded shouldBe 7
    replay.last shouldBe Map(first.key -> Some(Corrected), joined.key -> Some(Corrected))
  }
}

object CorrectionFlipReplaySpec {
  import CorrectionWorld._

  /** One replay, as the leg's worker runs one: each step's events, then an apply whose takes [[flips]] records. */
  final case class Replay() {
    private val world = new CorrectionWorld
    private var stage = world.started()

    val flips = new TakeFlips
    var last  = Map.empty[ListingKey, Option[Int]]
    private var listings = Seq(first)
    private var cluster  = Set.empty[ListingKey]
    private var steps    = 0
    /** One apply after `events`: its takes recorded, a listing's inputs moved when answers were filed or its cluster changed. */
    private def applied(events: String*): Unit = {
      val resolution = stage.apply(model(listings), key => listings.find(_.key == key), world.version)
      steps += 1
      val now = listings.map(_.key).toSet
      flips.record(s"$steps ${if (events.isEmpty) "re-apply" else events.mkString("+")}",
        resolution.decisions.iterator.flatMap(d => d.members.map(_ -> TakeFlips.Served(d.film.fold("none")(id => s"tmdb:$id"), s"${d.basis}"))),
        _ => events.contains("answer") || now != cluster)
      cluster = now
      last = resolution.decisions.flatMap(d => d.members.map(_ -> d.film)).toMap
    }

    applied("boot")
    world.file("filed|elsewhere") // past the boot's pass: what the worker's projection meets afresh is asked inline
    applied("answer")
    listings = Seq(first, joined)
    applied("scrape")
    applied()
    world.answers()
    applied("answer")
    applied("scrape")
    stage = world.started()
    applied("deploy")
  }
}
