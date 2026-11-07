package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Listing, Question}
import services.movies.ListingKey

import scala.collection.mutable

/** A corrected take must not depend on the order its inputs arrive in: a listing joining a cluster whose model take the
 *  evidence corrects, its own poster and its title's family answers filed before it joins or after, is taken as the
 *  corrected film at every apply — never as the model's film for the seconds its answers take (PL "Ktoś całkiem obcy",
 *  2026-10-25: Bałtyk's "Klub Konesrea: Ktoś całkiem obcy" joined Na Starówce's corrected cluster, and both listings showed
 *  the model's 2007 film until the families and the poster were in, 21 s later). Replayed over a [[CorrectionWorld]]. */
class CorrectionArrivalOrderSpec extends AnyFlatSpec with Matchers {
  import CorrectionWorld._
  import CorrectionArrivalOrderSpec.Run

  "a listing joining a corrected cluster" should "be taken as the corrected film at every apply, whichever of it and its answers comes first" in {
    val joinFirst    = Run(answersFirst = false)
    val answersFirst = Run(answersFirst = true)
    joinFirst.seen.flatMap(_.values).flatten should not contain ModelFilm
    answersFirst.seen.flatMap(_.values).flatten should not contain ModelFilm
    joinFirst.seen.last shouldBe answersFirst.seen.last
    joinFirst.seen.last shouldBe Map(joined.key -> Some(Corrected), first.key -> Some(Corrected))
  }

  it should "still be decided again once its own poster is in: a partial read of the posters is no final one" in {
    val run = Run(answersFirst = false)
    run.stage.wanted.collect { case Question.Poster(q) => q } shouldBe empty
    run.seen.last shouldBe Map(joined.key -> Some(Corrected), first.key -> Some(Corrected))
  }
}

object CorrectionArrivalOrderSpec {
  import CorrectionWorld._

  /** One history: the first listing alone, corrected; then the joining listing and its answers — its poster hashed and
   *  its title's family questions answered — in the order given. `seen`: every listing's take at each apply. */
  final case class Run(answersFirst: Boolean) {
    private val world = new CorrectionWorld
    val stage: AgreementStage = world.started()

    private var listings = Seq(first)
    private def listingOf: ListingKey => Option[Listing] = key => listings.find(_.key == key)
    val seen = mutable.ArrayBuffer.empty[Map[ListingKey, Option[Int]]]
    private def applied(): Unit = {
      val resolution = stage.apply(model(listings), listingOf, world.version)
      seen += resolution.decisions.flatMap(d => d.members.map(_ -> d.film)).toMap
    }

    applied()
    world.file("filed|elsewhere")   // past the boot's pass: what the worker's projection meets afresh is asked inline
    applied()
    if (answersFirst) world.answers()
    listings = Seq(first, joined)
    applied()
    if (!answersFirst) { applied(); world.answers() }
    applied()
    applied()
  }
}
