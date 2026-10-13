package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.SuspectTakes.Signal

/** The live resolve's suspect-take count ([[SuspectTakeCount]]): billings per signal, an agreed film's record read from
 *  the store, a listing as its venue's page states it. */
class SuspectTakeCountSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, venue: models.Cinema = KinoMuza) =
    Listing(venue, services.movies.ListingKey.Published(venue.displayName, title, None, Nil), title, title, title, None, Nil, None, None, None)
  private def decision(l: Seq[Listing], film: Int, basis: ResolverDecision.Basis = ResolverDecision.Basis.OwnMatch) =
    ResolverDecision(l.map(_.key), Some(film), 0.9, basis, Nil)()

  "the count" should "report every signal, counting each suspect billing once however many venues bill it" in {
    val fragment = Seq(listing("Giorgia Fumo - OUT OF OFFICE"), listing("Giorgia Fumo - OUT OF OFFICE", models.Helios))
    val old      = listing("Michael")
    val current  = Seq(listing("Michael", models.Helios), listing("Michael", models.Multikino))
    val films    = Map(1 -> IdentityMeasures.Film("Out of Office", year = Some(2022)), 3 -> IdentityMeasures.Film("Michael", year = Some(2026)))
    // the 1924 "Michael" is an agreed film: the model never decided it, its record is the store's
    val stored   = Map(2 -> IdentityMeasures.Film("Michael", year = Some(1924)))
    val resolution = resolutionOf(Seq(decision(fragment, 1), decision(Seq(old), 2, ResolverDecision.Basis.Agreed), decision(current, 3)), films)
    var reported = Map.empty[Signal, Int]
    new SuspectTakeCount(NoFilmLookups, stored.get, tools.SpecClock.Pinned, java.time.ZoneOffset.UTC, counts => reported = counts)(
      resolution, fragment ++ (old +: current))
    reported shouldBe Signal.values.map(_ -> 0).toMap ++ Map(Signal.Fragment -> 1, Signal.Era -> 1)
  }

  it should "read a listing's running time from its venue's page" in {
    val short = listing("Herr der Ringe")
    val page  = new IdentityLookups {
      override def hasDetail(l: Listing): Boolean = true
      override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(Some(DetailFacts(None, Nil, Some(263), None)))
      override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = Answer.Known(Nil)
      override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
    }
    var reported = Map.empty[Signal, Int]
    new SuspectTakeCount(page, _ => None, tools.SpecClock.Pinned, java.time.ZoneOffset.UTC, counts => reported = counts)(
      resolutionOf(Seq(decision(Seq(short), 1)), Map(1 -> IdentityMeasures.Film("Herr der Ringe", year = Some(2025), runtime = Some(14)))),
      Seq(short))
    reported(Signal.Runtime) shouldBe 1
  }

  private def resolutionOf(decisions: Seq[ResolverDecision], films: Map[Int, IdentityMeasures.Film]): Resolution =
    Resolution(decisions, decisions.map(_.members.size).sum, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, films)
}
