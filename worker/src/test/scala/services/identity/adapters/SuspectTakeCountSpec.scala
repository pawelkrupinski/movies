package services.identity
package adapters

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
    count(NoFilmLookups, ids => stored.view.filterKeys(ids.contains).toMap, counts => reported = counts)(resolution, fragment ++ (old +: current))
    reported shouldBe Signal.values.map(_ -> 0).toMap ++ Map(Signal.Fragment -> 1, Signal.Era -> 1)
  }

  it should "leave out a suspect a judgement judges, and count the billings taken as a film judged wrong apart" in {
    val fumo   = listing("Giorgia Fumo - OUT OF OFFICE")
    val other  = listing("Lorenzo Fumo - OUT OF OFFICE")
    val lalka  = listing("Lalka")
    val films  = Map(1 -> IdentityMeasures.Film("Out of Office", year = Some(2022)), 2 -> IdentityMeasures.Film("Lalka", year = Some(2026)))
    val judged = new TakeJudgements.Index(Seq(
      TakeJudgements.Judgement("pl", "*", "Giorgia  Fumo - OUT OF OFFICE", "tmdb:1", right = true),
      TakeJudgements.Judgement("pl", KinoMuza.displayName, "Lalka", "tmdb:2", right = false)))
    var reported = Map.empty[Signal, Int]
    var wrong    = -1
    count(NoFilmLookups, _ => Map.empty, counts => reported = counts, judgements = judged, reportWrong = n => wrong = n)(
      resolutionOf(Seq(decision(Seq(fumo), 1), decision(Seq(other), 1), decision(Seq(lalka), 2)), films), Seq(fumo, other, lalka))
    reported(Signal.Fragment) shouldBe 1 // Lorenzo's, which no judgement judges
    wrong shouldBe 1
  }

  it should "name the billings, the most venues first, at most a bounded number" in {
    val take = (raw: String, venue: models.Cinema) =>
      SuspectTakes.Take(listing(raw, venue), "tmdb:1", Some(1), IdentityMeasures.Film("Out of Office", year = Some(2022)),
        decision(Seq(listing(raw, venue)), 1))
    val found = Seq(take("A", KinoMuza) -> "fragment: x", take("B", KinoMuza) -> "era: y", take("B", models.Helios) -> "era: y")
    SuspectTakeCount.named(found) shouldBe Seq("B → tmdb:1 Out of Office (2022) [2 venue(s)]: era: y",
      "A → tmdb:1 Out of Office (2022) [1 venue(s)]: fragment: x")
    SuspectTakeCount.named((1 to 60).map(i => take(s"T$i", KinoMuza) -> "x")).size shouldBe SuspectTakeCount.Named
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
    count(page, _ => Map.empty, counts => reported = counts)(
      resolutionOf(Seq(decision(Seq(short), 1)), Map(1 -> IdentityMeasures.Film("Herr der Ringe", year = Some(2025), runtime = Some(14)))),
      Seq(short))
    reported(Signal.Runtime) shouldBe 1
  }

  it should "read the agreed films' records in one batch, not one read per film" in {
    val listings = (1 to 3).map(i => listing(s"Film $i"))
    val agreed   = listings.zipWithIndex.map { case (l, i) => decision(Seq(l), 101 + i, ResolverDecision.Basis.Agreed) }
    val asked    = scala.collection.mutable.ListBuffer.empty[Seq[Int]]
    count(NoFilmLookups, ids => { asked += ids; Map.empty }, _ => ())(resolutionOf(agreed, Map.empty), listings)
    asked.toSeq shouldBe Seq(Seq(101, 102, 103))
  }

  "a store's records" should "be read in one prefetch of every film asked for, and let go once read" in {
    val calls  = scala.collection.mutable.ListBuffer.empty[String]
    val store  = new IdentityLookups {
      private var held = Set.empty[Int]
      override def hasDetail(l: Listing): Boolean = false
      override def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
      override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = Answer.Known(Nil)
      override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = {
        calls += s"prefetch ${films.mkString(",")}"; held = films.toSet
      }
      override def prefetchAnswered(): Unit = { calls += "answered"; held = Set.empty }
      // a film the prefetch did not hold is a read of its own
      override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
        if (!held(id)) calls += s"read $id"
        Answer.Known(Option.when(id != 2)(IdentityMeasures.Film(s"film $id")))
      }
    }
    SuspectTakeCount.recordsOf(store)(Seq(1, 2, 3)).keySet shouldBe Set(1, 3)
    calls.toSeq shouldBe Seq("prefetch 1,2,3", "answered")
  }

  "a submitted count" should "run on its runner, not the projection's thread, counting only the latest submission waiting" in {
    val queued   = scala.collection.mutable.Queue.empty[Runnable]
    val reported = scala.collection.mutable.ListBuffer.empty[Map[Signal, Int]]
    val counter  = count(NoFilmLookups, _ => Map.empty, reported += _, runner = queued.enqueue(_))
    val (a, b)   = (listing("Giorgia Fumo - OUT OF OFFICE"), listing("Lalka"))
    val films    = Map(1 -> IdentityMeasures.Film("Out of Office", year = Some(2022)), 2 -> IdentityMeasures.Film("Lalka", year = Some(2026)))
    counter.submit(resolutionOf(Seq(decision(Seq(b), 2)), films), Seq(b))
    counter.submit(resolutionOf(Seq(decision(Seq(a), 1)), films), Seq(a))
    reported shouldBe empty
    queued.size shouldBe 1
    queued.dequeue().run()
    reported.toSeq.map(_(Signal.Fragment)) shouldBe Seq(1)
  }

  it should "log its counts as fields, under the id of the projection that submitted it, though another thread counts" in {
    val queued  = scala.collection.mutable.Queue.empty[Runnable]
    val counter = count(NoFilmLookups, _ => Map.empty, _ => (), runner = queued.enqueue(_))
    val a       = listing("Giorgia Fumo - OUT OF OFFICE")
    tools.LogFields.within("projection_id" -> "tick-7")(
      counter.submit(resolutionOf(Seq(decision(Seq(a), 1)), Map(1 -> IdentityMeasures.Film("Out of Office", year = Some(2022)))), Seq(a)))
    val events = tools.LogCapture.thisThread(classOf[SuspectTakeCount].getName)(queued.dequeue().run())
      .filter(_.getFormattedMessage.startsWith("identity suspect takes: counted"))
    events should have size 1
    events.head.getFormattedMessage should include("1 suspect billing(s) (fragment 1)")
    events.head.getMDCPropertyMap.get("projection_id") shouldBe "tick-7"
    val fields = scala.jdk.CollectionConverters.ListHasAsScala(events.head.getKeyValuePairs).asScala.map(p => p.key -> (p.value: Any)).toMap
    fields("suspected") shouldBe 1
    fields("suspect_fragment") shouldBe 1
    fields("suspect_era") shouldBe 0
  }

  it should "count the decisions and listings last counted over only once" in {
    var counted = 0
    val counter = count(NoFilmLookups, _ => Map.empty, _ => counted += 1)
    val l = listing("Lalka")
    val resolution = resolutionOf(Seq(decision(Seq(l), 2)), Map(2 -> IdentityMeasures.Film("Lalka", year = Some(2026))))
    counter.submit(resolution, Seq(l))
    counter.submit(resolution, Seq(l))
    counted shouldBe 1
    counter.submit(resolution, Seq(l, listing("Lalka", models.Helios)))
    counted shouldBe 2
  }

  it should "count nothing while the heap is pressed, and count on the next submission once it is not" in {
    var counted = 0
    var pressed = true
    val counter = count(NoFilmLookups, _ => Map.empty, _ => counted += 1, pressed = () => pressed)
    val l = listing("Lalka")
    val resolution = resolutionOf(Seq(decision(Seq(l), 2)), Map(2 -> IdentityMeasures.Film("Lalka", year = Some(2026))))
    counter.submit(resolution, Seq(l))
    counted shouldBe 0
    pressed = false
    counter.submit(resolution, Seq(l))
    counted shouldBe 1
  }

  private def count(venues: IdentityLookups, records: Seq[Int] => Map[Int, IdentityMeasures.Film], report: Map[Signal, Int] => Unit,
                    runner: java.util.concurrent.Executor = _.run(), pressed: () => Boolean = () => false,
                    judgements: TakeJudgements.Index = TakeJudgements.Index.Empty, reportWrong: Int => Unit = _ => ()) =
    new SuspectTakeCount(venues, records, tools.SpecClock.Pinned, java.time.ZoneOffset.UTC, report, runner, pressed, judgements, reportWrong)

  private def resolutionOf(decisions: Seq[ResolverDecision], films: Map[Int, IdentityMeasures.Film]): Resolution =
    Resolution(decisions, decisions.map(_.members.size).sum, Map.empty, Nil, Nil, 0, 0, 0, 0, 0, films)
}
