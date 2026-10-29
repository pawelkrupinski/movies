package scripts

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.IdentityCalibration
import services.identity.IdentityCalibration.{Calibration, CannotLinkRule, Condition, ScopeModel, SignalWeights, Threshold}
import services.identity.IdentityMeasures.{Category, Missing, Number}
import tools.UnmatchedClusters.{Label, Take}

/** The weekly rule refit: what it may move, how far, how it refits a signal's weights, and when it keeps a change. */
class IdentityRefitSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  import IdentityRefit._

  private val shipped = IdentityCalibration.resolver

  // ── refitting a signal ──

  /** A title-like table fitted by the calibration itself from `counts`. */
  private def table(counts: (String, (Int, Int))*): SignalWeights =
    fit("title", SignalWeights("categorical", counts = counts.map { case (k, (p, n)) => k -> Seq(p, n) }.toMap))

  "A shipped signal" should "refit to itself from its own counts, and move nothing without a unit" in {
    val signals = shipped.scopes("listing-film").signals
    signals.filter((name, w) => reproduces(name, w)).keySet should contain allOf ("title", "originalTitle", "search.rank", "venues.corroborating")
    signals.foreach { case (name, w) => refitSignal(name, w, Nil) shouldBe None }
  }

  "A refit" should "weigh a category up by the same-film units counted in it, at most a capped step" in {
    val was = table("exact" -> (3000, 600), "overlap" -> (40, 930), "none" -> (10, 250))
    val (few, share) = refitSignal("title", was, Seq.fill(3)(Some(Category("overlap")) -> true)).get
    few.categories("overlap") should be > was.categories("overlap")
    share shouldBe 1.0
    few.counts("overlap") shouldBe Seq(43, 930)
    val (many, capped) = refitSignal("title", was, Seq.fill(60)(Some(Category("overlap")) -> true)).get
    capped should be < 1.0
    (many.categories("overlap") - was.categories("overlap")) shouldBe MaxWeightStep +- 1e-9
    many.categories.map((k, x) => math.abs(x - was.categories(k))).max should be <= MaxWeightStep + 1e-9
  }

  it should "keep a directed signal monotone, on its own bins" in {
    // search.rank falls: a better rank never weighs less than a worse one, however many units land on a worse one
    val was   = shipped.scopes("listing-film").signals("search.rank")
    val units = Seq.fill(40)(Some(Number(6.0)) -> true)
    val (now, _) = refitSignal("search.rank", was, units).get
    now.bins.map(b => (b.atLeast, b.atMost)) shouldBe was.bins.map(b => (b.atLeast, b.atMost))
    now.bins.map(_.weight).sliding(2).foreach { case Seq(a, b) => a should be >= b; case _ => () }
    now.bins.find(_.contains(6.0)).get.positives shouldBe was.bins.find(_.contains(6.0)).get.positives + 40
    now.bins.find(_.contains(6.0)).get.weight should be > was.bins.find(_.contains(6.0)).get.weight
  }

  it should "relearn a signal from the fresh units alone when its inputs changed" in {
    val was = table("exact" -> (300, 60), "overlap" -> (4, 93), "none" -> (1, 25))
    val units = Seq.fill(5)(Some(Category("exact")) -> true) ++ Seq.fill(5)(Some(Category("none")) -> false) ++ Seq(Some(Missing("listing")) -> false)
    val (pooled, _) = refitSignal("title", was, units).get
    val (fresh, _)  = refitSignal("title", was, units, fresh = true).get
    pooled.counts("exact") shouldBe Seq(305, 60)
    fresh.counts("exact") shouldBe Seq(5, 0)
    fresh.counts("missing:listing") shouldBe Seq(0, 1)
  }

  "A table the calibration did not fit from its counts" should "never be refitted" in {
    val handWritten = table("exact" -> (300, 60)).copy(categories = Map("exact" -> 9.0))
    reproduces("title", handWritten) shouldBe false
    refitSignal("title", handWritten, Seq(Some(Category("exact")) -> true)) shouldBe None
  }

  // ── the cuts and bounds ──

  private val model = IdentityCalibration("v", Map(
    "listing-film" -> ScopeModel(0, Map.empty, Calibration("isotonic", Nil, Nil), Map("showRatings" -> Threshold(0.4), "cannotLink" -> Threshold(0.03))),
    "listing-listing" -> ScopeModel(0, Map.empty, Calibration("isotonic", Nil, Nil), Map("cannotLink" -> Threshold(0.3)))),
    cannotLinks = Seq(
      CannotLinkRule("runtime.delta >= 7 AND year.distance >= 4", "listing-film",
        Seq(Condition("runtime.delta", atLeast = Some(7)), Condition("year.distance", atLeast = Some(4))), 0, 10),
      CannotLinkRule("year.distance >= 27", "listing-film", Seq(Condition("year.distance", atLeast = Some(27))), 0, 10),
      CannotLinkRule("runtime.delta <= -1", "listing-film", Seq(Condition("runtime.delta", atMost = Some(-1))), 0, 10)))

  "A cut" should "move one bounded step each way" in {
    cutChanges(model).map(c => (c.scope, c.threshold, BigDecimal(c.now).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble)) should contain theSameElementsAs Seq(
      ("listing-film", "showRatings", 0.35), ("listing-film", "showRatings", 0.45),
      ("listing-film", "cannotLink", 0.0225), ("listing-film", "cannotLink", 0.0375),
      ("listing-listing", "cannotLink", 0.25), ("listing-listing", "cannotLink", 0.35))
    val moved = CutChange("listing-film", "showRatings", 0.4, 0.35).applyTo(model)
    moved.ratingCut shouldBe 0.35
    moved.scopes("listing-film").thresholds("showRatings").basis should include ("IdentityRefit")
  }

  "A learned cannot-link's bound" should "move a tenth of itself, at least 1, never across 0 — its name following" in {
    boundChanges(model).map(b => (b.rule, b.signal, b.now)) should contain theSameElementsAs Seq(
      ("runtime.delta >= 7 AND year.distance >= 4", "runtime.delta", 6.0), ("runtime.delta >= 7 AND year.distance >= 4", "runtime.delta", 8.0),
      ("runtime.delta >= 7 AND year.distance >= 4", "year.distance", 3.0), ("runtime.delta >= 7 AND year.distance >= 4", "year.distance", 5.0),
      ("year.distance >= 27", "year.distance", 24.0), ("year.distance >= 27", "year.distance", 30.0),
      ("runtime.delta <= -1", "runtime.delta", -2.0))
    val moved = BoundChange("listing-film", "runtime.delta >= 7 AND year.distance >= 4", "year.distance", 4, 5).applyTo(model)
    val rule  = moved.cannotLinks.head
    rule.name shouldBe "runtime.delta >= 7 AND year.distance >= 5"
    rule.all.map(_.atLeast) shouldBe Seq(Some(7.0), Some(5.0))
    rule.origin should include ("refit")
    moved.cannotLink("listing-film", Map("runtime.delta" -> Number(9), "year.distance" -> Number(4))) shouldBe None
    model.cannotLink("listing-film", Map("runtime.delta" -> Number(9), "year.distance" -> Number(4))).map(_.name) shouldBe
      Some("runtime.delta >= 7 AND year.distance >= 4")
  }

  // ── judging and keeping ──

  private def take(raw: String, film: Int) = Take("pl", "Kino", raw, Some(film), None, "model", s"film $film")
  private def label(raw: String, film: Int, right: Boolean = true) = Label("pl", "*", raw, s"tmdb:$film", right, "")
  private val none: ((String, String, String)) => Boolean = _ => false

  /** Raw titles whose label films fall in the fitting fold (`held` false) or the held-out one. */
  private def films(held: Boolean, n: Int): Seq[Int] = Iterator.from(1).filter(id => heldOut(s"tmdb:$id") == held).take(n).toSeq

  "Judging a refit" should "tell a right gain from a lost right take, a switch, an unjudged take and a lost expected line" in {
    val labels = Seq(label("A", 1), label("B", 2), label("C", 3), label("D", 4, right = false))
    val control = Seq(take("A", 1), take("B", 2))
    val now     = Seq(take("B", 9), take("C", 3), take("D", 4), take("E", 5))
    val (moved, lost) = judged(control, now, labels, Set(("pl", "Kino", "A", "tmdb:1")), none)
    moved.map(m => m.rawTitle -> m.verdict).toMap shouldBe Map("A" -> LostRight, "B" -> DecorationDiscovery.Switched,
      "C" -> DecorationDiscovery.Right, "D" -> DecorationDiscovery.Wrong, "E" -> DecorationDiscovery.Unjudged)
    lost shouldBe Seq("pl\tKino\tA\ttmdb:1")
  }

  "A change" should s"be kept only with $MinSupport films of the fitting fold newly right and nothing wrong, switched, unjudged or lost" in {
    val fitting = films(held = false, MinSupport)
    val held    = films(held = true, 2)
    val labels  = (fitting ++ held).map(id => label(s"t$id", id))
    def measured(now: Seq[Take], control: Seq[Take] = Nil, gaps: Int = 0) = {
      val (moved, lost) = judged(control, now, labels, Set.empty, none)
      Measured(CutChange("listing-film", "showRatings", 0.4, 0.35), moved, gaps, lost)
    }
    val all = measured((fitting ++ held).map(id => take(s"t$id", id)))
    all.kept shouldBe true
    all.supportFitting.size shouldBe MinSupport
    all.supportHeldOut.size shouldBe 2
    // held-out gains are reported, never support
    measured((fitting.drop(1) ++ held).map(id => take(s"t$id", id))).failures should contain (s"support ${MinSupport - 1} < $MinSupport")
    measured(fitting.map(id => take(s"t$id", id)) :+ take("unlabelled", 77)).kept shouldBe false
    measured(fitting.map(id => take(s"t$id", id)), gaps = 1).kept shouldBe false
    measured(fitting.map(id => take(s"t$id", id)), control = Seq(take(s"t${held.head}", held.head))).failures should contain ("1 right lost")
  }

  "The held-out fold" should "group by film, the same film always in the same fold" in {
    (1 to 2000).count(id => heldOut(s"tmdb:$id")).toDouble / 2000 shouldBe (1.0 / Folds) +- 0.03
    heldOut("tmdb:913760") shouldBe heldOut("tmdb:913760")
  }

  "The report" should "name each kept change old → new and the films it took" in {
    val fitting = films(held = false, MinSupport)
    val labels  = fitting.map(id => label(s"t$id", id))
    val now     = fitting.map(id => take(s"t$id", id))
    val m = Measured(BoundChange("listing-film", "year.distance >= 27", "year.distance", 27, 30), judged(Nil, now, labels, Set.empty, none)._1, 0, Nil)
    val text = report(Seq(m), Seq(Seq(m)), "test")
    text should include ("| 1 | listing-film cannot-link 'year.distance >= 27' on year.distance | `27` | `30` |")
    text should include (s"| pl | Kino | t${fitting.head} |")
    versioned(m.change.applyTo(shipped), shipped, Seq(m)).version should startWith (shipped.version.replaceAll("-refit-[0-9a-f]+$", "") + "-refit-")
  }

  "A refit run" should "write its report, and the versioned refit over the artefact only with --apply and a change kept" in {
    val dir = tempDirectory("refit")
    try {
      val fitting  = films(held = false, MinSupport)
      val m = Measured(BoundChange("listing-film", "year.distance >= 27", "year.distance", 27, 30),
        judged(Nil, fitting.map(id => take(s"t$id", id)), fitting.map(id => label(s"t$id", id)), Set.empty, none)._1, 0, Nil)
      val artefact = dir.resolve("identity-weights.json")
      val shippedText = play.api.libs.json.Json.prettyPrint(play.api.libs.json.Json.toJson(shipped)) + "\n"
      java.nio.file.Files.writeString(artefact, shippedText)
      val run      = Run(apply = false, artefact, Some(dir.resolve("out/report.md")), maxChanges = 5, relearn = Set.empty, threads = 1)
      val logged   = Seq.newBuilder[String]
      val keeping: Search = start => (m.change.applyTo(start), Seq(m), Seq(Seq(m)))
      refit(run, shipped, "3 labels", keeping, logged += _)
      java.nio.file.Files.readString(dir.resolve("out/report.md")) should (include("3 labels") and include("| 1 | listing-film cannot-link"))
      java.nio.file.Files.readString(artefact) shouldBe shippedText
      refit(run.copy(apply = true), shipped, "3 labels", _ => (shipped, Nil, Nil), logged += _)
      java.nio.file.Files.readString(artefact) shouldBe shippedText
      refit(run.copy(apply = true, report = None), shipped, "3 labels", keeping, logged += _)
      play.api.libs.json.Json.parse(java.nio.file.Files.readString(artefact)).as[IdentityCalibration] shouldBe versioned(m.change.applyTo(shipped), shipped, Seq(m))
      logged.result() should contain (s"wrote $artefact")
    } finally java.nio.file.Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(java.nio.file.Files.delete(_))
  }

  it should "read the weekly command line into its run" in {
    val run = Run.of(commandLine.parse(Seq("--apply", "--report", "r.md", "--threads", "3", "--relearn", "year.delta, runtime.delta,")).toOption.get)
    run shouldBe Run(apply = true, Artefact, Some(java.nio.file.Paths.get("r.md")), 5, Set("year.delta", "runtime.delta"), 3)
    notRefitted(shipped) should not contain ("title")
  }

  // ── the captures ──

  "A splice" should "keep every captured cluster a change decides alike, and replace one it decides otherwise whole" in {
    import services.identity.ResolverDecision
    def key(n: Int) = services.movies.ListingKey.Native("Kino", s"https://kino.example/$n", s"Film $n")
    def decision(film: Option[Int], members: Int*) = ResolverDecision(members.map(key), film, 0.9, ResolverDecision.Basis.values.head, Seq("why"))()
    val captured = Seq(decision(Some(1), 1, 2), decision(None, 3), decision(Some(5), 4))
    val was      = Seq(decision(Some(1), 1, 2), decision(None, 3), decision(Some(5), 4))
    // reweighed: the same outcomes at other confidences and explanations — nothing to splice
    CaptureReplay.spliced(captured, was, was.map(d => d.copy(confidence = 0.5, explanation = Seq("other"))(d.trace))) shouldBe captured
    // listing 3 now joins film 1's cluster: that cluster and listing 3's are replaced, film 5's kept as captured
    val now = Seq(decision(Some(1), 1, 2, 3), decision(Some(5), 4))
    CaptureReplay.spliced(captured, was, now) should contain theSameElementsAs Seq(decision(Some(5), 4), decision(Some(1), 1, 2, 3))
  }

  it should "replace whole every cluster a chain of shared listings reaches from a moved one, across the three partitions" in {
    import services.identity.ResolverDecision
    def key(n: Int) = services.movies.ListingKey.Native("Kino", s"https://kino.example/$n", s"Film $n")
    def decision(film: Option[Int], members: Int*) = ResolverDecision(members.map(key), film, 0.9, ResolverDecision.Basis.values.head, Seq("why"))()
    // listing 3 joins film 1 now; the capture held 3 beside 6, the control resolve 6 beside 7 and 8: one chain, every
    // capture cluster on it replaced — and film 9's, which shares no listing with it, kept as captured
    val captured = Seq(decision(Some(1), 1, 2), decision(None, 3, 6), decision(Some(7), 7, 8), decision(Some(9), 9))
    val was      = Seq(decision(Some(1), 1, 2), decision(None, 3), decision(Some(7), 6, 7, 8), decision(Some(9), 9))
    val now      = Seq(decision(Some(1), 1, 2, 3), decision(Some(7), 6, 7, 8), decision(Some(9), 9))
    CaptureReplay.spliced(captured, was, now) should contain theSameElementsAs
      Seq(decision(Some(9), 9), decision(Some(1), 1, 2, 3), decision(Some(7), 6, 7, 8))
  }

  "Asking TMDB" should s"run every question, at most ${CaptureReplay.AskConcurrency} at once" in {
    val running = new java.util.concurrent.atomic.AtomicInteger
    val most    = new java.util.concurrent.atomic.AtomicInteger
    val done    = new java.util.concurrent.atomic.AtomicInteger
    // the first asks hold until AskConcurrency of them are in flight together: asked one at a time, none ever is
    val full    = new java.util.concurrent.CountDownLatch(CaptureReplay.AskConcurrency)
    CaptureReplay.asked(Seq.fill(40)(() => {
      most.accumulateAndGet(running.incrementAndGet(), math.max)
      full.countDown()
      full.await(tools.SpecTimeouts.Settle.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
      running.decrementAndGet(); done.incrementAndGet(); ()
    }))
    done.get shouldBe 40
    most.get shouldBe CaptureReplay.AskConcurrency
  }
}
