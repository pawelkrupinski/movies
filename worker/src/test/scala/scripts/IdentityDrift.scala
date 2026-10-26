package scripts

import models.Country
import org.bson.BsonDocument
import services.identity._
import services.identity.adapters.{AgreementQuestions, FamilyAnswerStore, PosterAnswerStore, PosterHashing, TmdbDocuments, TmdbKind}
import services.identity.agreement.{AgreementStage, VoterFamily}
import services.movies.ListingKey
import tools.{CatalogueFill, IdentityJudgements, MutableClock, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

/**
 * INCREMENTAL = WHOLE, on a recording: production's identity state is kept event by event — the model
 * ([[IncrementalResolver]]) takes scrapes and filed answers in, the agreement stage ([[AgreementStage]]) keeps its
 * verdicts across ticks and restarts — and must end where a whole resolve of the same listings with the same answers
 * ends. This replays a realistic sequence over the whole corpus of one country and names every listing whose take
 * differs (`scripts/identity-slice <cc> --drift`, docs/design/identity-slice.md "Incremental drift"):
 *
 *  - BOOT: the model taken up over most venues' listings, a share of TMDB's and the families' answers not filed yet
 *    ([[Gate]], [[GatedDocuments]]): what a worker meets after a deploy brought new questions;
 *  - TICKS: answers filed a part at a time (each an [[AnswersChanged]] event, as the fill files them), new venues'
 *    scrapes, a re-scrape of half the venues (the changed projection's `republished`), listings that leave and return;
 *  - DEPLOYS: the model and the stage made anew from their stores — each family, verdict and cluster key written as BSON and read
 *    back, as every in-memory store does ([[services.BsonRoundTrip]]) — once under the same rules and once under others (every family and verdict outruled);
 *  - SETTLE: every answer filed and the stage applied until it asks nothing and has no backlog.
 *
 * CHECKPOINTS, each compared listing by listing with the WHOLE resolve ([[IdentityResolver.resolve]] and a fresh stage
 * asked to its fixpoint, settling as production does once every question was attempted): `settled`; then, twice, a re-scrape tick, a deploy's first
 * tick (no answer filed in between: prod's first projection after a boot) and the tick after it. Every listing whose
 * film differs at a checkpoint is a DIVERGENCE (`drift-<cc>.tsv`), unless the allow-list
 * (`test/resources/fixtures/identity-drift-allow.tsv`: cc, venue or `*`, raw title, checkpoint or `*`, reason) names it.
 *
 * {{{
 *   IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV]
 * }}}
 * Exit: 0 no divergence beyond the allow-list, 1 divergences, 2 usage.
 */
object IdentityDrift {
  import IdentitySlice.{Reads, Recording, filmOf}

  final case class Options(country: String = "", recording: Option[Path] = None, families: Option[Path] = None, seeds: Int = 2,
                           gate: Int = 15, out: Path = Path.of("target/identity-drift"), allow: Option[Path] = None, labels: Option[Path] = None,
                           /** `--seed S`: that seed alone, to replay one divergence. */
                           only: Option[Int] = None)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                            => o
    case "--recording" :: v :: rest     => parse(rest, o.copy(recording = Some(Path.of(v))))
    case "--families" :: v :: rest      => parse(rest, o.copy(families = Option.when(v != "none")(Path.of(v))))
    case "--seeds" :: v :: rest         => parse(rest, o.copy(seeds = v.toInt))
    case "--seed" :: v :: rest          => parse(rest, o.copy(only = Some(v.toInt)))
    case "--gate" :: v :: rest          => parse(rest, o.copy(gate = v.toInt))
    case "--out" :: v :: rest           => parse(rest, o.copy(out = Path.of(v)))
    case "--allow" :: v :: rest         => parse(rest, o.copy(allow = Some(Path.of(v))))
    case "--labels" :: v :: rest        => parse(rest, o.copy(labels = Some(Path.of(v))))
    case cc :: rest if !cc.startsWith("--") && o.country.isEmpty => parse(rest, o.copy(country = cc.toLowerCase(java.util.Locale.ROOT)))
    case other :: _                     => usage(s"unknown argument $other")
  }

  private def usage(why: String): Nothing = {
    System.err.println(s"IdentityDrift: $why\n  IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV]")
    sys.exit(2)
  }

  /** One listing's take: its film (`tmdb:<id>`, `<source>:<id>` or `none`), the basis, and the decision's last line. */
  final case class Take(film: String, basis: String, why: String)
  def takesOf(resolution: Resolution): Map[ListingKey, Take] =
    resolution.decisions.iterator.flatMap { d =>
      val take = Take(filmOf(d), d.basis.toString, d.explanation.lastOption.getOrElse(""))
      d.members.iterator.map(_ -> take)
    }.toMap

  /** A listing whose incremental take differs from the whole one at a checkpoint. */
  final case class Divergence(country: String, seed: Long, checkpoint: String, key: ListingKey, whole: Take, incremental: Take) {
    /** The checkpoint's kind: `settled`, `tick`, `deploy` or `after-deploy` (the run's number dropped). */
    def kind: String = checkpoint.takeWhile(c => !c.isDigit).stripSuffix("-")
    /** The class it falls in: which side took a film, and on which basis. */
    def cls: String = {
      val side = (whole.film, incremental.film) match {
        case ("none", _) => "incremental-only take"
        case (_, "none") => "incremental loss"
        case _           => "other film"
      }
      s"$kind: $side (whole ${whole.basis}, incremental ${incremental.basis})"
    }
    def tsv: String = Seq(country, seed.toString, checkpoint, key.venue, key.rawTitle, whole.film, whole.basis, incremental.film, incremental.basis,
      whole.why, incremental.why).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
  }
  val Header = "country\tseed\tcheckpoint\tvenue\trawTitle\twholeFilm\twholeBasis\tincrementalFilm\tincrementalBasis\twholeWhy\tincrementalWhy"

  /** An allow-list row: a divergence production's design keeps on purpose, with why. */
  final case class Allowed(country: String, venue: String, rawTitle: String, checkpoint: String, reason: String) {
    def covers(d: Divergence): Boolean = country == d.country && (venue == "*" || venue == d.key.venue) && rawTitle == d.key.rawTitle &&
      (checkpoint == "*" || checkpoint == d.kind)
  }
  def allowList(path: Path): Seq[Allowed] = if (!Files.exists(path)) Nil else
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.filter(l => l.nonEmpty && !l.startsWith("#")).drop(1).map(_.split("\t", -1)).map {
      case Array(cc, venue, title, checkpoint, reason, _*) if reason.trim.nonEmpty => Allowed(cc, venue, title, checkpoint, reason)
      case row => sys.error(s"identity-drift-allow: every row names cc, venue, raw title, checkpoint and a REASON: ${row.mkString("|")}")
    }

  // ── the answers production files over time ───────────────────────────────────────────────

  /** Whether a question is one a deploy's worker has not filed yet: a fixed share (`percent`) of every kind, by its id. */
  final class Share(seed: Long, percent: Int) {
    def held(id: String): Boolean = percent > 0 && math.floorMod(scala.util.hashing.MurmurHash3.stringHash(id, seed.toInt), 100) < percent
  }

  /** `inner`'s TMDB searches, records and venue pages, a [[Share]] of them `Unknown` until [[release]]d — each asked
   *  while held noted, as the worker's fill notes a gap and files its answer later. */
  final class Gate(inner: IdentityLookups, share: Share) extends ForwardingIdentityLookups(inner) {
    @volatile var open = false
    private val released = ConcurrentHashMap.newKeySet[String]()
    private val queries  = ConcurrentHashMap.newKeySet[CandidateQuery]()
    private val films    = ConcurrentHashMap.newKeySet[Int]()
    private val details  = ConcurrentHashMap.newKeySet[ListingKey]()
    private def held(id: String) = !open && !released.contains(id) && share.held(id)
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
      if (held(s"q:${query.sortKey}")) { queries.add(query); Answer.Unknown } else inner.candidates(query)
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
      if (held(s"f:$tmdbId")) { films.add(tmdbId); Answer.Unknown } else inner.film(tmdbId)
    override def detail(listing: Listing): Answer[Option[DetailFacts]] =
      if (held(s"d:${ListingKey.serialised(listing.key)}")) { details.add(listing.key); Answer.Unknown } else inner.detail(listing)
    /** Files the answers to the held questions asked so far — a random `fraction` of them — as one event. */
    def release(fraction: Double, rng: scala.util.Random): AnswersChanged = {
      def some[A](asked: java.util.Set[A], id: A => String): Set[A] = {
        val now = asked.asScala.toSeq.sortBy(id).filter(_ => rng.nextDouble() < fraction).toSet
        now.foreach { a => asked.remove(a); released.add(id(a)) }
        now
      }
      AnswersChanged(some[CandidateQuery](queries, q => s"q:${q.sortKey}"), some[Int](films, f => s"f:$f"),
        some[ListingKey](details, k => s"d:${ListingKey.serialised(k)}"))
    }
    def pending: Boolean = !queries.isEmpty || !films.isEmpty || !details.isEmpty
  }

  /** The family answers filed so far: a [[Share]] of the capture's family questions not filed until [[release]]d (a
   *  poster's hash or a catalogue answer is never held — the recording cannot hash it again as prod did). */
  final class GatedDocuments(inner: TmdbDocuments, share: Share) extends TmdbDocuments {
    @volatile var open = false
    private val released = ConcurrentHashMap.newKeySet[String]()
    /** The held answers a stage asked for and was refused: what [[openAll]] files. */
    private val withheld = ConcurrentHashMap.newKeySet[String]()
    private def question(id: String) = Seq("|title|", "|director|", "|record|", "|showing|").exists(id.contains)
    def held(id: String): Boolean = !open && !released.contains(id) && question(id) && share.held(id)
    def release(id: String): Unit = { released.add(id); () }
    def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
      inner.get(kind, ids).filter { case (id, _) => kind != TmdbKind.Family || !held(id) || { withheld.add(id); false } }
    /** Every answer filed: the gate open, and each held answer a stage was refused `note`d as filed now, as the queue files
     *  it in production — opened unnoted, a correction waiting on one stood as it was, never read again (UK Chichester's
     *  "The Ties That Bind Us", seed 2: a divergence of the harness's, not production's). */
    def openAll(note: String => Unit): Unit = {
      open = true
      withheld.asScala.toSeq.sorted.filter(released.add).foreach(note)
    }
    def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = { docs.foreach(d => released.add(d._1)); inner.put(kind, docs) }
  }

  // ── the agreement's answers: the families, the posters, the catalogue ────────────────────

  /** What one stage reads its answers from, and how its open questions get answered: the capture's filed answers first,
   *  the recording's sites for the rest — as identity-slice's `agree` reads them. */
  final class Answers(reads: Reads, docs: TmdbDocuments, clock: java.time.Clock,
                      /** An IMDb id's TMDB film as a store holds its find, ahead of the recording's ([[WholeLookups.found]]). */
                      found: String => Option[Option[Int]]) {
    val store     = new FamilyAnswerStore(docs, clock)
    val posters   = new PosterAnswerStore(store, clock)
    val catalogue = new CatalogueFill(store, clock, reads.fetch)
    private val hashing = new PosterHashing(new services.sharecards.EgressPosterDownload(reads.fetch), new services.sharecards.VipsPosterShrinker(binary = None),
      id => reads.tmdb.posters(id, also = Seq("en")), reads.country.language.getLanguage)
    private val failures = new ConcurrentHashMap[(VoterFamily, String), Integer]()
    /** What a round tried to answer and could not: attempted, as production's memory records a failed ask — a cluster
     *  waiting on it settles ([[AgreementStage.settles]]). An answer filed is attempted by itself. */
    def attempted(question: Question): Boolean = question match {
      case Question.Family(family, asked) => failures.containsKey((family, asked))
      case _                              => false
    }
    def unreachable: Set[(VoterFamily, String)] = failures.asScala.filter(_._2 >= tools.AgreementQuestionRounds.Attempts).keySet.toSet
    def tmdbOf(imdb: String): Answer[Option[Int]] = found(imdb).fold(reads.gaps.answered(reads.tmdb.findByImdbId(imdb).map(_.id)) match {
      case Answer.Unknown => reads.filed.finds.get(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(found => Answer.Known(found))
      case known          => known
    })(Answer.Known(_))
    def stage(lookups: IdentityLookups, verdicts: services.identity.agreement.AgreementVerdicts, budgets: Boolean,
              keys: services.identity.agreement.AgreementKeys = new services.identity.agreement.InMemoryAgreementKeys): AgreementStage =
      new AgreementStage(reads.families.map(f => f -> store.answers(f)).toMap, lookups, reads.normalizer, IdentityCalibration.resolver,
        tmdbOf = tmdbOf, stored = verdicts, clock = clock, changes = store, posters = posters, tmdb = Some(lookups),
        identities = modules.wiring.IdentityProjectionWiring.identities(reads.country.code), catalogue = catalogue.answers,
        listedOn = modules.wiring.IdentityProjectionWiring.listedOn(reads.country.code),
        correctionPostersAtOnce = if (budgets) AgreementStage.CorrectionPostersAtOnce else Int.MaxValue,
        correctionsPerApply = if (budgets) AgreementStage.CorrectionsPerApply else Int.MaxValue, attempted = attempted, keyStore = keys)
    /** The family questions the stage's last apply left open, none given up as unreachable. */
    def askable(stage: AgreementStage): Set[(VoterFamily, String)] =
      Question.families(stage.wanted) -- unreachable
    /** Whether the stage's last apply left something to answer. */
    def open(stage: AgreementStage): Boolean =
      askable(stage).nonEmpty || stage.wanted.exists { case Question.Poster(_) | Question.Catalogue(_) => true; case _ => false }
    /** What the stage's last apply left open, for a settle round's line. */
    def described(stage: AgreementStage): String =
      s"${askable(stage).size} question(s), ${stage.wanted.count(_.isInstanceOf[Question.Poster])} poster(s), " +
        s"${stage.wanted.count(_.isInstanceOf[Question.Catalogue])} catalogue question(s) open, backlog ${stage.backlog}"
    /** One round of the queue's work: the stage's open questions answered — a held filed answer released, the rest
     *  asked of the recording's sites — its posters hashed and its catalogue questions filed. */
    def answer(stage: AgreementStage, release: String => Boolean): Unit = {
      val open = askable(stage).toSeq.filterNot { case (family, question) => release(FamilyAnswerStore.questionId(family, question)) }
      tools.ConcurrentAsks.each(stage.wanted.toSeq.collect { case Question.Poster(q) => q }, atOnce = 8)(q => posters.file(q, Try(hashing.of(q)).getOrElse(Nil)))
      catalogue.file(stage.wanted.collect { case Question.Catalogue(q) => q })
      tools.ConcurrentAsks.inLanes(open.groupBy(_._1).values, 4) { case question @ (family, asked) =>
        try { AgreementQuestions.file(store, family, reads.sources(family), asked); () }
        catch { case NonFatal(_) => failures.merge(question, 1, (a, b) => a + b); () }
      }
    }
  }

  /** The recording's answers, each read once: a venue page is parsed once, not on every projection. */
  def memo(reads: Reads): IdentityLookups =
    OlderRefs.passing(classOf[IdentityLookups], new tools.MemoLookups(reads.lookups), reads.lookups, OlderRefs.NewerLookups)

  // ── the whole resolve ────────────────────────────────────────────────────────────────────

  /** The resolve's inputs over `reads`: prod's pins where they are read (the local mirror, a recording's capture of
   *  them), as the worker builds its own ([[ResolveInputs.of]]). */
  def inputsOf(reads: Reads): ResolveInputs = ResolveInputs.of(reads.prod.pins)

  /** What a whole resolve answers its questions from: `lookups`, the model's and the stage's, and `found`, the stage's IMDb
   *  finds ahead of the recording's — the CI gate's the recording's alone ([[WholeLookups.recorded]]), the live drift
   *  check's prod's store first, as the worker's are (`tools.ProdLookupInputs`). */
  final case class WholeLookups(lookups: IdentityLookups, found: String => Option[Option[Int]])
  object WholeLookups {
    /** The recording's answers, each read once ([[memo]]), and its finds. */
    def recorded(reads: Reads): WholeLookups = WholeLookups(memo(reads), _ => None)
  }

  /** The whole resolve of `listings`: the model's resolve, then a fresh stage asked to its fixpoint — what a worker with
   *  nothing kept would take — answered from `answering`. */
  def whole(reads: Reads, listings: Seq[Listing], clock: MutableClock, answering: WholeLookups): Resolution = {
    val answers  = new Answers(reads, new tools.FiledAnswers.Noting(reads.filed), clock, answering.found)
    val lookups  = answering.lookups
    val model    = UnmatchedClusters.resolutionOf(IdentityResolver.resolve(listings, lookups, reads.normalizer, inputsOf(reads)).decisions)
    val stage    = answers.stage(lookups, new services.identity.agreement.InMemoryAgreementVerdicts, budgets = false)
    val byKey    = listings.map(l => l.key -> l).toMap
    settle(stage, () => stage.apply(model, byKey.get, answers.store.version), answers, clock, _ => false)
  }

  /** Applies until the stage asks nothing and holds no backlog — answering between applies: what a round could not
   *  answer is attempted, and the clusters waiting on it settle as production's do. */
  def settle(stage: AgreementStage, apply: () => Resolution, answers: Answers, clock: MutableClock, release: String => Boolean,
             rounds: Int = 60): Resolution = {
    var now = apply()
    var n   = 0
    def quiet = !answers.open(stage) && !stage.backlog
    while (!quiet && n < rounds) {
      n += 1
      System.err.println(s"[identity-drift] settle round $n: ${answers.described(stage)}")
      answers.answer(stage, release); clock.advance(Duration.ofMinutes(1)); now = apply()
    }
    if (!quiet) System.err.println(s"[identity-drift] the stage did not settle in $rounds rounds: ${answers.described(stage)}")
    now
  }

  // ── the incremental sequence ─────────────────────────────────────────────────────────────

  /** A worker's identity state over one sequence of events: the model, the stage, their stores and the answers filed. */
  final class Worker(reads: Reads, seed: Long, gatePercent: Int, val clock: MutableClock) {
    private val share    = new Share(seed, gatePercent)
    val gate             = new Gate(memo(reads), share)
    val docs             = new GatedDocuments(new tools.FiledAnswers.Noting(reads.filed), share)
    val answers          = new Answers(reads, docs, clock, _ => None)
    private val families = new InMemoryIdentityModelStore
    private val verdicts = new StoredAsWritten.Verdicts
    /** The stage's cluster keys, kept across deploys as the worker's `identity_agreement_keys` keeps them. */
    private val keys     = new agreement.InMemoryAgreementKeys
    private var rules    = "rules-1"
    private val inputs   = inputsOf(reads)
    private def newModel() = inputs.incremental(gate, reads.normalizer, families, code = rules, IdentityTraceSink.Discard)
    var model: IncrementalResolver = newModel()
    var stage: AgreementStage = answers.stage(gate, verdicts, budgets = true, keys)
    /** The intake's listings by key: what a projection hands the stage. */
    var held: Map[ListingKey, Listing] = Map.empty
    /** Nothing built on yet: the next projection reads every cluster again (its `republished` is `None`). */
    private var fresh = true
    private var projections = 0

    def releaseFamily(id: String): Boolean = docs.held(id) && { docs.release(id); answers.store.noteFiled(id); true }

    /** One projection: the stage over the model's decisions now. */
    def project(republished: Iterable[ListingKey] = Nil): Resolution = {
      clock.advance(Duration.ofMinutes(5))
      val started = tools.Stopwatch.start()
      val r = stage.apply(UnmatchedClusters.resolutionOf(model.decisions), held.get, answers.store.version, if (fresh) None else Some(republished))
      fresh = false
      projections += 1
      System.err.println(f"[identity-drift] seed $seed projection $projections: ${held.size} listings, " +
        f"${r.decisions.count(d => d.film.isDefined || d.fallback.isDefined)} clusters taken, backlog ${stage.backlog} (${started.seconds}%.1fs)")
      r
    }
    def boot(listings: Seq[Listing]): Unit = { held = listings.map(l => l.key -> l).toMap; model.restore(listings) }
    def scraped(listings: Seq[Listing]): Unit = { held ++= listings.map(l => l.key -> l); model.batch(seen = listings) }
    def gone(keys: Seq[ListingKey]): Unit = { held --= keys; model.batch(gone = keys) }
    def filled(fraction: Double, rng: scala.util.Random): Unit = model.batch(answered = gate.release(fraction, rng))
    def answered(): Unit = answers.answer(stage, releaseFamily)
    /** A deploy: the model and the stage made anew from their stores; `outrule` — a deploy changing the rules. */
    def deploy(outrule: Boolean): Unit = {
      if (outrule) { rules = s"$rules+"; verdicts.outrule() }
      model = newModel()
      model.restore(held.values.toSeq.sorted)
      stage = answers.stage(gate, verdicts, budgets = true, keys)
      fresh = true
    }
    /** Every answer filed, and the stage asked until it settles. */
    def settled(): Resolution = {
      gate.open = true; docs.openAll(answers.store.noteFiled)
      model.batch(answered = gate.release(1.0, new scala.util.Random(0)))
      settle(stage, () => project(), answers, clock, releaseFamily)
    }
  }

  /** One seed's sequence; each checkpoint's resolution, named. */
  def sequence(reads: Reads, listings: Seq[Listing], seed: Long, gatePercent: Int, clock: MutableClock): Seq[(String, Resolution)] = {
    val rng     = new scala.util.Random(seed)
    val venues  = rng.shuffle(listings.map(_.venue).distinct.sorted)
    val (first, later) = venues.splitAt((venues.size * 0.7).toInt)
    val arrivals = later.grouped(math.max(1, later.size / 3 + 1)).toSeq.padTo(3, Nil)
    val atVenue  = listings.groupBy(_.venue)
    def of(vs: Seq[String]) = vs.flatMap(atVenue.getOrElse(_, Nil)).sorted
    val initial  = of(first)
    val leaving  = initial.filter(_ => rng.nextDouble() < 0.03)
    def half     = rng.shuffle(venues).take(venues.size / 2).toSet
    val w = new Worker(reads, seed, gatePercent, clock)
    val checkpoints = Seq.newBuilder[(String, Resolution)]
    def rescrape(): Resolution = { val vs = half; val again = listings.filter(l => vs(l.venue) && w.held.contains(l.key)); w.scraped(again); w.project(again.map(_.key)) }
    w.boot(initial); w.project()
    w.filled(0.5, rng); w.project()
    w.answered(); w.project()
    w.scraped(of(arrivals(0))); w.project()
    rescrape()
    w.gone(leaving.map(_.key)); w.project()
    w.deploy(outrule = false); w.project()
    w.filled(1.0, rng); w.answered(); w.project()
    w.scraped(of(arrivals(1))); w.project()
    w.scraped(leaving); w.project()
    w.deploy(outrule = true); w.project()
    w.filled(0.5, rng); w.project()
    w.scraped(of(arrivals(2))); w.project()
    checkpoints += "settled" -> w.settled()
    for (run <- 1 to 2) {
      checkpoints += s"tick-$run" -> rescrape()
      w.deploy(outrule = run == 2)
      checkpoints += s"deploy-$run" -> w.project()
      checkpoints += s"after-deploy-$run" -> rescrape()
    }
    checkpoints.result()
  }

  def main(args: Array[String]): Unit = {
    val o       = parse(args.toList)
    val country = Country.all.find(_.code == o.country).getOrElse(usage(s"no country '${o.country}'"))
    val root    = o.recording.getOrElse(usage("--recording DIR (a recording's test/resources/fixtures)"))
    val recording = Recording(root, country)
    val filed   = o.families.filter(Files.exists(_)).fold(tools.FiledAnswers.of(country))(tools.FiledAnswers.ofCapture)
    val opened  = IdentitySlice.openRecording(recording, filed)
    val listings = IdentitySlice.listingsOf(recording, opened.rows)
    val labels  = o.labels.filter(Files.exists(_)).fold(Seq.empty[UnmatchedClusters.Label])(IdentityJudgements.labels)
    val allowed = o.allow.fold(Seq.empty[Allowed])(allowList).filter(_.country == country.code)
    val started = tools.Stopwatch.start()
    System.err.println(s"[identity-drift] ${country.code}: ${listings.size} listings, ${filed.render}")
    val divergences = o.only.fold[Seq[Int]](1 to o.seeds)(Seq(_)).flatMap { seed =>
      val clock = new MutableClock(recording.recordedAt)
      // hermetic: no prod pins nor stored verdicts beside the CI gate's recording, on both sides of its comparison alike
      def readsAt(c: java.time.Clock) = new Reads(country, opened.fetch, opened.gaps, c, filed, tools.ProdState.none)
      val checkpoints = sequence(readsAt(clock), listings, seed.toLong, o.gate, clock)
      // the whole resolve at the sequence's last moment: the same answers' ages, the same year
      val wholeClock = new MutableClock(clock.instant())
      val wholeReads = readsAt(wholeClock)
      val reference  = takesOf(whole(wholeReads, listings, wholeClock, WholeLookups.recorded(wholeReads)))
      println(s"[${country.code} seed $seed] the whole resolve's takes by basis: " +
        reference.values.groupMapReduce(_.basis)(_ => 1)(_ + _).toSeq.sortBy(-_._2).map { case (b, n) => s"$b $n" }.mkString(", "))
      checkpoints.flatMap { case (name, resolution) =>
        val now = takesOf(resolution)
        listings.map(_.key).flatMap { key =>
          val (a, b) = (reference.getOrElse(key, Take("none", "absent", "")), now.getOrElse(key, Take("none", "absent", "")))
          Option.when(a.film != b.film)(Divergence(country.code, seed.toLong, name, key, a, b))
        }
      }
    }
    val (kept, open) = divergences.partition(d => allowed.exists(_.covers(d)))
    Files.createDirectories(o.out)
    Files.writeString(o.out.resolve(s"drift-${country.code}.tsv"), (Header +: divergences.map(_.tsv)).mkString("", "\n", "\n"))
    println(f"## ${country.code}: ${listings.size} listings, ${o.only.fold(o.seeds)(_ => 1)} seed(s), ${divergences.size} divergence(s) " +
      f"(${divergences.map(_.key).distinct.size} listings), ${kept.size} allow-listed, in ${started.seconds}%.0fs")
    open.groupBy(_.cls).toSeq.sortBy(-_._2.size).foreach { case (cls, ds) =>
      println(s"  ${ds.map(_.key).distinct.size} listing(s)  $cls")
      ds.distinctBy(_.key).sortBy(d => (d.key.rawTitle, d.key.venue)).take(6).foreach { d =>
        val label = verdictOf(country, d, labels)
        println(s"      ${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, incremental ${d.incremental.film} [${d.checkpoint}, seed ${d.seed}]$label")
      }
    }
    if (open.nonEmpty) sys.exit(1)
  }

  /** What the judgements say of the two sides' films, where they judge the listing. */
  private def verdictOf(country: Country, d: Divergence, labels: Seq[UnmatchedClusters.Label]): String = {
    val right = IdentitySlice.rightLabel(d.key, country, labels).map(_.film)
    right.fold("")(film => s" — judged ${film}")
  }
}
