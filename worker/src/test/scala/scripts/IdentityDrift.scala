package scripts

import models.Country
import org.bson.BsonDocument
import services.identity._
import services.identity.adapters.{AgreementQuestions, FamilyAnswerStore, PosterAnswerStore, PosterHashing, TmdbDocuments, TmdbKind}
import services.identity.agreement.{AgreementStage, AgreementStageComposition, VoterFamily}
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
 * FLIPS: every listing's served take is recorded at every projection of the sequence — each tick's, each settle round's
 * apply — and a listing whose take goes back to a film it left, changes more than once, or changes where none of its
 * deciding inputs did ([[TakeFlips]]) is a FLIP (`flips-<cc>.tsv`): what prod's regression watch counts
 * (`kinowo_identity_listing_flips`) and a checkpoint's end state cannot show. A NEW flip fails the run: one neither
 * `test/resources/fixtures/identity-flips-allow.tsv` (a newborn check's TEMPORARY rows only) nor the KNOWN-FLIPS
 * BASELINE (`--flips-baseline`, `test/resources/fixtures/identity-flips-baseline.tsv`, [[BaselineFlip]]) names. A
 * baselined flip passes, counted loudly ("N known flip(s) remaining"), and a baseline row of a seed this run replayed
 * whose listing flipped in none of them is named "fixed — remove its row" (the owner, 2026-10-29: "fail build only on
 * new flips, focus in eliminating all old flips and then fail on all flips"). The baseline only shrinks; once it is
 * empty every flip fails, the strict gate. Divergences have no baseline.
 *
 * SLICE SCOPE (`--slice DIR`, Tier 1 of the drift gate): the same sequence and the same checks over one drift slice —
 * `test/resources/fixtures/drift-slices/<cc>/<name>/`, a listing that once diverged or flipped, its families and corpus
 * context, and every answer they read — from its own answers alone ([[SliceRun]]; `IdentityDriftSlicesSpec` replays every
 * one in the unit suite). `--widen` with `--recording` (and `--families`) is the cut's second half
 * (`scripts/identity-slice <cc> SELECTOR --emit-drift-slice`): each answer the incremental sequence reads that the slice
 * lacks — a cluster at boot holding fewer listings asks otherwise than the whole resolve the slice was cut by — read
 * from the recording and kept in the slice, again until the slice replayed alone lacks nothing and finds what it finds
 * with the recording behind it ([[cut]]: CLOSED, else the cut fails).
 *
 * {{{
 *   IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV] [--flips-allow TSV] [--flips-baseline TSV]
 *   IdentityDrift <cc> --slice DIR [--widen --recording DIR [--families CAPTURE]] [--seeds N | --seed S] [--allow TSV] [--flips-allow TSV] [--out DIR]
 * }}}
 * Exit: 0 no divergence nor flip beyond the allow-lists, 1 divergences or flips, 2 usage, 3 a slice lacks answers, 4 a cut
 * (`--slice --widen`) not closed ([[cut]]).
 */
object IdentityDrift {
  import IdentitySlice.{Reads, Recording, filmOf}

  final case class Options(country: String = "", recording: Option[Path] = None, families: Option[Path] = None, seeds: Int = 2,
                           gate: Int = 15, out: Path = Path.of("target/identity-drift"), allow: Option[Path] = None, labels: Option[Path] = None,
                           /** `--seed S`: that seed alone, to replay one divergence. */
                           only: Option[Int] = None,
                           /** `--flips-allow TSV`: the reviewed flips ([[FlipAllowed]]) — kept empty. */
                           flipsAllow: Option[Path] = None,
                           /** `--flips-baseline TSV`: the known flips ([[BaselineFlip]]), which pass, counted, until fixed. */
                           flipsBaseline: Option[Path] = None,
                           /** `--slice DIR`: one drift slice's listings and answers in place of the recording's ([[SliceRun]]). */
                           slice: Option[Path] = None,
                           /** `--widen`: what the slice lacks read from `--recording`, and kept in it. */
                           widen: Boolean = false)

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
    case "--flips-allow" :: v :: rest   => parse(rest, o.copy(flipsAllow = Some(Path.of(v))))
    case "--flips-baseline" :: v :: rest => parse(rest, o.copy(flipsBaseline = Some(Path.of(v))))
    case "--slice" :: v :: rest         => parse(rest, o.copy(slice = Some(Path.of(v))))
    case "--widen" :: rest              => parse(rest, o.copy(widen = true))
    case cc :: rest if !cc.startsWith("--") && o.country.isEmpty => parse(rest, o.copy(country = cc.toLowerCase(java.util.Locale.ROOT)))
    case other :: _                     => usage(s"unknown argument $other")
  }

  private def usage(why: String): Nothing = {
    System.err.println(s"IdentityDrift: $why\n  IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV] [--flips-allow TSV] [--flips-baseline TSV]" +
      "\n  IdentityDrift <cc> --slice DIR [--widen --recording DIR [--families CAPTURE]] [--seeds N | --seed S] [--allow TSV] [--flips-allow TSV] [--out DIR]")
    sys.exit(2)
  }

  /** One listing's take: its film (`tmdb:<id>`, `<source>:<id>` or `none`), the basis, and the decision's last line. */
  final case class Take(film: String, basis: String, why: String)
  /** The listings a model's decisions take no film for YET ([[TakeOutcome.Pending]]): what [[TakeFlips]] does not count as a
   *  take left. */
  def pendingOf(decisions: Iterable[ResolverDecision]): Set[ListingKey] =
    decisions.iterator.filter(_.outcome.isInstanceOf[TakeOutcome.Pending]).flatMap(_.members).toSet
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

  /** A flip in the reviewed list: one production's design keeps on purpose, with why. */
  final case class FlipAllowed(country: String, venue: String, rawTitle: String, reason: String) {
    def covers(country: String, f: TakeFlips.Flip): Boolean =
      this.country == country && (venue == "*" || venue == f.key.venue) && rawTitle == f.key.rawTitle
  }
  def flipAllowList(path: Path): Seq[FlipAllowed] = if (!Files.exists(path)) Nil else
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.filter(l => l.nonEmpty && !l.startsWith("#")).drop(1).map(_.split("\t", -1)).map {
      case Array(cc, venue, title, reason, _*) if reason.trim.nonEmpty => FlipAllowed(cc, venue, title, reason)
      case row => sys.error(s"identity-flips-allow: every row names cc, venue, raw title and a REASON: ${row.mkString("|")}")
    }
  val FlipHeader = "country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes"

  /** A KNOWN flip (`identity-flips-baseline.tsv`, written by scripts/ci/identity_flips_baseline.py from a run's flips):
   *  the listing flipped on main before the gate failed on new flips only. It covers its listing's flips in any seed; it
   *  is FIXED when a run replayed its seed and the listing flipped in none of that run's seeds. */
  final case class BaselineFlip(country: String, seed: Int, venue: String, rawTitle: String, takes: String, firstSeen: String, owner: String,
                                note: String) {
    def covers(country: String, f: TakeFlips.Flip): Boolean = this.country == country && venue == f.key.venue && rawTitle == f.key.rawTitle
  }
  val BaselineHeader = "country\tseed\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote"
  def flipBaseline(path: Path): Seq[BaselineFlip] = if (!Files.exists(path)) Nil else
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.filter(l => l.nonEmpty && !l.startsWith("#")).drop(1).map(_.split("\t", -1)).map {
      case Array(cc, seed, venue, title, takes, first, owner, note, _*) if seed.toIntOption.isDefined && first.trim.nonEmpty && owner.trim.nonEmpty =>
        BaselineFlip(cc, seed.toInt, venue, title, takes, first, owner, note)
      case row => sys.error(s"identity-flips-baseline: every row names cc, seed, venue, raw title, takes, first seen and an OWNER: ${row.mkString("|")}")
    }

  /** Of `flips` (by seed) beyond the allow-list: the KNOWN ones `baseline` covers, the NEW ones it does not, and the
   *  baseline rows of `seeds` whose listing flipped in none of them — fixed, their rows to remove. */
  final case class FlipVerdict(known: Seq[(Int, TakeFlips.Flip)], fresh: Seq[(Int, TakeFlips.Flip)], fixed: Seq[BaselineFlip])
  def flipVerdict(country: String, flips: Seq[(Int, TakeFlips.Flip)], baseline: Seq[BaselineFlip], seeds: Seq[Int]): FlipVerdict = {
    val (known, fresh) = flips.partition { case (_, f) => baseline.exists(_.covers(country, f)) }
    val fixed          = baseline.filter(b => b.country == country && seeds.contains(b.seed) && !flips.exists { case (_, f) => b.covers(country, f) })
    FlipVerdict(known, fresh, fixed)
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
    /** How many attempts were marked: production's memory's marks (`QuestionMemory.marks`), which the stage reads to
     *  tell an attempt since its last apply. */
    private val marks = new java.util.concurrent.atomic.AtomicLong
    /** What a round tried to answer and could not: attempted, as production's memory records a failed ask — a cluster
     *  waiting on it settles ([[AgreementStage.settles]]). An answer filed is attempted by itself. */
    def attempted(question: Question): Boolean = question match {
      case Question.Family(family, asked) => failures.containsKey((family, asked))
      case _                              => false
    }
    def unreachable: Set[(VoterFamily, String)] = failures.asScala.filter(_._2 >= tools.AgreementQuestionRounds.Attempts).keySet.toSet
    def tmdbOf(imdb: String): Answer[Option[Int]] = found(imdb).fold(reads.gaps.answered(reads.tmdb.findByImdbId(imdb).map(_.id)) match {
      case Answer.Unknown => reads.filedFind(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(found => Answer.Known(found))
      case known          => known
    })(Answer.Known(_))
    def stage(lookups: IdentityLookups, verdicts: services.identity.agreement.AgreementVerdicts, budgets: Boolean,
              keys: services.identity.agreement.AgreementKeys = new services.identity.agreement.InMemoryAgreementKeys,
              askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never): AgreementStage =
      AgreementStageComposition(AgreementStageComposition.Sources(reads.families.map(f => f -> store.answers(f)).toMap,
        venuePages = lookups, tmdb = lookups, tmdbOf = tmdbOf, stored = verdicts, clock = clock, normalizer = reads.normalizer,
        identities = modules.wiring.IdentityProjectionWiring.identities(reads.country.code),
        listedOn = modules.wiring.IdentityProjectionWiring.listedOn(reads.country.code), changes = store, posters = posters,
        catalogue = catalogue.answers, attempted = attempted, attemptsMarked = Some(() => marks.get), keys = keys),
        AgreementStageComposition.Effects(askNow = askNow), overrides = if (budgets) Set.empty else Set(AgreementStageComposition.Override.Unbudgeted))
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
      val open = asks(store, askable(stage).toSeq, release)
      tools.ConcurrentAsks.each(stage.wanted.toSeq.collect { case Question.Poster(q) => q }, atOnce = 8)(q => posters.file(q, Try(hashing.of(q)).getOrElse(Nil)))
      catalogue.file(stage.wanted.collect { case Question.Catalogue(q) => q })
      file(open)
    }
    /** The family questions a stage asks INLINE ([[AgreementStage.AskNow]]): answered before its apply reads them again,
     *  as the worker's inline asks answer them ([[services.identity.adapters.InlineFamilyAsks]]) — else a take the worker
     *  reads its answers for at its first apply waited here for the queue's next round, served the model's film meanwhile. */
    def inline(release: String => Boolean): AgreementStage.AskNow = (questions, _) =>
      file(asks(store, (Question.families(questions) -- unreachable).toSeq.sortBy { case (f, q) => (f.ordinal, q) }, release))
    private def file(open: Seq[(VoterFamily, String)]): Unit =
      tools.ConcurrentAsks.inLanes(open.groupBy(_._1).values, 4) { case question @ (family, asked) =>
        try { AgreementQuestions.file(store, family, reads.sources(family), asked); () }
        catch { case NonFatal(_) => failures.merge(question, 1, (a, b) => a + b); marks.incrementAndGet(); () }
      }
  }

  /** Which of the family questions `open` a round asks the recording's sites: none `release` files (a held filed answer),
   *  nor one whose answer the store holds fresh — skipped, as production's asker skips it
   *  ([[services.identity.adapters.AgreementAsker]], `Fresh`). A wait lists the gaps it met while their answers were
   *  held, and keeps listing them once filed; asked again, the recording's answer replaced the filed one, which no whole
   *  resolve ever reads (PL "LALKA / DOLLY", seed 2, run 3154: RT's filed "Lalka Dolly" search read again as no hits). */
  def asks(store: FamilyAnswerStore, open: Seq[(VoterFamily, String)], release: String => Boolean): Seq[(VoterFamily, String)] =
    open.filter { case (family, question) =>
      val id = FamilyAnswerStore.questionId(family, question)
      !release(id) && store.wanted(id)
    }

  /** The recording's answers, each read once: a venue page is parsed once, not on every projection. */
  def memo(reads: Reads): IdentityLookups =
    OlderRefs.passing(classOf[IdentityLookups], new tools.MemoLookups(reads.lookups), reads.lookups, OlderRefs.NewerLookups)

  /** The store of prod's filed family answers a stage starts from: [[Noted]] in every run, a recording one while a
   *  slice is cut ([[SliceRun]]). */
  type FiledDocs = tools.FiledAnswers => TmdbDocuments
  val Noted: FiledDocs = new tools.FiledAnswers.Noting(_)

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
  def whole(reads: Reads, listings: Seq[Listing], clock: MutableClock, answering: WholeLookups, ask: Boolean = true,
            filedDocs: FiledDocs = Noted): Resolution = {
    val answers  = new Answers(reads, filedDocs(reads.filed), clock, answering.found)
    val lookups  = answering.lookups
    val model    = UnmatchedClusters.resolutionOf(IdentityResolver.resolve(listings, lookups, reads.normalizer, inputsOf(reads)).decisions)
    val stage    = answers.stage(lookups, new services.identity.agreement.InMemoryAgreementVerdicts, budgets = false)
    val byKey    = listings.map(l => l.key -> l).toMap
    val apply    = () => stage.apply(model, byKey.get, answers.store.version)
    if (ask) settle(stage, apply, answers, clock, _ => false) else filedOnly(stage, apply)
  }

  /** Applies until the stage holds no backlog, answering nothing: what the filed answers alone decide, every cluster
   *  waiting on a question none of them answers left waiting — as a worker leaves it until its queue asks. */
  def filedOnly(stage: AgreementStage, apply: () => Resolution, rounds: Int = 60): Resolution = {
    var now = apply()
    var n   = 0
    while (stage.backlog && n < rounds) { n += 1; now = apply() }
    now
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
  final class Worker(reads: Reads, seed: Long, gatePercent: Int, val clock: MutableClock, filedDocs: FiledDocs = Noted) {
    private val share    = new Share(seed, gatePercent)
    val gate             = new Gate(memo(reads), share)
    val docs             = new GatedDocuments(filedDocs(reads.filed), share)
    val answers          = new Answers(reads, docs, clock, _ => None)
    private val families = new InMemoryIdentityModelStore
    private val verdicts = new StoredAsWritten.Verdicts
    /** The stage's cluster keys, kept across deploys as the worker's `identity_agreement_keys` keeps them. */
    private val keys     = new agreement.InMemoryAgreementKeys
    private var rules    = "rules-1"
    private val inputs   = inputsOf(reads)
    private def newModel() = inputs.incremental(gate, reads.normalizer, families, code = rules, IdentityTraceSink.Discard)
    var model: IncrementalResolver = newModel()
    private def newStage() = answers.stage(gate, verdicts, budgets = true, keys, askNow = answers.inline(releaseFamily))
    var stage: AgreementStage = newStage()
    /** The intake's listings by key: what a projection hands the stage. */
    var held: Map[ListingKey, Listing] = Map.empty
    /** Nothing built on yet: the next projection reads every cluster again (its `republished` is `None`). */
    private var fresh = true
    private var projections = 0
    /** Every listing's served take at each projection, and the ones that flip-flop ([[TakeFlips]]). */
    val flips = new TakeFlips
    /** What happened since the last projection (`scrape`, `fill`, `deploy`, ...): its step's name. */
    private var events = Vector.empty[String]
    /** The events that change every listing's deciding inputs: answers filed, the rules. */
    private val Answering = Set("fill", "answer", "settle", "deploy-rules")
    /** Each listing's model cluster at the last projection, as a hash of its members, and the model's take of it
     *  ([[inputsMoved]]). */
    private var clusters = Map.empty[ListingKey, ModelCluster]
    private def event(name: String): Unit = events :+= name

    def releaseFamily(id: String): Boolean = docs.held(id) && { docs.release(id); answers.store.noteFiled(id); true }

    /** One projection: the stage over the model's decisions now, every listing's take recorded ([[flips]]). */
    /** The listings the last projection served Pending. */
    var lastPending: Set[ListingKey] = Set.empty
    def project(republished: Iterable[ListingKey] = Nil, checkpoint: String = ""): Resolution = {
      clock.advance(Duration.ofMinutes(5))
      val started = tools.Stopwatch.start()
      val decided = model.decisions
      val r = stage.apply(UnmatchedClusters.resolutionOf(decided), held.get, answers.store.version, if (fresh) None else Some(republished))
      fresh = false
      projections += 1
      val applied  = started.seconds
      val now      = decided.iterator.flatMap(d => { val cluster = ModelCluster(scala.util.hashing.MurmurHash3.unorderedHash(d.members), filmOf(d))
                                                    d.members.iterator.map(_ -> cluster) }).toMap
      val answered = events.exists(Answering)
      val scraped  = events.exists(ListingEvents)
      val step     = s"$projections ${if (events.isEmpty) "re-apply" else events.distinct.mkString("+")}${if (checkpoint.isEmpty) "" else s" ($checkpoint)"}"
      // the served take's outcome, typed (`AgreementStage.outcomeOf`): no film YET -- the model's or the stage's awaits
      // named -- is no take left (TakeFlips); never read off the explanation's prose
      val pending = r.decisions.iterator.filter(d => stage.outcomeOf(d).isInstanceOf[TakeOutcome.Pending]).flatMap(_.members).toSet
      lastPending = pending
      // each listing's receipt: what its stage verdict read, answer by answer -- an answer filed inline at a scrape (another
      // cluster's ask, or its own) moves it as a filing step does (DE "Titans", drift 4281: filled at a scrape whose apply
      // filed the answer its fill read)
      val receipts   = verdicts.all().iterator.map(v => v.id -> receiptOf(v)).toMap
      val receiptNow = r.decisions.iterator.flatMap(d => { val receipt = receipts.get(stage.keyOf(d)); d.members.iterator.map(_ -> receipt) }).toMap
      flips.record(step, takesOf(r).iterator.map { case (key, t) => key -> TakeFlips.Served(t.film, s"${t.basis}: ${t.why}", pending(key)) },
        key => answered || inputsMoved(clusters.get(key), now.get(key), scraped) ||
          // a title's venues moved: what reads the title across the corpus (the stage's venues.current fill) read new input
          held.get(key).exists(listing => movedTitles(ListingRetention.titleOf(listing, reads.normalizer))) ||
          receiptMoved(lastReceipt.get(key).flatten, receiptNow.get(key).flatten))
      movedTitles = Set.empty
      lastReceipt = receiptNow
      clusters = now
      events = Vector.empty
      System.err.println(f"[identity-drift] seed $seed projection $projections: ${held.size} listings, " +
        f"${r.decisions.count(d => d.film.isDefined || d.fallback.isDefined)} clusters taken, backlog ${stage.backlog} (${applied}%.1fs, " +
        f"takes recorded in ${started.seconds - applied}%.2fs)")
      r
    }
    def boot(listings: Seq[Listing]): Unit = { event("boot"); held = listings.map(l => l.key -> l).toMap; model.restore(listings) }
    /** The listings that left whose facts are kept while a present listing bills their title ([[ListingRetention]]). */
    val retained = scala.collection.mutable.Set.empty[ListingKey]
    /** The titles whose listings arrived, came back or left since the last projection. */
    private var movedTitles = Set.empty[String]
    /** Each listing's stage verdict's receipt at the last projection ([[receiptOf]]). */
    private var lastReceipt = Map.empty[ListingKey, Option[Int]]
    private def moved(listings: Iterable[Listing]): Unit = movedTitles ++= listings.map(ListingRetention.titleOf(_, reads.normalizer))
    // a listing back replaces what was kept of it: its current data wins
    def scraped(listings: Seq[Listing]): Unit = {
      event("scrape"); moved(listings); retained --= listings.map(_.key); held ++= listings.map(l => l.key -> l); model.batch(seen = listings)
    }
    // a listing leaving loses its showings; its facts stay while a present listing bills its title (ListingRetention), as
    // the worker's drain keeps them
    def gone(keys: Seq[ListingKey]): Unit = {
      event("gone")
      val leaving = keys.flatMap(held.get)
      moved(leaving)
      held --= keys; retained --= keys
      val left = ListingRetention.leave(leaving, held.values.filterNot(l => retained(l.key)), retained.toSeq.flatMap(held.get), reads.normalizer)
      held --= left.dropped; retained --= left.dropped
      held ++= left.kept.map(l => l.key -> l); retained ++= left.kept.map(_.key)
      model.batch(seen = left.kept, gone = left.dropped)
    }
    def filled(fraction: Double, rng: scala.util.Random): Unit = { event("fill"); model.batch(answered = gate.release(fraction, rng)) }
    def answered(): Unit = { event("answer"); answers.answer(stage, releaseFamily) }
    /** A deploy: the model and the stage made anew from their stores; `outrule` — a deploy changing the rules. */
    def deploy(outrule: Boolean): Unit = {
      event(if (outrule) "deploy-rules" else "deploy")
      if (outrule) { rules = s"$rules+"; verdicts.outrule() }
      model = newModel()
      model.restore(held.values.toSeq.sorted)
      stage = newStage()
      fresh = true
    }
    /** Every answer filed, and the stage asked until it settles. */
    def settled(): Resolution = {
      gate.open = true; docs.openAll(answers.store.noteFiled)
      model.batch(answered = gate.release(1.0, new scala.util.Random(0)))
      settle(stage, () => { event("settle"); project(checkpoint = "settled") }, answers, clock, releaseFamily)
    }
  }

  /** A listing's model cluster at a projection: a hash of its members, and the film the model took it as ([[filmOf]]). */
  final case class ModelCluster(members: Int, film: String)
  /** The events that move the listings the model holds — and with them the corpus every family reads ([[CorpusContext]]). */
  val ListingEvents: Set[String] = Set("boot", "scrape", "gone")

  /** Whether a listing's deciding inputs moved since the last projection, answers and rules aside: its model cluster gained
   *  or lost a listing — or, at a step that moved listings, the model took its cluster otherwise: a family reads the corpus
   *  beyond its own listings ([[CorpusContext]]), and [[IncrementalResolver]] resolves one again when its slice of it moves
   *  (PL Kino Eva's "LALKA", run 3509: its cluster kept its members while new venues' listings came in, and the model took
   *  it as no film — read as "inputs unchanged" when the members alone were its inputs). Where no listing moved — a deploy
   *  under the same rules, an apply after nothing — the model's take moving is no input moving: a flip. A listing no model
   *  cluster held at either step has nothing to tell its inputs apart by, and is read as moved, as the members alone read
   *  it: the model take's clause only ever removes a flag. */
  def inputsMoved(before: Option[ModelCluster], now: Option[ModelCluster], listingsMoved: Boolean): Boolean =
    before.isEmpty || before.map(_.members) != now.map(_.members) || (listingsMoved && before.map(_.film) != now.map(_.film))

  /** A stage verdict's receipt: the listings it was decided over and every answer it read, by digest. */
  def receiptOf(verdict: services.identity.agreement.StoredVerdict): Int = (verdict.listings, verdict.reads).##

  /** Whether what the stage read for a listing moved between two projections: its verdict's receipt is another, or it has
   *  one at one of them only. An answer filed by a scrape's apply -- an inline ask, another cluster's -- moves it as a
   *  filing step does; a take moving under the same receipt is still a flip. */
  def receiptMoved(before: Option[Int], now: Option[Int]): Boolean = before != now

  /** The share of the booted listings that leave and return: a venue's run ending. */
  val Leave = 0.03

  /** One seed's sequence: each checkpoint's resolution, named, and the listings whose take flip-flopped on the way. */
  /** Each checkpoint's resolution with the listings its served take leaves Pending ([[TakeOutcome.Pending]]), and the flips. */
  final case class Replay(checkpoints: Seq[(String, Resolution, Set[ListingKey])], flips: Seq[TakeFlips.Flip])
  def sequence(reads: Reads, listings: Seq[Listing], seed: Long, gatePercent: Int, clock: MutableClock, filedDocs: FiledDocs = Noted,
               /** At least one booted listing leaves and returns: a drift slice's few listings, where [[Leave]] would send
                *  none away — and a sibling's run ending is what moved DE Werkstattkino's "CAMOUFLAGE" (#2473, ecedac9aa). */
               leaveOne: Boolean = false): Replay = {
    val rng     = new scala.util.Random(seed)
    val venues  = rng.shuffle(listings.map(_.venue).distinct.sorted)
    val (first, later) = venues.splitAt((venues.size * 0.7).toInt)
    val arrivals = later.grouped(math.max(1, later.size / 3 + 1)).toSeq.padTo(3, Nil)
    val atVenue  = listings.groupBy(_.venue)
    def of(vs: Seq[String]) = vs.flatMap(atVenue.getOrElse(_, Nil)).sorted
    val initial  = of(first)
    val drawn    = initial.filter(_ => rng.nextDouble() < Leave)
    val leaving  = if (drawn.isEmpty && leaveOne && initial.nonEmpty) Seq(initial(rng.nextInt(initial.size))) else drawn
    def half     = rng.shuffle(venues).take(venues.size / 2).toSet
    val w = new Worker(reads, seed, gatePercent, clock, filedDocs)
    val checkpoints = Seq.newBuilder[(String, Resolution, Set[ListingKey])]
    def rescrape(checkpoint: String = ""): Resolution = {
      // a venue scraped again bills what it bills: never a listing it dropped, kept for its facts alone
      val vs = half; val again = listings.filter(l => vs(l.venue) && w.held.contains(l.key) && !w.retained(l.key)); w.scraped(again); w.project(again.map(_.key), checkpoint)
    }
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
    checkpoints += { val r = w.settled(); ("settled", r, w.lastPending) }
    for (run <- 1 to 2) {
      checkpoints += { val r = rescrape(s"tick-$run"); (s"tick-$run", r, w.lastPending) }
      w.deploy(outrule = run == 2)
      checkpoints += { val r = w.project(checkpoint = s"deploy-$run"); (s"deploy-$run", r, w.lastPending) }
      checkpoints += { val r = rescrape(s"after-deploy-$run"); (s"after-deploy-$run", r, w.lastPending) }
    }
    Replay(checkpoints.result(), w.flips.flips)
  }

  /** What one replay found, over every seed: each divergence from the whole resolve, and each flip by its seed. */
  final case class Findings(divergences: Seq[Divergence], flips: Seq[(Int, TakeFlips.Flip)]) {
    /** The divergences no `allowed` row names, and the flips no `flipsAllowed` row does: what fails the gate. */
    def open(country: String, allowed: Seq[Allowed], flipsAllowed: Seq[FlipAllowed]): Findings =
      Findings(divergences.filterNot(d => allowed.exists(_.covers(d))), flips.filterNot { case (_, f) => flipsAllowed.exists(_.covers(country, f)) })
    def isEmpty: Boolean = divergences.isEmpty && flips.isEmpty
  }

  /** The incremental sequence over `listings` for each of `seeds`, every checkpoint compared with the whole resolve at the
   *  sequence's last moment — the replay the CI gate and a drift slice share. `readsAt` makes the reads over a clock; each
   *  seed's whole-resolve takes are handed to `wholeTakes`. */
  def replay(country: Country, listings: Seq[Listing], recordedAt: java.time.Instant, seeds: Seq[Int], gate: Int,
             readsAt: java.time.Clock => Reads, filedDocs: FiledDocs = Noted, leaveOne: Boolean = false,
             wholeTakes: (Int, Map[ListingKey, Take]) => Unit = (_, _) => ()): Findings = {
    val flips = Seq.newBuilder[(Int, TakeFlips.Flip)]
    val divergences = seeds.flatMap { seed =>
      val clock  = new MutableClock(recordedAt)
      val replay = sequence(readsAt(clock), listings, seed.toLong, gate, clock, filedDocs, leaveOne)
      flips ++= replay.flips.map(seed -> _)
      // the whole resolve at the sequence's last moment: the same answers' ages, the same year
      val wholeClock = new MutableClock(clock.instant())
      val wholeReads = readsAt(wholeClock)
      val reference  = takesOf(whole(wholeReads, listings, wholeClock, WholeLookups.recorded(wholeReads), filedDocs = filedDocs))
      wholeTakes(seed, reference)
      replay.checkpoints.flatMap { case (name, resolution, pending) =>
        val now = takesOf(resolution)
        listings.map(_.key).flatMap { key =>
          val (a, b) = (reference.getOrElse(key, Take("none", "absent", "")), now.getOrElse(key, Take("none", "absent", "")))
          // an incremental take Pending -- no film YET, its awaits named -- is below every decided one (P1): no divergence
          Option.when(a.film != b.film && !(b.film == TakeFlips.NoFilm && pending(key)))(Divergence(country.code, seed.toLong, name, key, a, b))
        }
      }
    }
    Findings(divergences, flips.result())
  }

  /** One drift slice replayed (`test/resources/fixtures/drift-slices/<cc>/<name>/`, cut by `scripts/identity-slice
   *  --emit-drift-slice`): its listings — core and context — and its own HTTP answers and filed answers alone. Cutting it
   *  (`widenFrom`: the recording's fetch, with prod's filed answers), what the slice lacks is read from the recording,
   *  each filed answer the stages read — family answers, IMDb finds, TMDB searches, records and venue pages — noted, and
   *  [[keep]] writes both into the slice. [[cut]] repeats that until the slice is CLOSED. */
  final class SliceRun(dir: Path, widenFrom: Option[SliceRun.Widen] = None) {
    val slice: IdentitySlice.Slice = IdentitySlice.readSlice(dir)
    private val noting    = widenFrom.map(w => new tools.SliceHttp.Noting(w.fetch, w.metOnThread))
    val replaying         = new tools.SliceHttp.Replaying(slice.http, noting)
    private val filed     = widenFrom.fold(slice.filed)(_.filed)
    private val notings   = new java.util.concurrent.ConcurrentLinkedQueue[tools.FiledAnswers.Noting]
    private val reads     = new java.util.concurrent.ConcurrentLinkedQueue[Reads]
    val listings: Seq[Listing] = (slice.core ++ slice.context).sortBy(_.sortKey)
    def readsAt(clock: java.time.Clock): Reads = {
      val made = new Reads(slice.country, replaying, replaying.gaps, clock, filed, tools.ProdState.none)
      reads.add(made)
      made
    }
    val filedDocs: FiledDocs = f => { val n = new tools.FiledAnswers.Noting(f); notings.add(n); n }
    def run(seeds: Seq[Int], gate: Int): Findings = replay(slice.country, listings, slice.recordedAt, seeds, gate, readsAt, filedDocs, leaveOne = true)
    /** The requests the slice lacked (none read from a recording): a replay of it is not the cut's. */
    def lacking: Seq[String] = replaying.lacking.asScala.map(_.render).toSeq.sorted
    /** The slice with what this run read beside it — the recording's answers it lacked, the filed answers read — written back. */
    def keep(): Unit = {
      val families = notings.asScala.flatMap(_.read.asScala).toMap
      val finds    = reads.asScala.flatMap(_.findsRead.asScala).toMap
      val read     = reads.asScala.flatMap(_.filedLookups).foldLeft(tools.FiledAnswers.read(filed, families, finds, None)) { (sofar, l) =>
        tools.FiledAnswers.merge(sofar, tools.FiledAnswers.read(filed, Map.empty, Map.empty, Some(l)))
      }
      val kept = tools.FiledAnswers.merge(slice.filed, read)
      IdentitySlice.writeSlice(dir, slice.copy(http = slice.http ++ replaying.widened, filed = kept.copy(source = slice.filed.source)))
    }
  }

  object SliceRun {
    /** Where a cut reads what its slice lacks: the recording's fetch (`metOnThread` telling its gaps) and prod's filed answers. */
    final case class Widen(fetch: tools.HttpFetch, metOnThread: () => Long, filed: tools.FiledAnswers)
    object Widen {
      def of(recording: Recording, filed: tools.FiledAnswers): Widen = {
        val opened = IdentitySlice.openRecording(recording, filed)
        Widen(opened.fetch, () => opened.leaf.metOnThread, filed)
      }
    }
  }

  /** A cut's verdict: the slice replayed from its own answers alone (`alone`, the requests it `lacking`) against the same
   *  listings replayed with the recording and prod's filed answers behind it (`widened`) — the slice CLOSED when it lacks
   *  nothing and the two find the same divergences and flips. A slice is then green exactly when its listings' replay
   *  over everything the recording holds is: no red (or green) of the cut's own. */
  final case class Cut(rounds: Int, widened: Findings, alone: Findings, lacking: Seq[String]) {
    def differences: Seq[String] = {
      // by film, not prose: a take's explanation names what it read, which the two read through other stores
      def lines(f: Findings) = f.divergences.map(d => s"divergence ${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, incremental " +
        s"${d.incremental.film} [${d.checkpoint}, seed ${d.seed}]").toSet ++
        f.flips.map { case (seed, flip) => s"flip ${flip.key.venue} | ${flip.key.rawTitle}: ${flip.films} [seed $seed]" }
      val (w, a) = (lines(widened), lines(alone))
      (w -- a).toSeq.sorted.map(l => s"only with the recording: $l") ++ (a -- w).toSeq.sorted.map(l => s"only in the slice: $l")
    }
    def closed: Boolean = lacking.isEmpty && differences.isEmpty
  }

  /** Cuts the slice in `dir` CLOSED: replays it with `widen` behind it and keeps everything that read (the transitive
   *  closure of what its listings' resolution asks — searches, records, venue pages, the families' answers, finds, fills),
   *  then replays it alone; again, from what it holds now, while the replay alone lacks a request or finds otherwise, at
   *  most `rounds` times. The last round's verdict: a [[Cut]] not [[Cut.closed]] fails the cut. */
  def cut(dir: Path, seeds: Seq[Int], gate: Int, widen: SliceRun.Widen, rounds: Int = 3): Cut = {
    def round(n: Int): Cut = {
      val widening = new SliceRun(dir, Some(widen))
      val widened  = widening.run(seeds, gate)
      widening.keep()
      System.err.println(s"[identity-drift] cut round $n: the slice kept ${widening.replaying.widened.size} answer(s) read from the recording")
      val alone    = new SliceRun(dir)
      val verdict  = Cut(n, widened, alone.run(seeds, gate), alone.lacking)
      if (verdict.closed || n >= rounds) verdict else round(n + 1)
    }
    round(1)
  }

  def main(args: Array[String]): Unit = {
    val o       = parse(args.toList)
    val country = Country.all.find(_.code == o.country).getOrElse(usage(s"no country '${o.country}'"))
    val labels  = o.labels.filter(Files.exists(_)).fold(Seq.empty[UnmatchedClusters.Label])(IdentityJudgements.labels)
    val allowed = o.allow.fold(Seq.empty[Allowed])(allowList).filter(_.country == country.code)
    val flipsAllowed = o.flipsAllow.fold(Seq.empty[FlipAllowed])(flipAllowList).filter(_.country == country.code)
    val baseline     = o.flipsBaseline.fold(Seq.empty[BaselineFlip])(flipBaseline).filter(_.country == country.code)
    val seeds   = o.only.fold[Seq[Int]](1 to o.seeds)(Seq(_))
    val started = tools.Stopwatch.start()
    def filedOf(families: Option[Path]) = families.filter(Files.exists(_)).fold(tools.FiledAnswers.of(country))(tools.FiledAnswers.ofCapture)
    val (listings, findings, lacking) = o.slice match {
      case Some(dir) =>
        if (o.widen && o.recording.isEmpty) usage("--widen reads what the slice lacks from --recording DIR")
        val run = new SliceRun(dir)
        if (run.slice.country != country) usage(s"the slice $dir is ${run.slice.country.code}'s, not ${country.code}'s")
        System.err.println(s"[identity-drift] ${country.code} slice $dir: ${run.listings.size} listings, ${run.slice.http.size} answers")
        if (o.widen) {
          val verdict = cut(dir, seeds, o.gate, SliceRun.Widen.of(Recording(o.recording.get, country), filedOf(o.families)))
          if (!verdict.closed) {
            println(s"## CUT NOT CLOSED after ${verdict.rounds} round(s): the slice replayed alone lacks ${verdict.lacking.size} request(s) and " +
              s"finds ${verdict.differences.size} thing(s) otherwise than with the recording behind it -- its red or green would be the cut's")
            (verdict.lacking.take(20).map(l => s"  lacks $l") ++ verdict.differences.take(20).map(d => s"  $d")).foreach(println)
            sys.exit(4)
          }
          System.err.println(s"[identity-drift] the cut is closed in ${verdict.rounds} round(s): the slice alone replays as the recording does")
          (run.listings, verdict.alone, verdict.lacking)
        } else (run.listings, run.run(seeds, o.gate), run.lacking)
      case None =>
        val root      = o.recording.getOrElse(usage("--recording DIR (a recording's test/resources/fixtures)"))
        val recording = Recording(root, country)
        val filed     = filedOf(o.families)
        val opened    = IdentitySlice.openRecording(recording, filed)
        val listings  = IdentitySlice.listingsOf(recording, opened.rows)
        System.err.println(s"[identity-drift] ${country.code}: ${listings.size} listings, ${filed.render}")
        // hermetic: no prod pins nor stored verdicts beside the CI gate's recording, on both sides of its comparison alike
        val found = replay(country, listings, recording.recordedAt, seeds, o.gate,
          c => new Reads(country, opened.fetch, opened.gaps, c, filed, tools.ProdState.none),
          wholeTakes = (seed, reference) => println(s"[${country.code} seed $seed] the whole resolve's takes by basis: " +
            reference.values.groupMapReduce(_.basis)(_ => 1)(_ + _).toSeq.sortBy(-_._2).map { case (b, n) => s"$b $n" }.mkString(", ")))
        (listings, found, Nil)
    }
    val divergences = findings.divergences
    val open        = findings.open(country.code, allowed, flipsAllowed)
    Files.createDirectories(o.out)
    Files.writeString(o.out.resolve(s"drift-${country.code}.tsv"), (Header +: divergences.map(_.tsv)).mkString("", "\n", "\n"))
    println(f"## ${country.code}: ${listings.size} listings, ${seeds.size} seed(s), ${divergences.size} divergence(s) " +
      f"(${divergences.map(_.key).distinct.size} listings), ${divergences.size - open.divergences.size} allow-listed, in ${started.seconds}%.0fs")
    open.divergences.groupBy(_.cls).toSeq.sortBy(-_._2.size).foreach { case (cls, ds) =>
      println(s"  ${ds.map(_.key).distinct.size} listing(s)  $cls")
      ds.distinctBy(_.key).sortBy(d => (d.key.rawTitle, d.key.venue)).take(6).foreach { d =>
        val label = verdictOf(country, d, labels)
        println(s"      ${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, incremental ${d.incremental.film} [${d.checkpoint}, seed ${d.seed}]$label")
      }
    }
    val flipped = findings.flips
    Files.writeString(o.out.resolve(s"flips-${country.code}.tsv"), (FlipHeader +: flipped.map { case (seed, f) =>
      Seq(country.code, seed.toString, f.key.venue, f.key.rawTitle, f.films, f.reasons.mkString("; "), f.takes.map(_.line).mkString(" -> "))
        .map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
    }).mkString("", "\n", "\n"))
    val verdict = flipVerdict(country.code, open.flips, baseline, seeds)
    println(s"## ${country.code} flips: ${flipped.size} flip(s) (${flipped.map(_._2.key).distinct.size} listings), ${flipped.size - open.flips.size} allow-listed, " +
      s"${verdict.known.size} known")
    println(s"## ${country.code} known flips: ${verdict.known.size} known flip(s) remaining (${verdict.known.map(_._2.key).distinct.size} listings in " +
      s"identity-flips-baseline.tsv), ${verdict.fixed.size} fixed")
    verdict.fixed.sortBy(b => (b.rawTitle, b.venue)).foreach(b => println(s"      FIXED, remove its row: ${b.venue} | ${b.rawTitle} [seed ${b.seed}] — no flip in this run"))
    verdict.known.distinctBy(_._2.key).sortBy { case (_, f) => (f.key.rawTitle, f.key.venue) }.foreach { case (seed, f) => println(s"      KNOWN: ${f.line} [seed $seed]") }
    verdict.fresh.groupBy { case (_, f) => f.reasons.head }.toSeq.sortBy(-_._2.size).foreach { case (reason, fs) =>
      println(s"  ${fs.map(_._2.key).distinct.size} listing(s)  flip: $reason")
      fs.distinctBy(_._2.key).sortBy { case (_, f) => (f.key.rawTitle, f.key.venue) }.take(6).foreach { case (seed, f) => println(s"      ${f.line} [seed $seed]") }
    }
    if (lacking.nonEmpty) {
      println(s"## the slice lacks ${lacking.size} answer(s) — cut it again (scripts/identity-slice --emit-drift-slice): ${lacking.take(8).mkString("; ")}")
      sys.exit(3)
    }
    if (open.divergences.nonEmpty || verdict.fresh.nonEmpty) sys.exit(1)
  }

  /** What the judgements say of the two sides' films, where they judge the listing. */
  private def verdictOf(country: Country, d: Divergence, labels: Seq[UnmatchedClusters.Label]): String = {
    val right = IdentitySlice.rightLabel(d.key, country, labels).map(_.film)
    right.fold("")(film => s" — judged ${film}")
  }
}
