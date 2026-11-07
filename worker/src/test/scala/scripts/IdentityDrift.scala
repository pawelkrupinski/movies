package scripts

import models.Country
import org.bson.BsonDocument
import services.identity._
import services.identity.adapters.{AgreementReadiness, FamilyAnswerStore, ModelInputGate, PendingListingStore, TmdbDocuments, TmdbKind, TmdbStore}
import services.identity.agreement.{AgreementStage, VoterFamily}
import services.movies.ListingKey
import tools.{IdentityJudgements, MutableClock, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

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
 * BASELINE (`--flips-baseline`, the `drift` rows of `test/resources/fixtures/identity-flip-backlog.tsv`, [[BaselineFlip]]) names. A
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
 *   IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S[,S...]] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV] [--flips-allow TSV] [--flips-baseline TSV]
 *   IdentityDrift <cc> --slice DIR [--widen --recording DIR [--families CAPTURE]] [--seeds N | --seed S[,S...]] [--allow TSV] [--flips-allow TSV] [--out DIR]
 * }}}
 * Exit: 0 no divergence nor flip beyond the allow-lists, 1 divergences or flips, 2 usage, 3 a slice lacks answers, 4 a cut
 * (`--slice --widen`) not closed ([[cut]]).
 */
object IdentityDrift {
  import IdentitySlice.{Reads, Recording, filmOf}

  final case class Options(country: String = "", recording: Option[Path] = None, families: Option[Path] = None, seeds: Int = 2,
                           gate: Int = 15, out: Path = Path.of("target/identity-drift"), allow: Option[Path] = None, labels: Option[Path] = None,
                           /** `--seed S` (or `S1,S2,...`): those seeds alone -- one divergence's, or a takes-drift row's fixed set. */
                           only: Seq[Int] = Nil,
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
    // `--seed 8` or `--seed 2,47`: those seeds alone, in one JVM (a takes-drift row's fixed set, drift_seeds.py)
    case "--seed" :: v :: rest          => parse(rest, o.copy(only = v.split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(_.toInt)))
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
    System.err.println(s"IdentityDrift: $why\n  IdentityDrift <cc> --recording DIR [--families CAPTURE|none] [--seeds N | --seed S[,S...]] [--gate PERCENT] [--out DIR] [--allow TSV] [--labels TSV] [--flips-allow TSV] [--flips-baseline TSV]" +
      "\n  IdentityDrift <cc> --slice DIR [--widen --recording DIR [--families CAPTURE]] [--seeds N | --seed S[,S...]] [--allow TSV] [--flips-allow TSV] [--out DIR]")
    sys.exit(2)
  }

  /** One listing's take: its film (`tmdb:<id>`, `<source>:<id>` or `none`), the basis, and the decision's last line. */
  final case class Take(film: String, basis: String, why: String)

  /** The kind of input a Pending take awaits, as the pending tally counts it: a TMDB search, a venue page, a record of a
   *  film the family's pool weighs, a person, or one of the stage's own questions (a family's, TMDB's find of an IMDb id, a
   *  record the stage reads again, a poster, a catalogue id). */
  def kindOf(question: Question): String = question match {
    case Question.Candidate(_)  => "search"
    case Question.VenuePage(_)  => "page"
    case Question.FilmRecord(_) => "pool-record"
    case Question.Person(_)     => "person"
    case Question.Family(_, _)  => "family"
    case Question.Find(_)       => "find"
    case Question.Record(_)     => "stage-record"
    case Question.Poster(_)     => "poster"
    case Question.Catalogue(_)  => "catalogue"
  }
  /** A listing served Pending at one projection: the film it was served provisionally, and the kinds it awaited. */
  final case class PendingServed(key: ListingKey, film: String, kinds: Set[String])
  /** Per kind of awaited input: how many listing-projections were served Pending awaiting it, and how many of those served
   *  another film than the whole resolve's final take -- the price of serving the decision on the inputs as they stand
   *  (step 6). `all` counts each listing-projection once, whatever it awaits. */
  final case class PendingTally(byKind: Map[String, (Int, Int)], all: (Int, Int)) {
    def +(other: PendingTally): PendingTally = PendingTally(
      (byKind.keySet ++ other.byKind.keySet).iterator.map { k =>
        val (a, b) = (byKind.getOrElse(k, (0, 0)), other.byKind.getOrElse(k, (0, 0))); k -> (a._1 + b._1, a._2 + b._2) }.toMap,
      (all._1 + other.all._1, all._2 + other.all._2))
    def lines: Seq[String] = {
      def row(name: String, n: (Int, Int)) = f"$name%-13s ${n._1}%8d ${n._2}%8d ${if (n._1 == 0) 0.0 else 100.0 * n._2 / n._1}%6.1f%%"
      f"${"kind"}%-13s ${"pending"}%8s ${"differ"}%8s ${"share"}%7s" +: (byKind.toSeq.sortBy(-_._2._1).map(row.tupled) :+ row("any", all))
    }
  }
  object PendingTally {
    val Empty: PendingTally = PendingTally(Map.empty, (0, 0))
    /** `served` against the final takes `reference`: each listing-projection counted under every kind it awaited. */
    def of(served: Iterable[PendingServed], reference: Map[ListingKey, Take]): PendingTally = {
      val byKind = scala.collection.mutable.HashMap.empty[String, (Int, Int)]
      var all = (0, 0)
      served.foreach { p =>
        val differs = if (reference.get(p.key).fold(TakeFlips.NoFilm)(_.film) != p.film) 1 else 0
        all = (all._1 + 1, all._2 + differs)
        p.kinds.foreach(k => byKind.updateWith(k)(n => Some(n.fold((1, differs))(n => (n._1 + 1, n._2 + differs)))))
      }
      PendingTally(byKind.toMap, all)
    }
  }
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
  val VerdictPrefix = "## DRIFT VERDICT: "
  /** The replay's explicit verdict line: scripts/identity-slice reads it, never the exit status alone. */
  def verdictLine(red: Boolean): String = s"$VerdictPrefix${if (red) "RED" else "CLEAN"}"
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
  val FlipHeader = "country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\tcause"

  /** Why a flip's take moved, step by step ([[Worker.probed]]; the flip-unification design's stage 1): `answer` when a whole
   *  resolve of the inputs at the step takes what the model takes and the step filed answers, `population` when it does and
   *  listings arrived or left, `answer+population` for both, and `lag` when the model took otherwise than the whole resolve
   *  or nothing moved at all -- a mechanism's, not the inputs'. */
  object Cause {
    def of(faithful: Boolean, answered: Boolean, listings: Boolean): String =
      if (!faithful) "lag"
      else (answered, listings) match {
        case (true, true)   => "answer+population"
        case (true, false)  => "answer"
        case (false, true)  => "population"
        case (false, false) => "lag"
      }
    /** A flip's cause: the causes of every move after its first take, in order, distinct; `unreproduced` for a move the
     *  probed pass did not see. */
    def ofFlip(flip: TakeFlips.Flip, causes: Map[(ListingKey, String), String]): String =
      flip.takes.drop(1).map(t => causes.getOrElse((flip.key, t.step), "unreproduced")).distinct.mkString(",")
  }

  /** THE FLIP BACKLOG (`test/resources/fixtures/identity-flip-backlog.tsv`): every flip known on main, a row each, read by
   *  its header -- `kind` `drift` (a takes-drift seed found it) or `population` (PopulationMonotonicitySpec's walk did). */
  val BacklogFile: Path = Path.of("test", "resources", "fixtures", "identity-flip-backlog.tsv")
  val BacklogHeader     = Seq("kind", "country", "seed", "slice", "venue", "rawTitle", "takes", "firstSeen", "owner", "note")
  def backlog(path: Path, kind: String): Seq[Map[String, String]] = if (!Files.exists(path)) Nil else {
    val lines  = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.filter(l => l.nonEmpty && !l.startsWith("#"))
    val header = lines.headOption.map(_.split("\t", -1).toSeq).getOrElse(Nil)
    if (header.nonEmpty && !BacklogHeader.forall(header.contains))
      sys.error(s"identity-flip-backlog: the header names ${BacklogHeader.mkString(", ")}: ${header.mkString("|")}")
    lines.drop(1).map(l => header.zipAll(l.split("\t", -1).toSeq, "", "").toMap).filter(_.getOrElse("kind", "") == kind)
  }

  /** A KNOWN flip (a `drift` row of the backlog): the listing flipped on main. It covers its listing's flips in any seed;
   *  it is FIXED when a run replayed its seed and the listing flipped in none of that run's seeds. */
  final case class BaselineFlip(country: String, seed: Int, venue: String, rawTitle: String, takes: String, firstSeen: String, owner: String,
                                note: String) {
    def covers(country: String, f: TakeFlips.Flip): Boolean = this.country == country && venue == f.key.venue && rawTitle == f.key.rawTitle
  }
  def flipBaseline(path: Path): Seq[BaselineFlip] = backlog(path, "drift").map { r =>
    val seed = r("seed").toIntOption
    if (seed.isEmpty || r("firstSeen").trim.isEmpty || r("owner").trim.isEmpty)
      sys.error(s"identity-flip-backlog: every drift row names its country, seed, venue, raw title, takes, first seen and an OWNER: ${r.values.mkString("|")}")
    BaselineFlip(r("country"), seed.get, r("venue"), r("rawTitle"), r("takes"), r("firstSeen"), r("owner"), r("note"))
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

  /** Whether a fill files a held question: the replay's one random decision a question meets. [[Share.filed]] draws it by
   *  the question's own id; a draw from a stream every question shares (the harness before 3e97bde52) is what
   *  [[Scenario]] is there to catch. */
  trait FillDraw { def filed(id: String, round: Int, fraction: Double): Boolean }

  /** A replay's SCENARIO: what the harness drew for one seed, as against what the code under test decided -- the venue
   *  ARRANGEMENT (the listings it was dealt, the venues' order, who boots, the three arrivals, who leaves, each re-scrape's
   *  half) and each FILL DRAW (a held question filed or not at a fill that files a fraction of them). Head and base of the
   *  differential gate must meet the same scenario on a seed: the arrangement whenever they were dealt the same listings,
   *  and the draw of every question both ask. For two weeks they did not -- one `Random` drew the fills and the halves, so
   *  a change asking one question more re-dealt every later draw (DE seed 8's Geister, drift 42003442640) -- and nobody
   *  saw. Written as `scenario-<cc>.tsv` beside the flips (seed, kind, key, value), its [[digest]] printed per seed, and
   *  compared by scripts/ci/drift_scenario.py in identity-drift-check.sh: a difference is a HARNESS ERROR, never flips. */
  final class Scenario {
    private val arranged = scala.collection.mutable.LinkedHashMap.empty[String, String]
    private val draws    = scala.collection.mutable.TreeMap.empty[String, Boolean]
    /** One part of the arrangement, as the digest of its values in the order drawn. */
    def arrange(part: String, values: Iterable[String]): Unit = synchronized { arranged(part) = Scenario.hash(values) }
    def drew(round: Int, id: String, filed: Boolean): Unit = synchronized { draws(s"$round:${Scenario.clean(id)}") = filed }
    def arrangement: Seq[(String, String)] = synchronized(arranged.toSeq)
    def fills: Seq[(String, Boolean)] = synchronized(draws.toSeq)
    /** The scenario in one short line: the arrangement's digest and the fill draws'. */
    def digest: String = s"arrangement ${Scenario.hash(arrangement.map { case (p, h) => s"$p=$h" })}, ${fills.size} fill draw(s) " +
      Scenario.hash(fills.map { case (k, f) => s"$k=$f" })
    /** `scenario-<cc>.tsv`'s rows of this seed. */
    def rows(seed: Int): Seq[String] =
      arrangement.map { case (part, h) => s"$seed\tarrangement\t$part\t$h" } ++ fills.map { case (k, f) => s"$seed\tfill\t$k\t${if (f) 1 else 0}" }
  }
  object Scenario {
    val Header = "seed\tkind\tkey\tvalue"
    /** A short digest of `values`, in their order. */
    def hash(values: Iterable[String]): String = {
      val md = java.security.MessageDigest.getInstance("SHA-256")
      values.foreach(v => md.update((v + "\n").getBytes(StandardCharsets.UTF_8)))
      md.digest().take(6).map(b => f"${b & 0xff}%02x").mkString
    }
    def clean(text: String): String = text.replace('\t', ' ').replace('\n', ' ')
  }

  /** Whether a question is one a deploy's worker has not filed yet: a fixed share (`percent`) of every kind, by its id. */
  final class Share(seed: Long, percent: Int) extends FillDraw {
    def held(id: String): Boolean = percent > 0 && math.floorMod(scala.util.hashing.MurmurHash3.stringHash(id, seed.toInt), 100) < percent
    /** Is the held question `id` filed by the `round`-th fill, which files a random `fraction` of them? Drawn by the
     *  question's own id, never from a stream shared by every question asked: one draw a question from one `Random` made a
     *  change asking ONE question more or fewer (DE Babylon's "Halloween II [OV]" no longer searched as "Halloween") file a
     *  different half of every later question, and move every venue a re-scrape's half drew after it -- DE seed 8's
     *  Geister, four venues, flipped under the head and not the base for an arrangement neither change's code made
     *  (drift 42003442640). */
    def filed(id: String, round: Int, fraction: Double): Boolean =
      fraction >= 1.0 || math.floorMod(scala.util.hashing.MurmurHash3.stringHash(s"$round:$id", seed.toInt), 1000000) < fraction * 1000000
  }

  /** `inner`'s TMDB searches, records and venue pages, a [[Share]] of them `Unknown` until [[release]]d — each asked
   *  while held noted, as the worker's fill notes a gap and files its answer later. */
  final class Gate(inner: IdentityLookups, share: Share, draw: FillDraw, val scenario: Scenario) extends ForwardingIdentityLookups(inner) {
    /** The worker's gate: each held question filed by its own draw ([[Share.filed]]), every draw kept in a fresh [[Scenario]]. */
    def this(inner: IdentityLookups, share: Share) = this(inner, share, share, new Scenario)
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
    private var rounds = 0
    /** Files the answers to the held questions asked so far — a random `fraction` of them, each drawn by its own id
     *  ([[Share.filed]]) — as one event. */
    def release(fraction: Double): AnswersChanged = {
      rounds += 1
      val round = rounds
      def some[A](asked: java.util.Set[A], id: A => String): Set[A] = {
        // in the questions' order, each draw kept in the scenario (a fraction of 1 files every one: no draw)
        val now = asked.asScala.toSeq.sortBy(id).filter { a =>
          val filed = draw.filed(id(a), round, fraction)
          if (fraction < 1.0) scenario.drew(round, id(a), filed)
          filed
        }.toSet
        now.foreach { a => asked.remove(a); released.add(id(a)) }
        now
      }
      AnswersChanged(some[CandidateQuery](queries, q => s"q:${q.sortKey}"), some[Int](films, f => s"f:$f"),
        some[ListingKey](details, k => s"d:${ListingKey.serialised(k)}"))
    }
    def pending: Boolean = !queries.isEmpty || !films.isEmpty || !details.isEmpty
  }

  /** The feed keys production's TMDB store announces `changed`'s searches and records under ([[TmdbStore.keyOf]]). What
   *  the agreement stage's version counts of them is the family store's say ([[FamilyAnswerStore.agreementKeyOf]]), as in
   *  production; a venue page's announcement is no agreement key, and moves it in neither. */
  def filingKeys(changed: AnswersChanged, language: String): Seq[String] =
    changed.queries.toSeq.map(q => TmdbStore.keyOf(TmdbKind.Query, TmdbStore.questionId(language, q))).sorted ++
      changed.films.toSeq.sorted.map(f => TmdbStore.keyOf(TmdbKind.Film, f.toString))

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

  /** What one stage reads its answers from, and how its open questions get answered ([[StageAnswers]]): the capture's
   *  filed answers first, the recording's sites for the rest — as identity-slice's `agree` reads them. */
  final class Answers(reads: Reads, docs: TmdbDocuments, clock: java.time.Clock,
                      /** An IMDb id's TMDB film as a store holds its find, ahead of the recording's ([[WholeLookups.found]]). */
                      found: String => Option[Option[Int]]) extends StageAnswers(recorded(reads), docs, clock, found)

  /** A recording's sites as a stage's answer sources: its families, its fetch, its TMDB finds and posters — a find it
   *  does not hold read from prod's filed ones. */
  def recorded(reads: Reads): StageAnswers.Sources = new StageAnswers.Sources {
    def country: Country                                   = reads.country
    def normalizer: services.movies.TitleNormalizer        = reads.normalizer
    def families: Seq[VoterFamily]                         = reads.families
    def sources: Map[VoterFamily, services.identity.adapters.FamilySource] = reads.sources
    def fetch: tools.HttpFetch                             = reads.fetch
    def posterImages(tmdbId: Int): Seq[clients.TmdbClient.PosterImage] = reads.tmdb.posters(tmdbId, also = Seq("en"))
    def find(imdb: String): Answer[Option[Int]] = reads.gaps.answered(reads.tmdb.findByImdbId(imdb).map(_.id)) match {
      case Answer.Unknown => reads.filedFind(imdb).fold[Answer[Option[Int]]](Answer.Unknown)(found => Answer.Known(found))
      case known          => known
    }
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
   *  nothing kept would take — answered from `answering`. `resolved`: the model's resolve of `listings` when the caller
   *  holds it already (the population walk reads both takes), else resolved here; `log`: the settle rounds said. */
  def whole(reads: Reads, listings: Seq[Listing], clock: MutableClock, answering: WholeLookups, ask: Boolean = true,
            filedDocs: FiledDocs = Noted, resolved: Option[Resolution] = None, log: Boolean = true): Resolution = {
    val answers  = new Answers(reads, filedDocs(reads.filed), clock, answering.found)
    val lookups  = answering.lookups
    val model    = UnmatchedClusters.resolutionOf(resolved.getOrElse(IdentityResolver.resolve(listings, lookups, reads.normalizer, inputsOf(reads))).decisions)
    val stage    = answers.stage(lookups, new services.identity.agreement.InMemoryAgreementVerdicts, budgets = false)
    val byKey    = listings.map(l => l.key -> l).toMap
    val apply    = () => stage.apply(model, byKey.get, answers.store.version)
    if (ask) StageAnswers.settle(stage, apply, answers, clock, _ => false, log = log) else filedOnly(stage, apply)
  }

  /** Applies until the stage holds no backlog, answering nothing: what the filed answers alone decide, every cluster
   *  waiting on a question none of them answers left waiting — as a worker leaves it until its queue asks. */
  def filedOnly(stage: AgreementStage, apply: () => Resolution, rounds: Int = 60): Resolution = {
    var now = apply()
    var n   = 0
    while (stage.backlog && n < rounds) { n += 1; now = apply() }
    now
  }

  // ── the incremental sequence ─────────────────────────────────────────────────────────────

  /** How many rounds of the input gate's asks a step answers at most: each answer may lead to further questions (a title's
   *  search names films whose records the listing then waits on), each answered as the last was. */
  val GateAskRounds: Int = 8

  /** How many of an update's regions the replay's model resolves side by side: every core the JVM is given. */
  val RegionThreads: Int = math.max(1, Runtime.getRuntime.availableProcessors)

  /** A worker's identity state over one sequence of events: the model, the stage, their stores and the answers filed. */
  final class Worker(reads: Reads, seed: Long, gatePercent: Int, val clock: MutableClock, filedDocs: FiledDocs = Noted,
                     /** The model's input gate from the families' readiness, as the worker composes it
                      *  ([[modules.WorkerWiring.modelReadiness]]; the replay has every venue page it reads). */
                     gating: ListingReadiness => ListingReadiness = modules.WorkerWiring.modelReadiness(ListingReadiness.Always, _)) {
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
    // an update's regions resolved on every core: the replay is the model's work event after event, one thread's while
    // they were resolved in turn (a drift row busy on 2 of its 4 cores; docs/investigations/2026-11-01-twenty-runners.md)
    private def newModel() = inputs.incremental(gate, reads.normalizer, families, code = rules, IdentityTraceSink.Discard,
      regionThreads = RegionThreads)
    var model: IncrementalResolver = newModel()
    /** A stage wired to the answer feed as the worker wires the live one (IdentityProjectionWiring.agreementStage): the
     *  TMDB finds and records it waits on, and the records and searches it read, move its answers' version when filed. */
    private def newStage() = {
      val made = answers.stage(gate, verdicts, budgets = true, keys, askNow = answers.inline(releaseFamily))
      answers.store.awaitedBy(question => made.wanted.contains(question))
      answers.store.readBy(made.reads)
      made
    }
    var stage: AgreementStage = newStage()
    /** The questions the input gate asked for its held listings, answered before the next step: the worker asks them
     *  ahead of the queue, answered within seconds, and the drain their filings wake takes the listing in. */
    private val gateAsked = scala.collection.mutable.LinkedHashSet.empty[Question]
    /** A family question the gate waits on, attempted: filed (the store wants it no more), or asked and come to an outcome,
     *  as the worker's question memory reads it ([[AgreementReadiness]]); a question of another kind is no family the
     *  replay holds back. */
    private def attempted(question: Question): Boolean = question match {
      case Question.Family(family, asked) => !answers.store.wanted(FamilyAnswerStore.questionId(family, asked)) || answers.tried(question)
      case _                              => true
    }
    /** The worker's input gate ([[ModelInputGate]], fix 1), its readiness the live stage's questions of a listing against
     *  the replay's answers, its held listings persisted as the worker's `identity_pending` is -- resumed at a deploy. */
    val inputGate = new ModelInputGate(gating(new AgreementReadiness(listing => stage.questionsOf(listing), attempted, gateAsked ++= _)),
      new PendingListingStore.InMemory(), clock)
    /** The gate's asks answered, then every held listing they made ready promoted -- again while the answers led to more
     *  asks, as each filing wakes the worker's drain within seconds: what the next step's batch takes in. */
    private def promoted(): Seq[Listing] = {
      val taken = Seq.newBuilder[Listing]
      var rounds = 0
      while ({
        if (gateAsked.nonEmpty) { val asked = gateAsked.toSet; gateAsked.clear(); answers.fileNow(asked, releaseFamily) }
        taken ++= inputGate.admit(model, Nil)._2
        rounds += 1
        gateAsked.nonEmpty && rounds < GateAskRounds
      }) ()
      taken.result()
    }
    /** The venues of `listings` as the worker's drain hands the gate a scrape: each venue's listings billed now. */
    private def gated(venues: Iterable[String]): Seq[Listing] = {
      val billed = held.values.iterator.filterNot(l => retained(l.key)).toSeq.groupBy(_.venue)
      inputGate.admit(model, venues.toSeq.distinct.sorted.map(v => v -> billed.getOrElse(v, Nil).sorted)) match {
        case (taken, promoted) => taken ++ promoted
      }
    }
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
    /** The listings whose every served-take move is classified ([[Cause]]): a seed's flipped listings, on a second pass
     *  over its sequence. Each move's cause, by the listing and the step's name. */
    var probed: Set[ListingKey] = Set.empty
    val causes = scala.collection.mutable.HashMap.empty[(ListingKey, String), String]
    private var lastServed = Map.empty[ListingKey, (String, Boolean)]
    /** Each listing's model cluster at the last projection, as a hash of its members, and the model's take of it
     *  ([[inputsMoved]]). */
    private var clusters = Map.empty[ListingKey, ModelCluster]
    private def event(name: String): Unit = events :+= name

    def releaseFamily(id: String): Boolean = docs.held(id) && { docs.release(id); answers.store.noteFiled(id); true }

    /** One projection: the stage over the model's decisions now, every listing's take recorded ([[flips]]). */
    /** The listings the last projection served Pending. */
    var lastPending: Set[ListingKey] = Set.empty
    /** Every listing served Pending at a projection, with its provisional film and what it awaited. */
    val pendingServed = scala.collection.mutable.ArrayBuffer.empty[PendingServed]
    /** How many listings each projection served Pending, and for how long each stayed so ([[PendingSpans]]). */
    val pendingSpans = new PendingSpans
    def project(republished: Iterable[ListingKey] = Nil, checkpoint: String = ""): Resolution = {
      clock.advance(Duration.ofMinutes(5))
      val started = tools.Stopwatch.start()
      val decided = model.decisions
      val r = stage.apply(UnmatchedClusters.resolutionOf(decided), model.held, answers.store.version, if (fresh) None else Some(republished))
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
      val awaiting = r.decisions.iterator.flatMap(d => stage.outcomeOf(d) match {
        case TakeOutcome.Pending(awaits) => val kinds = awaits.map(kindOf); d.members.iterator.map(_ -> kinds)
        case _                           => Iterator.empty
      }).toMap
      val pending = awaiting.keySet
      lastPending = pending
      pendingSpans.record(projections, clock.instant(), held.size, pending)
      val served  = takesOf(r)
      // each provisional take, for the pending tally: how often it differs from the final one, by what it awaits
      pendingServed ++= awaiting.iterator.map { case (key, kinds) => PendingServed(key, served.get(key).fold(TakeFlips.NoFilm)(_.film), kinds) }
      // each listing's receipt: what its stage verdict read, answer by answer -- an answer filed inline at a scrape (another
      // cluster's ask, or its own) moves it as a filing step does (DE "Titans", drift 4281: filled at a scrape whose apply
      // filed the answer its fill read)
      val receipts   = verdicts.all().iterator.map(v => v.id -> receiptOf(v)).toMap
      val receiptNow = r.decisions.iterator.flatMap(d => { val receipt = receipts.get(stage.keyOf(d)); d.members.iterator.map(_ -> receipt) }).toMap
      flips.record(step, served.iterator.map { case (key, t) => key -> TakeFlips.Served(t.film, s"${t.basis}: ${t.why}", pending(key)) },
        key => answered || inputsMoved(clusters.get(key), now.get(key), scraped) ||
          // a title's venues moved: what reads the title across the corpus (the stage's venues.current fill) read new input
          held.get(key).exists(listing => movedTitles(titleOf(listing, reads.normalizer))) ||
          receiptMoved(lastReceipt.get(key).flatten, receiptNow.get(key).flatten))
      if (probed.nonEmpty) {
        // a probed listing's served take moved: the whole resolve of the inputs as they are now, once a step, says whether
        // the model took what the inputs decide (then the step's events say which inputs), or lagged
        val changed = probed.filter(key => served.get(key).exists(t => lastServed.get(key).exists(_ != (t.film, pending(key)))))
        if (changed.nonEmpty) {
          val whole = IdentityResolver.resolve(model.listings, gate, reads.normalizer, inputs)
          changed.foreach { key =>
            val faithful = whole.decisionOf.get(key).map(filmOf).getOrElse(TakeFlips.NoFilm) == now.get(key).fold(TakeFlips.NoFilm)(_.film)
            causes((key, step)) = Cause.of(faithful, answered, scraped)
          }
        }
        lastServed = lastServed ++ probed.iterator.flatMap(key => served.get(key).map(t => key -> (t.film, pending(key))))
      }
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
    /** The listings held as kept for their facts ([[ListingRetention]]): a cache of [[ListingRetention.retained]] of
     *  `departed` and the listings billed, recomputed whole at every arrival and leave, as the worker's drain has it. */
    val retained = scala.collection.mutable.Set.empty[ListingKey]
    /** Every listing that left, with its facts as last billed: the input the kept listings are read from (R1). */
    private val departed = scala.collection.mutable.Map.empty[ListingKey, Listing]
    /** The titles whose listings arrived, came back or left since the last projection. */
    private var movedTitles = Set.empty[String]
    /** Each listing's stage verdict's receipt at the last projection ([[receiptOf]]). */
    private var lastReceipt = Map.empty[ListingKey, Option[Int]]
    private def moved(listings: Iterable[Listing]): Unit = movedTitles ++= listings.map(titleOf(_, reads.normalizer))
    /** `seen` and `gone` handed the model with the kept listings recomputed from the inputs: one a related arrival bills
     *  again kept again, one no present listing relates to any more let go -- whatever order they came in. */
    private def retaining(seen: Seq[Listing], gone: Seq[ListingKey], answered: AnswersChanged = AnswersChanged.Empty): Unit = {
      // the listings present as the model holds them (a held one's ready version, none for one new to it), and `seen`
      val seenNow = seen.iterator.map(l => l.key -> l).toMap
      val present = held.values.iterator.filterNot(l => retained(l.key)).flatMap(l => seenNow.get(l.key).orElse(model.held(l.key))).toSeq
      val kept    = ListingRetention.retained(departed.values.toSeq.sortBy(l => ListingKey.serialised(l.key)), present, reads.normalizer)
      val keys    = kept.iterator.map(_.key).toSet
      val newly   = kept.filterNot(l => retained(l.key))
      val dropped = (gone.iterator ++ retained.iterator).filterNot(keys).toSeq.distinct
      moved(newly ++ dropped.flatMap(held.get))
      held --= dropped; held ++= newly.map(l => l.key -> l)
      retained.clear(); retained ++= keys
      model.batch(seen = seen ++ newly, gone = dropped, answered = answered)
    }
    /** The versions promoted ahead of a step (`ready`) and those its gate pass took in (`taken`), one per key: the step's own
     *  where both hold one -- the worker's drain took the promoted one a drain earlier, and the newer over it. */
    private def newestOf(ready: Seq[Listing], taken: Seq[Listing]): Seq[Listing] = {
      val takenKeys = taken.iterator.map(_.key).toSet
      ready.filterNot(l => takenKeys(l.key)) ++ taken.distinctBy(_.key)
    }
    // a listing back replaces what was kept of it: its current data wins
    def scraped(listings: Seq[Listing]): Unit = {
      event("scrape"); moved(listings)
      val ready = promoted()
      departed --= listings.map(_.key); retained --= listings.map(_.key); held ++= listings.map(l => l.key -> l)
      val keys  = listings.iterator.map(_.key).toSet
      // the gate takes in the scraped listings ready, or billed as the model holds them, and promotes the held ones answered
      val taken = gated(listings.map(_.venue)).filter(l => keys(l.key) || !model.held(l.key).contains(l))
      retaining(newestOf(ready, taken), Nil)
    }
    // a listing leaving loses its showings; its facts stay while a present listing bills its title (ListingRetention), as
    // the worker's drain keeps them
    def gone(keys: Seq[ListingKey]): Unit = {
      event("gone")
      val ready   = promoted()
      val leaving = keys.flatMap(held.get)
      moved(leaving)
      // what departs is the version the model holds: a listing the gate held new to it never was in it
      departed ++= leaving.filterNot(l => retained(l.key)).flatMap(l => model.held(l.key)).map(l => l.key -> l)
      held --= keys; retained --= keys
      val taken = gated(leaving.map(_.venue)).filterNot(l => model.held(l.key).contains(l))
      retaining(newestOf(ready, taken), keys)
    }
    def filled(fraction: Double): Unit = { event("fill"); retaining(promoted(), Nil, filed(gate.release(fraction))) }
    /** `changed` filed as production's fill files it: each answer announced on the feed the TMDB store publishes to --
     *  the stage's version moving for a search or record it read or waits on, as the live stage's wiring has it -- before
     *  the model is told. Unannounced, a search the stage read blind stayed blind while the version stood: UK Swan Lake,
     *  seed 3, Halstead's broadcast take none from the fill to a deploy's fresh stage. */
    private def filed(changed: AnswersChanged): AnswersChanged = {
      filingKeys(changed, reads.country.language.getLanguage).foreach(answers.store.noteFiled)
      changed
    }
    def answered(): Unit = { event("answer"); answers.answer(stage, releaseFamily); val ready = promoted(); if (ready.nonEmpty) retaining(ready, Nil) }
    /** A deploy: the model and the stage made anew from their stores; `outrule` — a deploy changing the rules. */
    def deploy(outrule: Boolean): Unit = {
      event(if (outrule) "deploy-rules" else "deploy")
      if (outrule) { rules = s"$rules+"; verdicts.outrule() }
      model = newModel()
      stage = newStage()
      // the take-up resumes the gate's held listings from its store: the model reads their last ready versions
      model.restore(inputGate.resume(held.values.toSeq.sorted).sorted)
      fresh = true
    }
    /** Every answer filed, and the stage asked until it settles. */
    def settled(): Resolution = {
      gate.open = true; docs.openAll(answers.store.noteFiled)
      retaining(promoted(), Nil, filed(gate.release(1.0)))
      StageAnswers.settle(stage, () => { event("settle"); project(checkpoint = "settled") }, answers, clock, releaseFamily)
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

  /** A listing's title as the agreement stage's venue count reads its title across the corpus (`venues.current`): the
   *  title it is searched under, a trailing parenthesis off, sanitised -- what [[Worker.project]] marks a title's venues
   *  moved by. The harness's key, not the model's: retention relates listings by the model's own title relation. */
  def titleOf(listing: Listing, normalizer: services.movies.TitleNormalizer): String = {
    val normalized = normalizer.sanitize(services.resolution.SearchTitles.withoutTrailingParenthesis(normalizer.searchQuery(listing.title)))
    if (normalized.nonEmpty) normalized else normalizer.sanitize(listing.title)
  }

  /** The share of the booted listings that leave and return: a venue's run ending. */
  val Leave = 0.03

  /** One seed's sequence: each checkpoint's resolution, named, and the listings whose take flip-flopped on the way. */
  /** Each checkpoint's resolution with the listings its served take leaves Pending ([[TakeOutcome.Pending]]), and the flips. */
  final case class Replay(checkpoints: Seq[(String, Resolution, Set[ListingKey])], flips: Seq[TakeFlips.Flip],
                          pendingServed: Seq[PendingServed] = Nil, spans: PendingSpans.Summary = PendingSpans.Summary.empty,
                          /** The probed listings' moves, each with its [[Cause]] ([[Worker.probed]]). */
                          causes: Map[(ListingKey, String), String] = Map.empty,
                          /** What the harness drew for the seed ([[Scenario]]). */
                          scenario: Scenario = new Scenario)

  /** Each listing's stretches served Pending ([[TakeOutcome.Pending]]) over a replay's projections: how many are Pending
   *  at once, and how long -- in projections and in the replay clock's minutes -- each stretch lasted before its take
   *  was decided, or stayed Pending to the end. What the settle rule's give-up costs (the owner, 2026-10-31: a take settles
   *  only once every question it depends on is answered or given up on). */
  final class PendingSpans {
    private val open   = scala.collection.mutable.HashMap.empty[ListingKey, (Int, java.time.Instant)]
    private val closed = Seq.newBuilder[(Int, Long)]
    private val counts = Seq.newBuilder[(Int, Int)]
    private val ever   = scala.collection.mutable.HashSet.empty[ListingKey]
    private var last   = (0, java.time.Instant.EPOCH)
    def record(projection: Int, at: java.time.Instant, listings: Int, pending: Set[ListingKey]): Unit = {
      open.keySet.diff(pending).toSeq.foreach { key =>
        val (from, since) = open.remove(key).get
        closed += ((projection - from, java.time.Duration.between(since, at).toMinutes))
      }
      pending.foreach(key => if (!open.contains(key)) open(key) = (projection, at))
      ever ++= pending
      counts += ((pending.size, listings))
      last = (projection, at)
    }
    def summary: PendingSpans.Summary = PendingSpans.Summary(counts.result(), closed.result(),
      open.valuesIterator.map { case (from, since) => (last._1 - from + 1, java.time.Duration.between(since, last._2).toMinutes) }.toSeq, ever.size)
  }
  object PendingSpans {
    /** `counts`: (Pending, listings held) at each projection; `decided`: each stretch that ended, (projections, minutes);
     *  `open`: each still Pending at the last projection; `listings`: how many were ever Pending. */
    final case class Summary(counts: Seq[(Int, Int)], decided: Seq[(Int, Long)], open: Seq[(Int, Long)], listings: Int) {
      def line: String = {
        def pct(xs: Seq[Long], p: Double) = if (xs.isEmpty) "-" else xs.sorted.apply(math.min(xs.size - 1, (xs.size * p).toInt)).toString
        val peak = counts.maxByOption(_._1).getOrElse((0, 0))
        f"pending at projection: peak ${peak._1} of ${peak._2}, mean ${if (counts.isEmpty) 0.0 else counts.map(_._1).sum.toDouble / counts.size}%.1f, " +
          s"last ${counts.lastOption.fold(0)(_._1)}; $listings listing(s) ever Pending; ${decided.size} stretch(es) decided, " +
          s"projections p50 ${pct(decided.map(_._1.toLong), 0.5)} p90 ${pct(decided.map(_._1.toLong), 0.9)} max ${pct(decided.map(_._1.toLong), 1.0)}, " +
          s"minutes p50 ${pct(decided.map(_._2), 0.5)} p90 ${pct(decided.map(_._2), 0.9)} max ${pct(decided.map(_._2), 1.0)}; " +
          s"${open.size} still Pending at the end (longest ${pct(open.map(_._1.toLong), 1.0)} projections, ${pct(open.map(_._2), 1.0)} min)"
      }
    }
    object Summary { val empty: Summary = Summary(Nil, Nil, Nil, 0) }
  }
  def sequence(reads: Reads, listings: Seq[Listing], seed: Long, gatePercent: Int, clock: MutableClock, filedDocs: FiledDocs = Noted,
               /** At least one booted listing leaves and returns: a drift slice's few listings, where [[Leave]] would send
                *  none away — and a sibling's run ending is what moved DE Werkstattkino's "CAMOUFLAGE" (#2473, ecedac9aa). */
               leaveOne: Boolean = false,
               /** The listings whose served-take moves are classified ([[Cause]]): a second pass over a seed that flipped. */
               probe: Set[ListingKey] = Set.empty): Replay = {
    // the arrangement's draws (venues, who leaves, each re-scrape's half) alone: a fill draws by question ([[Share.filed]]),
    // so the arrangement is the seed's whatever the code asks
    val rng     = new scala.util.Random(seed)
    val w = new Worker(reads, seed, gatePercent, clock, filedDocs)
    val scenario = w.gate.scenario
    scenario.arrange("inputs", listings.map(l => ListingKey.serialised(l.key)).sorted)
    val venues  = rng.shuffle(listings.map(_.venue).distinct.sorted)
    val (first, later) = venues.splitAt((venues.size * 0.7).toInt)
    val arrivals = later.grouped(math.max(1, later.size / 3 + 1)).toSeq.padTo(3, Nil)
    val atVenue  = listings.groupBy(_.venue)
    def of(vs: Seq[String]) = vs.flatMap(atVenue.getOrElse(_, Nil)).sorted
    val initial  = of(first)
    val drawn    = initial.filter(_ => rng.nextDouble() < Leave)
    val leaving  = if (drawn.isEmpty && leaveOne && initial.nonEmpty) Seq(initial(rng.nextInt(initial.size))) else drawn
    scenario.arrange("venues", venues); scenario.arrange("boot", first)
    arrivals.zipWithIndex.foreach { case (vs, i) => scenario.arrange(s"arrival-${i + 1}", vs) }
    scenario.arrange("leaving", leaving.map(l => ListingKey.serialised(l.key)))
    var halves = 0
    def half     = { val vs = rng.shuffle(venues).take(venues.size / 2); halves += 1; scenario.arrange(s"half-$halves", vs); vs.toSet }
    w.probed = probe
    val checkpoints = Seq.newBuilder[(String, Resolution, Set[ListingKey])]
    def rescrape(checkpoint: String = ""): Resolution = {
      // a venue scraped again bills what it bills: never a listing it dropped, kept for its facts alone
      val vs = half; val again = listings.filter(l => vs(l.venue) && w.held.contains(l.key) && !w.retained(l.key)); w.scraped(again); w.project(again.map(_.key), checkpoint)
    }
    w.boot(initial); w.project()
    w.filled(0.5); w.project()
    w.answered(); w.project()
    w.scraped(of(arrivals(0))); w.project()
    rescrape()
    w.gone(leaving.map(_.key)); w.project()
    w.deploy(outrule = false); w.project()
    w.filled(1.0); w.answered(); w.project()
    w.scraped(of(arrivals(1))); w.project()
    w.scraped(leaving); w.project()
    w.deploy(outrule = true); w.project()
    w.filled(0.5); w.project()
    w.scraped(of(arrivals(2))); w.project()
    checkpoints += { val r = w.settled(); ("settled", r, w.lastPending) }
    for (run <- 1 to 2) {
      checkpoints += { val r = rescrape(s"tick-$run"); (s"tick-$run", r, w.lastPending) }
      w.deploy(outrule = run == 2)
      checkpoints += { val r = w.project(checkpoint = s"deploy-$run"); (s"deploy-$run", r, w.lastPending) }
      checkpoints += { val r = rescrape(s"after-deploy-$run"); (s"after-deploy-$run", r, w.lastPending) }
    }
    Replay(checkpoints.result(), w.flips.flips, w.pendingServed.toSeq, w.pendingSpans.summary, w.causes.toMap, scenario)
  }

  /** What one replay found, over every seed: each divergence from the whole resolve, and each flip by its seed. */
  final case class Findings(divergences: Seq[Divergence], flips: Seq[(Int, TakeFlips.Flip)], pending: PendingTally = PendingTally.Empty,
                            /** Each seed's Pending stretches ([[PendingSpans]]). */
                            spans: Seq[(Int, PendingSpans.Summary)] = Nil,
                            /** Each flip's [[Cause]], by its seed and listing. */
                            causes: Map[(Int, ListingKey), String] = Map.empty,
                            /** Each seed's [[Scenario]]. */
                            scenarios: Seq[(Int, Scenario)] = Nil) {
    /** The divergences no `allowed` row names, and the flips no `flipsAllowed` row does: what fails the gate. */
    def open(country: String, allowed: Seq[Allowed], flipsAllowed: Seq[FlipAllowed]): Findings =
      Findings(divergences.filterNot(d => allowed.exists(_.covers(d))), flips.filterNot { case (_, f) => flipsAllowed.exists(_.covers(country, f)) }, pending, spans,
        causes, scenarios)
    def isEmpty: Boolean = divergences.isEmpty && flips.isEmpty
  }

  /** The incremental sequence over `listings` for each of `seeds`, every checkpoint compared with the whole resolve at the
   *  sequence's last moment — the replay the CI gate and a drift slice share. `readsAt` makes the reads over a clock; each
   *  seed's whole-resolve takes are handed to `wholeTakes`. */
  def replay(country: Country, listings: Seq[Listing], recordedAt: java.time.Instant, seeds: Seq[Int], gate: Int,
             readsAt: java.time.Clock => Reads, filedDocs: FiledDocs = Noted, leaveOne: Boolean = false,
             wholeTakes: (Int, Map[ListingKey, Take]) => Unit = (_, _) => (), classify: Boolean = true): Findings = {
    val flips = Seq.newBuilder[(Int, TakeFlips.Flip)]
    val causes = Map.newBuilder[(Int, ListingKey), String]
    var tally = PendingTally.Empty
    val spans = Seq.newBuilder[(Int, PendingSpans.Summary)]
    val scenarios = Seq.newBuilder[(Int, Scenario)]
    val divergences = seeds.flatMap { seed =>
      val clock  = new MutableClock(recordedAt)
      val replay = sequence(readsAt(clock), listings, seed.toLong, gate, clock, filedDocs, leaveOne)
      flips ++= replay.flips.map(seed -> _)
      scenarios += seed -> replay.scenario
      // each flip's cause: the seed's sequence again, the flipped listings' moves compared with a whole resolve at each
      if (classify && replay.flips.nonEmpty) {
        val again  = new MutableClock(recordedAt)
        val probed = sequence(readsAt(again), listings, seed.toLong, gate, again, filedDocs, leaveOne, probe = replay.flips.map(_.key).toSet)
        replay.flips.foreach(f => causes += (seed, f.key) -> Cause.ofFlip(f, probed.causes))
      }
      spans += seed -> replay.spans
      // the whole resolve at the sequence's last moment: the same answers' ages, the same year
      val wholeClock = new MutableClock(clock.instant())
      val wholeReads = readsAt(wholeClock)
      val reference  = takesOf(whole(wholeReads, listings, wholeClock, WholeLookups.recorded(wholeReads), filedDocs = filedDocs))
      wholeTakes(seed, reference)
      tally = tally + PendingTally.of(replay.pendingServed, reference)
      replay.checkpoints.flatMap { case (name, resolution, pending) =>
        val now = takesOf(resolution)
        listings.map(_.key).flatMap { key =>
          val (a, b) = (reference.getOrElse(key, Take("none", "absent", "")), now.getOrElse(key, Take("none", "absent", "")))
          // an incremental take Pending -- its awaits named, its film or none the decision on the inputs as they stand -- is
          // provisional (P1, step 6): no divergence; how often it differs from the final take is the pending tally
          Option.when(a.film != b.film && !pending(key))(Divergence(country.code, seed.toLong, name, key, a, b))
        }
      }
    }
    Findings(divergences, flips.result(), tally, spans.result(), causes.result(), scenarios.result())
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
    // each store's READ answers alone: a store holds the whole capture's family answers, and a family check's lattice
    // makes one a subset (hundreds a slice) -- kept whole, they ran a cut out of heap
    private val notings   = new java.util.concurrent.ConcurrentLinkedQueue[java.util.Map[String, org.bson.BsonDocument]]
    private val reads     = new java.util.concurrent.ConcurrentLinkedQueue[Reads]
    val listings: Seq[Listing] = (slice.core ++ slice.context).sortBy(_.sortKey)
    def readsAt(clock: java.time.Clock): Reads = {
      val made = new Reads(slice.country, replaying, replaying.gaps, clock, filed, tools.ProdState.none)
      reads.add(made)
      made
    }
    val filedDocs: FiledDocs = f => { val n = new tools.FiledAnswers.Noting(f); notings.add(n.read); n }
    def run(seeds: Seq[Int], gate: Int): Findings = replay(slice.country, listings, slice.recordedAt, seeds, gate, readsAt, filedDocs, leaveOne = true)
    /** The requests the slice lacked (none read from a recording): a replay of it is not the cut's. */
    def lacking: Seq[String] = replaying.lacking.asScala.map(_.render).toSeq.sorted
    /** The slice with what this run read beside it — the recording's answers it lacked, the filed answers read — written back. */
    def keep(): Unit = {
      val families = notings.asScala.flatMap(_.asScala).toMap
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
  final case class Cut(rounds: Int, widened: Findings, alone: Findings, lacking: Seq[String],
                       /** The family check's flips over the slice's lattice ([[latticeOf]]), with the recording behind it and alone. */
                       latticeWidened: Set[String] = Set.empty, latticeAlone: Set[String] = Set.empty) {
    def differences: Seq[String] = {
      // by film, not prose: a take's explanation names what it read, which the two read through other stores
      def lines(f: Findings) = f.divergences.map(d => s"divergence ${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, incremental " +
        s"${d.incremental.film} [${d.checkpoint}, seed ${d.seed}]").toSet ++
        f.flips.map { case (seed, flip) => s"flip ${flip.key.venue} | ${flip.key.rawTitle}: ${flip.films} [seed $seed]" }
      val (w, a) = (lines(widened), lines(alone))
      val (lw, la) = (latticeWidened, latticeAlone)
      ((w -- a) ++ (lw -- la)).toSeq.sorted.map(l => s"only with the recording: $l") ++
        ((a -- w) ++ (la -- lw)).toSeq.sorted.map(l => s"only in the slice: $l")
    }
    def closed: Boolean = lacking.isEmpty && differences.isEmpty
  }

  /** The family check's flips over `run`'s slice: its families' whole population lattice walked
   *  ([[services.identity.FamilyConvergence]]), each sub-population's own answers read through `run` -- what a cut keeps,
   *  so a subset's answers are the slice's too, not only those the incremental sequence's seeds happened to ask. */
  def latticeOf(run: SliceRun): Set[String] =
    services.identity.FamilyConvergence.check(services.identity.FamilyConvergence.sliceCorpus(run, "cut"), Map.empty,
      services.identity.FamilyConvergence.Options()).flips.map { case (_, f) => s"family ${f.stage} ${f.kind} ${f.listing}" }.toSet

  /** Cuts the slice in `dir` CLOSED: replays it with `widen` behind it -- the incremental sequence and the family check's
   *  lattice ([[latticeOf]]) -- and keeps everything that read (the transitive closure of what its listings' resolution
   *  and every sub-population's asks — searches, records, venue pages, the families' answers, finds, fills), then replays
   *  both alone; again, from what it holds now, while the replay alone lacks a request or finds otherwise, at most `rounds`
   *  times. The last round's verdict: a [[Cut]] not [[Cut.closed]] fails the cut. */
  def cut(dir: Path, seeds: Seq[Int], gate: Int, widen: SliceRun.Widen, rounds: Int = 3): Cut = {
    def round(n: Int): Cut = {
      val widening = new SliceRun(dir, Some(widen))
      val widened  = widening.run(seeds, gate)
      val wideFlips = latticeOf(widening)
      widening.keep()
      System.err.println(s"[identity-drift] cut round $n: the slice kept ${widening.replaying.widened.size} answer(s) read from the recording")
      val alone    = new SliceRun(dir)
      val replayed = alone.run(seeds, gate)
      val flips    = latticeOf(alone)
      val verdict  = Cut(n, widened, replayed, alone.lacking, wideFlips, flips)
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
    val seeds   = if (o.only.nonEmpty) o.only.distinct else 1 to o.seeds
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
    // what the harness drew per seed: the differential gate holds head and base to the same (scripts/ci/drift_scenario.py)
    findings.scenarios.foreach { case (seed, scenario) => println(s"## ${country.code} scenario [seed $seed]: ${scenario.digest}") }
    Files.writeString(o.out.resolve(s"scenario-${country.code}.tsv"),
      (Scenario.Header +: findings.scenarios.flatMap { case (seed, scenario) => scenario.rows(seed) }).mkString("", "\n", "\n"))
    val flipped = findings.flips
    Files.writeString(o.out.resolve(s"flips-${country.code}.tsv"), (FlipHeader +: flipped.map { case (seed, f) =>
      Seq(country.code, seed.toString, f.key.venue, f.key.rawTitle, f.films, f.reasons.mkString("; "), f.takes.map(_.line).mkString(" -> "),
        findings.causes.getOrElse((seed, f.key), "unclassified"))
        .map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
    }).mkString("", "\n", "\n"))
    // step 6's price: how often a provisional (Pending) take differed from the final one, by the kind of input it awaited
    println(s"## ${country.code} pending takes vs the final take, by awaited input (listing-projections, all seeds):")
    findings.pending.lines.foreach(line => println(s"      $line"))
    // the settle rule's price (the owner, 2026-10-31): how many listings stay Pending, and how long
    findings.spans.foreach { case (seed, summary) => println(s"## ${country.code} pending [seed $seed]: ${summary.line}") }
    Files.writeString(o.out.resolve(s"pending-${country.code}.tsv"), ("kind\tpending\tdiffer" +: (findings.pending.byKind.toSeq.sortBy(_._1) :+
      ("any" -> findings.pending.all)).map { case (k, (n, d)) => s"$k\t$n\t$d" }).mkString("", "\n", "\n"))
    val verdict = flipVerdict(country.code, open.flips, baseline, seeds)
    println(s"## ${country.code} flips: ${flipped.size} flip(s) (${flipped.map(_._2.key).distinct.size} listings), ${flipped.size - open.flips.size} allow-listed, " +
      s"${verdict.known.size} known")
    println(s"## ${country.code} known flips: ${verdict.known.size} known flip(s) remaining (${verdict.known.map(_._2.key).distinct.size} listings in " +
      s"identity-flip-backlog.tsv), ${verdict.fixed.size} fixed")
    verdict.fixed.sortBy(b => (b.rawTitle, b.venue)).foreach(b => println(s"      FIXED, remove its row: ${b.venue} | ${b.rawTitle} [seed ${b.seed}] — no flip in this run"))
    // each flip's cause (answer, population, lag), counted: what the flip-unification design's stage 1 tallies
    println(s"## ${country.code} flip causes: " + flipped.map { case (seed, f) => findings.causes.getOrElse((seed, f.key), "unclassified") }
      .groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sorted.map { case (cause, n) => s"$cause $n" }.mkString(", "))
    def cause(seed: Int, f: TakeFlips.Flip) = s" {cause ${findings.causes.getOrElse((seed, f.key), "unclassified")}}"
    verdict.known.distinctBy(_._2.key).sortBy { case (_, f) => (f.key.rawTitle, f.key.venue) }.foreach { case (seed, f) => println(s"      KNOWN: ${f.line} [seed $seed]${cause(seed, f)}") }
    verdict.fresh.groupBy { case (_, f) => f.reasons.head }.toSeq.sortBy(-_._2.size).foreach { case (reason, fs) =>
      println(s"  ${fs.map(_._2.key).distinct.size} listing(s)  flip: $reason")
      fs.distinctBy(_._2.key).sortBy { case (_, f) => (f.key.rawTitle, f.key.venue) }.take(6).foreach { case (seed, f) => println(s"      ${f.line} [seed $seed]${cause(seed, f)}") }
    }
    if (lacking.nonEmpty) {
      println(s"## the slice lacks ${lacking.size} answer(s) — cut it again (scripts/identity-slice --emit-drift-slice): ${lacking.take(8).mkString("; ")}")
      sys.exit(3)
    }
    // the explicit verdict line scripts/identity-slice reads: a crash prints none, so it is never read as red or clean
    val red = open.divergences.nonEmpty || verdict.fresh.nonEmpty
    println(verdictLine(red))
    if (red) sys.exit(1)
  }

  /** What the judgements say of the two sides' films, where they judge the listing. */
  private def verdictOf(country: Country, d: Divergence, labels: Seq[UnmatchedClusters.Label]): String = {
    val right = IdentitySlice.rightLabel(d.key, country, labels).map(_.film)
    right.fold("")(film => s" — judged ${film}")
  }
}
