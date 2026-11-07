package services.identity

import play.api.libs.json.{Json, OFormat}
import services.identity.agreement.FamilyFinalReads
import services.movies.ListingKey
import tools.UnmatchedClusters

import java.nio.file.{Files, Path}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.util.Random

/**
 * THE PER-FAMILY CONVERGENCE CHECK, keyed by the family index ([[FamilyIndex]]): every family's POPULATION LATTICE walked
 * -- subsets of its listings, each resolved by the model against the corpus's context with the rest of the family
 * absent ([[PopulationMonotonicity.Without]]) and settled by a fresh stage -- and a flip named wherever some arrival or
 * add/remove sequence takes a listing A -> B -> A (the prototype's FamilyReplay, 2026-11-04). Since incremental == whole,
 * the state after any sequence IS the whole resolve of the listings present, so the lattice holds every order's states.
 *
 * What the walk read -- the family's entry and final reads, and every read a SUBSET made that the whole family does not
 * (an answer only a sub-population asks) -- is its CLOSURE. A family's verdict is cached under the rules version and
 * its entry's digest with that closure; a later run walks again only the families whose digest differs or one of whose
 * closure reads digests otherwise now. One rule changed re-walks only the families whose decisions read its evidence
 * when the rules version is per rule; here it is the code's.
 *
 * Tiers: every subset of a family of [[Options.exhaustive]] listings or fewer; the levels 0..k and n-k..n of one of 63 or
 * fewer; seeded arrival orders of a larger one.
 */
object FamilyConvergence {

  /** `shard`: (i, n), the families whose [[shardOf]] is i of n alone -- a CI row's share of a country. */
  final case class Options(exhaustive: Int = 7, levels: Int = 1, orders: Int = 8, steps: Int = 8, threads: Int = 4,
                           shard: Option[(Int, Int)] = None,
                           /** THE CHANGE'S REACH (scripts/identity_what_if.py `select`'s rule patterns): a cached verdict
                            *  stands unless a rule it or its family fired matches one -- the rest of the families are served
                            *  from the cache under rules that name no code. `None`: the cache stands by its key alone. */
                           reach: Option[Seq[String]] = None,
                           /** What a family cost to walk, in seconds, by its name ([[loadCosts]]): the shards are balanced by it. A
                            *  family it does not name costs what its members do on average (1 a member when it names none). */
                           costs: Map[String, Double] = Map.empty,
                           /** Added to a sampled family's seed: a week's number gives each week other arrival orders. */
                           orderSalt: Int = 0)

  /** The bin (1-based) each cost falls in, longest first into the lightest bin: the largest family alone in its row rather
   *  than beside a dozen more, the rest filling up around it. Deterministic (ties by name, then position), so every row of
   *  a country computes the same split and a family is walked by exactly one. */
  def balance(costed: Seq[(String, Double)], bins: Int): Vector[Int] = {
    val load  = Array.fill(bins)(0.0)
    val where = Array.fill(costed.size)(0)
    costed.indices.sortBy(i => (-costed(i)._2, costed(i)._1, i)).foreach { i =>
      val bin = load.indices.minBy(load(_))
      load(bin) += costed(i)._2
      where(i) = bin + 1
    }
    where.toVector
  }

  /** The cost of each of `entries`: its recorded walk seconds, else the average a member costs in the recorded ones. */
  def costOf(entries: Seq[FamilyIndex.Entry], recorded: Map[String, Double]): Seq[Double] = {
    val known = entries.filter(e => recorded.contains(e.name))
    val rate  = if (known.isEmpty) 1.0 else known.map(e => recorded(e.name)).sum / known.map(_.members.size).sum
    entries.map(e => recorded.getOrElse(e.name, rate * e.members.size))
  }

  /** The costs of a `family-walks-<cc>.tsv` (family, tier, subsets, seconds, ...), a sampled family's seconds scaled to
   *  `orders` arrival orders of the N it was walked with. */
  def loadCosts(path: Path, orders: Int): Map[String, Double] = {
    val sampled = """(\d+) orders x .*""".r
    Files.readAllLines(path).asScala.drop(1).filter(_.nonEmpty).map(_.split("\t", -1)).map { cols =>
      cols(0) -> (cols(1) match {
        case sampled(walked) => cols(3).toDouble / walked.toInt * orders
        case _               => cols(3).toDouble
      })
    }.toMap
  }

  /** One flip: the stage (`model` or `final`), the listing, its kind (`arrival`, `add/remove`) and the takes it went through. */
  final case class Flip(stage: String, listing: String, kind: String, witness: String,
                        /** The rule ids that decided the listing in any subset walked ([[DecisionTrace.rulesOf]], the stage's). */
                        rules: Seq[String] = Nil)
  /** A read of the closure: its kind (`context`, `lookup`, `receipt`), its id and digest. */
  final case class Read(kind: String, id: String, digest: Long)
  /** A family's verdict, as cached: its name, key, tier, flips and the closure it was walked over; the rule ids any
   *  subset's decisions fired (what a change must reach to move it, [[Options.reach]]) and the walk's seconds. */
  final case class Verdict(family: String, key: Long, tier: String, subsets: Int, flips: Seq[Flip], closure: Seq[Read],
                           rules: Seq[String] = Nil, seconds: Double = 0)
  implicit val flipFormat: OFormat[Flip]       = Json.using[Json.WithDefaultValues].format[Flip]
  implicit val readFormat: OFormat[Read]       = Json.format[Read]
  implicit val verdictFormat: OFormat[Verdict] = Json.using[Json.WithDefaultValues].format[Verdict]

  /** Whether any of `rules` matches any of `patterns` (`*` any run): scripts/identity_what_if.py's `matches`, the
   *  what-if's rule roots read the same way. */
  def reaches(rules: Iterable[String], patterns: Seq[String]): Boolean = {
    val regexes = patterns.map(p => ("\\Q" + p.replace("*", "\\E.*\\Q") + "\\E").r)
    rules.exists(rule => regexes.exists(_.matches(rule)))
  }

  def load(path: Path): Map[Long, Verdict] =
    if (!Files.exists(path)) Map.empty
    else Json.parse(Files.readString(path)).as[Seq[Verdict]].map(v => v.key -> v).toMap
  def save(path: Path, verdicts: Seq[Verdict]): Unit = { val _ = Files.writeString(path, Json.prettyPrint(Json.toJson(verdicts.sortBy(_.family)))) }

  /** A run: every family's verdict, how many were walked again and how many reused from the cache, and the requests the
   *  corpus could not answer (a slice cut short of what its lattice asks: cut it again). */
  final case class Run(verdicts: Seq[Verdict], walked: Int, reused: Int, lacking: Seq[String] = Nil) {
    def flips: Seq[(Verdict, Flip)] = verdicts.flatMap(v => v.flips.map(v -> _))
  }

  /** One corpus to check: its listings, lookups, the stage's answers and clock, the resolve's inputs and the rules version;
   *  the store prod's filed answers are read through (a slice's cut notes them, [[scripts.IdentityDrift.SliceRun]]) and
   *  what the corpus lacked. */
  final case class Corpus(listings: Seq[Listing], lookups: IdentityLookups, reads: scripts.IdentitySlice.Reads, at: java.time.Instant,
                          inputs: ResolveInputs, rules: String, docs: scripts.IdentityDrift.FiledDocs = scripts.IdentityDrift.Noted,
                          lacking: () => Seq[String] = () => Nil)

  def check(corpus: Corpus, cache: Map[Long, Verdict], options: Options = Options()): Run = {
    import corpus.{inputs, lookups, reads}
    val all        = corpus.listings.sorted.distinctBy(_.key)
    val built      = FamilyIndex.build(all, lookups, reads.normalizer, inputs, threads = options.threads)
    val generation = new CandidateGeneration(all, lookups, reads.normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val without    = new PopulationMonotonicity.Without.Index(generation.nodes)
    val whole      = FamilyFinalReads.settle(reads, all, built.whole, lookups, corpus.at, corpus.docs)
    val oracle     = new FamilyFinalReads.AnswerOracle(reads, lookups, corpus.at, corpus.docs)
    val byKey      = all.map(l => l.key -> l).toMap

    /** What a closure read digests to now: the context and lookups of this corpus, its whole stage's receipts. */
    def now(read: Read): Option[Long] = read.kind match {
      case "context" => FamilyIndex.ContextRead.parse(read.id).map(_.digestIn(generation.context))
      case "lookup"  => FamilyIndex.Lookups.digestIn(Dependency.parse(read.id), lookups, byKey.get)
      case "receipt" => FamilyFinalReads.digestNow(Dependency.parse(read.id), whole)
      case "answer"  => oracle.digest(Dependency.parse(read.id))
      case _         => None
    }
    def keyOf(entry: FamilyIndex.Entry, family: FamilyFinalReads.Settled): Long =
      ContentHash.of((corpus.rules, entry.digest, family.receipts.map { case (d, v) => Dependency.encode(d) -> v },
        family.lookups.map { case (d, v) => Dependency.encode(d) -> v }))

    val walked = new java.util.concurrent.atomic.AtomicInteger
    def verdictOf(entry: FamilyIndex.Entry, inner: Int): Verdict = {
      val members = all.filter(l => entry.members.contains(l.key))
      val family  = FamilyFinalReads.settle(reads, members, built.whole, lookups, corpus.at, corpus.docs)
      val key     = keyOf(entry, family)
      def reached(v: Verdict) = options.reach.exists(p => reaches(v.rules ++ entry.rules.values.flatten, p))
      cache.get(key).filter(_.closure.forall(read => now(read).contains(read.digest))).filterNot(reached).getOrElse {
        walked.incrementAndGet()
        walk(entry, members, family, key, inner)
      }
    }

    def walk(entry: FamilyIndex.Entry, members: Seq[Listing], family: FamilyFinalReads.Settled, key: Long, inner: Int): Verdict = {
      val n       = members.size
      val keys    = members.map(_.key).toSet
      val started = tools.Stopwatch.start()
      val closure = TrieMap.empty[Read, Unit]
      val fired   = TrieMap.empty[ListingKey, Set[String]]
      def note(kind: String, id: String, digest: Long): Unit = closure.update(Read(kind, id, digest), ())
      type Takes = Map[ListingKey, Option[String]]
      val memo = TrieMap.empty[(Subset, Subset), (Takes, Takes)]
      def takes(subset: Subset): (Takes, Takes) = takesWith(subset, Subset.empty)
      // `subset` billed, and `gone` of the family departed: kept beside it as the worker keeps a departed listing
      // ([[ListingRetention.retained]], a billed listing relating to it by title), else absent
      def takesWith(subset: Subset, gone: Subset): (Takes, Takes) = {
        val billed = subset.toSeq.map(members)
        val kept   = if (gone.isEmpty) Nil else ListingRetention.retained(gone.toSeq.map(members), billed, reads.normalizer)
        if (gone.nonEmpty && kept.isEmpty) takes(subset) else memo.getOrElseUpdate((subset, Subset(kept.map(l => members.indexWhere(_.key == l.key))*)), {
        val present  = billed ++ kept
        val context  = new FamilyIndex.RecordingContext(without(generation.context, keys -- present.map(_.key)))
        val recorded = new FamilyIndex.RecordingLookups(lookups)
        val regions  = IdentityResolver.resolveRegion(present, recorded, reads.normalizer, inputs.calibration, inputs.pins, inputs.decorations, context)
        val decided  = regions.flatMap(_.decisions)
        decided.foreach(d => d.members.foreach(k => fired.updateWith(k)(was => Some(was.getOrElse(Set.empty) ++ d.trace.rulesOf(k) ++ d.trace.stage))))
        val settled  = FamilyFinalReads.settle(reads, present, UnmatchedClusters.resolutionOf(decided), lookups, corpus.at, corpus.docs)
        // a context read by the WHOLE corpus's value: the subset's view of it (its absent listings taken out) is a function
        // of that value and the family's members, which the key holds
        context.reads.keys.foreach(read => note("context", read.id, read.digestIn(generation.context)))
        recorded.reads.foreach { case (read, d) => note("lookup", Dependency.encode(read), d) }
        settled.lookups.foreach { case (read, d) => note("lookup", Dependency.encode(read), d) }
        // the year: the one receipt read of no question. A family answer asked only by a sub-population: no settle of the
        // whole family files it, so it is read again alone (the oracle's own store) -- by that digest; the ask memory's
        // attempts are each settle's own path state
        settled.receipts.foreach {
          case (read @ Dependency.Year, d)                         => note("receipt", Dependency.encode(read), d)
          case (read @ Dependency.Answer(_), _)                 => oracle.digest(read).foreach(d => note("answer", Dependency.encode(read), d))
          case _                                                   =>
        }
        val model = decided.flatMap(d => d.members.map(_ -> PopulationMonotonicity.takeOf(d))).toMap
        (model, settled.takes)
      })
      }
      def label(subset: Subset) = subset.toSeq.map(members(_).venue).mkString("{", ", ", "}")
      val everyone = Subset(members.indices*)
      val (tier, subsets) =
        if (n <= options.exhaustive) (s"lattice 2^$n", (0 until (1 << n)).map(mask => Subset(members.indices.filter(i => (mask & (1 << i)) != 0)*)))
        else if (n <= 63) {
          def choose(k: Int) = members.indices.combinations(k).map(Subset(_*)).toSeq
          (s"levels 0-${options.levels}", (0 to options.levels).flatMap(k => choose(k) ++ choose(k).map(everyone -- _)).distinct)
        } else {
          val rng = new Random(MurmurHashOf(members.head.sortKey) + options.orderSalt)
          (s"${options.orders} orders x ${options.steps} steps", (0 until options.orders).flatMap { _ =>
            val order = rng.shuffle(members.indices.toVector)
            val size  = math.max(1, math.ceil(n.toDouble / options.steps).toInt)
            ((size until n by size).map(order.take) :+ order).map(Subset(_*))
          }.distinct)
        }
      if (inner <= 1) subsets.foreach(takes)
      else {
        val pool = java.util.concurrent.Executors.newFixedThreadPool(inner)
        try subsets.map(s => pool.submit(() => takes(s))).foreach(_.get) finally pool.shutdown()
      }
      val flips = for {
        (stage, pick) <- Seq[(String, ((Takes, Takes)) => Takes)]("model" -> (_._1), "final" -> (_._2))
        own           <- members.indices
        flip          <- flipOf(own, everyone, subsets, subset => pick(takes(subset)).get(members(own).key).flatten,
                           (subset, gone) => pick(takesWith(subset, Subset(gone))).get(members(own).key).flatten, label)
      } yield Flip(stage, s"${members(own).venue} | ${members(own).rawTitle}", flip._1, flip._2, fired.getOrElse(members(own).key, Set.empty).toSeq.sorted)
      // the closure: what the walk read beyond the family's own entry and settle, which the key already holds
      val own = entry.context.keySet.map(_.id) ++ entry.answers.keySet.map(Dependency.encode) ++
        family.receipts.keySet.map(Dependency.encode) ++ family.lookups.keySet.map(Dependency.encode)
      Verdict(entry.name, key, tier, memo.size, flips, closure.keys.filterNot(read => own(read.id)).toSeq.sortBy(r => (r.kind, r.id)),
        fired.values.flatten.toSeq.distinct.sorted, started.seconds)
    }

    val multi    = built.index.entries.filter(_.members.sizeIs >= 2)
    val entries  = options.shard.fold(multi) { case (i, n) =>
      val bins = balance(multi.map(_.name).zip(costOf(multi, options.costs)), n)
      multi.indices.filter(bins(_) == i).map(multi)
    }
    // at most `threads` resolves at once: the small families side by side, each walked on one thread; a large one (a
    // whole corpus's 375-listing family holds GBs per resolve) alone, its subsets side by side
    val (small, large) = entries.indices.partition(entries(_).members.sizeIs <= options.exhaustive)
    val walkedSmall =
      if (options.threads <= 1) small.map(i => i -> verdictOf(entries(i), 1))
      else {
        val pool = tools.DaemonExecutors.boundedEC("family-convergence", options.threads)
        try small.map(i => i -> pool.submit(() => verdictOf(entries(i), 1))).map((i, f) => i -> f.get)
        finally { val _ = pool.shutdownNow() }
      }
    val byIndex  = (walkedSmall ++ large.map(i => i -> verdictOf(entries(i), options.threads))).toMap
    val verdicts = entries.indices.map(byIndex)
    Run(verdicts, walked.get, verdicts.size - walked.get, corpus.lacking())
  }

  /** A family's subset: the indices of the listings present. */
  type Subset = scala.collection.immutable.BitSet
  val Subset = scala.collection.immutable.BitSet

  /** A listing's first flip over the computed `subsets` (each holding it): an arrival A -> B -> A along nested subsets,
   *  else an add/remove: A at m, B once listing x arrives, and A again once x DEPARTS -- `departed(m, x)`, the take with m
   *  billed and x kept as the worker keeps a departed listing ([[ListingRetention.retained]]: a billed listing relating to
   *  it by title), absent where nothing keeps it. A departure is no absence: the explorer walked it as one and named 144
   *  DE and UK flips prod cannot reach, its kept listing deciding as when billed (convergence run 42358278155,
   *  recording 573: DE 78 and UK 66 add/remove flips, none once the departed listing is kept; the 51 arrival flips stand). */
  def flipOf(own: Int, everyone: Subset, subsets: Seq[Subset], take: Subset => Option[String],
             departed: (Subset, Int) => Option[String], label: Subset => String): Option[(String, String)] = {
    val vals = subsets.filter(_(own)).flatMap(m => take(m).map(m -> _))
    def within(a: Subset, b: Subset) = a != b && a.subsetOf(b)
    vals.iterator.flatMap { case (m2, b) =>
      vals.iterator.filter { case (m1, a) => a != b && within(m1, m2) }.flatMap { case (m1, a) =>
        vals.find { case (m3, c) => c == a && within(m2, m3) }.map { case (m3, _) => s"$a@${label(m1)} -> $b@${label(m2)} -> $a@${label(m3)}" }
      }
    }.nextOption().map("arrival" -> _).orElse {
      val at = vals.toMap
      vals.iterator.flatMap { case (m, a) =>
        (everyone -- m).iterator.flatMap(x => at.get(m + x).filter(_ != a).filter(_ => departed(m, x).contains(a))
          .map(b => s"$a@${label(m)} -> $b@${label(m + x)} -> $a@${label(m)} + ${label(Subset(x))} departed"))
      }.nextOption().map("add/remove" -> _)
    }
  }

  private def MurmurHashOf(s: String): Long = scala.util.hashing.MurmurHash3.stringHash(s).toLong

  /**
   *   worker/Test/runMain services.identity.FamilyConvergence [--cache FILE] [--rules VERSION] [--exhaustive 7] [--levels 1] [--orders 8] [--order-salt 0] [--costs WALKS_TSV] [--threads 4] SLICE_DIR...
   * Each slice's families checked against the cache, the cache written back.
   */
  /** What a corpus run is pointed at: `--corpus CC --recording DIR [--families CAPTURE|none]`, its TSV written to `--out`. */
  final case class CorpusArgs(country: String = "", recording: Option[Path] = None, families: Option[Path] = None, out: Path = Path.of("target/family-check"),
                              costs: Option[Path] = None)

  def main(args: Array[String]): Unit = {
    def parse(rest: List[String], o: Options, cache: Option[Path], rules: String, dirs: Seq[Path], c: CorpusArgs): (Options, Option[Path], String, Seq[Path], CorpusArgs) =
      rest match {
        case Nil                          => (o, cache, rules, dirs, c)
        case "--cache" :: v :: more       => parse(more, o, Some(Path.of(v)), rules, dirs, c)
        case "--rules" :: v :: more       => parse(more, o, cache, v, dirs, c)
        case "--exhaustive" :: v :: more  => parse(more, o.copy(exhaustive = v.toInt), cache, rules, dirs, c)
        case "--levels" :: v :: more      => parse(more, o.copy(levels = v.toInt), cache, rules, dirs, c)
        case "--orders" :: v :: more      => parse(more, o.copy(orders = v.toInt), cache, rules, dirs, c)
        case "--order-salt" :: v :: more  => parse(more, o.copy(orderSalt = v.toInt), cache, rules, dirs, c)
        case "--costs" :: v :: more       => parse(more, o, cache, rules, dirs, c.copy(costs = Some(Path.of(v))))
        case "--threads" :: v :: more     => parse(more, o.copy(threads = v.toInt), cache, rules, dirs, c)
        case "--shard" :: v :: more       =>
          val (i, n) = v.split('/') match { case Array(i, n) => (i.toInt, n.toInt); case _ => sys.error(s"--shard $v: i/n") }
          require(i >= 1 && i <= n, s"--shard $v: i in 1..n")
          parse(more, o.copy(shard = Some(i -> n)), cache, rules, dirs, c)
        // a file of the change's rule patterns (scripts/identity_what_if.py select's), one a line; `*` every rule: walk ALL
        case "--reach" :: v :: more       =>
          val patterns = Files.readAllLines(Path.of(v)).asScala.map(_.trim).filter(_.nonEmpty).toSeq
          parse(more, o.copy(reach = Some(patterns)), cache, rules, dirs, c)
        case "--corpus" :: v :: more      => parse(more, o, cache, rules, dirs, c.copy(country = v))
        case "--recording" :: v :: more   => parse(more, o, cache, rules, dirs, c.copy(recording = Some(Path.of(v))))
        case "--families" :: v :: more    => parse(more, o, cache, rules, dirs, c.copy(families = Option.when(v != "none")(Path.of(v))))
        case "--out" :: v :: more         => parse(more, o, cache, rules, dirs, c.copy(out = Path.of(v)))
        case dir :: more                  => parse(more, o, cache, rules, dirs :+ Path.of(dir), c)
      }
    // the rules version a cached verdict stands under: the caller's (a code digest), `head` when it names none
    val (parsed, cachePath, rules, dirs, corpusArgs) = parse(args.toList, Options(), None, "head", Nil, CorpusArgs())
    val options = corpusArgs.costs.fold(parsed)(file => parsed.copy(costs = loadCosts(file, parsed.orders)))
    var cache = cachePath.fold(Map.empty[Long, Verdict])(load)
    if (corpusArgs.country.nonEmpty) {
      val country = models.Country.all.find(_.code == corpusArgs.country).getOrElse(sys.error(s"no country '${corpusArgs.country}'"))
      val started = tools.Stopwatch.start()
      val run     = check(recordingCorpus(country, corpusArgs.recording.getOrElse(sys.error("--corpus reads --recording DIR")), corpusArgs.families, rules),
        cache, options)
      cache ++= run.verdicts.map(v => v.key -> v)
      cachePath.foreach(save(_, cache.values.toSeq))
      java.nio.file.Files.createDirectories(corpusArgs.out)
      java.nio.file.Files.writeString(corpusArgs.out.resolve(s"family-flips-${country.code}.tsv"), flipsTsv(country.code, run))
      // each family's walk: what a change reaching its rules costs (scripts/ci/family_reach.py prices a commit by it)
      java.nio.file.Files.writeString(corpusArgs.out.resolve(s"family-walks-${country.code}.tsv"),
        ("family\ttier\tsubsets\tseconds\tflips\trules" +: run.verdicts.map(v => Seq(v.family, v.tier, v.subsets, "%.2f".formatLocal(java.util.Locale.ROOT, v.seconds), v.flips.size,
          v.rules.mkString(" ")).map(_.toString.replace('\t', ' ').replace('\n', ' ')).mkString("\t"))).mkString("", "\n", "\n"))
      val shard = options.shard.fold("")({ case (i, n) => s", shard $i/$n" })
      val reach = options.reach.fold("")(p => s", reach ${if (p.contains("*")) "ALL" else p.size.toString + " rule pattern(s)"}")
      println(f"## ${country.code} family check: ${run.verdicts.size} families$shard$reach, ${run.walked} walked, ${run.reused} reused, " +
        f"${run.flips.size} flip(s) over ${run.flips.map(_._2.listing).distinct.size} listing(s), levels ${options.levels}, ${started.seconds}%.0fs")
      run.verdicts.groupBy(_.tier.takeWhile(_ != ' ')).toSeq.sortBy(_._1).foreach { case (tier, vs) =>
        println(s"## ${country.code} tier $tier: ${vs.size} families, ${vs.map(_.subsets).sum} subsets") }
      run.flips.foreach { case (v, f) => println(s"  ${f.stage}\t${f.kind}\t${f.listing}\t${f.witness}\t[${v.tier}]") }
      if (run.lacking.nonEmpty) println(s"## ${country.code} the recording lacks ${run.lacking.size} answer(s), read as unknown")
      return
    }
    val slices = Seq.newBuilder[String]
    dirs.foreach { dir =>
      val started = tools.Stopwatch.start()
      val run     = check(sliceCorpus(dir, rules), cache, options)
      cache ++= run.verdicts.map(v => v.key -> v)
      if (run.lacking.nonEmpty) println(s"# ${dir.getFileName}: LACKS ${run.lacking.size} answer(s), cut it again: ${run.lacking.take(5).mkString("; ")}")
      println(f"# ${dir.getFileName}: ${run.verdicts.size} families, ${run.walked} walked, ${run.reused} reused, ${run.flips.size} flip(s), " +
        f"${started.seconds}%.1fs")
      run.flips.foreach { case (v, f) => println(s"  ${f.stage}\t${f.kind}\t${f.listing}\t${f.witness}\t[${v.tier}]") }
      slices += flipsTsv(dir.getParent.getFileName.toString, run,
        slice = s"drift-slices/${dir.getParent.getFileName}/${dir.getFileName}", header = false)
    }
    cachePath.foreach(save(_, cache.values.toSeq))
    // the slices' flips as identity-flip-backlog.tsv rows (kind `family`): what a check's birth files, generated
    java.nio.file.Files.createDirectories(corpusArgs.out)
    java.nio.file.Files.writeString(corpusArgs.out.resolve("family-flips-slices.tsv"), header + "\n" + slices.result().mkString)
  }

  val header = "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote"

  /** A run's flips in identity-flip-backlog.tsv's row shape (kind `family`), one row a listing: its stages and kinds in
   *  the note, its first witness as the takes. */
  def flipsTsv(country: String, run: Run, slice: String = "", header: Boolean = true): String = {
    val rows = run.flips.groupBy(_._2.listing).toSeq.sortBy(_._1).map { case (listing, fs) =>
      val (venue, raw) = listing.indexOf(" | ") match { case -1 => (listing, ""); case i => (listing.take(i), listing.drop(i + 3)) }
      val rules = fs.flatMap(_._2.rules).distinct.sorted
      val note = fs.map { case (_, f) => s"${f.stage} ${f.kind}" }.distinct.sorted.mkString(", ") + s" [${fs.head._1.tier}] ${fs.head._1.family}" +
        (if (rules.isEmpty) "" else s" rules: ${rules.mkString(" ")}")
      Seq("family", country, "", slice, venue, raw, fs.head._2.witness, "", "", note).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")
    }
    ((if (header) Seq(this.header) else Nil) ++ rows).map(_ + "\n").mkString
  }

  /** A recording's whole corpus (a hermetic leg's pinned pair), prod's filed answers from the unmatched-cluster capture
   *  `families` beside it -- as the takes and drift rows read it (scripts.IdentityDrift's recording scope). */
  def recordingCorpus(country: models.Country, root: Path, families: Option[Path], rules: String): Corpus = {
    val recording = scripts.IdentitySlice.Recording(root, country)
    val filed     = families.filter(Files.exists(_)).fold(tools.FiledAnswers.of(country))(tools.FiledAnswers.ofCapture)
    val opened    = scripts.IdentitySlice.openRecording(recording, filed)
    val listings  = scripts.IdentitySlice.listingsOf(recording, opened.rows)
    val reads     = new scripts.IdentitySlice.Reads(country, opened.fetch, opened.gaps,
      java.time.Clock.fixed(recording.recordedAt, java.time.ZoneOffset.UTC), filed, tools.ProdState.none)
    Corpus(listings, scripts.IdentityDrift.memo(reads), reads, recording.recordedAt, scripts.IdentityDrift.inputsOf(reads), rules)
  }

  /** A committed slice as a corpus: its listings, its own answers, read at its recording's time. */
  def sliceCorpus(dir: Path, rules: String): Corpus = sliceCorpus(new scripts.IdentityDrift.SliceRun(dir), rules)
  /** `run`'s slice as a corpus, its filed answers read through `run`'s noting store (what a cut keeps). */
  def sliceCorpus(run: scripts.IdentityDrift.SliceRun, rules: String): Corpus = {
    val reads = run.readsAt(new tools.MutableClock(run.slice.recordedAt))
    Corpus(run.listings, scripts.IdentityDrift.memo(reads), reads, run.slice.recordedAt, ResolveInputs.Unpinned, rules, run.filedDocs, () => run.lacking)
  }
}
