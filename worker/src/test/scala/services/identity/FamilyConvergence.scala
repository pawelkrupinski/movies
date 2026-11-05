package services.identity

import play.api.libs.json.{Json, OFormat}
import services.identity.agreement.FamilyFinalReads
import services.movies.ListingKey
import tools.UnmatchedClusters

import java.nio.file.{Files, Path}
import scala.collection.concurrent.TrieMap
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

  final case class Options(exhaustive: Int = 7, levels: Int = 1, orders: Int = 8, steps: Int = 8, threads: Int = 4)

  /** One flip: the stage (`model` or `final`), the listing, its kind (`arrival`, `add/remove`) and the takes it went through. */
  final case class Flip(stage: String, listing: String, kind: String, witness: String)
  /** A read of the closure: its kind (`context`, `lookup`, `receipt`), its id and digest. */
  final case class Read(kind: String, id: String, digest: Long)
  /** A family's verdict, as cached: its name, key, tier, flips and the closure it was walked over. */
  final case class Verdict(family: String, key: Long, tier: String, subsets: Int, flips: Seq[Flip], closure: Seq[Read])
  implicit val flipFormat: OFormat[Flip]       = Json.format[Flip]
  implicit val readFormat: OFormat[Read]       = Json.format[Read]
  implicit val verdictFormat: OFormat[Verdict] = Json.format[Verdict]

  def load(path: Path): Map[Long, Verdict] =
    if (!Files.exists(path)) Map.empty
    else Json.parse(Files.readString(path)).as[Seq[Verdict]].map(v => v.key -> v).toMap
  def save(path: Path, verdicts: Seq[Verdict]): Unit = { val _ = Files.writeString(path, Json.prettyPrint(Json.toJson(verdicts.sortBy(_.family)))) }

  /** A run: every family's verdict, how many were walked again and how many reused from the cache. */
  final case class Run(verdicts: Seq[Verdict], walked: Int, reused: Int) {
    def flips: Seq[(Verdict, Flip)] = verdicts.flatMap(v => v.flips.map(v -> _))
  }

  /** One corpus to check: its listings, lookups, the stage's answers and clock, the resolve's inputs and the rules version. */
  final case class Corpus(listings: Seq[Listing], lookups: IdentityLookups, reads: scripts.IdentitySlice.Reads, at: java.time.Instant,
                          inputs: ResolveInputs, rules: String)

  def check(corpus: Corpus, cache: Map[Long, Verdict], options: Options = Options()): Run = {
    import corpus.{inputs, lookups, reads}
    val all        = corpus.listings.sorted.distinctBy(_.key)
    val built      = FamilyIndex.build(all, lookups, reads.normalizer, inputs, threads = options.threads)
    val generation = new CandidateGeneration(all, lookups, reads.normalizer, inputs.pins, inputs.decorations, lazyLookups = false)
    val without    = new PopulationMonotonicity.Without.Index(generation.nodes)
    val whole      = FamilyFinalReads.settle(reads, all, built.whole, lookups, corpus.at)
    val byKey      = all.map(l => l.key -> l).toMap

    /** What a closure read digests to now: the context and lookups of this corpus, its whole stage's receipts. */
    def now(read: Read): Option[Long] = read.kind match {
      case "context" => FamilyIndex.ContextRead.parse(read.id).map(_.digestIn(generation.context))
      case "lookup"  => FamilyIndex.Lookups.digestIn(Dependency.parse(read.id), lookups, byKey.get)
      case "receipt" => FamilyFinalReads.digestNow(Dependency.parse(read.id), whole)
      case _         => None
    }
    def keyOf(entry: FamilyIndex.Entry, family: FamilyFinalReads.Settled): Long =
      ContentHash.of((corpus.rules, entry.digest, family.receipts.map { case (d, v) => Dependency.encode(d) -> v },
        family.lookups.map { case (d, v) => Dependency.encode(d) -> v }))

    val walked = new java.util.concurrent.atomic.AtomicInteger
    def verdictOf(entry: FamilyIndex.Entry): Verdict = {
      val members = all.filter(l => entry.members.contains(l.key))
      val family  = FamilyFinalReads.settle(reads, members, built.whole, lookups, corpus.at)
      val key     = keyOf(entry, family)
      cache.get(key).filter(_.closure.forall(read => now(read).contains(read.digest))).getOrElse {
        walked.incrementAndGet()
        walk(entry, members, family, key)
      }
    }

    def walk(entry: FamilyIndex.Entry, members: Seq[Listing], family: FamilyFinalReads.Settled, key: Long): Verdict = {
      val n       = members.size
      val keys    = members.map(_.key).toSet
      val closure = TrieMap.empty[Read, Unit]
      def note(kind: String, id: String, digest: Long): Unit = closure.update(Read(kind, id, digest), ())
      type Takes = Map[ListingKey, Option[String]]
      val memo = TrieMap.empty[Subset, (Takes, Takes)]
      def takes(subset: Subset): (Takes, Takes) = memo.getOrElseUpdate(subset, {
        val present  = subset.toSeq.map(members)
        val context  = new FamilyIndex.RecordingContext(without(generation.context, keys -- present.map(_.key)))
        val recorded = new FamilyIndex.RecordingLookups(lookups)
        val regions  = IdentityResolver.resolveRegion(present, recorded, reads.normalizer, inputs.calibration, inputs.pins, inputs.decorations, context)
        val decided  = regions.flatMap(_.decisions)
        val settled  = FamilyFinalReads.settle(reads, present, UnmatchedClusters.resolutionOf(decided), lookups, corpus.at)
        // a context read by the WHOLE corpus's value: the subset's view of it (its absent listings taken out) is a function
        // of that value and the family's members, which the key holds
        context.reads.keys.foreach(read => note("context", read.id, read.digestIn(generation.context)))
        recorded.reads.foreach { case (read, d) => note("lookup", Dependency.encode(read), d) }
        settled.lookups.foreach { case (read, d) => note("lookup", Dependency.encode(read), d) }
        // the year: the one receipt read of no question. A family answer asked only by a sub-population is read off the
        // answers it was asked of (the recording's, a store's filings), which no settle of the whole family holds; the ask
        // memory's attempts are each settle's own path state
        settled.receipts.keys.filter(_ == Dependency.Year).foreach(read => note("receipt", Dependency.encode(read), settled.receipts(read)))
        val model = decided.flatMap(d => d.members.map(_ -> PopulationMonotonicity.takeOf(d))).toMap
        (model, settled.takes)
      })
      def label(subset: Subset) = subset.toSeq.map(members(_).venue).mkString("{", ", ", "}")
      val everyone = Subset(members.indices*)
      val (tier, subsets) =
        if (n <= options.exhaustive) (s"lattice 2^$n", (0 until (1 << n)).map(mask => Subset(members.indices.filter(i => (mask & (1 << i)) != 0)*)))
        else if (n <= 63) {
          def choose(k: Int) = members.indices.combinations(k).map(Subset(_*)).toSeq
          (s"levels 0-${options.levels}", (0 to options.levels).flatMap(k => choose(k) ++ choose(k).map(everyone -- _)).distinct)
        } else {
          val rng = new Random(MurmurHashOf(members.head.sortKey))
          (s"${options.orders} orders x ${options.steps} steps", (0 until options.orders).flatMap { _ =>
            val order = rng.shuffle(members.indices.toVector)
            val size  = math.max(1, math.ceil(n.toDouble / options.steps).toInt)
            ((size until n by size).map(order.take) :+ order).map(Subset(_*))
          }.distinct)
        }
      if (options.threads <= 1) subsets.foreach(takes)
      else {
        val pool = java.util.concurrent.Executors.newFixedThreadPool(options.threads)
        try subsets.map(s => pool.submit(() => takes(s))).foreach(_.get) finally pool.shutdown()
      }
      val flips = for {
        (stage, pick) <- Seq[(String, ((Takes, Takes)) => Takes)]("model" -> (_._1), "final" -> (_._2))
        own           <- members.indices
        flip          <- flipOf(own, everyone, subsets, subset => pick(takes(subset)).get(members(own).key).flatten, label)
      } yield Flip(stage, s"${members(own).venue} | ${members(own).rawTitle}", flip._1, flip._2)
      // the closure: what the walk read beyond the family's own entry and settle, which the key already holds
      val own = entry.context.keySet.map(_.id) ++ entry.answers.keySet.map(Dependency.encode) ++
        family.receipts.keySet.map(Dependency.encode) ++ family.lookups.keySet.map(Dependency.encode)
      Verdict(entry.name, key, tier, memo.size, flips, closure.keys.filterNot(read => own(read.id)).toSeq.sortBy(r => (r.kind, r.id)))
    }

    val entries  = built.index.entries.filter(_.members.sizeIs >= 2)
    val verdicts =
      if (options.threads <= 1) entries.map(verdictOf)
      else {
        val pool = tools.DaemonExecutors.boundedEC("family-convergence", options.threads)
        try entries.map(e => pool.submit(() => verdictOf(e))).map(_.get)
        finally { val _ = pool.shutdownNow() }
      }
    Run(verdicts, walked.get, verdicts.size - walked.get)
  }

  /** A family's subset: the indices of the listings present. */
  type Subset = scala.collection.immutable.BitSet
  val Subset = scala.collection.immutable.BitSet

  /** A listing's first flip over the computed `subsets` (each holding it): an arrival A -> B -> A along nested subsets,
   *  else an add/remove A -> B one listing of `everyone` apart. */
  def flipOf(own: Int, everyone: Subset, subsets: Seq[Subset], take: Subset => Option[String], label: Subset => String): Option[(String, String)] = {
    val vals = subsets.filter(_(own)).flatMap(m => take(m).map(m -> _))
    def within(a: Subset, b: Subset) = a != b && a.subsetOf(b)
    vals.iterator.flatMap { case (m2, b) =>
      vals.iterator.filter { case (m1, a) => a != b && within(m1, m2) }.flatMap { case (m1, a) =>
        vals.find { case (m3, c) => c == a && within(m2, m3) }.map { case (m3, _) => s"$a@${label(m1)} -> $b@${label(m2)} -> $a@${label(m3)}" }
      }
    }.nextOption().map("arrival" -> _).orElse {
      val at = vals.toMap
      vals.iterator.flatMap { case (m, a) =>
        (everyone -- m).iterator.flatMap(x => at.get(m + x).filter(_ != a).map(b => s"$a@${label(m)} -> $b@${label(m + x)} -> $a"))
      }.nextOption().map("add/remove" -> _)
    }
  }

  private def MurmurHashOf(s: String): Long = scala.util.hashing.MurmurHash3.stringHash(s).toLong

  /**
   *   worker/Test/runMain services.identity.FamilyConvergence [--cache FILE] [--rules VERSION] [--exhaustive 7] [--levels 1] [--threads 4] SLICE_DIR...
   * Each slice's families checked against the cache, the cache written back.
   */
  def main(args: Array[String]): Unit = {
    def parse(rest: List[String], o: Options, cache: Option[Path], rules: String, dirs: Seq[Path]): (Options, Option[Path], String, Seq[Path]) =
      rest match {
        case Nil                          => (o, cache, rules, dirs)
        case "--cache" :: v :: more       => parse(more, o, Some(Path.of(v)), rules, dirs)
        case "--rules" :: v :: more       => parse(more, o, cache, v, dirs)
        case "--exhaustive" :: v :: more  => parse(more, o.copy(exhaustive = v.toInt), cache, rules, dirs)
        case "--levels" :: v :: more      => parse(more, o.copy(levels = v.toInt), cache, rules, dirs)
        case "--threads" :: v :: more     => parse(more, o.copy(threads = v.toInt), cache, rules, dirs)
        case dir :: more                  => parse(more, o, cache, rules, dirs :+ Path.of(dir))
      }
    // the rules version a cached verdict stands under: the caller's (a code digest), `head` when it names none
    val (options, cachePath, rules, dirs) = parse(args.toList, Options(), None, "head", Nil)
    var cache = cachePath.fold(Map.empty[Long, Verdict])(load)
    dirs.foreach { dir =>
      val started = tools.Stopwatch.start()
      val run     = check(sliceCorpus(dir, rules), cache, options)
      cache ++= run.verdicts.map(v => v.key -> v)
      println(f"# ${dir.getFileName}: ${run.verdicts.size} families, ${run.walked} walked, ${run.reused} reused, ${run.flips.size} flip(s), " +
        f"${started.seconds}%.1fs")
      run.flips.foreach { case (v, f) => println(s"  ${f.stage}\t${f.kind}\t${f.listing}\t${f.witness}\t[${v.tier}]") }
    }
    cachePath.foreach(save(_, cache.values.toSeq))
  }

  /** A committed slice as a corpus: its listings, its own answers, read at its recording's time. */
  def sliceCorpus(dir: Path, rules: String): Corpus = {
    val run   = new scripts.IdentityDrift.SliceRun(dir)
    val reads = run.readsAt(new tools.MutableClock(run.slice.recordedAt))
    Corpus(run.listings, scripts.IdentityDrift.memo(reads), reads, run.slice.recordedAt, ResolveInputs.Unpinned, rules)
  }
}
