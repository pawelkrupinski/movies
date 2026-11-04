package scripts

import services.identity._
import services.movies.ListingKey
import tools.MutableClock

import java.nio.file.{Files, Path}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.util.Random

/**
 * PROTOTYPE (investigation 2026-11-04, family replay; not wired into any gate): a family replayed in ISOLATION with its
 * read set -- a committed slice's core and context, its own answers -- over its POPULATION LATTICE. Every subset of the
 * varying listings (the target family's, or every listing of the slice with `--vary all`) is resolved whole: the model
 * (`IdentityResolver.resolve`) and the final take (the model, then a fresh agreement stage asked to its fixpoint,
 * [[IdentityDrift.whole]] -- what the drift gate compares against). Since incremental == whole (the equivalence
 * property), the state after any add/remove sequence IS the whole resolve of the listings present, so the lattice holds
 * every arrival order's and every add/remove sequence's states at once:
 *
 *  - ARRIVAL flip: S1 < S2 < S3 (each holding the listing) with take(S1) == take(S3) != take(S2), every take decided --
 *    exactly "some arrival order goes A -> B -> A" (a chain through S1, S2, S3 extends to a full order);
 *  - ADD/REMOVE flip: take(S) != take(S +- x) for one listing x: x arriving and leaving again is A -> B -> A.
 *
 * Families over `--max` listings walk `--orders` random full orders instead (at most `--steps` prefixes each).
 *
 *   worker/Test/runMain scripts.FamilyReplay [--vary family|all] [--max 7] [--orders 40] [--steps 12] [--threads 4]
 *     [--target 'venue|raw title']... [--model-only] [--out FILE] SLICE_DIR...
 */
object FamilyReplay {

  final case class Options(vary: String = "family", max: Int = 7, orders: Int = 40, steps: Int = 12, levels: Int = 3, threads: Int = 4,
                           targets: Seq[(String, String)] = Nil, modelOnly: Boolean = false, out: Option[Path] = None,
                           dirs: Seq[Path] = Nil)

  def parse(args: List[String], o: Options = Options()): Options = args match {
    case Nil                        => o
    case "--vary" :: v :: rest      => parse(rest, o.copy(vary = v))
    case "--max" :: v :: rest       => parse(rest, o.copy(max = v.toInt))
    case "--orders" :: v :: rest    => parse(rest, o.copy(orders = v.toInt))
    case "--steps" :: v :: rest     => parse(rest, o.copy(steps = v.toInt))
    case "--levels" :: v :: rest    => parse(rest, o.copy(levels = v.toInt))
    case "--threads" :: v :: rest   => parse(rest, o.copy(threads = v.toInt))
    case "--target" :: v :: rest    => val Array(a, b) = v.split("\\|", 2); parse(rest, o.copy(targets = o.targets :+ (a.trim -> b.trim)))
    case "--model-only" :: rest     => parse(rest, o.copy(modelOnly = true))
    case "--out" :: v :: rest       => parse(rest, o.copy(out = Some(Path.of(v))))
    case dir :: rest                => parse(rest, o.copy(dirs = o.dirs :+ Path.of(dir)))
  }

  /** A take as the property reads it: the film or `none`, `None` while Pending. */
  type Takes = Map[ListingKey, Option[String]]

  final case class Found(stage: String, key: ListingKey, kind: String, witness: String)

  final case class SliceResult(name: String, varying: Int, fixed: Int, mode: String, resolves: Int, seconds: Double,
                               found: Seq[Found], full: Map[ListingKey, (Option[String], Option[String])], lacking: Seq[String] = Nil)

  def replay(dir: Path, o: Options): Seq[SliceResult] = {
    val run      = new IdentityDrift.SliceRun(dir)
    val slice    = run.slice
    val all      = run.listings
    val name     = dir.getFileName.toString
    val inputs   = ResolveInputs.Unpinned
    def readsAt(c: java.time.Clock) = new IdentitySlice.Reads(slice.country, run.replaying, run.replaying.gaps, c, slice.filed, tools.ProdState.none)
    val reads0   = readsAt(new MutableClock(slice.recordedAt))
    val lookups  = IdentityDrift.memo(reads0)
    val whole0   = IdentityResolver.resolve(all, lookups, reads0.normalizer, inputs)
    // the varying sets: the families of the targets (or of every core listing), else every listing at once
    val coreKeys = slice.core.map(_.key).toSet
    val targeted = if (o.targets.isEmpty) coreKeys else all.map(_.key).filter(k => o.targets.exists { case (v, t) => k.venue == v && k.rawTitle == t }).toSet
    val groups: Seq[Seq[Listing]] =
      if (o.vary == "all") Seq(all)
      else all.groupBy(l => whole0.familyOf.getOrElse(l.key, -1)).toSeq.collect {
        case (id, ls) if id >= 0 && ls.sizeIs >= 2 && ls.exists(l => targeted(l.key)) => ls.sortBy(_.sortKey)
      }.sortBy(_.head.sortKey)
    groups.map { members =>
      val started = System.nanoTime()
      val rest    = all.filterNot(l => members.exists(_.key == l.key))
      val n       = members.size
      val memo    = TrieMap.empty[Set[Int], (Takes, Takes)]
      def takes(subset: Set[Int]): (Takes, Takes) = memo.getOrElseUpdate(subset, {
        val listings = subset.toSeq.sorted.map(members) ++ rest
        val model    = IdentityResolver.resolve(listings, lookups, reads0.normalizer, inputs)
        val m: Takes = listings.flatMap(l => model.decisionOf.get(l.key).map(d => l.key -> PopulationMonotonicity.takeOf(d))).toMap
        val f: Takes = if (o.modelOnly) Map.empty else {
          val clock = new MutableClock(slice.recordedAt)
          val reads = readsAt(clock)
          val r     = IdentityDrift.whole(reads, listings, clock, IdentityDrift.WholeLookups(lookups, _ => None), filedDocs = IdentityDrift.Noted)
          val pend  = IdentityDrift.pendingOf(r.decisions)
          IdentityDrift.takesOf(r).map { case (k, t) => k -> Option.when(!pend(k))(t.film) }
        }
        (m, f)
      })
      val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, o.threads))
      def parallel(sets: Seq[Set[Int]]): Unit = {
        val fs = sets.filterNot(memo.contains).map(s => pool.submit(new java.util.concurrent.Callable[Unit] { def call(): Unit = { takes(s); () } }))
        fs.foreach(_.get)
      }
      val keys  = members.map(_.key) ++ rest.map(_.key)
      def label(s: Set[Int]) = s.toSeq.sorted.map(i => members(i).venue).mkString("{", ", ", "}")
      val found = Seq.newBuilder[Found]
      val stages = Seq[(String, ((Takes, Takes)) => Takes)]("model" -> (_._1)) ++ Option.unless(o.modelOnly)("final" -> ((t: (Takes, Takes)) => t._2))
      // the flips over a (partial) lattice of computed subsets, n <= 63: masks
      def latticeCheck(subsets: Seq[Set[Int]]): Unit = {
        val masked = subsets.distinct.map(s => (s, s.foldLeft(0L)((m, i) => m | (1L << i))))
        val byMask = masked.map { case (s, m) => m -> s }.toMap
        for ((stage, pick) <- stages; key <- keys) {
          val own  = members.indexWhere(_.key == key)
          val vals = masked.flatMap { case (s, m) => if (own >= 0 && !s(own)) None else pick(takes(s)).get(key).flatten.map(t => (m, s.size, t)) }.toArray
          val witness = vals.iterator.flatMap { case (m2, n2, b) =>
            val below = vals.filter { case (m1, n1, a) => n1 < n2 && (m1 & ~m2) == 0 && a != b }
            if (below.isEmpty) Iterator.empty
            else {
              val above = vals.filter { case (m3, n3, _) => n3 > n2 && (m2 & ~m3) == 0 }.map(v => v._3 -> v._1).toMap
              below.iterator.flatMap { case (m1, _, a) => above.get(a).map(m3 => s"$a@${label(byMask(m1))} -> $b@${label(byMask(m2))} -> $a@${label(byMask(m3))}") }
            }
          }.nextOption()
          witness.foreach(w => found += Found(stage, key, "arrival", w))
          if (witness.isEmpty) {
            val at = vals.map(v => v._1 -> v._3).toMap
            val ar = vals.iterator.flatMap { case (m, _, a) => (0 until n).iterator.filter(x => x != own && (m & (1L << x)) == 0).flatMap { x =>
              at.get(m | (1L << x)).filter(_ != a).map(b => s"$a@${label(byMask(m))} -> $b@${label(byMask(m | (1L << x)))} (+${members(x).venue}) -> $a")
            } }.nextOption()
            ar.foreach(w => found += Found(stage, key, "add/remove", w))
          }
        }
      }
      val mode =
        try if (n <= o.max) {
          // every subset (the empty one too: the fixed listings alone)
          val subsets = (0 until (1 << n)).map(mask => (0 until n).filter(i => (mask & (1 << i)) != 0).toSet)
          parallel(subsets.sortBy(-_.size))
          latticeCheck(subsets)
          s"lattice 2^$n"
        } else if (n <= 63 && o.levels > 0) {
          // the lattice's bottom and top levels: every subset of at most `levels` listings, and every one missing at most `levels`
          def choose(k: Int): Seq[Set[Int]] = (0 until n).combinations(k).map(_.toSet).toSeq
          val subsets = (0 to math.min(o.levels, n)).flatMap(k => choose(k) ++ choose(k).map(c => (0 until n).toSet -- c)).distinct
          parallel(subsets.sortBy(-_.size))
          latticeCheck(subsets)
          s"lattice levels 0-${o.levels} and ${n - o.levels}-$n (${subsets.size} subsets)"
        } else {
          val rng    = new Random(MurmurHash(members.head.sortKey))
          val chains = (0 until o.orders).map { _ =>
            val order = rng.shuffle((0 until n).toVector)
            val size  = math.max(1, math.ceil(n.toDouble / o.steps).toInt)
            ((size until n by size).map(k => order.take(k).toSet) :+ order.toSet).distinct
          }
          parallel(chains.flatten.distinct)
          for ((stage, pick) <- stages) {
            keys.foreach { key =>
              val own = members.indexWhere(_.key == key)
              chains.iterator.flatMap { chain =>
                val seq = chain.filter(s => own < 0 || s(own)).map(s => s.size -> pick(takes(s)).get(key).flatten)
                Option.when(PopulationMonotonicity.returns(seq.map(_._2)))(seq.collect { case (k, Some(t)) => s"$t@$k" }.foldLeft(Vector.empty[String])((v, x) =>
                  if (v.lastOption.exists(_.takeWhile(_ != '@') == x.takeWhile(_ != '@'))) v else v :+ x).mkString(" -> "))
              }.nextOption().foreach(w => found += Found(stage, key, "arrival", w))
            }
          }
          s"${o.orders} orders x ${o.steps} steps"
        }
        finally pool.shutdown()
      val fullSet = (0 until n).toSet
      val full    = keys.map(k => k -> (takes(fullSet)._1.get(k).flatten, takes(fullSet)._2.get(k).flatten)).toMap
      SliceResult(name, n, rest.size, mode, memo.size, (System.nanoTime() - started) / 1e9, found.result().distinct, full, run.lacking)
    }
  }

  private def MurmurHash(s: String): Long = scala.util.hashing.MurmurHash3.stringHash(s).toLong

  def main(args: Array[String]): Unit = {
    val o   = parse(args.toList)
    val out = Seq.newBuilder[String]
    def say(s: String): Unit = { println(s); out += s }
    o.dirs.foreach { dir =>
      scala.util.Try(replay(dir, o)).fold(e => { say(s"# $dir: FAILED $e"); e.printStackTrace() }, results => results.foreach { r =>
        say(f"# ${r.name}: family of ${r.varying} varying + ${r.fixed} fixed, ${r.mode}, ${r.resolves} subsets resolved in ${r.seconds}%.1fs " +
          f"(${r.seconds / math.max(1, r.resolves)}%.3fs each), ${r.found.size} finding(s), ${r.lacking.size} request(s) the slice lacks")
        r.lacking.take(6).foreach(l => say(s"  lacks\t$l"))
        r.found.sortBy(f => (f.key.rawTitle, f.key.venue, f.stage)).foreach(f => say(s"  ${f.stage}\t${f.kind}\t${f.key.venue} | ${f.key.rawTitle}\t${f.witness}"))
        r.full.toSeq.sortBy(e => (e._1.rawTitle, e._1.venue)).foreach { case (k, (m, f)) => say(s"  full\t${k.venue} | ${k.rawTitle}\tmodel ${m.getOrElse("pending")}\tfinal ${f.getOrElse("pending")}") }
      })
    }
    o.out.foreach(p => Files.write(p, out.result().asJava))

  }
}
