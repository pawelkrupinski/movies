package scripts

import services.identity.{IdentityResolver, Listing, ListingRetention, PopulationMonotonicity, ResolveInputs}

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.jdk.CollectionConverters._

/**
 * BENCHMARK (not for landing): whole-resolve cost against kept listings. The newest recording's corpus, plus the departed
 * listings of the last W days (synthetic history, as scripts.RetentionWindows) kept by [[ListingRetention.retained]],
 * each kept listing replicated R times (`--replicate`: copy i of a kept listing is the same listing under a distinct key,
 * at a venue rotated through the country's venues — 90-day volume from 24 days of history).
 *
 *   worker/Test/runMain scripts.KeptListingsBench <cc> <fixtures> --older <corpus>@<ISO> ... --windows 0,9,18
 *     --replicate 1,3 --repeat 3 --jfr 18x3 --out DIR
 *
 * Lookups are memoised and warmed by one resolve of the largest set, so each timed resolve is the resolver's own CPU.
 * Prints per (window, replicas, repeat): kept, wall s, process CPU s, allocated GB, live heap MB after the resolve, and
 * whether every present listing's take equals the 0-kept one at that window. With --jfr W x R one resolve is profiled and
 * its hottest frames (self and inclusive, services.identity) printed and the .jfr written to DIR.
 */
object KeptListingsBench {
  def main(args: Array[String]): Unit = {
    val country = models.Country.byCode(args(0)).getOrElse(sys.error(s"no country ${args(0)}"))
    val rest    = args.drop(2).toList
    def values(flag: String): List[String] = rest.dropWhile(_ != flag).drop(1).takeWhile(!_.startsWith("--"))
    val older   = values("--older").map { spec =>
      val (path, at) = spec.splitAt(spec.lastIndexOf('@'))
      Path.of(path) -> Instant.parse(at.drop(1))
    }
    val windows   = values("--windows").flatMap(_.split(',')).map(_.toInt)
    val replicas  = values("--replicate").flatMap(_.split(',')).map(_.toInt) match { case Nil => List(1); case r => r }
    val repeat    = values("--repeat").headOption.fold(3)(_.toInt)
    val jfr       = values("--jfr").flatMap(_.split(',')).map { s => val Array(w, r) = s.split('x'); (w.toInt, r.toInt) }.toSet
    val out       = Path.of(values("--out").headOption.getOrElse(sys.error("--out DIR")))
    Files.createDirectories(out)

    val recording = IdentitySlice.Recording(Path.of(args(1)), country)
    val filed     = tools.FiledAnswers.of(country)
    val opened    = IdentitySlice.openRecording(recording, filed)
    val now       = recording.recordedAt
    val reads     = IdentitySlice.readsOf(opened, opened.fetch, opened.gaps, now, None, tools.ProdState.none)
    val lookups   = new tools.MemoLookups(reads.lookups)
    val present   = IdentitySlice.listingsOf(recording, opened.rows)
    val billed    = present.map(_.key).toSet
    val venues    = present.map(_.cinema).distinct.sortBy(_.displayName)
    val departed: Map[services.movies.ListingKey, (Listing, Instant)] = older.sortBy(_._2).foldLeft(Map.empty[services.movies.ListingKey, (Listing, Instant)]) {
      case (acc, (corpus, at)) =>
        val listings = IdentitySlice.listingsOf(recording, tools.CorpusFixture.readFrom(corpus))
        acc ++ listings.filterNot(l => billed(l.key)).map(l => l.key -> (l, at))
    }
    def keptAt(days: Int): Seq[Listing] = {
      val inWindow = departed.values.filter { case (_, at) => !at.isBefore(now.minus(java.time.Duration.ofDays(days.toLong))) }.map(_._1).toSeq
      ListingRetention.retained(inWindow, present, reads.normalizer)
    }
    def replicated(kept: Seq[Listing], r: Int): Seq[Listing] = kept ++ (1 until r).flatMap { i =>
      kept.zipWithIndex.map { case (l, n) =>
        val cinema = venues((n * 7 + i * 13) % venues.size)
        val key    = services.movies.ListingKey.Native(cinema.displayName, s"kept-replica-$i-$n", l.rawTitle)
        l.copy(key = key, cinema = cinema)
      }
    }
    def resolve(kept: Seq[Listing]) = IdentityResolver.resolve(present ++ kept, lookups, reads.normalizer, ResolveInputs.Unpinned)
    def takes(resolution: services.identity.Resolution): Map[services.movies.ListingKey, String] =
      present.flatMap(l => resolution.decisionOf.get(l.key).map(d => l.key -> PopulationMonotonicity.takeOf(d).getOrElse(d.film.fold("none")(id => s"tmdb:$id") + "?"))).toMap

    val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val os      = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]
    def allocated: Long = threads.getAllThreadIds.map(id => threads.getThreadAllocatedBytes(id)).filter(_ > 0).sum
    def liveMb: Double = { System.gc(); System.gc(); val rt = Runtime.getRuntime; (rt.totalMemory - rt.freeMemory) / 1e6 }

    // warm: every lookup the largest set asks memoised, the JIT warm
    val largest = replicated(keptAt(windows.max), replicas.max)
    println(s"[KeptListingsBench] ${country.code}: present ${present.size}, departed ${departed.size}, largest kept ${largest.size}")
    val cold = System.nanoTime(); resolve(largest); println(f"[KeptListingsBench] warm-up resolve ${(System.nanoTime() - cold) / 1e9}%.1fs")
    resolve(Nil)

    val baseline = takes(resolve(Nil))
    val summary  = new StringBuilder("window\treplicas\trepeat\tkept\twallSeconds\tcpuSeconds\tallocatedGb\tliveHeapMb\tpresentTakesMoved\n")
    for (days <- windows; r <- replicas if days > 0 || r == 1) {
      val kept = replicated(keptAt(days), r)
      (1 to repeat).foreach { n =>
        val profiling = jfr.contains((days, r)) && n == repeat
        val recordingJfr = Option.when(profiling) {
          val rec = new jdk.jfr.Recording(jdk.jfr.Configuration.getConfiguration("profile")); rec.start(); rec
        }
        val baseLive = liveMb
        val a0 = allocated; val c0 = os.getProcessCpuTime; val t0 = System.nanoTime()
        val resolution = resolve(kept)
        val wall = (System.nanoTime() - t0) / 1e9; val cpu = (os.getProcessCpuTime - c0) / 1e9; val gb = (allocated - a0) / 1e9
        val live  = liveMb - baseLive
        val moved = takes(resolution).count { case (k, t) => !baseline.get(k).contains(t) }
        recordingJfr.foreach { rec =>
          rec.stop(); val file = out.resolve(s"resolve-${country.code}-${days}d-x$r.jfr"); rec.dump(file); rec.close()
          hottest(file)
        }
        summary.append(f"$days\t$r\t$n\t${kept.size}\t$wall%.1f\t$cpu%.1f\t$gb%.2f\t$live%.0f\t$moved\n")
        println(f"[KeptListingsBench] ${country.code} ${days}d x$r #$n: kept ${kept.size}, wall $wall%.1fs, cpu $cpu%.1fs, alloc $gb%.2fGB, live +$live%.0fMB, present takes moved vs 0-kept $moved")
      }
    }
    Files.write(out.resolve(s"bench-${country.code}.tsv"), summary.toString.getBytes(StandardCharsets.UTF_8))
    print(summary)
  }

  /** The profile's hottest frames: self (top frame) and inclusive (any frame of the stack, counted once a sample), each
   *  as a share of the execution samples. */
  private def hottest(file: Path): Unit = {
    val self      = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val inclusive = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    var samples   = 0
    val events    = jdk.jfr.consumer.RecordingFile.readAllEvents(file).asScala.filter(_.getEventType.getName == "jdk.ExecutionSample")
    events.foreach { e =>
      val frames = Option(e.getStackTrace).map(_.getFrames.asScala.toSeq).getOrElse(Nil)
        .map(f => s"${f.getMethod.getType.getName}.${f.getMethod.getName}")
      if (frames.nonEmpty) {
        samples += 1
        self(frames.head) += 1
        frames.distinct.foreach(inclusive(_) += 1)
      }
    }
    def show(label: String, counts: collection.Map[String, Int], n: Int, keep: String => Boolean) = {
      println(s"[KeptListingsBench] JFR $label ($samples samples, ${file.getFileName}):")
      counts.toSeq.filter(c => keep(c._1)).sortBy(-_._2).take(n).foreach { case (m, c) => println(f"  ${100.0 * c / samples}%5.1f%% $c%6d $m") }
    }
    show("self", self, 40, _ => true)
    show("inclusive services.identity", inclusive, 80, m => m.startsWith("services.") || m.startsWith("scripts."))
  }
}
