package services.identity

import models.{Cinema, CinemaMovie, Country, Movie, Showtime, UsCinema}
import services.movies.{CinemaSlotBuilder, ListingKey, ScreeningTokens, SingleCountryNormalizer, StoredMovieRecord, StringPool}

import java.time.{Instant, LocalDateTime}

/** A steady identity-projection draft at worker-us's size (~100k listings, ~2.1k films, a skewed
 *  venue spread), timed and allocation-counted per tick: run with
 *  `sbt "common/Test/runMain services.identity.IdentityDraftBench"`. Not a spec — a measuring tool. */
object IdentityDraftBench {
  // `sbt "common/Test/runMain services.identity.IdentityDraftBench 8 300 <dir> [adopt]"` projects 8 ticks, re-reading 300 venues
  // a tick, scopes after the first as production does, and writes class histograms of the state kept between ticks to
  // <dir> (optional); a trailing `whole` projects the whole corpus every tick. `shifts=N` moves N rows a tick (200);
  // `churn=N` runs N young collections of garbage after each tick, as the ~2 minutes between two of worker-us's light
  // projections do, and prints what each tick left promoted to the old generation — run it with production's collector
  // and heap (`-XX:+UseSerialGC -Xms1152m -Xmx1152m`).
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val slots      = new CinemaSlotBuilder(Country.Poland.language, new StringPool)
  private val tokens     = ScreeningTokens.of(Country.Poland)
  private val start      = LocalDateTime.of(2026, 10, 4, 12, 0)
  private val at         = Instant.parse("2026-10-04T10:00:00Z")

  def main(args: Array[String]): Unit = {
    val venueCount = 2500
    val filmCount  = 2150
    val ticks      = args.headOption.map(_.toInt).getOrElse(6)
    val venues     = (0 until venueCount).map(v => new UsCinema(s"Venue $v Theatre", s"Venue $v"): Cinema)
    val rng        = new scala.util.Random(7)
    def cm(cinema: Cinema, film: Int, shift: Int): CinemaMovie =
      CinemaMovie(Movie(s"Film Number $film", releaseYear = Some(2000 + film % 26)), cinema, None, Some(s"https://v/${film}"), None, Nil, Nil,
        (0 until 14).map(h => Showtime(start.plusHours((h * 11 + shift).toLong), None)))
    val placement: Seq[(Int, Cinema)] = (0 until filmCount).flatMap { f =>
      val n = math.min(venueCount, math.max(1, 13000 / (f + 1)))
      rng.shuffle(venues).take(n).map(f -> _)
    }
    // As production: the archive's rows (each a scrape's own objects), the identity model's listings (read from its store,
    // so its own objects and keys), and the intake's per venue, projected anew from a scrape's rows each time a venue is
    // read again — with the model's object for a listing it holds the same when `adopt` is given.
    val shift = scala.collection.mutable.HashMap.empty[(Int, Cinema), Int]
    var rows: Map[(Int, Cinema), CinemaMovie] = placement.map { case (f, c) => shift((f, c)) = 0; (f, c) -> cm(c, f, 0) }.toMap
    val adopt   = args.contains("adopt")
    var modelled = rows.map { case ((f, c), _) => val l = Listing.of(c, cm(c, f, shift((f, c))), normalizer); l.key -> l }
    def scraped(c: Cinema): Seq[ProjectedListing] = rows.toSeq.collect { case ((f, cc), _) if cc eq c =>
      val row = cm(c, f, shift((f, c)))
      rows += (f, c) -> row
      val fresh = Listing.of(c, row, normalizer)
      ProjectedListing.of(if (adopt) modelled.get(fresh.key).filter(_ == fresh).getOrElse(fresh) else fresh, row)
    }
    val rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]] = vs => rows.toSeq.collect { case ((_, c), row) if vs(c) => c -> row }.groupMap(_._1)(_._2)
    val byFilm = modelled.values.toSeq.groupBy(_.rawTitle)
    // As the identity model holds them after a restart: each film's family decoded from its store (keys and node texts
    // of their own), then — unless `decoded` is given, as before 2026-10-05 — named by the held listings' key objects
    // and the corpus's node texts (`RegionFamily.sharing`, what `IncrementalResolver.remember` does).
    val corpusText = modelled.map { case (key, listing) => key -> listing.rawTitle }
    var families = byFilm.toSeq.sortBy(_._1).zipWithIndex.map { case ((title, ls), i) =>
      val keys     = ls.map(_.key)
      val decision = ResolverDecision(keys.sorted, Some(i + 1), 0.9, ResolverDecision.Basis.OwnMatch, Nil)()
      val family   = IdentityResolver.RegionFamily(keys.toSet, Seq(decision), Set(s"t:$title"), Set.empty, Set(i + 1),
        CorpusContext.Reads(Set(title), Set.empty, Set.empty, Set.empty, Set(i + 1), Set.empty), keys.map(k => k -> new String(title)).toMap)
      val stored   = MongoIdentityModelStore.decode(new org.bson.RawBsonDocument(  // as read off the wire: every string anew
        MongoIdentityModelStore.encode(StoredFamily(StoredFamily.idOf(keys), family, 0L)), new org.bson.codecs.BsonDocumentCodec)).family
      if (args.contains("decoded")) stored
      else stored.sharing(k => modelled.get(k).fold(k)(_.key), (k, text) => corpusText.get(k).filter(_ == text).getOrElse(text))
    }
    var resolution = { val decisions = families.flatMap(_.decisions).sortBy(_.members.head)(using ListingKey.ordering)
      Resolution(decisions, decisions.size, decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap,
        Nil, Nil, 0, 0, 0, 0, 0, Map.empty) }
    println(s"${rows.size} listings, ${resolution.decisions.size} films, ${(0 until filmCount).map(f => normalizer.sanitize(s"Film Number $f")).distinct.size} distinct slot titles")

    var memo     = new VenueSlotMemo(0L)
    var stored   = Map.empty[services.movies.FilmId, StoredMovieRecord]
    var counters = FilmIdCounters.empty
    var live     = new LiveProjectionIndex(normalizer)
    var shapes   = FilmShapes()
    var lastOk   = false
    val scoped   = !args.contains("whole")
    def timed[A](body: => A): (A, Double, Double) = {
      val (stopwatch, allocated) = tools.ThreadAllocation.of(tools.Stopwatch.timed(body))
      (stopwatch.value, stopwatch.seconds, allocated / 1e6)
    }
    // As the intake: a venue's listing is the same object until the venue is read again — and, as production scrapes
    // every venue on a cadence, `rereads` venues a tick are read again into new objects whether or not they moved.
    val rereads   = args.lift(1).flatMap(_.toIntOption).getOrElse(300)
    var venueSeqs = Map.empty[String, Seq[ProjectedListing]]
    var moved     = venues.toSet
    def byVenue(): Seq[(String, Seq[ProjectedListing])] = {
      val reread = rng.shuffle(venues).take(rereads).toSet
      venues.foreach(c => if (moved(c) || reread(c)) { val ls = scraped(c); if (ls.nonEmpty) venueSeqs += c.displayName -> ls })
      moved = Set.empty
      venueSeqs.toSeq
    }
    val shifts   = args.collectFirst { case s"shifts=$n" => n.toInt }.getOrElse(200)
    val churnGcs = args.collectFirst { case s"churn=$n" => n.toInt }
    import scala.jdk.CollectionConverters.*
    val beans   = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans.asScala.filter(_.getName == "Copy")
    def youngCollections(): Long = beans.map(_.getCollectionCount).sum
    val tenured = java.lang.management.ManagementFactory.getMemoryPoolMXBeans.asScala.find(_.getName == "Tenured Gen").orNull
    var tenuredBefore = Option(tenured).fold(0L)(_.getUsage.getUsed)
    var sink: Array[Byte] = null
    // The heap, unreachable objects included, before any collection: what the ticks since the last full collection
    // promoted and left to die in the old generation is the difference of two of these, by class.
    def heapNow(name: String): Unit = args.lift(2).filterNot(a => a == "whole" || a == "adopt" || a == "decoded" || a == "-" || a.contains("=")).foreach { dir =>
      new ProcessBuilder("jcmd", ProcessHandle.current.pid.toString, "GC.class_histogram", "-all")
        .redirectOutput(new java.io.File(s"$dir/$name.txt")).start().waitFor(); ()
    }
    (1 to ticks).foreach { tick =>
      if (tick > 2) rng.shuffle(rows.keys.toSeq).take(shifts).foreach { k =>
        shift(k) = tick; moved += k._2
        val l = Listing.of(k._2, cm(k._2, k._1, tick), normalizer)
        modelled += l.key -> l
      }
      val held = modelled.keys
      val ((index, changes), indexS, indexMB) = timed {
        val changes = live.update(byVenue(), held, resolution.decisions, stored.values.toSeq)
        (live.index(counters), changes)
      }
      val (scope, scopeS, scopeMB) = timed(if (!lastOk || !scoped) ProjectionScope.Whole else
        ProjectionScope.close(index, changes ++ ProjectionScope.standing(index)))
      val (undetailed, draftS, draftMB) = timed(IdentityProjectionPlan.draftOf(index, scope, normalizer, slots, tokens, at, rowsOf, memo,
        shapes, changes.listings))
      // As the projection's details fetch: a film new to its record gets TMDB's details, once.
      val draft = undetailed.copy(drafts = undetailed.drafts.map(d => d.needsDetails.fold(d)(film =>
        d.copy(record = d.record.copy(data = d.record.data + (models.Tmdb -> models.SourceData(title = Some(s"Film $film"))))))))
      val counts = memo.endTick(retainUnseen = !scope.whole)
      val ((plan, changed), restS, restMB) = timed {
        val plan = IdentityProjectionPlan.finish(draft, normalizer, stored.contains)
        // As IdentityProjection.write: only the films that differ from the stored (lean) records are completed.
        val changed = draft.complete(plan.films.filter(f => stored.get(f.id).forall(s =>
          s.key(normalizer) != f.key || !services.movies.ShowtimesDigest.leanEqual(f.record, s.record))), id => stored.get(id).map(_.record))
        (plan, changed)
      }
      val size = if (scope.whole) "whole" else s"${scope.films.size} films/${scope.listings.size} listings"
      println(f"tick $tick ($size): index ${indexS}%.2fs ${indexMB}%.0fMB, scope ${scopeS}%.2fs ${scopeMB}%.0fMB, " +
        f"draft ${draftS}%.2fs ${draftMB}%.0fMB, finish+compare ${restS}%.2fs ${restMB}%.0fMB; total ${indexS + scopeS + draftS + restS}%.2fs " +
        f"${indexMB + scopeMB + draftMB + restMB}%.0fMB; changed ${changed.size}; slots reused ${counts._1}, built ${counts._2} ${memo.lastMisses()}")
      // As the writes: only the changed films are stored again, every other keeps its record as it was.
      // As the cache keeps them: a slot it holds already stays the object held, every other is stripped (`forCacheOver`).
      stored   = stored -- plan.retired ++ changed.map { f =>
        val held = stored.get(f.id).fold(Map.empty[models.Source, models.SourceData])(_.record.data)
        val data = f.record.data.foldLeft(f.record.data) { case (data, (source, slot)) =>
          if (held.get(source).exists(_ eq slot)) data else data.updated(source, services.movies.ShowtimesDigest.stripSlot(slot)) }
        f.id -> StoredMovieRecord(f.title, f.year, f.record.copy(data = data), f.id, Some(f.key))
      }
      counters = counters.appended(plan.counterAdditions).toOption.get
      val (_, writtenS, writtenMB) = timed(live.written(changed, plan.retired))
      println(f"  written ${writtenS}%.3fs ${writtenMB}%.0fMB")
      shapes.commit(whole = scope.whole)
      lastOk   = true
      churnGcs.foreach { gcs =>
        val until = youngCollections() + gcs
        while (youngCollections() < until) sink = new Array[Byte](4096)
        val now = tenured.getUsage.getUsed
        println(f"  promoted ${(now - tenuredBefore) / 1e6}%.1f MB")
        tenuredBefore = now
        if (tick == 3) heapNow("afterTick3")
      }
    }
    if (churnGcs.isDefined) heapNow("afterTicks")
    // Retained heap: what each piece of kept state holds beyond the listings, rows and stored records the
    // worker keeps anyway (those stay reachable below).
    def used(): Long = { (1 to 4).foreach { _ => System.gc(); Thread.sleep(200) }; val r = Runtime.getRuntime; r.totalMemory - r.freeMemory }
    val all = used()
    def histogram(name: String): Unit = args.lift(2).filterNot(a => a == "whole" || a == "adopt" || a == "decoded" || a == "-" || a.contains("=")).foreach { dir =>
      val out = new ProcessBuilder("jcmd", ProcessHandle.current.pid.toString, "GC.class_histogram").redirectOutput(new java.io.File(s"$dir/$name.txt")).start()
      out.waitFor(); ()
    }
    histogram("all")
    shapes = null; val noShapes = used()
    histogram("noShapes")
    live = null; val noLive = used()
    histogram("noLive")
    memo = null; val noMemo = used()
    histogram("noMemo")
    families = null; resolution = null; val noModel = used()
    histogram("noModel")
    println(f"retained: FilmShapes ${(all - noShapes) / 1e6}%.0fMB, LiveProjectionIndex ${(noShapes - noLive) / 1e6}%.0fMB, " +
      f"VenueSlotMemo ${(noLive - noMemo) / 1e6}%.0fMB, model families+resolution ${(noMemo - noModel) / 1e6}%.0fMB; " +
      f"total ${all / 1e6}%.0fMB, still live ${noModel / 1e6}%.0fMB (${stored.size} stored, ${venueSeqs.size} venues, ${rows.size} rows)")
  }
}
