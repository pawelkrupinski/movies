package services.identity

import models.{Cinema, CinemaMovie, Country, Movie, Showtime, UsCinema}
import services.movies.{CinemaSlotBuilder, ListingKey, ScreeningTokens, SingleCountryNormalizer, StoredMovieRecord, StringPool}

import java.lang.management.ManagementFactory
import java.time.{Instant, LocalDateTime}

/** A steady identity-projection draft at worker-us's size (~100k listings, ~2.1k films, a skewed
 *  venue spread), timed and allocation-counted per tick: run with
 *  `sbt "common/Test/runMain services.identity.IdentityDraftBench"`. Not a spec — a measuring tool. */
object IdentityDraftBench {
  // `sbt "common/Test/runMain services.identity.IdentityDraftBench 8"` projects scopes after the first tick, as production
  // does; a trailing `whole` projects the whole corpus every tick.
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
    var rows: Map[(Int, Cinema), CinemaMovie] = placement.map { case (f, c) => (f, c) -> cm(c, f, 0) }.toMap
    def listings = rows.toSeq.map { case ((_, c), row) => ProjectedListing.of(Listing.of(c, row, normalizer), row) }
    val rowsOf: Set[Cinema] => Map[Cinema, Seq[CinemaMovie]] = vs => rows.toSeq.collect { case ((_, c), row) if vs(c) => c -> row }.groupMap(_._1)(_._2)
    val first  = listings
    val byFilm = first.groupBy(l => l.listing.rawTitle)
    val decisions = byFilm.toSeq.zipWithIndex.map { case ((_, ls), i) =>
      ResolverDecision(ls.map(_.listing.key).sorted, Some(i + 1), 0.9, ResolverDecision.Basis.OwnMatch, Nil)() }
      .sortBy(_.members.head)(using ListingKey.ordering)
    val resolution = Resolution(decisions, decisions.size, decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap,
      Nil, Nil, 0, 0, 0, 0, 0, Map.empty)
    println(s"${first.size} listings, ${decisions.size} films, ${(0 until filmCount).map(f => normalizer.sanitize(s"Film Number $f")).distinct.size} distinct slot titles")

    var memo     = new VenueSlotMemo(0L)
    var stored   = Map.empty[services.movies.FilmId, StoredMovieRecord]
    var counters = FilmIdCounters.empty
    var live     = new LiveProjectionIndex(normalizer)
    var shapes   = FilmShapes()
    var lastOk   = false
    val scoped   = !args.contains("whole")
    val threads  = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    def timed[A](body: => A): (A, Double, Double) = {
      val (b, t) = (threads.getCurrentThreadAllocatedBytes, System.nanoTime())
      val a = body
      (a, (System.nanoTime() - t) / 1e9, (threads.getCurrentThreadAllocatedBytes - b) / 1e6)
    }
    // As the intake: a venue's listing is the same object until the venue is read again.
    var venueSeqs = Map.empty[String, Seq[ProjectedListing]]
    def byVenue(all: Seq[ProjectedListing]): Seq[(String, Seq[ProjectedListing])] = {
      all.groupBy(_.listing.venue).foreach { case (venue, ls) =>
        if (!venueSeqs.get(venue).exists(_ == ls)) venueSeqs += venue -> ls
      }
      venueSeqs.toSeq
    }
    (1 to ticks).foreach { tick =>
      if (tick > 2) rng.shuffle(rows.keys.toSeq).take(200).foreach(k => rows += k -> cm(k._2, k._1, tick))
      val now = listings
      val storedSeq = stored.values.toSeq
      val held = (_: services.movies.ListingKey) => true
      val ((index, changes), indexS, indexMB) = timed {
        val changes = live.update(byVenue(now), held, resolution.decisions, stored.values.toSeq)
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
      stored   = stored -- plan.retired ++ changed.map(f =>
        f.id -> StoredMovieRecord(f.title, f.year, services.movies.ShowtimesDigest.stripForCache(f.record), f.id, Some(f.key)))
      counters = counters.appended(plan.counterAdditions).toOption.get
      live.written(changed, plan.retired)
      shapes.commit(whole = scope.whole)
      lastOk   = true
    }
    // Retained heap: what each piece of kept state holds beyond the listings, rows and stored records the
    // worker keeps anyway (those stay reachable below).
    def used(): Long = { (1 to 4).foreach { _ => System.gc(); Thread.sleep(200) }; val r = Runtime.getRuntime; r.totalMemory - r.freeMemory }
    val all = used()
    shapes = null; val noShapes = used()
    live = null; val noLive = used()
    memo = null; val noMemo = used()
    println(f"retained: FilmShapes ${(all - noShapes) / 1e6}%.0fMB, LiveProjectionIndex ${(noShapes - noLive) / 1e6}%.0fMB, " +
      f"VenueSlotMemo ${(noLive - noMemo) / 1e6}%.0fMB; still live ${noMemo / 1e6}%.0fMB (${stored.size} stored, ${venueSeqs.size} venues, ${rows.size} rows)")
  }
}
