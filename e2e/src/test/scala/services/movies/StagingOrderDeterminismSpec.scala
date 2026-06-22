package services.movies

import controllers.{FilmSchedule, MovieControllerService}
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FixtureTestWiring, SameThreadExecutionBudget}

import java.time.LocalDateTime
import scala.collection.mutable

/**
 * Determinism guard for the STAGING path — the one `ScrapeOrderDeterminismSpec`
 * deliberately does NOT exercise. That spec harvests with `staging = None` (the
 * direct cache path) and settles each replay via `converge()`; both absorb every
 * cinema's reported director hint into one row BEFORE resolving, so an
 * arrival-order resolution race can never surface there.
 *
 * Production is different: staging ingest is always-on, every newcomer incubates
 * in `pending_movies`, and the `StagingReaper` fires every couple of minutes
 * while cinemas are still trickling in — so a film's hint group can resolve
 * against a PARTIAL set of cinemas, before the cinema carrying the
 * canonical-order director spelling has landed. `bootStartupInterleaved` recreates
 * exactly that: cinemas arrive in a shuffled order and `advanceStagingOnce` runs
 * the reaper between arrivals. The perturbation is pure ORDERING (the seeded
 * arrival shuffle), not timing — the enrichment cascade runs on a deterministic
 * same-thread executor, so each seed is exactly reproducible.
 *
 * This is the disorder behind the CI-only `FilmScheduleEndToEndSpec` flake on
 * "Zawieście czerwone latarnie": one cinema reports the director as "Yimou Zhang"
 * (Western order) vs TMDB's "Zhang Yimou", and before
 * `MovieService.directorNameMatches` was made token-order-insensitive that row
 * resolved only when a sibling's canonical-order spelling had already merged in —
 * an arrival-order dependence. The persisted corpus + every rendered city row
 * must now be byte-identical across arrival orders.
 */
@CorpusReplay // CI runs this heavy spec on its own parallel e2e shard — see CorpusReplay.java
class StagingOrderDeterminismSpec extends AnyFlatSpec with Matchers {

  private val Fixture        = "08-06-2026"
  private val Now            = LocalDateTime.of(2026, 6, 8, 0, 0)
  private val Iterations     = 3

  /** Boot the whole corpus through the real staging path with a shuffled,
   *  reaper-interleaved cinema arrival (seeded), the cascade on a same-thread
   *  executor (ordering, not timing),
   *  THEN settle to the deterministic steady state with `converge` (re-resolve +
   *  re-fold + collapse, the same eventual-consistency sweep prod reaches via the
   *  daily retry), and capture the persisted records + the rendered rows across
   *  every city. The assertion is about the SETTLED corpus — prod doesn't promise
   *  the unsettled inline transient is stable, but it does promise this. */
  private def replay(seed: Long): (Seq[StoredMovieRecord], Seq[FilmSchedule]) = {
    val rnd = new scala.util.Random(seed)
    // Perturb by ORDERING, not timing: the seeded cinema-arrival shuffle
    // (`bootStartupInterleaved`) decides the order rows enter resolution, and the
    // whole background cascade runs on a deterministic SAME-THREAD budget — so a seed
    // is perfectly reproducible with no `Thread.sleep` jitter and no thread-pool race.
    // `converge(Some(rnd))` then additionally shuffles the re-enrich/settle sweep.
    val w = new FixtureTestWiring(Fixture) {
      override lazy val backgroundBudget: tools.ExecutionBudget = new SameThreadExecutionBudget
    }
    w.bootStartupInterleaved(rnd)
    w.converge(Some(rnd))
    w.readModelProjector.reconcile()
    w.webReadModel.reload()
    val record = w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    val service = new MovieControllerService(w.webReadModel)
    val rows = City.all.sortBy(_.slug).flatMap(c => service.toSchedules(c, Now))
    (record, rows)
  }

  "the staging path under shuffled, reaper-interleaved cinema arrival" should
    "persist an identical record set + rendered rows regardless of arrival order" in {
    val started = System.nanoTime()
    // The Iterations replays are independent — run them concurrently (each in its
    // own isolated wiring) instead of serially. See ParallelReplays.
    val replays = ParallelReplays((0 until Iterations).map(700000L + _))(replay)
    val (record0, rows0) = replays.head
    val divergences = mutable.ListBuffer.empty[String]
    (1 until Iterations).foreach { i =>
      val (recordI, rowsI) = replays(i)
      if (recordI != record0) {
        val key = (r: StoredMovieRecord) => (r.title, r.year, r.record.tmdbId)
        val a = record0.map(key).toSet
        val b = recordI.map(key).toSet
        // For records present under the SAME (title,year,tmdbId) key in both boots
        // but differing in a non-key field, surface the fields that gate read-model
        // visibility — resolvedYear (the read-model id), readyToProject (whether it
        // projects at all), and the cinema set — so a future flake names its cause.
        val by0 = record0.groupBy(key); val byI = recordI.groupBy(key)
        val fieldDiffs = (a intersect b).toSeq.flatMap { k =>
          val (r0, rI) = (by0(k).head.record, byI(k).head.record)
          if (r0 == rI) None
          else {
            val c0 = r0.cinemaData.keySet.map(_.displayName); val cI = rI.cinemaData.keySet.map(_.displayName)
            Some(s"    $k resolvedYear ${r0.resolvedYear}/${rI.resolvedYear} " +
              s"readyToProject ${r0.readyToProject}/${rI.readyToProject} " +
              s"cinemas only-in-0=${c0 -- cI} only-in-$i=${cI -- c0}")
          }
        }
        divergences += s"RECORD iter $i: only-in-0=${(a -- b).take(8)} only-in-$i=${(b -- a).take(8)}" +
          (if (fieldDiffs.nonEmpty) s"\n  same-key field diffs (${fieldDiffs.size}):\n${fieldDiffs.take(12).mkString("\n")}" else "")
      }
      if (rowsI != rows0) {
        // Set-difference by a stable content identity (positional zipAll lies once
        // one row near the top is missing — every later index then "differs").
        val rkey = (f: FilmSchedule) =>
          (f.movie.title, f.movie.releaseYear, f.cinemaFilmUrls.map(_._1.displayName).sorted.mkString(","))
        val s0 = rows0.map(rkey).toSet; val sI = rowsI.map(rkey).toSet
        divergences += s"ROW iter $i differs (${rows0.size} vs ${rowsI.size}):\n" +
          s"  only-in-0 (${(s0 -- sI).size}): ${(s0 -- sI).take(10).mkString("; ")}\n" +
          s"  only-in-$i (${(sI -- s0).size}): ${(sI -- s0).take(10).mkString("; ")}"
      }
    }
    val elapsedMs = (System.nanoTime() - started) / 1000000
    info(s"$Iterations interleaved staging boots (${record0.size} films) in ${elapsedMs}ms")
    withClue(s"${divergences.size} divergence(s):\n${divergences.take(40).mkString("\n")}\n") {
      divergences.toList shouldBe empty
    }
  }

  // ── Targeted fast reproduction ──────────────────────────────────────────────
  // Boot ONLY the cinemas that report a film of interest (their REAL scrapers,
  // real deferred-detail) — seconds instead of the 14-min full corpus —
  // to pin a single film's arrival-order resolution race.
  private val HindRajabCinemas: Set[Cinema] =
    Set(CharlieMonroe, KinoMuranow, KinoAmondo, SluzewskiDomKultury)

  private def replaySubset(cinemas: Set[Cinema], seed: Long): Seq[StoredMovieRecord] = {
    val rnd = new scala.util.Random(seed)
    val w = new FixtureTestWiring(Fixture) {
      override lazy val backgroundBudget: tools.ExecutionBudget = new SameThreadExecutionBudget
    }
    w.bootStartupInterleaved(rnd, cinemas.contains)
    w.converge(Some(rnd))
    w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
  }

  private val CaravaggioCinemas: Set[Cinema] =
    Set(KinoNoweHoryzonty, KinoMuranow, KinoZamekSzczecin, KinoAmok)

  "Caravaggio. Arcydzieła niepokornego geniusza, booted from just its cinemas" should
    "settle to an identical readyToProject state regardless of arrival order" in {
    def caravaggio(rs: Seq[StoredMovieRecord]) =
      rs.filter(r => r.title.toLowerCase.contains("arcydzieła niepokornego"))
    def shape(rs: Seq[StoredMovieRecord]) = caravaggio(rs).map { x =>
      (x.title, x.year, x.record.tmdbId, x.record.tmdbNoMatch, x.record.detailPending,
        x.record.readyToProject, x.record.cinemaData.keySet.map(_.displayName))
    }.mkString("\n  ")
    val ref = replaySubset(CaravaggioCinemas, 700000L)
    (1 to 5).foreach { i =>
      val r = replaySubset(CaravaggioCinemas, 700000L + i)
      withClue(s"seed ${700000L + i} vs 700000:\n  ref=${shape(ref)}\n  r  =${shape(r)}\n")(
        caravaggio(r).map(_.record.readyToProject) shouldBe caravaggio(ref).map(_.record.readyToProject))
    }
  }

  "Głos Hind Rajab, booted from just its cinemas" should
    "settle to one identical record regardless of arrival order" in {
    def hind(rs: Seq[StoredMovieRecord]) = rs.filter(_.title.toLowerCase.contains("hind rajab"))
    def shape(rs: Seq[StoredMovieRecord]) =
      hind(rs).map(x => (x.title, x.year, x.record.tmdbId, x.record.cinemaData.keySet)).mkString("\n  ")
    val ref = replaySubset(HindRajabCinemas, 700000L)
    (1 to 5).foreach { i =>
      val r = replaySubset(HindRajabCinemas, 700000L + i)
      withClue(s"seed ${700000L + i} vs 700000:\n  ref=${shape(ref)}\n  r  =${shape(r)}\n")(hind(r) shouldBe hind(ref))
    }
  }
}
