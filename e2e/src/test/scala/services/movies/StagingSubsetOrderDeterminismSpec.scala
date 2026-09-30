package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FixtureTestWiring, SameThreadExecutionBudget}

/**
 * Targeted fast reproductions of the staging-path arrival-order races that
 * [[StagingOrderDeterminismSpec]] guards over the whole corpus: each test boots
 * ONLY the cinemas reporting one film of interest, through the same shuffled,
 * reaper-interleaved staging arrival, across six seeds.
 *
 * Kept out of the whole-corpus spec so these seconds-long boots run beside its
 * minutes-long one instead of after it. Untagged, so CI runs it in the `rest`
 * e2e shard (see CorpusReplay.java).
 */
class StagingSubsetOrderDeterminismSpec extends AnyFlatSpec with Matchers {

  private val Fixture = "08-06-2026"

  // Boot ONLY the cinemas that report a film of interest (their REAL scrapers,
  // real deferred-detail) — seconds instead of the 14-min full corpus —
  // to pin a single film's arrival-order resolution race.
  private val HindRajabCinemas: Set[Cinema] =
    Set(CharlieMonroe, KinoMuranow, KinoAmondo, SluzewskiDomKultury)

  /** Seed 700000 is every test's reference boot; 700001..700005 must match it. */
  private val Seeds: Seq[Long] = (0 to 5).map(700000L + _)

  /** Boot `cinemas` once per [[Seeds]] entry — concurrently, each in its own
   *  isolated wiring — returning the settled records in seed order. */
  private def replaySubsets(cinemas: Set[Cinema]): Seq[Seq[StoredMovieRecord]] =
    ParallelReplays(Seeds, parallelism = ParallelReplays.ManyReplays)(replaySubset(cinemas, _))

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
    val runs = replaySubsets(CaravaggioCinemas)
    val ref  = runs.head
    Seeds.zip(runs).tail.foreach { case (seed, r) =>
      withClue(s"seed $seed vs ${Seeds.head}:\n  ref=${shape(ref)}\n  r  =${shape(r)}\n")(
        caravaggio(r).map(_.record.readyToProject) shouldBe caravaggio(ref).map(_.record.readyToProject))
    }
  }

  // Kino Sfinks lists "Robin Hood: Koniec legendy" three times — bare, "Tani wtorek: …"
  // and "Filmowy Klub Seniora i Seniorki: …" — and its detail resolves the film; Kino
  // Pionier Żary lists it yearless as "Robin Hood:Koniec Legendy", which TMDB can't
  // resolve on its own. When Sfinks folds first its resolved row is keyed under the
  // decorated "Tani wtorek" spelling, so Pionier's row folds beside it, and only the
  // settle's search-title edge joins the two. A converge that runs its enrichment sweep
  // before that settle audits Filmweb on the two halves and never on the merged film,
  // so its "steady state" is not steady: a second pass still finds the film's page.
  private val RobinHoodCinemas: Set[Cinema] = Set(KinoSfinks, KinoPionierZary)

  "converge" should "leave a staging-booted corpus that a second converge does not change" in {
    val passes = ParallelReplays(Seeds, parallelism = ParallelReplays.ManyReplays) { seed =>
      val w = new FixtureTestWiring(Fixture) {
        override lazy val backgroundBudget: tools.ExecutionBudget = new SameThreadExecutionBudget
      }
      val rnd = new scala.util.Random(seed)
      w.bootStartupInterleaved(rnd, RobinHoodCinemas.contains)
      w.converge(Some(rnd))
      def robin = OrderIndependentIds(w.movieRepository.findAll().filter(_.title.toLowerCase.contains("robin hood")), w.titleNormalizer)
        .stableRecords.map(r => (r.title, r.year, r.record.tmdbId, r.record.filmwebUrl, r.record.cinemaData.keySet))
      val settled = robin
      w.converge()
      (settled, robin)
    }
    Seeds.zip(passes).foreach { case (seed, (settled, reconverged)) =>
      withClue(s"seed $seed:\n")(reconverged shouldBe settled)
    }
  }

  "Głos Hind Rajab, booted from just its cinemas" should
    "settle to one identical record regardless of arrival order" in {
    // Ids are opaque and depend on which key the row was FIRST created under — i.e. on
    // arrival order, by design (`FilmId`); everything else about the row must not.
    def hind(rs: Seq[StoredMovieRecord]) =
      OrderIndependentIds(rs.filter(_.title.toLowerCase.contains("hind rajab")), TitleNormalizer.forCountry(Country.Poland)).stableRecords
    def shape(rs: Seq[StoredMovieRecord]) =
      hind(rs).map(x => (x.title, x.year, x.record.tmdbId, x.record.cinemaData.keySet)).mkString("\n  ")
    val runs = replaySubsets(HindRajabCinemas)
    val ref  = runs.head
    Seeds.zip(runs).tail.foreach { case (seed, r) =>
      withClue(s"seed $seed vs ${Seeds.head}:\n  ref=${shape(ref)}\n  r  =${shape(r)}\n")(hind(r) shouldBe hind(ref))
    }
  }
}
