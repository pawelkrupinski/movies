package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The per-film diff a convergence leg prints when its coverage drifts from production's: the band says
 * HOW FAR (`tmdbId run=1606 prod=1645`), this says WHICH films and WHY, so a red leg is read in minutes
 * rather than reproduced locally for hours.
 */
class FilmCoverageDiffSpec extends AnyFlatSpec with Matchers {

  private def listing(venue: String, title: String) = RunListing(s"$venue␟${title.toLowerCase}", venue, title)

  private def run(id: String, listings: Seq[RunListing], tmdb: Option[Int] = None, covered: Set[String] = Set.empty) =
    RunFilm(id, id, tmdb, None, covered ++ tmdb.map(_ => "tmdbId"), listings)

  private def prod(id: String, slots: Seq[String], tmdb: Option[Int] = None, covered: Set[String] = Set.empty) =
    ProdFilm(id, s"$id|", tmdb, tmdb.map(_ => "search"), None, covered ++ tmdb.map(_ => "tmdbId"), slots)

  private val noHit  = IdentityVerdict("no TMDB search hit", "no film (93.0%, NoCandidate) — 1 listing(s)\n    searched 'Dune Sing-Along'")
  private val vetoed = IdentityVerdict("vetoed", "no film (60.0%, Vetoed) — 1 listing(s)\n    1004 denied: year 1984 vs 2021")

  private val duneRio   = listing("Rio", "Dune Sing-Along")
  private val vetoRio   = listing("Rio", "Macbeth")
  private val sameRio   = listing("Rio", "Anora")
  private val extraRio  = listing("Rio", "Flow")
  private val goneRio   = listing("Rio", "Conclave")
  private val splitA    = listing("Rio", "Wicked")
  private val splitB    = listing("Odeon", "Wicked (Sing-Along)")
  private val mergeRio  = listing("Rio", "Kill Bill Double")

  private val runFilms = Seq(
    run("dune", Seq(duneRio)),
    run("macbeth", Seq(vetoRio)),
    run("anora", Seq(sameRio), tmdb = Some(1064213)),
    run("flow", Seq(extraRio), tmdb = Some(823219)),
    run("wicked", Seq(splitA), tmdb = Some(402431)),
    run("wicked-sing", Seq(splitB)),
    run("kill-bill", Seq(mergeRio), tmdb = Some(24)),
    run("run-only", Seq(listing("Rio", "Secret Screening"))))

  private val prodFilms = Seq(
    prod("p-dune", Seq(duneRio.slotKey), tmdb = Some(438631)),
    prod("p-macbeth", Seq(vetoRio.slotKey), tmdb = Some(1004)),
    prod("p-anora", Seq(sameRio.slotKey), tmdb = Some(1064213)),
    prod("p-flow", Seq(extraRio.slotKey)),
    prod("p-wicked", Seq(splitA.slotKey, splitB.slotKey), tmdb = Some(402431)),
    prod("p-kill-bill-1", Seq(mergeRio.slotKey), tmdb = Some(24)),
    prod("p-kill-bill-2", Seq(mergeRio.slotKey), tmdb = Some(393)),
    prod("p-conclave", Seq(goneRio.slotKey), tmdb = Some(974576)))

  private val verdicts = Map("dune" -> noHit, "macbeth" -> vetoed)
  private val diff = FilmCoverageDiff(runFilms, prodFilms, film => verdicts.get(film.id))

  "the per-film diff" should "name each film production identified that the run did not, with the cause the identity decision gives" in {
    val lacking = diff.gaps.filter(gap => gap.axis == "tmdbId" && gap.side == FilmGap.Side.RunLacks)
    lacking.map(gap => gap.run.map(_.id).getOrElse(gap.prod.map(_.id).mkString) -> gap.cause) should contain theSameElementsAs Seq(
      "dune"        -> "no TMDB search hit",
      "macbeth"     -> "vetoed",
      "wicked-sing" -> "split — a sibling run film holds it",
      "p-conclave"  -> "prod-only listing — no run film carries its slots")
    lacking.find(_.run.exists(_.id == "dune")).flatMap(_.verdict) shouldBe Some(noHit)
    lacking.find(_.run.exists(_.id == "dune")).map(_.prod.flatMap(_.tmdbId)) shouldBe Some(Seq(438631))
  }

  it should "name the films the run identified that production did not, and those it took a different film for" in {
    diff.gaps.filter(gap => gap.axis == "tmdbId" && gap.side == FilmGap.Side.RunOnly).map(gap => gap.run.map(_.id) -> gap.cause) should
      contain theSameElementsAs Seq(Some("flow") -> "prod lacks it")
    diff.gaps.filter(gap => gap.axis == "tmdbId" && gap.side == FilmGap.Side.Differs).flatMap(_.run.map(_.id)) shouldBe empty
  }

  it should "account for the film-count gap as run-only, prod-only, split and merged films" in {
    diff.runOnly.map(_.id) shouldBe Seq("run-only")
    diff.prodOnly.map(_.id) shouldBe Seq("p-conclave")
    diff.splits.map { case (prods, runs) => prods.map(_.id) -> runs.map(_.id) } shouldBe Seq(Seq("p-wicked") -> Seq("wicked", "wicked-sing"))
    diff.merges.map { case (prods, runs) => prods.map(_.id) -> runs.map(_.id) } shouldBe Seq(Seq("p-kill-bill-1", "p-kill-bill-2") -> Seq("kill-bill"))
    // 8 run films against 8 prod films: +1 run-only −1 prod-only +1 split −1 merged.
    diff.summary.head should include ("films run=8 prod=8: +1 run-only, -1 prod-only, +1 from 1 split(s), -1 from 1 merge(s)")
  }

  it should "lead with the gap grouped by cause, then list the films behind it, capped" in {
    val summary = diff.summary.mkString("\n")
    summary should include ("tmdbId: prod has, run lacks 4 (")
    summary should include ("no TMDB search hit 1")
    summary should include ("run has, prod lacks 1 (prod lacks it 1)")

    val details = diff.details(Set("tmdbId"), limit = 2)
    details.size shouldBe 3
    details.last should include ("…and 3 more")
    diff.details(Set("tmdbId"), limit = 40).mkString("\n") should (include ("Rio: 'Dune Sing-Along'") and include ("prod tmdb 438631"))
  }

  it should "write every film with its whole identity decision to the full report" in {
    val full = diff.full.mkString("\n")
    full should include ("searched 'Dune Sing-Along'")
    full should include ("1004 denied: year 1984 vs 2021")
    full should include ("Odeon: 'Wicked (Sing-Along)'")
    full should include ("Rio: 'conclave'")
  }

  it should "pair a run film with a different film production took as a difference, not as agreement" in {
    val differing = FilmCoverageDiff(Seq(run("dune", Seq(duneRio), tmdb = Some(1))), Seq(prod("p-dune", Seq(duneRio.slotKey), tmdb = Some(438631))),
      _ => None)
    differing.gaps.map(gap => (gap.axis, gap.side)) shouldBe Seq("tmdbId" -> FilmGap.Side.Differs)
  }

  it should "say a rating is missing because the run never identified the film, not because the rating was not fetched" in {
    val ratings = FilmCoverageDiff(
      Seq(run("dune", Seq(duneRio)), run("anora", Seq(sameRio), tmdb = Some(1064213))),
      Seq(prod("p-dune", Seq(duneRio.slotKey), tmdb = Some(438631), covered = Set("imdbRating")),
          prod("p-anora", Seq(sameRio.slotKey), tmdb = Some(1064213), covered = Set("imdbRating"))),
      _ => None)
    ratings.gaps.filter(_.axis == "imdbRating").map(gap => gap.run.map(_.id) -> gap.cause) should contain theSameElementsAs Seq(
      Some("dune") -> "run did not identify the film", Some("anora") -> "identified, not rated")
  }
}
