package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools._

/**
 * The convergence legs' identity claims, over the HARD CLUSTERS only, in minutes rather than hours.
 *
 * Every identity regression the country convergence legs caught lived in a few dozen films —
 * franchise siblings joined by arrival order, a contested film address decided by which listing came
 * first, a decorated re-release year read as a second film, a programme-prefixed listing moving on an
 * identical rescrape. `tools.HardClusters` extracts those clusters (and the shapes around them) from
 * the recorded corpora into a few hundred listings, and this spec runs the REAL pipeline over them —
 * real Mongo, the listing intake, the identity projection and the read-model projection — with every
 * HTTP answer the clusters need replayed from one checked-in file (`tools.RecordedResponses`).
 *
 * Per country it asserts:
 *
 *   1. ORDER-INDEPENDENCE — three seeded arrival orders (cinemas shuffled, each cinema's films
 *      shuffled), a SPLIT arrival (half the cinemas, projected, then the rest) and an arrival whose
 *      venue page writes each time out once (UNACKED) come out as the same films: identity, title,
 *      year, tmdbId, imdbId, cinemas and the film's public address.
 *   2. THE RIGHT ANSWER — the films checked in for the clusters (`expected-hard-clusters-<cc>.txt`).
 *   3. NO CHURN — a further projection, and two identical rescrapes each projected, write nothing.
 *   4. AN OUTAGE IS NOT AN ANSWER — with TMDB failing half its requests, then back, the films come
 *      out as the undisturbed boot's, IMDb ids included.
 *
 * Each pass runs in its own uniquely-named database (`IsolatedMongoDatabase`), dropped
 * in `afterAll`.
 *
 * The passes are [[HardClusterPasses]]'; [[HardClusterRecord]] re-records the responses file they replay after the
 * fixture grows (see `scripts/hard-clusters.sh`). `KINOWO_HARD_CLUSTERS_COUNTRIES=uk,us` narrows a local run to those
 * countries; `KINOWO_HARD_CLUSTERS_DUMP=1` prints every pass's films — the first thing to read when a cluster moves.
 */
class HardClusterConvergenceIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val passes = new HardClusterPasses(mongoTarget, harness.fixtureRoot, configuration.env,
    HardClusterPasses.countries(harness.hardClusterCountries.map(_.value)), recording = false)

  override def afterAll(): Unit = {
    passes.close()
    super.afterAll()
  }

  passes.countries.foreach { country =>
    val name = country.displayName

    s"the $name hard clusters" should "come out as the same films whatever order, and however split, they arrive in" in {
      val all = passes.booted(country)
      val (reference, refFilms) = all.head
      info(s"$name: ${refFilms.size} films from ${reference.scrapers.size} cinemas, " +
           s"${refFilms.count(_.tmdbId.isDefined)} resolved, ${refFilms.count(_.address.isDefined)} projected; " +
           s"${passes.responses(country).size} recorded responses, ${passes.responses(country).misses} unrecorded request(s)")
      refFilms should not be empty
      if (harness.hardClusterDump.value)
        all.foreach { case (p, fs) => println(s"[${country.code}] ${p.label}:\n  ${fs.mkString("\n  ")}") }
      val (divergences, fixed) = passes.divergences(country)
      withClue(s"$name: ${divergences.size} pass(es) diverged:\n${divergences.mkString("\n")}\n") {
        divergences shouldBe empty
      }
      withClue(s"$name: ${fixed.mkString(", ")} no longer diverge(s) on the split arrival — delete the entry from " +
               "HardClusterExemptions.SplitArrivalDivergences so the exemption cannot hide the next regression: ") {
        fixed shouldBe empty
      }
    }

    /* The claims above are all RELATIVE — every pass agrees with every other, a settle agrees
     * with itself — and a relative claim holds just as well over a wrong answer. A franchise
     * sibling folded onto the first film, a re-release given the wrong year, two films under
     * one title collapsed into one: each is deterministic, so every pass makes it identically
     * and every one of those claims stays green. These clusters are exactly the films where
     * those mistakes have been made before, and their inputs are all checked in, so their
     * RIGHT answer can be too. */
    it should "come out as the films checked in for them — the clusters' answer, not merely a consistent one" in {
      val path = HardClusters.expectedFilmsPath(country)
      if (!java.nio.file.Files.exists(path)) {
        java.nio.file.Files.writeString(path, passes.actualFilms(country))
        fail(s"no expected films for $name — wrote $path. Review it, commit it, and re-run.")
      }
      val moved = passes.movedFilms(country, java.nio.file.Files.readString(path))
      withClue(s"$name's hard clusters no longer come out as $path says (${moved.size} film(s)). If the change is " +
               s"intended, delete the file, re-run to regenerate it, and commit it with the change:\n" +
               s"${moved.take(20).mkString("\n")}\n") {
        moved shouldBe empty
      }
    }

    it should "serve every listing of every pass under the one film that holds it" in {
      val problems = passes.servedProblems(country)
      withClue(s"$name's served hard clusters do not match their listings:\n${problems.mkString("\n")}\n") {
        problems shouldBe empty
      }
    }

    it should "come out as the undisturbed boot's films once TMDB is back after an outage" in {
      val outage = passes.outages(country)
      withClue(s"$name: premise — the outage refused no TMDB request, so it tested nothing: ")(outage.refused should be > 0L)
      if (outage.unmatched.nonEmpty)
        info(s"$name: ${outage.unmatched.size} film(s) unmatched while TMDB failed (held open, not concluded):\n  " +
             outage.unmatched.take(12).mkString("\n  "))
      val drift = passes.outageDrift(country)
      withClue(s"$name: ${drift.size} film(s) not back to the undisturbed boot after TMDB recovered:\n${drift.take(12).mkString("\n")}\n") {
        drift shouldBe empty
      }
    }

    it should "not rewrite a settled film on a further projection or an identical rescrape" in {
      val problems = passes.churnProblems(country)
      withClue(s"$name did not stay settled:\n${problems.mkString("\n")}\n") {
        problems shouldBe empty
      }
    }
  }
}
