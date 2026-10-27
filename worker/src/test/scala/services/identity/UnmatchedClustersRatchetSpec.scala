package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.UnmatchedClustersRatchet
import tools.{IdentityJudgements, UnmatchedClusters}

/**
 * The unmatched-cluster ratchet ([[UnmatchedClustersRatchet]]: zero wrong takes but those known-wrong in scope
 * `capture`, every take judged, every right take of `expected-matches.tsv` kept) over the checked-in captures, one test
 * a check — the gate CI runs. Every take's finding is written to `target/identity-unmatched/ratchet-takes.tsv` before
 * any check reads it, so a red ratchet still leaves it to read; a run that takes more right ones writes the new list to
 * `target/identity-unmatched/expected-matches.tsv` — copy it over the checked-in one to ratchet.
 */
class UnmatchedClustersRatchetSpec extends AnyFlatSpec with Matchers with tools.CountryScope {

  private def runOf(countries: Set[String]) = new UnmatchedClustersRatchet.Run(UnmatchedClusters.Directory,
    IdentityJudgements.CheckedIn, UnmatchedClusters.FindingsPath.getParent, measure = false, countries)

  // Every check decomposes by country (tools.CountryScope): the run replays only the change's countries' captures, built
  // once, on the first test; the tests are named by a run over none, which reads no capture.
  private val names = runOf(Set.empty).checks.map(_.name)
  private lazy val run = runOf(UnmatchedClustersRatchet.Countries.intersect(countriesInScope))

  names.foreach { name =>
    "the unmatched-cluster ratchet" should name in {
      assumeInScope(UnmatchedClustersRatchet.Countries.toSeq*)
      if (name == names.last) run.rightSummary.foreach(info(_))
      run.checks.find(_.name == name).getOrElse(fail(s"no check $name")).failure.foreach(fail(_))
    }
  }
}
