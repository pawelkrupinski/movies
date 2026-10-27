package tools

import org.scalatest.{Canceled, Outcome, TestSuite}

/**
 * A spec over recorded corpora whose checks DECOMPOSE BY COUNTRY: each country's part reads only that country's
 * inputs (its capture, its slices, its rows of the shared judgement and take files, its title rules) and holds or fails
 * on its own. The identity area's test-impact selection (scripts/ci/identity_impact.py) passes the countries a change
 * can reach as ScalaTest's config map entry `-Didentity.countries=pl,de`; a test reads [[countriesInScope]] and checks
 * only those, or cancels when none of its countries is in scope (by [[assumeInScope]], or before it starts when the
 * suite names a test's countries by [[countriesOf]]). Without the entry every country is in scope.
 *
 * Only a check true of the whole exactly when it is true of every country part may scope: "no wrong take" (none in any
 * country), "every expected take kept" (each country's), "an event found in each of pl, uk, de, us" (each one's). A sum
 * compared across countries does not decompose, and stays whole.
 */
trait CountryScope extends TestSuite {

  @volatile private var scope: Option[Set[String]] = None

  abstract override def withFixture(test: NoArgTest): Outcome = {
    scope = test.configMap.getOptional[String](CountryScope.Key).map(CountryScope.parse)
    val countries = countriesOf(test.name)
    if (countries.isEmpty || countries.exists(countriesInScope)) super.withFixture(test) else Canceled(outOfScope)
  }

  /** The countries a test checks, by its name, when the suite knows them before the test runs: the test cancels without
   *  starting unless one is in scope. Nil (the default): the test scopes itself, if at all, by [[assumeInScope]]. */
  protected def countriesOf(testName: String): Seq[String] = Nil

  /** The countries this test checks: the config map's, else every one. */
  def countriesInScope: Set[String] = scope.getOrElse(CountryScope.All)

  /** Cancels the test unless one of `countries` is in scope. */
  def assumeInScope(countries: String*): Unit =
    assume(countries.exists(countriesInScope), outOfScope)

  private def outOfScope = s"out of the change's country scope (${countriesInScope.toSeq.sorted.mkString(",")})"
}

object CountryScope {
  val Key = "identity.countries"
  val All: Set[String] = models.Country.all.map(_.code).toSet
  def parse(csv: String): Set[String] = csv.split(",").map(_.trim).filter(_.nonEmpty).toSet
}
