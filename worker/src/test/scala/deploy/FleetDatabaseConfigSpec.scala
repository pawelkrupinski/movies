package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.FleetDatabase

/**
 * Locks the ONE database every web and worker keeps the fleet's shared state in: the Facebook
 * re-scrape queue and quota, the Facebook fetches, the fleet host paces.
 *
 * `services.FleetDatabase.name` defaults to `kinowo_fleet`, and movies-gitops has set
 * `MONGODB_FLEET_DB: kinowo_fleet` in both ConfigMaps since the move (docs/fleet-database-migration.md,
 * step 5, 2026-10-25). The value is unreachable from every running-JVM layer, and a tier that
 * disagreed would not fail: web and worker would each keep a queue and a quota of their own, and
 * Facebook would see the fleet's pace doubled. Same reasoning as [[SharedUsersDatabaseConfigSpec]],
 * which this sits beside.
 */
class FleetDatabaseConfigSpec extends AnyFlatSpec with Matchers {

  /** `MONGODB_FLEET_DB` as a deploy config SETS it: comment lines dropped. */
  private def fleetDbIn(text: String): Option[String] =
    text.linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"MONGODB_FLEET_DB:$rest" => rest.trim.replaceAll("^['\"]|['\"]$", "") }
      .filter(_.nonEmpty)

  private val Bases = Seq("infra/kubernetes/web/base/all.yaml", "infra/kubernetes/worker/base/all.yaml")

  "Every web and worker" should "keep the fleet's state in the fleet's own database, the code's default" in {
    Bases.foreach { path =>
      withClue(s"$path: ")(fleetDbIn(RepoFile.read(path)).fold(FleetDatabase.Dedicated.value)(identity) shouldBe
        FleetDatabase.Dedicated.value)
    }
  }

  // An overlay with its own value would split one country's queue off from the fleet's.
  it should "let no country's overlay point somewhere else" in {
    for (tier <- Seq("web", "worker"); country <- Country.all) {
      val overlay = scala.util.Try(RepoFile.read(s"infra/kubernetes/$tier/overlays/${country.code}/patch.yaml")).toOption
      withClue(s"$tier/${country.code} overlay sets MONGODB_FLEET_DB: ")(overlay.flatMap(fleetDbIn) shouldBe empty)
    }
  }
}
