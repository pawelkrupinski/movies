package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The weekly UK discovery job (`discover-cinemas.yml`) wires a new venue, and a new
 * venue in a new town is a new PAGE: the pages, the generated Scala, the apps' city
 * lists and their bundled catalog seeds all move. Its PR has to regenerate and carry
 * every one of them, or it opens red — `CatalogSeedSpec` and `tree-unchanged.sh` fail
 * on a seed the PR left stale.
 */
class DiscoveryPullRequestArtefactsSpec extends AnyFlatSpec with Matchers {

  private val workflow = RepoFile.read(".github/workflows/discover-cinemas.yml")
  private val scripts  = RepoFile.runSteps(workflow).map(_.script).mkString("\n")

  /** The paths the PR commits: `add-paths`' block. */
  private val addPaths: Seq[String] = {
    val lines = workflow.linesIterator.toSeq
    lines.dropWhile(!_.trim.startsWith("add-paths:")).drop(1).takeWhile(_.trim.matches("""[\w./-]+"""))
      .map(_.trim)
  }

  "the discovery PR" should "commit every artefact a new UK page moves" in {
    addPaths should contain allOf (
      "common/src/main/scala/models/Cinema.scala",
      "worker/src/main/scala/services/cinemas/UkScraperCatalog.scala",
      "data/uk/venues.json", "data/uk/towns.json", "data/uk/pages.json",
      "common/src/main/scala/models/UkPages.scala", "common/src/main/scala/models/VenueTowns.scala",
      "android/app/src/main/java/pl/kinowo/model/City.kt", "ios/Kinowo/Models/City.swift",
      "android/app/src/main/assets/catalog-seed.json", "ios/Kinowo/catalog-seed.json")
  }

  it should "regenerate them before committing" in {
    scripts should include ("data/uk/scripts/geocode_towns.py")
    scripts should include ("data/uk/scripts/build_pages.py")
    scripts should include ("data/uk/scripts/generate_roster.py")
    scripts should include ("tools.CatalogSeedSpec")
  }
}
