package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The XCUITest job's simulator cold-boots (~2 min) under the app build rather than in front of the
 * first UI test: it is picked and its boot started before "Build the app target", and collected
 * just before "Run XCUITest".
 */
class IosSimulatorPrebootSpec extends AnyFlatSpec with Matchers {

  private lazy val uiTests: String = {
    val yaml = RepoFile.read(".github/workflows/ios.yml")
    yaml.substring(yaml.indexOf("\n    ui-tests:"))
  }

  private def at(fragment: String): Int = {
    val index = uiTests.indexOf(fragment)
    withClue(s"ios.yml's ui-tests job has no `$fragment`: ")(index should be >= 0)
    index
  }

  "The XCUITest job" should "start the simulator's boot before it builds the app" in {
    at("xcrun simctl boot") should be < at("- name: Build the app target")
  }

  it should "wait for that boot only once the build is done, right before the UI tests" in {
    at("- name: Build the app target") should be < at("xcrun simctl bootstatus")
    at("xcrun simctl bootstatus") should be < at("- name: Run XCUITest")
  }
}
