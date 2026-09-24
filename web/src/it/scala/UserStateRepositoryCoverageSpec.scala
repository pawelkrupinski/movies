package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository
import services.users.UserStateRepository
import tools.contracts.Implementations

import java.io.File
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/**
 * Every [[UserStateRepository]] implementation on the class path is held to
 * `UserStateWritesContract` — the in-memory store by `UserStateRepositorySpec`, Mongo's by
 * `UserRepositoryIntegrationSpec`.
 *
 * The contract cannot be run by reflection the way the worker's contract suites are: each
 * store seeds a whole row its own way (the trait has no whole-row write), so a store has
 * to be wired to it by hand. What CAN be automatic is noticing one that is not: an
 * implementation found here with no `atomicWritesBehaviour("<its name>")` in a web spec
 * fails this, instead of drifting from the others unseen.
 */
class UserStateRepositoryCoverageSpec extends AnyFlatSpec with Matchers {

  private val repoRoot: File =
    Iterator.iterate(new File(".").getCanonicalFile)(_.getParentFile).takeWhile(_ != null)
      .find(dir => new File(dir, "build.sbt").exists()).getOrElse(fail("no build.sbt above the working directory"))

  private val contractRuns: String =
    Seq("web/src/test/scala", "web/src/it/scala").map(new File(repoRoot, _).toPath).filter(Files.isDirectory(_))
      .flatMap(dir => Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).map(Files.readString).toSeq)
      .mkString("\n")

  "every UserStateRepository implementation" should "run UserStateWritesContract" in {
    val implementations = Implementations.of(classOf[UserStateRepository], classOf[UserStateRepository], classOf[InMemoryStagingRepository])
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryUserStateRepository", "MongoUserStateRepository")
    withClue("wire the store into UserStateWritesContract (`atomicWritesBehaviour(\"<name>\")`): ") {
      implementations.map(_.getSimpleName).filterNot(name => contractRuns.contains(s"""atomicWritesBehaviour("$name")""")) shouldBe empty
    }
  }
}
