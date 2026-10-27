package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/**
 * A scratch repository's commands stay in the scratch repository even when the JVM was handed
 * the git environment of a hook (sbt started by pre-push passes GIT_DIR to its forked test JVMs).
 * A decoy repository stands in for the one being pushed; nothing here touches this checkout.
 */
class ScratchGitRepositorySpec extends AnyFlatSpec with Matchers {

  "an isolated command" should "act on the directory it runs in, not on a GIT_DIR it inherited" in {
    val decoy   = new ScratchGitRepository
    val before  = decoy.commit("the decoy's own history", "a" -> "1")
    val config  = Files.readString(decoy.root.resolve(".git/config"))
    val scratch = new ScratchGitRepository
    Files.writeString(scratch.root.resolve("f"), "x")

    def asHookChild(command: String*): Int = {
      val builder = new java.lang.ProcessBuilder(command*).directory(scratch.root.toFile)
      val inherited = builder.environment()
      inherited.put("GIT_DIR", decoy.root.resolve(".git").toString)
      inherited.put("GIT_INDEX_FILE", decoy.root.resolve(".git/index").toString)
      inherited.put("GIT_WORK_TREE", decoy.root.toString)
      ScratchGitRepository.isolate(inherited)
      builder.redirectErrorStream(true).start().waitFor()
    }
    asHookChild("git", "add", "f") shouldBe 0
    asHookChild("git", "commit", "-q", "-m", "scratch") shouldBe 0

    decoy.git("rev-parse", "HEAD") shouldBe before
    Files.readString(decoy.root.resolve(".git/config")) shouldBe config
    scratch.git("log", "-1", "--format=%s %an") shouldBe "scratch Spec"
  }

  it should "not read the developer's global git config" in {
    val repo = new ScratchGitRepository
    repo.process(Seq("git", "config", "--global", "--list")).lazyLines_!.toSeq shouldBe empty
  }
}
