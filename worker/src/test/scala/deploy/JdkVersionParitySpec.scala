package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Keeps the JDK CI builds and tests on the same major as the JRE the image ships.
 *
 * The runtime is the Dockerfile's `JAVA_VERSION`; the build JDK is every `setup-java`
 * `java-version:` across the workflows and composite actions. A job may sit BELOW the
 * runtime only down to the bytecode floor build.sbt emits (`-java-output-version`), which is
 * where the deliberately older jobs live: Android's Gradle toolchain and the mobile
 * LocalServer run that proves the 21 class files load on a 21 JVM. Anything else is a job
 * compiling or testing on a JDK production never runs -- the drift a runtime bump that
 * forgets a workflow, or a workflow bump that forgets the image, would otherwise leave behind.
 */
class JdkVersionParitySpec extends AnyFlatSpec with Matchers {

  private lazy val dockerfile = RepoFile.read("Dockerfile")

  private lazy val runtimeMajor: Int =
    """(?m)^ENV JAVA_VERSION=jdk-(\d+)""".r
      .findFirstMatchIn(dockerfile)
      .map(_.group(1).toInt)
      .getOrElse(fail("the Dockerfile no longer names `ENV JAVA_VERSION=jdk-<major>...`"))

  private lazy val bytecodeFloor: Int =
    """"-java-output-version",\s*"(\d+)"""".r
      .findFirstMatchIn(RepoFile.read("build.sbt"))
      .map(_.group(1).toInt)
      .getOrElse(fail("build.sbt no longer sets -java-output-version"))

  private lazy val ciJavaVersions: Seq[(String, Int)] =
    RepoFile.ciFiles().flatMap { path =>
      """java-version:\s*'?(\d+)'?""".r.findAllMatchIn(RepoFile.read(path)).map(m => path -> m.group(1).toInt)
    }

  "every CI setup-java" should "build on the runtime image's JDK, or deliberately on the bytecode floor or below" in {
    ciJavaVersions should not be empty
    val strays = ciJavaVersions.filter { case (_, v) => v != runtimeMajor && v > bytecodeFloor }
    withClue(s"the image runs JDK $runtimeMajor and build.sbt emits Java $bytecodeFloor bytecode; these jobs are on neither: ") {
      strays shouldBe empty
    }
  }

  // Until Temurin 27's official image existed, the `jdk` stage downloaded the JDK tarball with apt + wget and checked a
  // SHA-256 pinned per architecture by hand: every bump meant two URLs and two checksums to copy, and 26.04's GNU tar
  // could not extract under QEMU, so the stage would not even build cross-arch on a Mac. The official image, pinned by
  // its manifest-list digest, carries the same JDK with none of that.
  "the image's jdk stage" should "be Temurin's official JDK image of the runtime's major, pinned by digest, not a downloaded tarball" in {
    val jdkBase = """(?m)^FROM\s+(\S+)\s+AS\s+jdk\s*$""".r.findFirstMatchIn(dockerfile).map(_.group(1))
      .getOrElse(fail("the Dockerfile no longer has a `FROM ... AS jdk` stage"))
    jdkBase should fullyMatch regex s"""mirror\\.gcr\\.io/library/eclipse-temurin:$runtimeMajor-jdk@sha256:[0-9a-f]{64}"""
    for (download <- Seq("wget", "curl", ".tar.gz", "sha256sum", "temurin" + runtimeMajor + "-binaries"))
      withClue(s"the Dockerfile fetches a JDK by hand again ($download): ")(dockerfile should not include download)
  }
}
