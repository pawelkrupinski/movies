package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every sbt `target/` cache in CI leaves the packaged jars out. The `target/scala-*` glob also held
 * web's three packaged jars (~0.5 GB of the ~0.9 GB per entry); four cache families each writing a
 * fresh ~0.9 GB entry on most pushes kept the repository over GitHub's 10 GB cache limit, so the
 * rarely-run workflows' caches (the Android Gradle home, Playwright's WebKit) were always evicted
 * before their next run. sbt re-packages a jar in seconds; the compiled classes stay cached.
 */
class SbtTargetCacheJarsSpec extends AnyFlatSpec with Matchers {
  private val TargetGlob = "*/target/scala-*"
  private val JarsOut    = "!*/target/scala-*/*.jar"

  "every sbt target cache in CI" should "exclude the packaged jars" in {
    val caching = RepoFile.ciFiles().filter(p => RepoFile.read(p).linesIterator.exists(_.trim == TargetGlob))
    caching should not be empty
    for (p <- caching) {
      val lines   = RepoFile.read(p).linesIterator.map(_.trim).toVector
      val targets = lines.count(_ == TargetGlob)
      withClue(s"$p: $targets `$TargetGlob` path(s), ")(lines.count(_ == JarsOut) shouldBe targets)
    }
  }
}
