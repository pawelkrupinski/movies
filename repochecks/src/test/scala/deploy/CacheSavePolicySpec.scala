package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Only main saves a large or shared cache family: sbt's compiled classes (each module's `target/scala-*`), its
 * dependency cache (`~/.ivy2/cache`, `~/.cache/coursier`), and the pinned tools' verified downloads (setup-jdk's
 * JDK tarball, ~140 MB a platform, and install-actionlint's), ONE entry each that every ref reads. Every other ref
 * restores and never saves. A measure
 * or landing run's entries are read by nobody after it, and a save of a key another job already saved is a second
 * copy. A step that can save one of those paths must say
 * `github.ref == 'refs/heads/main'` in its `if:`; restoring (`actions/cache/restore`) is free.
 */
class CacheSavePolicySpec extends AnyFlatSpec with Matchers {
  private val LargePaths =
    Seq("*/target/scala-*", "~/.ivy2/cache", "~/.cache/coursier", "${{ runner.temp }}/jdk-archive",
      "${{ runner.temp }}/actionlint", "${{ runner.temp }}/coursier-launcher")
  private val Saving     = Seq("uses: actions/cache@", "uses: actions/cache/save@")
  private val MainOnly   = "github.ref == 'refs/heads/main'"

  /** Each step of a CI file, as its own lines: from a `- ` item to the next item at the same indent. */
  private def steps(text: String): Seq[Vector[String]] = {
    val lines = RepoFile.withoutComments(text).linesIterator.toVector
    val starts = lines.indices.filter { i =>
      val t = lines(i).trim
      t.startsWith("- ") && (t.startsWith("- uses:") || t.startsWith("- name:") || t.startsWith("- if:") ||
        t.startsWith("- id:") || t.startsWith("- run:"))
    }
    starts.map { s =>
      val indent = lines(s).indexOf('-')
      val end = (s + 1 until lines.length).find { i =>
        val l = lines(i)
        l.trim.nonEmpty && l.indexWhere(_ != ' ') <= indent
      }.getOrElse(lines.length)
      lines.slice(s, end)
    }
  }

  private def savesLarge(step: Vector[String]): Boolean =
    step.exists(l => Saving.exists(l.contains)) &&
      step.exists(l => LargePaths.exists(p => l.trim == p || l.trim == s"path: $p"))

  private def ifLine(step: Vector[String]): String =
    step.map(_.trim.stripPrefix("- ")).find(_.startsWith("if:")).getOrElse("")

  "every CI step that can save a large cache family" should "run on main only" in {
    val saving = for {
      p    <- RepoFile.ciFiles()
      step <- steps(RepoFile.read(p))
      if savesLarge(step)
    } yield p -> step
    // The sbt target cache's two saving steps, the dependency cache's one, the JDK's and actionlint's, at least.
    saving.size should be >= 5
    for ((p, step) <- saving)
      withClue(s"$p: a step saving ${LargePaths.mkString(", ")} runs on `${ifLine(step)}`, ") {
        ifLine(step) should include(MainOnly)
      }
  }

  "the step finder" should "see a saving step's `if:` and miss nothing that saves" in {
    val yml =
      """steps:
        |    - name: one
        |      if: github.ref == 'refs/heads/main'
        |      uses: actions/cache@v5
        |      with:
        |        path: |
        |          */target/scala-*
        |    - uses: actions/cache/save@v5
        |      with:
        |        path: |
        |          ~/.ivy2/cache
        |    - uses: actions/cache/restore@v5
        |      with:
        |        path: |
        |          ~/.cache/coursier
        |    - uses: actions/cache/save@v5
        |      with:
        |        path: ${{ runner.temp }}/jdk-archive
        |""".stripMargin
    val found = steps(yml).filter(savesLarge)
    found.map(ifLine) shouldBe Seq("if: github.ref == 'refs/heads/main'", "", "")
  }
}
