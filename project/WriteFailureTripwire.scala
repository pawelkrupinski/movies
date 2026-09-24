import sbt._
import sbt.Keys._

/**
 * The build half of `tools.WriteFailureTripwire` (testkit): registers that reporter on a
 * test configuration and fails the configuration's run when it recorded a repository write
 * failure no test excused.
 *
 * The reporter runs in sbt's JVM beside the tests (the config is kept unforked for exactly
 * that) and writes its findings into a per-module, per-config directory under the root
 * `target/`, which `Tests.Cleanup` reads and empties afterwards. A reporter can observe
 * tests but not fail them, so the verdict crosses back through that directory.
 */
object WriteFailureTripwire {

  private val DirKey = "kinowo.writeFailureTripwire.dir"

  def settings(config: Configuration): Seq[Setting[_]] = Seq(
    // UNFORKED, always. ScalaTest builds a `-C` reporter wherever its framework runs, and
    // for a forked config that is sbt's JVM — outside the test JVM whose log lines the
    // reporter has to see, so a fork would leave the tripwire silently blind. (PlayScala
    // forks `web`'s tests by default; none of its `it` specs needs it.)
    config / fork := false,
    config / testOptions ++= {
      val dir = (LocalRootProject / target).value / "write-failure-tripwire" / s"${thisProject.value.id}-${config.name}"
      Seq(
        Tests.Argument(TestFrameworks.ScalaTest, "-C", "tools.WriteFailureTripwire", s"-D$DirKey=${dir.getAbsolutePath}"),
        // No Setup that empties the directory: sbt runs user setup CONCURRENTLY with the
        // framework's, which is what builds the reporter, so a delete there raced it.
        // Cleanup consumes what it reads instead.
        Tests.Cleanup(() => verify(dir))
      )
    }
  )

  private def verify(dir: File): Unit = {
    val files    = (dir ** "*.txt").get.sorted
    val findings = files.flatMap(IO.readLines(_)).filter(_.trim.nonEmpty)
    IO.delete(files)
    // Retires every sink of the run just read — see FindingSink in testkit.
    IO.write(dir / "generation", System.nanoTime().toString)
    if (findings.nonEmpty)
      throw new MessageOnlyException(
        s"""WriteFailureTripwire: ${findings.size} repository write failure(s) were logged during this run.
           |A swallowed write failure is a lost write in production — fix it, or, if the test breaks
           |the write ON PURPOSE, excuse it in WriteFailureTripwire.expected (testkit) with the reason.
           |${findings.map("  " + _).mkString("\n")}""".stripMargin)
  }
}
