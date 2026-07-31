package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Named imports, not `scala.sys.process._`: the wildcard also brings in an implicit
// conversion from `java.net.URL` to a process Source, which silently hijacks any
// method call on the classloader URLs below.
import scala.sys.process.{Process, ProcessLogger}

/**
 * `Cinema` and `GermanRoster` initialise each other — `Cinema.byCity` appends
 * `GermanRoster.byCity`, and `GermanRoster` reads `Cinema.polishAndUk` to find the display
 * names already claimed. Whether that works used to depend on which of the two the JVM
 * touched first: through `Cinema` the fields were assigned in time, through `GermanRoster`
 * it read a null and died with `ExceptionInInitializerError`.
 *
 * The symptom was not in this file at all. `CinemaScraperCatalogSpec` simply could not be
 * run on its own — it passed inside a full `testUnit` where some earlier spec happened to
 * touch `Cinema` first, and aborted the moment anyone ran it alone or a CI shard put it
 * first. That is the worst shape a bug can have: invisible in the run everyone does, fatal
 * in the run someone does when they are already debugging something else.
 *
 * A fresh JVM is the whole point, so this forks one. Asserting it in-process would prove
 * nothing: by the time any spec runs, some other suite has almost certainly initialised
 * `Cinema` already, and the assertion would pass whatever the field strictness is.
 */
class RosterInitOrderSpec extends AnyFlatSpec with Matchers {

  /** This run's real classpath, walked off the loader chain.
   *
   *  NOT `java.class.path`: under sbt that property is the launcher's own classpath, so a
   *  JVM forked with it cannot see the classes under test and dies with
   *  `ClassNotFoundException` — a failure that looks exactly like the bug this spec is
   *  guarding against, which would make the guard worthless in the direction that matters.
   *  sbt's loaders are `URLClassLoader`s, so the entries can be collected directly. */
  private def testClasspath: String = {
    val entries = Iterator.iterate(getClass.getClassLoader)(_.getParent).takeWhile(_ != null)
      .collect { case loader: java.net.URLClassLoader => loader.getURLs.toSeq }
      .flatten.map(entry => new java.io.File(entry.toURI).getPath).toSeq.distinct
    entries.mkString(java.io.File.pathSeparator)
  }

  /** Run `entryPoint`'s main in a new JVM on this run's classpath, returning its output. */
  private def inFreshJvm(entryPoint: String): (Int, String) = {
    val java      = s"${System.getProperty("java.home")}/bin/java"
    val classpath = testClasspath
    val output    = new StringBuilder
    val logger    = ProcessLogger(line => output.append(line).append('\n'),
                                  line => output.append(line).append('\n'))
    val exit = Process(Seq(java, "-cp", classpath, entryPoint)).!(logger)
    (exit, output.toString)
  }

  "The German roster" should "initialise when it is the first thing a JVM touches" in {
    val (exit, output) = inFreshJvm("models.TouchGermanRosterFirst")
    withClue(s"fresh JVM said:\n$output") {
      exit shouldBe 0
      output should include("roster-ok")
    }
  }

  it should "still initialise when Cinema is the first thing a JVM touches" in {
    val (exit, output) = inFreshJvm("models.TouchCinemaFirst")
    withClue(s"fresh JVM said:\n$output") {
      exit shouldBe 0
      output should include("cinema-ok")
    }
  }
}
