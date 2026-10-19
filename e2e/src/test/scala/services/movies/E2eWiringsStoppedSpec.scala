package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Every pipeline an e2e spec boots is one [[StopsItsWirings]] stops: e2e runs inside the kept sbt server, where an
 *  unstopped wiring's threads pin the run's classloader and all it built, run after run. Read off the sources, since
 *  a leak shows only in the server's heap several runs later. */
class E2eWiringsStoppedSpec extends AnyFlatSpec with Matchers {

  private val sources: Map[Path, String] =
    Files.walk(Paths.get("e2e/src/test/scala")).iterator.asScala.filter(_.toString.endsWith(".scala"))
      .map(path => path -> Files.readString(path)).toMap

  /** FixtureTestWiring and every e2e class extending it, transitively. */
  private val wirings: Set[String] = {
    val extending = sources.values.flatMap("""class (\w+)\s+extends\s+(\w+)""".r.findAllMatchIn(_)).map(m => m.group(1) -> m.group(2)).toSeq
    Iterator.iterate(Set("FixtureTestWiring"))(known => known ++ extending.collect { case (sub, sup) if known(sup) => sub })
      .sliding(2).collectFirst { case Seq(a, b) if a == b => a }.get
  }

  "every wiring an e2e spec builds" should "be stopped after its suite" in {
    wirings.size should be > 1
    val unstopped = for {
      (path, text) <- sources.toSeq
      (line, n)    <- text.linesIterator.zipWithIndex
      name         <- wirings if line.contains(s"new $name") && !line.contains(s"stoppedAfterAll(new $name")
    } yield s"$path:${n + 1}: ${line.trim}"
    withClue("wrap it as stoppedAfterAll(new …) in a suite mixing in StopsItsWirings:\n")(unstopped shouldBe empty)
  }
}
