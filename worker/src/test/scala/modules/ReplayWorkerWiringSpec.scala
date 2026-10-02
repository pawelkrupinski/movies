package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpFetch

/**
 * The replay wiring must reach the network through NONE of production's fetch seams: it is what the
 * image build runs to train the JVM's AOT cache, and what `sbt localStack` runs on a laptop. A seam
 * it leaves alone keeps production's chain — the enrichment phase fetch, the Zyte fallback, the
 * residential proxy — and that chain goes to the live site.
 *
 * Asked of the class, not an instance: the wiring builds its Mongo repository eagerly, so an instance
 * needs a database, and the question is only which seams this class takes over.
 */
class ReplayWorkerWiringSpec extends AnyFlatSpec with Matchers {

  private def fetchSeams(cls: Class[?]): Set[String] =
    Iterator.iterate[Class[?]](cls)(_.getSuperclass).takeWhile(_ != null)
      .flatMap(_.getDeclaredMethods)
      .filter(m => m.getParameterCount == 0 && classOf[HttpFetch].isAssignableFrom(m.getReturnType) && !m.isSynthetic)
      .map(_.getName).filterNot(_.contains("$")).toSet

  "ReplayWorkerWiring" should "override every HttpFetch seam production's wiring has" in {
    val production = fetchSeams(classOf[WorkerWiring])
    val replayed   = classOf[ReplayWorkerWiring].getDeclaredMethods.map(_.getName).toSet
    production should not be empty
    production should contain("realHttpLeaf")
    (production -- replayed) shouldBe empty
  }
}
