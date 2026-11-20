package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import testsupport.SourceTrees

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * ONE MODULE A METRICS PACKAGE. `services.metrics` once held classes of three production modules (common's shared
 * plumbing, web's series, the worker's), so the coupling report measured it as one package that no module owned -- a
 * split package with a distance D of 0.38, frozen past the 0.20 limit. Each tier's series now live in a package of their
 * own beside the shared one (`scripts/coupling-report` lists split packages): common keeps `services.metrics`, web holds
 * `services.metrics.web`, the worker `services.metrics.worker`, and every source's `package` clause names its directory.
 * testkit is test support, outside the coupling graph: its spec helpers for both tiers (MetricCatalogue, ExpositionSamples,
 * CacheMetricSamples) stay in `services.metrics`, beside the shared plumbing they read.
 */
class MetricsPackagesSpec extends AnyFlatSpec with Matchers {

  import MetricsPackagesSpec.*

  "the production metrics packages" should "each live in one module, the tier's own" in {
    val sources = metricsSources()
    sources.size should be > 30
    sources.groupMap(_.pkg)(_.module).view.mapValues(_.toSet).toMap shouldBe Map(
      "services.metrics" -> Set("common"), "services.metrics.web" -> Set("web"), "services.metrics.worker" -> Set("worker"))
  }

  it should "declare the package their directory names" in {
    metricsSources().filterNot(s => s.pkg == s.directoryPackage) shouldBe empty
  }
}

object MetricsPackagesSpec {
  final case class Source(path: Path, pkg: String) {
    /** The module: the path's first segment (`<module>/src/main/scala/...`). */
    def module: String = path.getName(0).toString
    /** The package the directory under `<module>/src/main/scala` names. */
    def directoryPackage: String = path.subpath(4, path.getNameCount - 1).iterator.asScala.mkString(".")
  }

  /** Every module's production metrics tree. */
  private val Trees = SourceTrees("*/src/main/scala/services/metrics")

  /** Modules whose `src/main` is test support, not production code. */
  private val TestSupport = Set("testkit")

  private val PackageClause = """(?m)^package\s+([\w.]+)\s*$""".r

  /** Every production source under a module's `services/metrics` tree, with the package its clauses declare. */
  def metricsSources(): Seq[Source] =
    Trees.scalaFiles.map(path => Source(path, PackageClause.findAllMatchIn(Files.readString(path)).map(_.group(1)).mkString(".")))
      .filterNot(source => TestSupport(source.module))
}
