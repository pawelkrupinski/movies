package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Every pod's JAVA_OPTS must let the image's AOT cache map (Dockerfile, `tools.ClassArchiveTraining`).
 *  The cache is trained through the launcher under the options baked into it (infra/jvm/<tier>.options),
 *  and the overlay's JAVA_OPTS come AFTER those, so an overlay naming another collector or
 *  type-speculation setting would win at run time and the cache would not map — silently: every
 *  class back in metaspace, where worker-pl died at its 128m cap. A CDS archive flag is worse: the
 *  JVM refuses to start beside -XX:AOTCache, which is why the Dockerfile then leaves the cache out. */
class AotCacheOptionsSpec extends AnyFlatSpec with Matchers {

  private val Collector   = """-XX:\+Use\w*GC""".r
  private val Speculation = """-XX:[+-]UseTypeSpeculation""".r
  private val CdsArchive  = """-XX:(SharedArchiveFile=\S*|\+AutoCreateSharedArchive)|-Xshare:\w+""".r

  private def baked(tier: String): Set[String] =
    RepoFile.read(s"infra/jvm/$tier.options").linesIterator.map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#")).toSet

  /** Each manifest under the tier that sets JAVA_OPTS, with them. */
  private def javaOpts(tier: String): Seq[(String, String)] = {
    RepoFile.read(s"infra/kubernetes/$tier/base/all.yaml") // the missing-checkout message, if absent
    Files.walk(Paths.get(s"infra/kubernetes/$tier")).iterator.asScala.toSeq
      .filter(_.toString.endsWith(".yaml")).map(_.toString).sorted
      .flatMap(path => """JAVA_OPTS: >-\n\s+(.+)""".r.findFirstMatchIn(RepoFile.read(path)).map(path -> _.group(1)))
  }

  Seq("web", "worker").foreach { tier =>
    s"the $tier tier's JAVA_OPTS" should "name no collector or type speculation other than its launcher's" in {
      val manifests = javaOpts(tier)
      manifests should not be empty
      manifests.foreach { case (path, opts) =>
        val overridden = (Collector.findAllIn(opts) ++ Speculation.findAllIn(opts)).toSet -- baked(tier)
        withClue(s"$path overrides infra/jvm/$tier.options, which the AOT cache is trained under: ")(overridden shouldBe empty)
      }
    }

    it should "carry no CDS archive flag, beside which the JVM refuses the AOT cache" in {
      javaOpts(tier).foreach { case (path, opts) =>
        withClue(s"$path: ")(CdsArchive.findAllIn(opts).toSeq shouldBe empty)
      }
    }
  }
}
