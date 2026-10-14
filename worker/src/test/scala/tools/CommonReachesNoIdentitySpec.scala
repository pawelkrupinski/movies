package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/** The identity resolver (`services.identity`) builds on `common` — its models, titles, stores — and nothing else in
 *  `common` builds on it: a reference back makes the two one tangle, which no change can then move or build apart.
 *  What `common` needs of identity it is handed (a lean read's `keepsDays`, the projector's `RatingGate`); a doc comment
 *  may still name it. */
class CommonReachesNoIdentitySpec extends AnyFlatSpec with Matchers {
  private val Identity = "common/src/main/scala/services/identity"
  private val Reference = """\bservices\.identity\b""".r

  "common's code outside services.identity" should "reach no identity code" in {
    val root = Paths.get("common/src/main/scala")
    val reaching = Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala"))
      .filterNot(_.toString.startsWith(Identity)).flatMap { path =>
        Files.readAllLines(path).asScala.zipWithIndex.collect {
          case (line, i) if !line.trim.startsWith("*") && !line.trim.startsWith("//") && !line.trim.startsWith("/**") &&
                            Reference.findFirstIn(line.replaceAll("`[^`]*`", "")).isDefined => s"$path:${i + 1}: ${line.trim}"
        }
      }.toSeq
    reaching shouldBe empty
  }
}
