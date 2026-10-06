package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** The rules table of `identity-resolver.md` §21.2 lists every stage's rules in the order the code tries them — the
 *  order is behaviour, the first to take deciding — so a rule added, dropped or moved in a registry moves the table. */
class IdentityRulesDocSpec extends AnyFlatSpec with Matchers {

  private val rows = Files.readAllLines(Path.of("docs", "design", "identity-resolver.md"), StandardCharsets.UTF_8).asScala.toSeq
    .map(_.split("\\|", -1).map(_.trim).toSeq).collect { case Seq("", stage, n, rule, _, "") if n.forall(_.isDigit) && n.nonEmpty => (stage, n.toInt, rule) }
  private def listed(stage: String) = rows.filter(_._1 == stage).sortBy(_._2).map(_._3)
  private val acceptance = new Acceptance(IdentityCalibration.resolver)

  "the rules table" should "list the alone rules in the order Acceptance tries them" in {
    listed("alone") shouldBe acceptance.aloneOrder
  }

  it should "list the pooled rules in the order Acceptance tries them" in {
    listed("pooled") shouldBe acceptance.pooledOrder
  }

  it should "list the agreement stage's takes in the order it reads them" in {
    listed("agreement") shouldBe agreement.AgreementStage.FallThrough
  }
}
