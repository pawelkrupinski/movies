package tools

import kinowo.build.{ClassReferences, ExemptionSlack}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The slack in a fan-out exemption (project/ExemptionSlack.scala, `scripts/coupling-report --exemption-slack`): which of
 *  a class's collaborators its class files' methods show it could give up, over the fixture's compiled classes
 *  (tools/couplingfixture/slack/Asker.scala). */
class ExemptionSlackSpec extends AnyFlatSpec with Matchers {

  private val pkg = "tools.couplingfixture.slack"
  private lazy val compiled: Seq[ClassReferences.Compiled] = {
    val dir = java.nio.file.Paths.get(getClass.getResource("/" + pkg.replace('.', '/')).toURI)
    Files.list(dir).iterator.asScala.toSeq.filter(_.toString.endsWith(".class")).map(p => ClassReferences.read(Files.readAllBytes(p)))
  }
  private def in(name: String) = s"$pkg.$name"
  private val asker = in("Asker")
  private val targets = Set("Store", "Hashing", "Metrics", "Tally", "Corpus", "Fence", "Decoder", "Cursor", "Seed").map(in)
  // a binary name's top-level class, the target it counts as
  private def top(binary: String): Option[String] = Some(binary.takeWhile(_ != '$')).filter(n => n.startsWith(pkg + ".") && n != asker)
  private def classesOf(name: String) = compiled.filter(c => c.name == name || c.name.startsWith(name + "$"))
  private lazy val slack = ExemptionSlack.of(asker, targets, classesOf(asker), b => top(b).filter(targets),
    (provider, collaborator) => ExemptionSlack.handsOut(classesOf(provider).filter(c => c.name == provider || c.name == provider + "$"), top, collaborator))
  private def use(name: String) = slack.uses.find(_.target == in(name)).get

  "a class's methods" should "each be named as the source names them, a lambda's and a default argument's as the method holding it" in {
    ExemptionSlack.enclosing("a.X", "run$$anonfun$1") shouldBe "run"
    ExemptionSlack.enclosing("a.X", "run$$anonfun$1$$anonfun$2") shouldBe "run"
    ExemptionSlack.enclosing("a.X", "$anonfun$run$1") shouldBe "run"
    ExemptionSlack.enclosing("a.X", "$anonfun$1") shouldBe "a lambda"
    ExemptionSlack.enclosing("a.X", "$anonfun$adapted$2") shouldBe "a lambda"
    ExemptionSlack.enclosing("a.X", "run$default$2") shouldBe "run"
    ExemptionSlack.enclosing("a.X", "a$X$$run") shouldBe "run"
    ExemptionSlack.enclosing("a.X", "$lessinit$greater$default$1") shouldBe "<init>"
    ExemptionSlack.enclosing("a.X", "rows$lzyINIT1") shouldBe "rows"
    ExemptionSlack.enclosing("a.X", "run") shouldBe "run"
  }

  "a collaborator only forwarded into a constructor" should "be a pass-through, and one only read while the class is built a builder" in {
    use("Decoder").passThrough shouldBe true
    use("Seed").passThrough shouldBe false
    use("Seed").buildsAnother shouldBe true
    use("Cursor").methods shouldBe Set("ask")
    Seq("Cursor", "Store", "Metrics").map(use).exists(u => u.passThrough || u.buildsAnother) shouldBe false
  }

  "collaborators used by exactly the same methods" should "be one co-used group" in {
    use("Store").methods shouldBe Set("file", "count")
    slack.coUsed shouldBe Seq(Seq(in("Hashing"), in("Store")))
  }

  "a collaborator used only beside another" should "be reached through it when that one hands it out, else only a hint" in {
    slack.through shouldBe Seq(in("Fence") -> in("Corpus"))
    // the cursor only where the corpus (and the metrics) are used, the tally only where the metrics are: neither handed out
    slack.beside shouldBe Seq(in("Cursor") -> in("Corpus"), in("Tally") -> in("Metrics"))
  }

  "the estimate" should "take out the pass-through, the builder, a co-used group's all but one, and the reached-through" in {
    slack.fanOut shouldBe 9
    slack.after shouldBe 5
  }

  "the report" should "name each collaborator's flags and the cut, and call an exemption within the limit after it stale" in {
    val report = ExemptionSlack.report(Seq(slack), _ => "state")
    report should include(s"## `$asker`: 9 -> 5 (within the limit: the exemption may be stale)")
    report should include(s"- `${in("Decoder")}` (state): no method -- **pass-through**")
    report should include("**reached through** `Corpus`")
    report should include("one collaborator holding `Hashing`, `Store` (-1)")
    report should include("1 of 1 would come within the limit (9): `Asker` 9 -> 5.")
  }
}
