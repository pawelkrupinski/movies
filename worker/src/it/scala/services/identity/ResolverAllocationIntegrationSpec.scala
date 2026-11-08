package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[ResolverAllocationSpec]]'s allocation budgets, measured in a fresh JVM without escape analysis: per call, the bytes
 *  its code allocates, whatever the JIT made of it and whatever ran before it in the shared test JVM. */
class ResolverAllocationIntegrationSpec extends AnyFlatSpec with Matchers {
  "the resolver's hottest helpers" should "allocate within their budgets with nothing scalar-replaced" in {
    val (exit, output) = tools.ChildJvm(settings.HarnessConfiguration.resolve())
      .run("services.identity.ResolverAllocationSpec", jvmArgs = Seq("-XX:-DoEscapeAnalysis", "-XX:AutoBoxCacheMax=16384"))
    withClue(s"ResolverAllocationSpec without escape analysis said:\n$output\n")(exit shouldBe 0)
  }
}
