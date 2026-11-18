package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE NODE LEDGERS ARE ONLY AS GOOD AS THEIR POD LIST. [[NodeMemoryBudgetSpec]] and
 * [[NodeCpuBudgetSpec]] both sum [[NodePods]]; this holds that list to everything that actually
 * schedules a pod on k3s-worker-1. Until 2026-11-18 each spec subtracted a hand-set "system
 * reserve" (576Mi, 250m) and nothing asked whether it covered every pod: coredns -- a k3s add-on,
 * in no manifest of ours -- was never counted, and a fifth system manifest would have been missed
 * the same way.
 */
class NodePodsSpec extends AnyFlatSpec with Matchers {

  "the node's pod ledger" should "read every manifest under infra/kubernetes that schedules a pod" in {
    withClue("these manifests schedule pods on the node and are in no row of the ledger -- add them to " +
             "NodePods.SystemManifests (or OtherWorkloads) so their requests count against the surge: ") {
      (NodePods.WorkloadManifests -- NodePods.CountedManifests) shouldBe empty
    }
    withClue("the ledger names manifests that schedule no pod (renamed or removed?): ") {
      (NodePods.CountedManifests -- NodePods.WorkloadManifests) shouldBe empty
    }
  }

  it should "count every k3s add-on that is left on" in {
    withClue("k3s-server.nix disables an add-on the ledger does not know (a typo, or a new k3s add-on to add a row for): ") {
      (NodePods.K3sDisabled -- NodePods.K3sAddOns.keySet) shouldBe empty
    }
    NodePods.EnabledAddOns.map(_._1) should contain("coredns")
  }

  it should "give every system manifest a request of each resource" in {
    Seq(NodePods.Memory, NodePods.Cpu).foreach { resource =>
      NodePods.SystemManifests.foreach { case (name, path) =>
        withClue(s"$name declares no ${resource.key} request in $path: ")(NodePods.requestsIn(path, resource).sum should be > 0)
      }
      info(s"${resource.key}: ${NodePods.systemPods(resource).map { case (n, pods) => s"$n ${pods.sum}${resource.unit}" }.mkString(", ")}")
    }
  }

  // The same list, so the same reserve: what the two ledgers subtract for system pods is computed
  // from one pod list, never a constant either spec keeps.
  it should "count the same pods for memory as for cpu" in {
    NodePods.systemPods(NodePods.Memory).map(p => p._1 -> p._2.size) shouldBe
      NodePods.systemPods(NodePods.Cpu).map(p => p._1 -> p._2.size)
  }
}
