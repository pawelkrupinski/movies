package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RunScopedDatabaseNameSpec extends AnyFlatSpec with Matchers {

  private val pid = ProcessHandle.current().pid()

  "a run-scoped name" should "carry this run's pid where the sweep reads it back" in {
    RunScopedDatabaseName.owner(RunScopedDatabaseName.forThisRun("kinowo_stamped-rows")) shouldBe Some(pid)
    RunScopedDatabaseName.owner(RunScopedDatabaseName.fresh("kinowo_isolated_cut_pl")) shouldBe Some(pid)
    RunScopedDatabaseName.owner(RunScopedDatabaseName.fresh("kinowo_it_sharedusers") + "_users") shouldBe Some(pid)
    RunScopedDatabaseName.forThisRun("x") shouldBe RunScopedDatabaseName.forThisRun("x")
    RunScopedDatabaseName.fresh("x") should not be RunScopedDatabaseName.fresh("x")
  }

  it should "read the owner of an isolated database named before the marker" in {
    RunScopedDatabaseName.owner("kinowo_isolated_hc_pl_p0_76415_175368434956833") shouldBe Some(76415L)
  }

  "the orphan sweep" should "pick only run-scoped databases whose run has ended" in {
    val names = Seq(
      "kinowo_stamped-rows_pid111", "kinowo_isolated_cut_pl_pid111_998", "kinowo_isolated_hc_pl_p0_111_175368434956833",
      "kinowo_stamped-rows_pid222", "kinowo_isolated_cut_pl_pid222_998",
      // Never a run's: a developer's corpus, a mirror, numbers that are not a pid marker.
      "kinowo", "kinowo_de_prod_mirror", "kinowo_it_59", "kinowo_rapid12", "kinowo_pidgin_3")
    RunScopedDatabaseName.orphans(names, alive = _ == 222L) shouldBe Seq(
      "kinowo_stamped-rows_pid111", "kinowo_isolated_cut_pl_pid111_998", "kinowo_isolated_hc_pl_p0_111_175368434956833")
  }

  it should "sweep only a server on this machine's loopback, where a dead pid is a dead run" in {
    Seq("mongodb://127.0.0.1:28017/?directConnection=true", "mongodb://localhost:28017", "mongodb://[::1]:27017/kinowo",
        "mongodb://127.0.0.1:27017,localhost:27018/?replicaSet=rs0")
      .foreach(uri => withClue(uri)(RunScopedDatabaseName.isLoopbackOnly(uri) shouldBe true))
    // Another machine's runs live in another pid space: a pid dead here may be its live run.
    Seq("mongodb://ci-mongo.internal:27017", "mongodb://user:pw@127.0.0.1.example.com:27017",
        "mongodb+srv://cluster0.example.mongodb.net", "mongodb://127.0.0.1:27017,db2.example.com:27017", "")
      .foreach(uri => withClue(uri)(RunScopedDatabaseName.isLoopbackOnly(uri) shouldBe false))
  }
}
