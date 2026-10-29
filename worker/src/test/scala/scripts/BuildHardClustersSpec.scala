package scripts

import tools.ScratchDirectories
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HardClusters.{Reason, Seed}

import java.nio.file.Files

/** What the hard-cluster ratchet adds: the findings it reads, the ones it already holds, and the corpora it grows from. */
class BuildHardClustersSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  import BuildHardClusters._

  private def finding(code: String, title: String) = Seed(code, title, Reason.Finding.label)

  "A titles file" should "read each code and title as a finding, a line with no title as none" in {
    findings(Seq("DE\t Lalka \textra", "pl\t", "uk", "es\tLa sociedad de la nieve")) shouldBe
      Seq(finding("de", "Lalka"), finding("es", "La sociedad de la nieve"))
  }

  "The fresh findings" should "be the ones no seed holds, each once" in {
    fresh(Seq(finding("de", "Lalka"), finding("de", "Lalka"), finding("pl", "Lalka"), finding("de", "Heidi")),
      known = Seq(Seed("de", "Heidi", Reason.Franchise.label))) shouldBe Seq(finding("de", "Lalka"), finding("pl", "Lalka"))
  }

  "The extensions" should "grow each country from its own corpus, and skip a country with none or a code no country has" in {
    val dir = tempDirectory("hard-clusters")
    try {
      val de = Files.writeString(dir.resolve("cinema-scrapes-de.json.gz"), "")
      corporaIn(dir.toString) shouldBe Seq(models.Country.Germany -> de)
      extensions(Seq(finding("pl", "Lalka"), finding("de", "Heidi"), finding("xx", "Lalka"), finding("de", "Lalka")), dir.toString) shouldBe Seq(
        ("de", Some(models.Country.Germany -> de), Seq(finding("de", "Heidi"), finding("de", "Lalka"))),
        ("pl", None, Seq(finding("pl", "Lalka"))),
        ("xx", None, Seq(finding("xx", "Lalka"))))
    } finally { Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_)) }
  }

  "Its command line" should "read both of the hard-cluster script's commands, and refuse an extend missing its titles" in {
    BuildHardClusters.commandLine.parse(Seq("select", "corpora", "40")).toOption.map(_._2.positionalInt("budget")) shouldBe Some(Some(40))
    BuildHardClusters.commandLine.accepts(Seq("extend", "corpora", "findings.tsv")).isRight shouldBe true
    an[tools.CommandLine.InvalidArguments] should be thrownBy
      BuildHardClusters.commandLine.parse(Seq("extend", "corpora")).toOption.get._2.requiredPositional("titles")
    BuildHardClusters.commandLine.accepts(Seq("prune", "corpora")).isLeft shouldBe true
  }
}
