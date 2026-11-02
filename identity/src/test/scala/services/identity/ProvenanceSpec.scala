package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * THE EVIDENCE PROVENANCE TABLE ([[Authority]]; docs/investigations/2026-11-01-flip-unification.md, stage 3): what each
 * kind of evidence may do is said once, and every rule site reads it. Two readers deriving it apart is how UK Keswick
 * flipped (#629: the group vote spared a crediting node a sibling's "not the work" denial, the sibling withdrawal did not)
 * and how DE "Eternity" took the wrong film (871125cf7: a kept listing backed a present listing's feed link toward its
 * overrule). The behaviour of each row is held by its case specs (`KeptOverruledApartSpec`, the Keswick resolver cases
 * and slices); here, the table's rows themselves and that no rule site reads provenance past it.
 */
class ProvenanceSpec extends AnyFlatSpec with Matchers {
  import Authority.{Act, Provenance}

  "The provenance table" should "let a kept listing back, corroborate and withdraw as it did present, and never overrule a link" in {
    Authority.may(Provenance.Kept, Act.Back) shouldBe true
    Authority.may(Provenance.Kept, Act.Corroborate) shouldBe true
    Authority.may(Provenance.Kept, Act.Withdraw) shouldBe true
    Authority.may(Provenance.Kept, Act.Overrule) shouldBe false       // DE "Eternity", 871125cf7
    Authority.may(Provenance.Kept, Act.JoinOverruled) shouldBe false  // KeptOverruledApart
    Provenance.values.filterNot(_ == Provenance.Kept).foreach(p => withClue(p)(Authority.may(p, Act.Overrule) shouldBe true))
  }

  it should "count a backing entry only where its provenance may" in {
    val kept    = Backer.of(FilmTable.listing(models.KinoMuza, "Eternity").copy(kept = true))
    val present = Backer.of(FilmTable.listing(models.KinoMuza, "Eternity"))
    Authority.counts(kept, Act.Overrule) shouldBe false
    Authority.counts(kept, Act.Corroborate) shouldBe true
    Authority.counts(present, Act.Overrule) shouldBe true
  }

  // the identity code reading provenance -- a listing's kept mark, a backer's, a denial's credit or house -- anywhere but
  // the table: a rule re-deriving what the table says (Keswick #629's siblingDenying beside ClusterVoting.onlyWithdrawn)
  "Every rule site" should "read provenance through the table, never re-derive it" in {
    val root  = Path.of("identity", "src", "main", "scala", "services", "identity")
    val files = Files.walk(root).iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq
    // where a provenance is DEFINED or carried, not weighed: the table, the listing and its codec, retention's mark, the
    // backer's key, the denials' kinds and the scoring that makes them
    val definers = Set("Authority.scala", "IdentityModel.scala", "ListingBson.scala", "ListingRetention.scala", "VenueBacking.scala",
      "Denial.scala", "CandidateScoring.scala")
    val Reads = Seq(
      "a listing's kept mark"           -> """(?<![\w.])(_|l|listing|node\.listings\.head)\.kept\b""".r,
      "a backer's kept mark"            -> """Backer\.kept\(""".r,
      "a credit sparing a denial"       -> """creditOverrules\s*&&|NotTheWork\(creditOverrules = true\)|NotTheWork\(true\)""".r,
      "a house sparing a denial"        -> """_\.kind == Denial\.Kind\.AnotherHousesSeason""".r,
      "a present-only flag"             -> """presentOnly""".r)
    files.size should be > 100
    val found = for {
      file <- files if !definers(file.getFileName.toString)
      (line, n) <- Files.readAllLines(file, StandardCharsets.UTF_8).asScala.zipWithIndex
      code = line.replaceAll("""//.*$""", "") if !code.trim.startsWith("*") && !code.trim.startsWith("/**")
      (what, pattern) <- Reads if pattern.findFirstIn(code).isDefined
    } yield s"${file.getFileName}:${n + 1} reads $what past the table: ${line.trim}"
    found shouldBe empty
  }

  "A withdrawal row" should "spare a node crediting the film's director a 'not the work' denial a credit overrules, and no other" in {
    // UK Keswick (#629): the row both the sibling withdrawal and the group vote read
    def scored(directors: Seq[String]) = {
      val record = FilmTable.F(1462368, "These Violent Delights", 2026, "Christopher Hampson", 70, 0.6)
      val l      = FilmTable.listing(models.KinoMuza, "Scottish Ballet: These Violent Delights", None, directors.headOption, Some(95))
      val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
      val stages = new IdentityResolver.Stages(Seq(l), new FilmTable(Seq(record), normalizer), normalizer, IdentityCalibration.resolver,
        IdentityResolver.Mutation.None, PinConstraints(Nil), TitleDecorations.None)
      val node   = stages.generation.nodes.head
      stages.families.scopeOf(node).of(node).find(_.candidate.tmdbId == 1462368).get
    }
    val credited   = scored(Seq("Christopher Hampson"))
    val uncredited = scored(Nil)
    Authority.spares(credited, Denial.Kind.NotTheWork(creditOverrules = true)) shouldBe defined
    Authority.spares(credited, Denial.Kind.NotTheWork(creditOverrules = false)) shouldBe empty
    Authority.spares(uncredited, Denial.Kind.NotTheWork(creditOverrules = true)) shouldBe empty
  }
}
