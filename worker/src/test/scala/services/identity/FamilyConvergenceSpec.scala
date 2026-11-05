package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The per-family convergence check ([[FamilyConvergence]]) over committed slices of known flips: it finds them by the
 *  lattice -- Lichtblick "Buddies", a final-take flip the drift seeds needed luck for, and Quad Derby's "Oxide Ghosts" --
 *  and a second run over its cache walks nothing again, unless a read of a family's closure moved. */
class FamilyConvergenceSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {
  private val slices  = Path.of("test", "resources", "fixtures", "drift-slices")
  private val buddies = slices.resolve("de/programmkino-lichtblick-e-v-buddies-69181215")
  private val oxide   = slices.resolve("uk/quad-derby-oxide-ghosts-the-brass-eye-tapes-q-a-18--7c64f181")
  private val options = FamilyConvergence.Options(threads = 2)

  private lazy val first = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), Map.empty, options)

  "The per-family check" should "find Lichtblick's Buddies flip in the final take, which the model alone does not take" in {
    first.walked shouldBe first.verdicts.size
    val flips = first.flips.map(_._2)
    flips.filter(f => f.stage == "final" && f.listing.startsWith("Programmkino Lichtblick")) should not be empty
  }

  it should "find Quad Derby's Oxide Ghosts flip" in {
    val run = FamilyConvergence.check(FamilyConvergence.sliceCorpus(oxide, "rules-1"), Map.empty, options)
    run.flips.map(_._2.listing).exists(_.startsWith("Quad Derby")) shouldBe true
  }

  it should "reuse every cached verdict whose key and closure stand, and walk again one whose closure moved or whose rules did" in {
    val cache = first.verdicts.map(v => v.key -> v).toMap
    val again = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), cache, options)
    again.walked shouldBe 0
    again.verdicts.map(_.flips) shouldBe first.verdicts.map(_.flips)
    // a closure read recorded otherwise than it reads now: that family is walked again
    val walkedOnce = first.verdicts.head
    val moved = cache.updated(walkedOnce.key, walkedOnce.copy(closure = FamilyConvergence.Read("context", "spread|buddies", 0L) +: walkedOnce.closure))
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), moved, options).walked shouldBe 1
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-2"), cache, options).walked shouldBe first.verdicts.size
  }

  it should "hold in a family's closure the answers only a sub-population asks, and walk it again when one reads otherwise" in {
    // Kinepolis "Piesek": a subset of the family asks Filmweb of records no settle of the whole family files -- the
    // closure keeps them by what they read asked alone, and a cache read under another answer is walked again
    val piesek = FamilyConvergence.sliceCorpus(slices.resolve("pl/cinema-city-kinepolis-piesek-1d2222d6"), "rules-1")
    val run    = FamilyConvergence.check(piesek, Map.empty, options)
    val asked  = run.verdicts.filter(_.closure.exists(_.kind == "answer"))
    asked should not be empty
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(slices.resolve("pl/cinema-city-kinepolis-piesek-1d2222d6"), "rules-1"),
      run.verdicts.map(v => v.key -> v).toMap, options).walked shouldBe 0
    val one   = asked.head
    val moved = one.copy(closure = one.closure.map(r => if (r.kind == "answer") r.copy(digest = r.digest + 1) else r))
    FamilyConvergence.check(FamilyConvergence.sliceCorpus(slices.resolve("pl/cinema-city-kinepolis-piesek-1d2222d6"), "rules-1"),
      run.verdicts.map(v => v.key -> v).toMap.updated(one.key, moved), options).walked shouldBe 1
  }

  "A corpus run's shards" should "split its families, each walked by exactly one, and its flips be the backlog's rows" in {
    // a CI row walks one shard of a country (scripts/ci/identity-family-check.sh): together they walk every family once
    val corpus = FamilyConvergence.sliceCorpus(slices.resolve("uk/nerve-centre-londonderry-the-ladykillers-039701b7"), "rules-1")
    val whole  = FamilyConvergence.check(corpus, Map.empty, options)
    val parts  = (1 to 3).map(i => FamilyConvergence.check(corpus, Map.empty, options.copy(shard = Some(i -> 3))))
    parts.flatMap(_.verdicts.map(_.family)).sorted shouldBe whole.verdicts.map(_.family).sorted
    parts.flatMap(_.flips.map(_._2)).toSet shouldBe whole.flips.map(_._2).toSet
    // identity-flip-backlog.tsv's shape, kind `family`: one row a listing, its venue and raw title apart
    val rows = FamilyConvergence.flipsTsv("uk", whole).linesIterator.toSeq
    rows.head shouldBe "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote"
    val ladykillers = rows.tail.map(_.split("\t", -1).toSeq).find(_(5) == "The Ladykillers | Cinematic Breakfast")
    ladykillers.map(r => (r(0), r(1), r(4))) shouldBe Some(("family", "uk", "Nerve Centre Londonderry"))
  }

  "A cache" should "read back what it wrote" in {
    val file = tempFile("family-convergence", ".json")
    FamilyConvergence.save(file, first.verdicts)
    FamilyConvergence.load(file).values.toSeq.sortBy(_.family) shouldBe first.verdicts.sortBy(_.family)
  }
}
