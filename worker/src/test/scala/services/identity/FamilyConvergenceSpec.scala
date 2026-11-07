package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The per-family convergence check ([[FamilyConvergence]]) over committed slices of known flips: it finds them by the
 *  lattice -- Filmwelt Grünstadt's "Titans", a final-take arrival flip, and CinemaxX's bare "Titans" in the model -- names
 *  none where a departure is only ever a kept listing (Lichtblick "Buddies", Quad Derby's "Oxide Ghosts": add/remove flips
 *  of a departure read as an absence), and a second run over its cache walks nothing again, unless a read of a family's
 *  closure moved. */
class FamilyConvergenceSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {
  private val slices  = Path.of("test", "resources", "fixtures", "drift-slices")
  private val buddies = slices.resolve("de/programmkino-lichtblick-e-v-buddies-69181215")
  private val oxide   = slices.resolve("uk/quad-derby-oxide-ghosts-the-brass-eye-tapes-q-a-18--7c64f181")
  private val titans  = slices.resolve("de/dammtor-titans-0bd19a4c")
  private val options = FamilyConvergence.Options(threads = 2)

  private lazy val first = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), Map.empty, options)

  private lazy val titansRun = FamilyConvergence.check(FamilyConvergence.sliceCorpus(titans, "rules-1"), Map.empty, options)

  "The per-family check" should "find Filmwelt Grünstadt's Titans flip in the final take, which the model alone does not take" in {
    val flips = titansRun.flips.map(_._2).filter(_.listing.startsWith("Filmwelt Grünstadt"))
    flips.map(_.stage) shouldBe Seq("final")
    flips.map(_.kind) shouldBe Seq("arrival")
  }

  it should "find the bare CinemaxX Titans arrival flip in the model" in {
    titansRun.flips.map(_._2).exists(f => f.stage == "model" && f.kind == "arrival" && f.listing.startsWith("CinemaxX Dammtor")) shouldBe true
  }

  // A departure is no absence: the worker keeps a departed listing a billed one relates to by title, through the very
  // function the walk keeps it by (ListingRetention.retained; IdentityModelServiceSpec holds the worker to it, whatever the
  // order of leaves and arrivals). Buddies and Oxide Ghosts flipped only when a listing that had arrived left again.
  it should "name no add/remove flip where the departed listing is kept as the worker keeps it (Buddies, Oxide Ghosts)" in {
    first.walked shouldBe first.verdicts.size
    first.flips.map(_._2).filter(_.listing.startsWith("Programmkino Lichtblick")) shouldBe empty
    val run = FamilyConvergence.check(FamilyConvergence.sliceCorpus(oxide, "rules-1"), Map.empty, options)
    run.flips.map(_._2).filter(_.listing.startsWith("Quad Derby")) shouldBe empty
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

  it should "with a change's reach, walk again exactly the families a reached rule decided, the rest from the cache" in {
    // option 1 (the owner, 2026-11-06): the cache kept under rules naming no code, a commit's reach
    // (scripts/ci/family_reach.py) re-walking only the families whose recorded rules it matches
    val cache   = first.verdicts.map(v => v.key -> v).toMap
    def run(reach: Seq[String]) = FamilyConvergence.check(FamilyConvergence.sliceCorpus(buddies, "rules-1"), cache, options.copy(reach = Some(reach)))
    first.verdicts.foreach(_.rules should not be empty)
    run(Nil).walked shouldBe 0
    run(Seq("*")).walked shouldBe first.verdicts.size
    val rule = first.verdicts.head.rules.head
    run(Seq(rule)).walked shouldBe first.verdicts.count(v => FamilyConvergence.reaches(v.rules, Seq(rule)))
    run(Seq("accept:no-such-rule")).walked shouldBe 0
  }

  // A departure is no absence: the worker keeps a departed listing a billed one relates to by title
  // (ListingRetention.retained), and it decides as when billed. The walk took a departure for an absence and named an
  // add/remove flip wherever a listing's arrival moved a take: 144 DE and UK flips of the explorer, none reachable
  // (convergence run 42358278155). Listing 0 takes A alone and B beside listing 1.
  "An add/remove flip" should "be named only where listing 1's departure, kept as the worker keeps it, gives A back" in {
    val everyone = FamilyConvergence.Subset(0, 1)
    val subsets  = Seq(FamilyConvergence.Subset(0), everyone)
    def take(m: FamilyConvergence.Subset) = Some(if (m(1)) "tmdb:2" else "tmdb:1")
    def label(m: FamilyConvergence.Subset) = m.toSeq.map(i => s"v$i").mkString("{", ", ", "}")
    // kept on departure, it decides as when billed: B stays, no flip
    FamilyConvergence.flipOf(0, everyone, subsets, take, (_, _) => Some("tmdb:2"), label) shouldBe None
    // nothing keeps it (no billed listing relates to it): its departure is an absence, and the take goes back to A
    FamilyConvergence.flipOf(0, everyone, subsets, take, (m, _) => take(m), label) shouldBe
      Some("add/remove" -> "tmdb:1@{v0} -> tmdb:2@{v0, v1} -> tmdb:1@{v0} + {v1} departed")
  }

  "A corpus run's shards" should "split its families, each walked by exactly one, and its flips be the backlog's rows" in {
    // a CI row walks one shard of a country (scripts/ci/identity-family-check.sh): together they walk every family once
    val corpus = FamilyConvergence.sliceCorpus(titans, "rules-1")
    val whole  = FamilyConvergence.check(corpus, Map.empty, options)
    val parts  = (1 to 3).map(i => FamilyConvergence.check(corpus, Map.empty, options.copy(shard = Some(i -> 3))))
    parts.flatMap(_.verdicts.map(_.family)).sorted shouldBe whole.verdicts.map(_.family).sorted
    parts.flatMap(_.flips.map(_._2)).toSet shouldBe whole.flips.map(_._2).toSet
    // identity-flip-backlog.tsv's shape, kind `family`: one row a listing, its venue and raw title apart
    val rows = FamilyConvergence.flipsTsv("de", whole).linesIterator.toSeq
    rows.head shouldBe "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote"
    val dammtor = rows.tail.map(_.split("\t", -1).toSeq).find(r => r(4) == "CinemaxX Dammtor" && r(5) == "Titans")
    dammtor.map(r => (r(0), r(1))) shouldBe Some(("family", "de"))
  }

  "Balanced shards" should "put the costliest family alone and fill the lightest row first, the same every time" in {
    val costed = Seq("a" -> 100.0, "b" -> 30.0, "c" -> 30.0, "d" -> 30.0, "e" -> 10.0)
    FamilyConvergence.balance(costed, 2) shouldBe Vector(1, 2, 2, 2, 2)
    FamilyConvergence.balance(costed.reverse, 2).reverse shouldBe FamilyConvergence.balance(costed, 2)
    FamilyConvergence.balance(Seq("x" -> 1.0), 3) shouldBe Vector(1)
  }

  it should "cost an unrecorded family by what a member costs in the recorded ones, and a sampled one by the orders asked" in {
    val file = tempFile("family-walks", ".tsv")
    java.nio.file.Files.writeString(file,
      "family\ttier\tsubsets\tseconds\tflips\trules\nbig\t8 orders x 8 steps\t57\t80.0\t0\t\nsmall\tlattice 2^3\t8\t2.5\t0\t\n")
    FamilyConvergence.loadCosts(file, orders = 2) shouldBe Map("big" -> 20.0, "small" -> 2.5)
    FamilyConvergence.loadCosts(file, orders = 8) shouldBe Map("big" -> 80.0, "small" -> 2.5)
  }

  it should "still walk every family once, whatever the costs" in {
    val corpus = FamilyConvergence.sliceCorpus(titans, "rules-1")
    val whole  = FamilyConvergence.check(corpus, Map.empty, options)
    val costs  = whole.verdicts.zipWithIndex.map { case (v, i) => v.family -> (i + 1.0) * 10 }.toMap
    val parts  = (1 to 2).map(i => FamilyConvergence.check(corpus, Map.empty, options.copy(shard = Some(i -> 2), costs = costs)))
    parts.flatMap(_.verdicts.map(_.family)).sorted shouldBe whole.verdicts.map(_.family).sorted
  }

  "A cache" should "read back what it wrote" in {
    val file = tempFile("family-convergence", ".json")
    FamilyConvergence.save(file, first.verdicts)
    FamilyConvergence.load(file).values.toSeq.sortBy(_.family) shouldBe first.verdicts.sortBy(_.family)
  }
}
