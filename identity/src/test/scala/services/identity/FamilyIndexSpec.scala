package services.identity

import models.{Helios, KinoMuza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** The family index's query side ([[FamilyIndex]]): which family holds a listing, which families read a lookup or a
 *  context fact, in which a rule fired, its content digest, and which entries a moved input leaves standing. Its
 *  completeness over real slices is the worker's `FamilyIndexCompletenessSpec`. */
class FamilyIndexSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val dune  = FilmTable.F(438631, "Dune", 2021, "Denis Villeneuve", 155)
  private val alien = FilmTable.F(348, "Alien", 1979, "Ridley Scott", 117)
  private val listings = Seq(
    FilmTable.listing(KinoMuza, "Dune", Some(2021), Some("Denis Villeneuve"), Some(155)),
    FilmTable.listing(Helios, "Dune", Some(2021), Some("Denis Villeneuve"), Some(155)),
    FilmTable.listing(KinoMuza, "Alien", Some(1979), Some("Ridley Scott"), Some(117)),
    FilmTable.listing(Helios, "Alien", Some(1979), Some("Ridley Scott"), Some(117)))
  private def built(films: Seq[FilmTable.F]) =
    FamilyIndex.build(listings, new FilmTable(films, normalizer), normalizer, ResolveInputs.Unpinned)
  private val base = built(Seq(dune, alien))
  private def familyOf(title: String) = base.index.familyOf(listings.find(_.title == title).get.key).get

  "A family index" should "hold one entry per family, each member's take and the rules that decided it" in {
    base.apart shouldBe empty
    base.index.entries should have size 2
    familyOf("Dune").members.keySet shouldBe listings.filter(_.title == "Dune").map(_.key).toSet
    familyOf("Dune").takes.values.toSet shouldBe Set(Some("tmdb:438631"))
    familyOf("Alien").takes.values.toSet shouldBe Set(Some("tmdb:348"))
    val rule = familyOf("Dune").rules.values.flatten.find(_.startsWith("accept:")).get
    base.index.firing(rule) should contain(familyOf("Dune"))
  }

  it should "name the families that read a lookup or a context fact" in {
    // its title's search, asked of the lookups; the record it found, read off the corpus's context
    val searches = familyOf("Dune").answers.keySet.collect { case d @ Dependency.Lookup(_, _, Question.Candidate(_)) => d }
    searches should not be empty
    searches.foreach(search => base.index.readersOf(search) shouldBe Seq(familyOf("Dune")))
    base.index.readersOf(FamilyIndex.ContextRead.Record(438631)) shouldBe Seq(familyOf("Dune"))
  }

  it should "digest equal reads equally, and read again as standing against its own corpus" in {
    built(Seq(dune, alien)).index.entries.map(_.digest) shouldBe base.index.entries.map(_.digest)
    base.index.stale(FamilyIndex.now(listings, new FilmTable(Seq(dune, alien), normalizer), normalizer, ResolveInputs.Unpinned, base)) shouldBe empty
  }

  it should "leave standing exactly the families whose reads did not move" in {
    // Dune's record moves (its runtime), as the corpus's context answers it; Alien read nothing of it
    val moved = Seq(dune.copy(runtime = 156), alien)
    val stale = base.index.stale(FamilyIndex.now(listings, new FilmTable(moved, normalizer), normalizer, ResolveInputs.Unpinned, built(moved)))
    stale.map(_._1) shouldBe Seq(familyOf("Dune"))
    stale.head._2 should contain(FamilyIndex.ContextRead.Record(438631).id)
  }

  it should "replay a family from its recorded reads alone, and refuse a read it does not hold" in {
    val kept = FamilyIndex.build(listings, new FilmTable(Seq(dune, alien), normalizer), normalizer, ResolveInputs.Unpinned, values = true)
    kept.index.entries.zip(kept.recorded).foreach { case (entry, recorded) =>
      FamilyIndex.replayAlone(recorded, normalizer, ResolveInputs.Unpinned) shouldBe entry.takes
    }
    val dunes = kept.recorded.find(_.members.exists(_.title == "Dune")).get
    an[FamilyIndex.Unindexed] should be thrownBy FamilyIndex.replayAlone(dunes.copy(answers = Map.empty), normalizer, ResolveInputs.Unpinned)
  }

  it should "name each context read by an id that parses back to it" in {
    val readsAll = base.index.entries.flatMap(_.context.keys)
    readsAll should not be empty
    readsAll.foreach(read => FamilyIndex.ContextRead.parse(read.id) shouldBe Some(read))
  }

  it should "digest a member as the model reads it: a showtime or a poster moves nothing, a year does" in {
    val dune   = listings.head
    val digest = FamilyIndex.modelDigest(dune)
    FamilyIndex.modelDigest(dune.copy(poster = Some("https://example.org/p.jpg"))) shouldBe digest
    FamilyIndex.modelDigest(dune.copy(year = Some(2022))) should not be digest
  }
}
