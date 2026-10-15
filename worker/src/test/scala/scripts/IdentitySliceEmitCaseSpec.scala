package scripts

import models.Country
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.UnmatchedClusters.Label

import java.nio.file.Files
import java.time.Instant

/** identity-slice --emit-case: a pinned case replays as the slice it was cut from decided, so it carries every answer that
 *  decision read — the recording's, and prod's filed family answers beside them. Without the filed ones the replay's
 *  agreement asks its families afresh (a director's Wikidata search, an IMDb name search) and the case lacks answers. */
class IdentitySliceEmitCaseSpec extends AnyFlatSpec with Matchers {

  private val uk   = Country.all.find(_.code == "uk").get
  private val seed = ListingKey.Published("Picture House Uckfield", "Gremlins Special Event", None, Nil)
  private val family = new BsonDocument("family", new BsonString("wiki")).append("answer", new BsonString("Q1"))

  "An emitted case" should "keep the filed family answers its slice's decision read" in {
    val root   = Files.createTempDirectory("identity-slice-emit")
    val from   = root.resolve("slice")
    val target = root.resolve("case")
    val filed  = tools.FiledAnswers(tools.FiledAnswers.of(uk).capture.copy(families = Map("wiki|joe dante" -> family)), "identity-unmatched/uk.json.gz")
    IdentitySlice.writeSlice(from, IdentitySlice.Slice(uk, "recording", Instant.EPOCH, "a selector", Set(seed), Nil, Nil, Map.empty, Seq("ref"), Nil, filed))

    IdentitySlice.emitCase(IdentitySlice.readSlice(from), from, Seq(Label(uk.code, "*", seed.rawTitle, "tmdb:927", right = true, "Gremlins")), target)

    val replayed = IdentitySlice.readSlice(target).filed
    replayed.families shouldBe filed.families
    replayed.source shouldBe filed.source
  }

  it should "name its filed answers from the checkout they were read in, wherever the slice ran" in {
    // `--ref` runs the slice in the ref's checkout, while `--families` names the capture of the checkout the tool was
    // called from: its absolute path was kept ("/Users/…/movies-idc-4/test/resources/…").
    val root     = Files.createTempDirectory("identity-slice-emit")
    val checkout = Files.createDirectories(root.resolve("caller"))
    Files.writeString(checkout.resolve(".git"), "gitdir: elsewhere\n")
    val capture  = Files.createDirectories(checkout.resolve("test/resources/fixtures/identity-unmatched")).resolve("uk.json.gz")
    val from     = root.resolve("slice")
    val filed    = tools.FiledAnswers(tools.FiledAnswers.of(uk).capture.copy(families = Map("wiki|joe dante" -> family)), s"prod mirror x (over $capture)")
    IdentitySlice.writeSlice(from, IdentitySlice.Slice(uk, "recording", Instant.EPOCH, "a selector", Set(seed), Nil, Nil, Map.empty, Seq("ref"), Nil, filed))

    IdentitySlice.checkoutOf(capture) shouldBe Some(checkout.toAbsolutePath.normalize)
    IdentitySlice.emitCase(IdentitySlice.readSlice(from), from, Seq(Label(uk.code, "*", seed.rawTitle, "tmdb:927", right = true, "Gremlins")),
      root.resolve("case"), IdentitySlice.checkoutOf(capture).toSeq)

    IdentitySlice.readSlice(root.resolve("case")).filed.source shouldBe "prod mirror x (over test/resources/fixtures/identity-unmatched/uk.json.gz)"
  }
}
