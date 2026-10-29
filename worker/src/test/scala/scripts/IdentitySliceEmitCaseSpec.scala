package scripts

import tools.ScratchDirectories
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
class IdentitySliceEmitCaseSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private val uk   = Country.all.find(_.code == "uk").get
  private val seed = ListingKey.Published("Picture House Uckfield", "Gremlins Special Event", None, Nil)
  private val family = new BsonDocument("family", new BsonString("wiki")).append("answer", new BsonString("Q1"))

  "An emitted case" should "keep the filed family answers its slice's decision read" in {
    val root   = tempDirectory("identity-slice-emit")
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
    val root     = tempDirectory("identity-slice-emit")
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

  /** A checkout (a `.git` file, as a worktree has) holding an unmatched-cluster capture at the repository's path. */
  private def checkoutWithCapture(root: java.nio.file.Path, name: String): java.nio.file.Path = {
    val checkout = Files.createDirectories(root.resolve(name))
    Files.writeString(checkout.resolve(".git"), "gitdir: elsewhere\n")
    Files.createDirectories(checkout.resolve("test/resources/fixtures/identity-unmatched")).resolve("uk.json.gz")
  }

  private def sliceFiled(source: String) =
    tools.FiledAnswers(tools.FiledAnswers.of(uk).capture.copy(families = Map("wiki|joe dante" -> family)), source)

  it should "be pinned under --emit-case, its filed answers named from the checkout --families is in" in {
    val root    = tempDirectory("identity-slice-emit")
    val capture = checkoutWithCapture(root, "caller")
    val o = IdentitySlice.parse(List("uk", "--families", capture.toString, "--emit-case", root.resolve("cases").toString,
      "--key", s"${seed.venue}|${seed.rawTitle}"))
    val selector = o.selectors.head
    val from     = root.resolve("slice")
    // FiledAnswers.ofCapture names the capture by the path it was handed: identity-slice's absolute --families
    IdentitySlice.writeSlice(from, IdentitySlice.Slice(uk, "recording", Instant.EPOCH, selector.label, Set(seed), Nil, Nil, Map.empty, Seq("ref"), Nil,
      sliceFiled(capture.toString)))

    IdentitySlice.emitInto(IdentitySlice.readSlice(from), from, Seq(Label(uk.code, "*", seed.rawTitle, "tmdb:927", right = true, "Gremlins")),
      o, uk, selector)

    IdentitySlice.readSlice(root.resolve("cases").resolve(s"uk-${selector.directoryName}")).filed.source shouldBe
      "test/resources/fixtures/identity-unmatched/uk.json.gz"
  }

  it should "be cut apart from the slice another checkout left in the shared work dir" in {
    // The shared work dir (one per recording, every checkout's) kept another checkout's slice, its filed answers named
    // from that checkout; extract merges whatever it finds there, so an emitted case carried them and their path.
    val root   = tempDirectory("identity-slice-emit")
    val cache  = root.resolve("cache")
    val other  = checkoutWithCapture(root, "another-checkout")
    val o      = IdentitySlice.parse(List("uk", "--emit-case", root.resolve("cases").toString, "--key", s"${seed.venue}|${seed.rawTitle}"))
    val selector = o.selectors.head
    val shared = cache.resolve("slices").resolve(selector.directoryName)
    IdentitySlice.writeSlice(shared, IdentitySlice.Slice(uk, "recording", Instant.EPOCH, selector.label, Set(seed), Nil, Nil, Map.empty, Seq("ref"), Nil,
      sliceFiled(other.toString)))

    IdentitySlice.inWorkDir(o.copy(emitCase = None), cache, selector)(identity) shouldBe shared // the positive control
    val (cutIn, leftover) = IdentitySlice.inWorkDir(o, cache, selector)(dir => dir -> Files.exists(dir.resolve("rows.json.gz")))
    cutIn should not be shared
    leftover shouldBe false
    Files.exists(cutIn.getParent) shouldBe false // removed once the case is copied out
  }
}
