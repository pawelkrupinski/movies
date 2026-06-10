package services.titlerules

import org.bson.BsonDocument
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** In-memory round-trip through `TitleRuleCodecs` — the storage-shape guardrail
 *  for `StoredTitleRule`, with no Mongo connection. */
class StoredTitleRuleCodecSpec extends AnyFlatSpec with Matchers {

  private val codec: Codec[StoredTitleRule] = TitleRuleCodecs.registry.get(classOf[StoredTitleRule])

  private def roundTrip(s: StoredTitleRule): StoredTitleRule = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), s, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  "StoredTitleRule" should "round-trip a Search rule with a tag and None note" in {
    val r = StoredTitleRule.fromDomain(
      TitleRule("search-prog", RuleScope.Search, None, "(?i)^Klub: ", "",
        applyAll = false, order = 10, enabled = true, tag = Some("programmePrefix")))
    val back = roundTrip(r)
    back shouldBe r
    StoredTitleRule.toDomain(back).map(_.scope) shouldBe Some(RuleScope.Search)
  }

  it should "round-trip a per-cinema rule carrying a cinemaId and a replacement" in {
    val r = StoredTitleRule.fromDomain(
      TitleRule("bok-pipe", RuleScope.PerCinema, Some("bok"), """\s*\|\s*""", ": ",
        applyAll = true, order = 10, note = Some("pipe → colon")))
    roundTrip(r) shouldBe r
  }

  it should "decode an unknown scope to None rather than throwing" in {
    StoredTitleRule.toDomain(
      StoredTitleRule("z", "BogusScope", None, "x", "", applyAll = false, order = 0,
        enabled = true, tag = None, note = None)) shouldBe None
  }
}
