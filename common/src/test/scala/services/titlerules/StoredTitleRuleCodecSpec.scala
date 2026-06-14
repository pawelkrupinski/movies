package services.titlerules

import org.bson.BsonDocument
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonDocumentReader, BsonDocumentWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** In-memory round-trip through `TitleRuleCodecs` — the storage-shape guardrail
 *  for `StoredTitleRuleRecord` (and its nested `StoredRule`), with no Mongo
 *  connection. */
class StoredTitleRuleCodecSpec extends AnyFlatSpec with Matchers {

  private val codec: Codec[StoredTitleRuleRecord] =
    TitleRuleCodecs.registry.get(classOf[StoredTitleRuleRecord])

  private def roundTrip(s: StoredTitleRuleRecord): StoredTitleRuleRecord = {
    val out = new BsonDocument()
    codec.encode(new BsonDocumentWriter(out), s, EncoderContext.builder().build())
    codec.decode(new BsonDocumentReader(out), DecoderContext.builder().build())
  }

  "StoredTitleRuleRecord" should "round-trip a global record with normal + last rules" in {
    val stored = StoredTitleRuleRecord.fromDomain(
      TitleRuleRecord("GlobalStructural", RuleScope.GlobalStructural, None,
        rules     = Seq(TitleRule("prog", RuleScope.GlobalStructural, None, "(?i)^Klub: ", "",
          applyAll = false, order = 0, tag = Some("programmePrefix"))),
        lastRules = Seq(TitleRule("tail", RuleScope.GlobalStructural, None, "Z$", "",
          applyAll = true, order = 0, last = true))))
    val back = roundTrip(stored)
    back shouldBe stored
    back.scope shouldBe "GlobalStructural"          // encode of GlobalStructural is its own name, never "Search"

    val domain = StoredTitleRuleRecord.toDomain(back).value
    domain.scope shouldBe RuleScope.GlobalStructural
    domain.rules.map(_.id) shouldBe Seq("prog")
    domain.lastRules.map(r => (r.id, r.last)) shouldBe Seq(("tail", true))
  }

  it should "decode a legacy stored scope of \"Search\" to GlobalStructural (alias)" in {
    // Old prod docs were persisted with scope:"Search"; the alias keeps them loading.
    val legacy = StoredTitleRuleRecord("Search", "Search", None,
      rules     = Seq(StoredRule("prog", "(?i)^Klub: ", "", applyAll = false, enabled = true,
        tag = Some("programmePrefix"), note = None)),
      lastRules = Nil)
    val domain = StoredTitleRuleRecord.toDomain(roundTrip(legacy)).value
    domain.scope shouldBe RuleScope.GlobalStructural
    domain.rules.map(_.id) shouldBe Seq("prog")
  }

  it should "round-trip a per-cinema record carrying a cinemaId and a replacement" in {
    val stored = StoredTitleRuleRecord.fromDomain(
      TitleRuleRecord("bok", RuleScope.PerCinema, Some("bok"),
        rules     = Seq(TitleRule("bok-pipe", RuleScope.PerCinema, Some("bok"), """\s*\|\s*""", ": ",
          applyAll = true, order = 0, note = Some("pipe → colon"))),
        lastRules = Nil))
    roundTrip(stored) shouldBe stored
  }

  it should "decode an unknown scope to None rather than throwing" in {
    StoredTitleRuleRecord.toDomain(
      StoredTitleRuleRecord("z", "BogusScope", None, Nil, Nil)) shouldBe None
  }

  // helper: .value on an Option for a crisp failure message
  private implicit class OptionValue[A](o: Option[A]) {
    def value: A = o.getOrElse(fail("expected Some, got None"))
  }
}
