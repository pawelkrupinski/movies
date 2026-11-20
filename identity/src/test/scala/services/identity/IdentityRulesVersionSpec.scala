package services.identity

import testsupport.SourceTrees
import kinowo.build.{SourceDigest, StampCarry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.VersionedSources.{main, resource, unlexable}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The rules version digests exactly what the resolver is built from (`IdentityRulesSources`, `build.sbt`): every
 *  stored family is re-resolved when it moves, so a change to code the resolver never reaches must not move it —
 *  it used to digest all of `common`, and nearly every push re-resolved every worker's whole corpus — while a change
 *  to anything it does reach must. */
class IdentityRulesVersionSpec extends AnyFlatSpec with Matchers {

  private lazy val digested: Seq[String] = resource("/services/identity/identity-rules-sources.txt").linesIterator.toSeq
  // the raw digest; the version the worker reads is what the stamp carry makes of it
  private val Digest = "/services/identity/identity-rules-digest.txt"

  "the rules version" should "digest the resolver, what it reaches and the data it reads" in {
    digested should contain allOf ("identity/src/main/scala/services/identity/IncrementalResolver.scala", "identity/src/main/scala/services/identity/IdentityModelStore.scala",
      "identity/src/main/scala/services/identity/measures/IdentityMeasures.scala", "common/src/main/scala/services/titles/TitleNormalizer.scala", "common/src/main/scala/models/MovieRecord.scala",
      "identity/src/main/resources/identity-weights.json", "identity/src/main/resources/identity-decorations.json", "identity/src/main/resources/identity-stage-works.tsv")
  }

  it should "leave out code and data the resolver never reaches" in {
    digested should contain noneOf ("common/src/main/scala/services/readmodel/ReadModelProjection.scala", "common/src/main/scala/tools/PacedHttpFetch.scala",
      "common/src/main/scala/services/movies/MovieCache.scala", "identity/src/main/scala/services/identity/IdentityProjectionPlan.scala", "http/src/main/resources/certs/certum-trusted-root-ca.pem")
  }

  // The HTTP leaf module is outside the identity area: the pre-push hook runs no identity test for an http/ edit and
  // identity_impact.py gives it no country. Both hold only while no digest lists an http/ source.
  it should "list no source of the http module, which the hook and the impact plan read as outside the identity area" in {
    val lists = Seq("identity-rules", "venue-slot", "agreement-rules")
      .map(name => name -> resource(s"/services/identity/$name-sources.txt").linesIterator.toSeq)
    lists.flatMap { case (name, paths) => paths.filter(_.startsWith("http/")).map(path => s"$name: $path") } shouldBe empty
    lists.map(_._2.size).min should be > 0
  }

  // Each of these changes, often, for reasons no decision reads: every setting's value type (a new setting
  // anywhere), how a decision's explanation is built and written, the trace store's Mongo writes. The resolver
  // reaches them through narrow files of their own (`settings.DatabaseNames`, `IdentityTraceSink`,
  // `DocumentDigest`), so a change to them re-resolves no worker's corpus.
  it should "leave out the volatile files beside what the resolver reads, which change no decision" in {
    digested should contain noneOf ("common/src/main/scala/settings/ConfigurationValues.scala", "identity/src/main/scala/services/identity/IdentityTraceStore.scala")
    digested should contain ("identity/src/main/scala/services/identity/IdentityTraceSink.scala")
  }

  // The title readers cap a year at next year in UTC; read through VenueClock, every venue-timezone fix moved the
  // rules and re-resolved every worker's corpus. They read it through `LatestTitleYear`, which reaches no venue.
  it should "leave out the venue clock, reading only the year a title may name" in {
    digested should contain ("common/src/main/scala/services/evidence/LatestTitleYear.scala")
    digested should not contain "common/src/main/scala/models/VenueClock.scala"
  }

  // A film id only NAMES a film (docs/design/identity-no-drift.md): neither the resolver nor the agreement stage reaches the
  // code that hands one out, so no grouping or take can read one.
  it should "leave out the projection's film ids, which neither the resolver nor the agreement stage may read" in {
    val ids = Set("IdAssigner", "FilmIdCounters", "IdSeeding").map(name => SourceTrees("identity/src/main/scala/services/identity").file(s"$name.scala").toString)
    ids.filterNot(path => Files.exists(main.resolve(path))) shouldBe empty
    digested.filter(ids) shouldBe empty
    resource("/services/identity/agreement-rules-sources.txt").linesIterator.filter(ids).toSeq shouldBe empty
  }

  // The model records each country's title rules with its families (TitleRuleRecord) and, on a change to them alone,
  // re-resolves only the families that read what moved (TitleRuleChange): digested here, every title-rule edit
  // re-resolved every family of every country, a rule for one billing at one venue too.
  it should "leave out the title-rule table, which the model versions per country with its families" in {
    digested should not contain "common/src/main/resources/title-rules.tsv"
    digested should contain allOf ("common/src/main/resources/title-words.tsv", "common/src/main/scala/services/titles/rules/TitleRuleSet.scala")
  }

  // What lets the rules version leave the table out: the resolver reads it only through the country's normaliser it is
  // handed (whose rules the model records) and the relay rows (which `IncrementalResolver.rulesVersion` digests). A
  // source it reaches that read the table another way -- the whole table, another country's set -- would decide by rules
  // no record holds.
  it should "reach the title-rule table only through the country's normaliser and the relay rows" in {
    // each source's code, its comments off (what the digest reads), so a doc naming one is no reader
    def mentioning(pattern: String) = digested.filter(_.endsWith(".scala")).filter { path =>
      SourceDigest.lexed(new String(Files.readAllBytes(main.resolve(path)), StandardCharsets.UTF_8)).exists(pattern.r.findFirstIn(_).isDefined)
    }.map(_.split('/').last).toSet
    mentioning("\\bTitleRuleTable\\b") shouldBe Set("TitleRuleTable.scala", "TitleRules.scala", "ExtraTitleRules.scala", "RelayBanner.scala",
      "TitleRuleRecord.scala")
    mentioning("\\b(?:Extra)?TitleRules\\.") shouldBe Set("TitleRuleSet.scala")
    mentioning("\\b(?:TitleRuleSet|TitleNormalizer)\\.forCountry\\b") shouldBe Set("TitleNormalizer.scala")
  }

  private def read(edit: (String, String => String)*)(path: String): Array[Byte] = {
    val bytes = Files.readAllBytes(main.resolve(path))
    edit.find(_._1 == path).fold(bytes)(e => e._2(new String(bytes, StandardCharsets.UTF_8)).getBytes(StandardCharsets.UTF_8))
  }

  it should "be the digest of exactly those files, so no other file can move it" in {
    resource(Digest) shouldBe SourceDigest.of(digested, read())
    IdentityRules.codeVersion shouldBe resource(IdentityRules.Resource)
  }

  // A proven-equivalent change keeps the stamp its stored families were decided under (`project/StampCarry.scala`):
  // the version is the digest's carried stamp, which is the digest itself when no row of the table carries it.
  it should "be the stamp the carry table carries its digest to" in {
    val table = main.resolve(StampCarry.Table)
    val rows  = StampCarry.parse(Files.readAllLines(table, StandardCharsets.UTF_8).toArray(Array.empty[String]).toSeq)
    IdentityRules.codeVersion shouldBe StampCarry.version("identity-rules", resource(Digest), rows)
  }

  it should "lex every Scala source it digests, so none falls back to its bytes and moves on a comment edit" in {
    unlexable(digested) shouldBe empty
  }

  // Every move re-resolves every family of every worker on its next boot (US: ~2,330 families, ~45–60 s), and on
  // 2026-10-03/04 comment-only edits moved it several times a day.
  it should "not move on a comment-only edit to a file it digests, and move on a code edit" in {
    val measures = "identity/src/main/scala/services/identity/measures/IdentityMeasures.scala"
    val commented = SourceDigest.of(digested, read(measures -> (text => text.replaceFirst("\n", "\n// a note, and /* another */   \n\n"))))
    commented shouldBe resource(Digest)
    val edited = SourceDigest.of(digested, read(measures -> (text => text + "\nprivate object SourceDigestProbe { val x = 1 }\n")))
    edited should not be resource(Digest)
  }
}
