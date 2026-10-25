package services.identity

import kinowo.build.SourceDigest
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

  private lazy val digested: Seq[String] = resource("/identity-rules-sources.txt").linesIterator.toSeq

  "the rules version" should "digest the resolver, what it reaches and the data it reads" in {
    digested should contain allOf ("identity/src/main/scala/services/identity/IncrementalResolver.scala", "identity/src/main/scala/services/identity/IdentityModelStore.scala",
      "identity/src/main/scala/services/identity/IdentityMeasures.scala", "common/src/main/scala/services/movies/TitleNormalizer.scala", "common/src/main/scala/models/MovieRecord.scala",
      "identity/src/main/resources/identity-weights.json", "identity/src/main/resources/identity-decorations.json", "identity/src/main/resources/identity-stage-works.tsv")
  }

  it should "leave out code and data the resolver never reaches" in {
    digested should contain noneOf ("common/src/main/scala/services/readmodel/ReadModelProjection.scala", "common/src/main/scala/tools/HttpFetch.scala",
      "common/src/main/scala/services/movies/MovieCache.scala", "identity/src/main/scala/services/identity/IdentityProjectionPlan.scala", "common/src/main/resources/certs/certum-trusted-root-ca.pem")
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
    digested should contain ("common/src/main/scala/services/movies/LatestTitleYear.scala")
    digested should not contain "common/src/main/scala/models/VenueClock.scala"
  }

  // A film id only NAMES a film (docs/design/identity-no-drift.md): neither the resolver nor the agreement stage reaches the
  // code that hands one out, so no grouping or take can read one.
  it should "leave out the projection's film ids, which neither the resolver nor the agreement stage may read" in {
    val ids = Set("IdAssigner", "FilmIdCounters", "IdSeeding").map(name => s"identity/src/main/scala/services/identity/$name.scala")
    ids.filterNot(path => Files.exists(main.resolve(path))) shouldBe empty
    digested.filter(ids) shouldBe empty
    resource("/agreement-rules-sources.txt").linesIterator.filter(ids).toSeq shouldBe empty
  }

  private def read(edit: (String, String => String)*)(path: String): Array[Byte] = {
    val bytes = Files.readAllBytes(main.resolve(path))
    edit.find(_._1 == path).fold(bytes)(e => e._2(new String(bytes, StandardCharsets.UTF_8)).getBytes(StandardCharsets.UTF_8))
  }

  it should "be the digest of exactly those files, so no other file can move it" in {
    resource(IdentityRules.Resource) shouldBe SourceDigest.of(digested, read())
    IdentityRules.codeVersion shouldBe resource(IdentityRules.Resource)
  }

  it should "lex every Scala source it digests, so none falls back to its bytes and moves on a comment edit" in {
    unlexable(digested) shouldBe empty
  }

  // Every move re-resolves every family of every worker on its next boot (US: ~2,330 families, ~45–60 s), and on
  // 2026-10-03/04 comment-only edits moved it several times a day.
  it should "not move on a comment-only edit to a file it digests, and move on a code edit" in {
    val measures = "identity/src/main/scala/services/identity/IdentityMeasures.scala"
    val commented = SourceDigest.of(digested, read(measures -> (text => text.replaceFirst("\n", "\n// a note, and /* another */   \n\n"))))
    commented shouldBe resource(IdentityRules.Resource)
    val edited = SourceDigest.of(digested, read(measures -> (text => text + "\nprivate object SourceDigestProbe { val x = 1 }\n")))
    edited should not be resource(IdentityRules.Resource)
  }
}
