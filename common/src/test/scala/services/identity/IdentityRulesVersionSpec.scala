package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** The rules version digests exactly what the resolver is built from (`IdentityRulesSources`, `build.sbt`): every
 *  stored family is re-resolved when it moves, so a change to code the resolver never reaches must not move it —
 *  it used to digest all of `common`, and nearly every push re-resolved every worker's whole corpus — while a change
 *  to anything it does reach must. */
class IdentityRulesVersionSpec extends AnyFlatSpec with Matchers {

  private def resource(name: String): String = {
    val stream = getClass.getResourceAsStream(name)
    withClue(s"$name is not on the classpath: ")(stream should not be null)
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim finally stream.close()
  }
  private lazy val digested: Seq[String] = resource("/identity-rules-sources.txt").linesIterator.toSeq
  private lazy val main: Path =
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent).takeWhile(_ != null).map(_.resolve("common/src/main"))
      .find(Files.isDirectory(_)).getOrElse(fail("common/src/main not found above the working directory"))

  "the rules version" should "digest the resolver, what it reaches and the data it reads" in {
    digested should contain allOf ("scala/services/identity/IncrementalResolver.scala", "scala/services/identity/IdentityModelStore.scala",
      "scala/services/identity/IdentityMeasures.scala", "scala/services/movies/TitleNormalizer.scala", "scala/models/MovieRecord.scala",
      "resources/identity-weights.json", "resources/identity-decorations.json", "resources/identity-stage-works.tsv")
  }

  it should "leave out code and data the resolver never reaches" in {
    digested should contain noneOf ("scala/services/readmodel/ReadModelProjector.scala", "scala/tools/OgCardRenderer.scala",
      "scala/services/movies/MovieCache.scala", "scala/services/identity/IdentityProjectionPlan.scala", "resources/fonts/DejaVuSans.ttf")
  }

  it should "be the digest of exactly those files, so no other file can move it" in {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    digested.foreach { path =>
      digest.update(path.getBytes(StandardCharsets.UTF_8))
      digest.update(Files.readAllBytes(main.resolve(path)))
    }
    resource(IdentityRules.Resource) shouldBe digest.digest.map("%02x".format(_)).mkString
    IdentityRules.codeVersion shouldBe resource(IdentityRules.Resource)
  }
}
