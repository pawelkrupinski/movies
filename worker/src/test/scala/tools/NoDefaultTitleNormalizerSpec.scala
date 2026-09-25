package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * No production or testkit parameter may default to a title normalizer.
 *
 * A title normalizer decides the key a film is stored under, and each country folds
 * titles under its own rules. For a long time the stores defaulted theirs to
 * `TitleNormalizer.deployment`, a rule set resolved from `KINOWO_COUNTRY`, so a caller
 * that forgot the argument got whatever country the process happened to be configured
 * for — and a multi-country worker had to be refused at boot because it had no one
 * answer. A constructor that is handed its normalizer from the composition root (or,
 * in a spec, from `SingleCountryNormalizer`) cannot inherit the wrong one.
 *
 * The rule: a class/def parameter in production code or `testkit` must not default to
 * anything naming `TitleNormalizer`, `SingleCountryNormalizer`, or a normalizer's rule
 * set. Take it as a required argument instead.
 */
class NoDefaultTitleNormalizerSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan._

  private val NormalizerDefault = """\b(?:TitleNormalizer|SingleCountryNormalizer)\.|\.deployment\b""".r

  /** "file: Owner.param" → why it still defaults to a normalizer. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala: this.titles" ->
      "the auxiliary constructor exists for the Poland-only operator tools (FilmwebDiff, RosterAudit)"
  )

  private lazy val offenders: Seq[String] =
    parameterDefaults(MainRoots :+ "testkit/src/main").collect {
      case p if NormalizerDefault.findFirstIn(p.default).isDefined => p.label
    }.distinct

  "Production and testkit parameters" should "not default to a title normalizer" in {
    val fresh = offenders.filterNot(Allowlist.contains)
    withClue(
      "These parameters default to a title normalizer. Take it as a required argument, passed " +
        "from the composition root (or SingleCountryNormalizer in a spec):\n" + fresh.mkString("\n") + "\n") {
      fresh shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a parameter that still defaults to one" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot(offenders.contains)
    withClue("Allowlisted but no longer defaulting to a normalizer — drop the entry:\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }

  "TitleNormalizer" should "offer no rule set resolved from the process environment" in {
    val companion = Class.forName("services.movies.TitleNormalizer$")
    val names     = companion.getMethods.map(_.getName).toSet
    names should contain("forCountry")
    names should not contain "deployment"
  }
}
