package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * No production parameter may default to Poland.
 *
 * Poland was the only country for years, so "the country" defaulted to it wherever a
 * parameter was added: `country: Country = Country.default`, `language: Locale = pl-PL`,
 * `screeningTokens = ScreeningTokens.Default`. Every such default is a leak waiting for a
 * caller that forgets the argument — Germany and Spain carried Poland's `LEK` voice-over
 * badge for months through one (47cec8241), the UK corpus stored Polish TMDB synopses
 * through another. A default that resolves the deployment from the environment
 * (`Country.fromEnv`) is the same thing one step removed: unset, it IS Poland, and a
 * component reading it has stopped taking its country from the composition root.
 *
 * The rule: a class/def parameter's default must not name `Country.default`,
 * `Country.Poland`, `Country.fromEnv`, or the Polish constants that stand in for them.
 * Pass the country (or its language / tokens) from the root instead. The entries below
 * predate the rule; each says why it is still there and what holds the production
 * wiring to its own country regardless (`CountryIsolationMatrixSpec` walks every
 * country's worker graph for a stray `Country` or sibling locale). Shrink the list;
 * don't grow it.
 */
class NoPolandDefaultCountrySpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan._

  /** What a Poland default looks like, directly or through a Polish constant. */
  private val PolandDefault =
    ("""\b(?:models\.)?Country\.(?:default|Poland|fromEnv)\b|\bScreeningTokens\.Default\b|""" +
      """\bTmdbClient\.DefaultLanguage\b|\bCountryNames\.DefaultLanguage\b|forLanguageTag\("pl""").r

  private val Header = """\b(?:class|trait|def)\s+([\w$]+)\s*(?:\[[^\]]*\])?\s*(?=\()""".r
  private val Parameter =
    """(?s)^\s*(?:@\w+\s+)*(?:(?:private(?:\[\w+\])?|protected|override|implicit|using|final|val|var)\s+)*([\w$]+)\s*:(.*?)=(?!>)(.*)$""".r

  /** "file: Owner.param" for every parameter whose default names Poland. */
  private lazy val offenders: Seq[String] = scalaFiles(MainRoots).flatMap { path =>
    val src = codeOf(path)
    Header.findAllMatchIn(src).flatMap { header =>
      // Every clause of the signature: `(a)(b)(using c)`.
      Iterator.iterate(Option(header.end)) {
        case Some(open) =>
          val next = src.indexWhere(!_.isWhitespace, closingParen(src, open) + 1)
          Option.when(next > 0 && src(next) == '(')(next)
        case None => None
      }.takeWhile(_.isDefined).flatten.flatMap { open =>
        topLevelParts(argumentsAt(src, open)).collect {
          case Parameter(name, _, default) if PolandDefault.findFirstIn(default).isDefined =>
            s"$path: ${header.group(1)}.$name"
        }
      }
    }.toSeq
  }.distinct

  private val Tests =
    "test seam: specs construct it without a country and get Poland's historical behaviour; every " +
      "production construction passes its own country, which CountryIsolationMatrixSpec checks per country"

  /** "file: Owner.param" → why it still defaults to Poland. */
  private val Allowlist: Map[String, String] = Map(
    "common/src/main/scala/services/movies/MovieCache.scala: CaffeineMovieCache.enrichmentLanguage" -> Tests,
    "common/src/main/scala/services/movies/MovieCache.scala: CaffeineMovieCache.screeningTokens"    -> Tests,
    "worker/src/main/scala/services/TmdbClient.scala: TmdbClient.language"                          -> Tests,
    "worker/src/main/scala/services/enrichment/ImdbRatings.scala: ImdbRatings.enrichmentLanguage"   -> Tests,
    "worker/src/main/scala/services/staging/StagingSteps.scala: StagingSteps.screeningTokens"       -> Tests,
    "worker/src/main/scala/services/tasks/EnrichDetailsHandler.scala: EnrichDetailsHandler.screeningTokens" -> Tests,
    "worker/src/main/scala/services/tasks/QueueEnrichmentRetrigger.scala: QueueEnrichmentRetrigger.country" -> Tests,
    "worker/src/main/scala/services/tasks/RatingEnqueuer.scala: RatingEnqueuer.country"             -> Tests,
    "worker/src/main/scala/services/tasks/UnresolvedTmdbReaper.scala: UnresolvedTmdbReaper.country" -> Tests,
    "worker/src/main/scala/modules/WorkerWiring.scala: WorkerWiring.country" ->
      ("the worker test seam `TestWiring` is a trait, so it can only extend the no-argument constructor; " +
        "WorkerMain passes every country explicitly"),
    "worker/src/main/scala/services/cinemas/CinemaScraperCatalog.scala: this.titles" ->
      "the auxiliary constructor exists for the Poland-only operator tools (FilmwebDiff, RosterAudit)",
    "worker/src/main/scala/tools/FilmwebDiffTitleNormalizer.scala: normalize.titles" ->
      "Filmweb is Polish-only, so a Filmweb title is a Polish title by definition",
    "web/src/main/scala/controllers/DebugController.scala: DebugController.servingCountry"   -> Tests,
    "web/src/main/scala/controllers/LandingController.scala: LandingController.country"      -> Tests,
    "web/src/main/scala/controllers/MetricsController.scala: MetricsController.country"      -> Tests,
    "web/src/main/scala/controllers/MovieController.scala: MovieController.servingCountry"   -> Tests,
    "web/src/main/scala/controllers/WebMovieMetrics.scala: WebMovieMetrics.country"          -> Tests
  )

  "Production parameters" should "not default to Poland" in {
    val fresh = offenders.filterNot(Allowlist.contains)
    withClue(
      "These parameters default to Poland. Take the country (or its language / tokens) from the " +
        "composition root instead, or allowlist the parameter with the reason it must stay:\n" +
        fresh.mkString("\n") + "\n") {
      fresh shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a parameter that still defaults to Poland" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot(offenders.contains)
    withClue("Allowlisted but no longer defaulting to Poland — drop the entry:\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }
}
