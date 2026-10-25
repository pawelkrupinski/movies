package integration

import models.Country
import play.api.libs.json.{JsNull, JsNumber, JsObject, JsString, JsValue, Json}
import services.identity._
import services.identity.adapters.TmdbIdentityLookups
import services.movies.{ListingKey, TitleNormalizer}
import services.titlerules.{ExtraTitleRules, TitleRuleSet, TitleRules}
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/**
 * RULE ABLATION over a recorded full corpus: each configuration of a plan resolves every listing with some rules
 * switched off, and the takes that moved against the plan's baseline (nothing switched off) are written out — the
 * JVM half of `scripts/identity-rule-ablation`, which writes the plans, bisects, and judges the moves.
 *
 * {{{
 *   scripts/it-local.sh "worker/IntegrationTest/runMain integration.IdentityRuleAblation --plan plan.json --out DIR \
 *     --full pl,de --corpus-dir <recording>/corpus --fixtures <recording>"
 * }}}
 *
 * `plan.json` is `{"configs": [{"id": "b001", "rules": ["title:xtra-foo", "veto:seasonsapart", ...],
 * "countries": ["pl"]}, ...]}` (`countries` optional: every country of `--full`). A `title:<id>` rule is left out of
 * the normalizer the configuration resolves with; every other name is switched off through [[RuleAblation]]. Each
 * country's corpus is seeded and its lookups memoised once, and its baseline resolved first and again last (a
 * replay that is not a pure function of its configuration would show as moves of the second baseline, `base-again`).
 *
 * Out: `<out>/<config>/moves-<cc>.jsonl` (one line per listing whose take moved: its key, venue, raw title, and the
 * baseline's and the configuration's film), `<out>/<config>/done-<cc>` once written, `<out>/base/fired-<cc>.tsv` (each
 * rule the baseline's trace names, with its listings) and `<out>/title-rules.tsv` (every title rule: id, scope,
 * countries).
 */
object IdentityRuleAblation {

  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("IdentityRuleAblation", "Resolves recorded full corpora with rules switched off, writing the takes that moved.",
    CommandLine.Value("plan", "file", "the configurations to resolve (required)"),
    CommandLine.Value("out", "dir", "where each configuration's moves are written (required)"),
    CommandLine.Value("full", "codes", "the countries whose recorded corpora are resolved, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"))

  final case class Config(id: String, rules: Set[String], countries: Option[Set[String]]) {
    def titleRules: Set[String] = rules.collect { case r if r.startsWith("title:") => r.stripPrefix("title:") }
    def identityRules: Set[String] = rules.filterNot(_.startsWith("title:"))
  }
  private val Base = Config("base", Set.empty, None)
  private val BaseAgain = Config("base-again", Set.empty, None)

  private final case class Take(film: Option[Int], title: Option[String], basis: String)

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code")))
    val out       = arguments.requiredPath("out")
    val plan      = readPlan(arguments.requiredPath("plan"))
    val corpusDir = arguments.requiredPath("corpus-dir")
    val configuration = settings.ProcessConfiguration.resolve()
    val root      = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target    = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to seed the corpora into"))
    target.requireThrowaway()
    Files.createDirectories(out)
    Files.writeString(out.resolve("title-rules.tsv"), (TitleRules.all ++ ExtraTitleRules.all).map(r =>
      s"${r.id}\t${r.scope}\t${r.countries.fold("*")(_.map(_.code).toSeq.sorted.mkString(","))}").mkString("", "\n", "\n"))
    val corpora   = IdentityShadow.full(countries.toSet, corpusDir, root, None)
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    val failed    = try corpora.filterNot { c =>
      Try(ablate(configuration, target, root, c, plan, out, storages)).fold({ e =>
        System.err.println(s"[${c.label}] FAILED: $e"); e.printStackTrace(); false }, _ => true)
    } finally storages.foreach(s => Try(s.close()))
    if (failed.nonEmpty || corpora.size < countries.size) sys.exit(1)
  }

  private def readPlan(path: Path): Seq[Config] =
    (Json.parse(Files.readString(path)) \ "configs").as[Seq[JsValue]].map { c =>
      Config((c \ "id").as[String], (c \ "rules").as[Seq[String]].toSet, (c \ "countries").asOpt[Seq[String]].map(_.toSet))
    }

  private def ablate(configuration: settings.ProcessConfiguration, target: IntegrationMongoTarget, root: settings.FixtureRoot, c: Corpus,
                     plan: Seq[Config], out: Path, storages: mutable.ListBuffer[ConvergenceStorage]): Unit = {
    val cc       = c.country.code
    val w        = wiring(target, c, storages, root, configuration.env)
    val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
      language = c.country.language), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers,
      c.gaps))
    val every    = (TitleRules.all ++ ExtraTitleRules.all).filter(_.appliesTo(c.country))
    def resolve(config: Config): (Seq[Listing], Map[ListingKey, Take], Resolution) = {
      val normalizer = new TitleNormalizer(TitleRuleSet(every.filterNot(r => config.titleRules(r.id))), facts = c.country.titleFacts)
      val listings   = listingsOf(w, normalizer)
      val resolution = RuleAblation.disabling(config.identityRules)(IdentityResolver.resolve(listings, lookups, normalizer, ResolveInputs.Unpinned))
      val takes      = listings.map { l =>
        val d = resolution.decisionOf(l.key)
        l.key -> Take(d.film, d.film.flatMap(resolution.films.get).map(f => s"${f.title} (${f.year.getOrElse("?")})"), d.basis.toString)
      }.toMap
      (listings, takes, resolution)
    }
    val ((listings, base, resolution), seconds) = timed(resolve(Base))
    val fired = mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val byClean = mutable.HashMap.empty[String, Seq[String]]
    listings.foreach { l =>
      (resolution.decisionOf(l.key).trace.rulesOf(l.key) ++ c.normalizer.firedRules(l.cinema, l.rawTitle, byClean)).distinct.foreach(r => fired(r) += 1)
    }
    write(out.resolve("base"), s"fired-$cc.tsv", fired.toSeq.sortBy(-_._2).map { case (r, n) => s"$r\t$n" })
    done(out.resolve("base"), cc)
    println(f"[ablation ${c.label}] base: ${listings.size} listings in $seconds%.1fs")
    val mine = plan.filter(_.countries.forall(_.contains(cc))) :+ BaseAgain
    mine.foreach { config =>
      val dir = out.resolve(config.id)
      if (Files.exists(dir.resolve(s"done-$cc"))) println(s"[ablation ${c.label}] ${config.id}: already resolved")
      else {
        val ((_, head, _), took) = timed(resolve(config))
        val moved = listings.flatMap { l =>
          val (b, h) = (base(l.key), head.getOrElse(l.key, Take(None, None, "absent")))
          Option.when(b.film != h.film)(Json.stringify(JsObject(Seq(
            "key" -> JsString(ListingKey.serialised(l.key)), "venue" -> JsString(l.key.venue), "rawTitle" -> JsString(l.key.rawTitle),
            "base" -> json(b), "head" -> json(h)))))
        }
        write(dir, s"moves-$cc.jsonl", moved)
        done(dir, cc)
        println(f"[ablation ${c.label}] ${config.id}: ${moved.size} moved in $took%.1fs")
      }
    }
  }

  private def json(t: Take): JsObject = JsObject(Seq(
    "film" -> t.film.fold[JsValue](JsNull)(JsNumber(_)), "filmTitle" -> t.title.fold[JsValue](JsNull)(JsString(_)), "basis" -> JsString(t.basis)))

  private def write(dir: Path, name: String, lines: Seq[String]): Unit = {
    Files.createDirectories(dir)
    Files.writeString(dir.resolve(name), lines.mkString("", "\n", if (lines.isEmpty) "" else "\n"))
  }
  private def done(dir: Path, cc: String): Unit = Files.writeString(dir.resolve(s"done-$cc"), "")
}
