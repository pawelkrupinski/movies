package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsObject, JsString, Json}
import services.identity._
import services.movies.ListingKey
import tools._

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Try

/**
 * The resolver alone over a recorded full corpus, every listing's decision written out — the fast loop for
 * a resolver change: seconds per country, no pipeline boot, against the same recording the CI measure
 * replays. `decisions-<cc>.jsonl` holds one line per listing (its serialised key, venue, raw title, the
 * film its cluster takes and the decision's explanation); with `KINOWO_IDENTITY_FOCUS` the focused titles'
 * explanations and candidates are written beside it, as the CI measure's focus mode prints them.
 *
 * Opt-in: runs when `KINOWO_IDENTITY_DUMP` (the output directory), `KINOWO_IDENTITY_FULL`,
 * `KINOWO_IDENTITY_CORPUS_DIR` and `KINOWO_FIXTURE_ROOT` are set. Nothing is judged here — the CI measure
 * judges; this says what moved.
 */
class IdentityResolveDumpIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]
  private val out      = configuration.identityDump
  private val corpora: Seq[Corpus] = for {
    _      <- out.toSeq
    dir    <- configuration.identityCorpusDirectory.toSeq
    corpus <- IdentityShadow.full(configuration.identityFullCorpora.value, dir.value, configuration.fixtureRoot)
  } yield corpus

  corpora.foreach { c =>
    "The resolver" should s"write every listing's decision on ${c.label}" in {
      val dir      = out.get.value
      val w        = wiring(mongoTarget, c, storages, configuration.fixtureRoot, configuration.env)
      val listings = listingsOf(w, c.normalizer)
      val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers,
        new TmdbIdentityLookups.CountedGaps(c.misses)))
      val (resolution, seconds) = timed(IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver))
      val clusterOf = resolution.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
      Files.createDirectories(dir)
      val lines = listings.map { l =>
        val d = resolution.decisionOf(l.key)
        Json.stringify(JsObject(Seq(
          "key"         -> JsString(ListingKey.serialised(l.key)),
          "venue"       -> JsString(l.key.venue),
          "rawTitle"    -> JsString(l.key.rawTitle),
          "cluster"     -> JsNumber(clusterOf.getOrElse(l.key, -1)),
          "film"        -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
          "filmTitle"   -> d.film.flatMap(resolution.films.get).fold[play.api.libs.json.JsValue](JsNull)(f => JsString(s"${f.title} (${f.year.getOrElse("?")})")),
          "confidence"  -> JsNumber(BigDecimal(d.confidence).setScale(4, BigDecimal.RoundingMode.HALF_UP)),
          "basis"       -> JsString(d.basis.toString),
          "explanation" -> JsArray(d.explanation.take(4).map(JsString(_))),
          "rules"       -> JsArray((d.trace.rulesOf(l.key) ++ c.normalizer.firedRules(l.cinema, l.rawTitle)).map(JsString(_))),
          // why not: what stopped a listing left with no film, what it searched, what it weighed
          "blocker"     -> (if (d.film.isDefined) JsNull else JsString(d.trace.nodes.get(l.key).flatMap(_.blocker).getOrElse("pooled:no-film"))),
          "searched"    -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.searched).map(JsString(_))),
          "candidates"  -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.candidates).map(JsString(_))))))
      }
      Files.writeString(dir.resolve(s"decisions-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
      configuration.identityFocus.foreach { f =>
        val tokens  = (l: Listing) => services.movies.TitleContainment.tokens(l.rawTitle).toSet ++ services.movies.TitleContainment.tokens(l.cleanTitle).toSet
        val focused = listings.filter(l => f.covers(tokens(l))).map(_.key).toSet
        val trace   = mutable.ListBuffer.empty[String]
        IdentityResolver.explain(listings, lookups, c.normalizer, IdentityCalibration.resolver)(focused).foreach(_.render.foreach(trace += _))
        IdentityResolver.candidatesOf(listings, lookups, c.normalizer, IdentityCalibration.resolver)(l => focused(l.key)).foreach { node =>
          trace += node.label
          node.banners.foreach(b => trace += s"  $b")
          node.candidates.take(8).foreach(cand => trace += s"  ${cand.render}")
        }
        Files.writeString(dir.resolve(s"focus-${c.country.code}.txt"), trace.mkString("", "\n", "\n"))
      }
      println(f"[${c.label}] ${listings.size} listings → ${resolution.decisions.size} clusters " +
        f"(${resolution.decisions.count(_.film.isDefined)} with a film) in $seconds%.1fs; unanswered queries ${resolution.unknownQueries}")
    }
  }

  override protected def afterAll(): Unit = {
    storages.foreach(s => Try(s.close()))
    super.afterAll()
  }
}
