package integration

import models.Country
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsObject, JsString, Json}
import services.identity._
import services.movies.ListingKey
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.util.Try

/**
 * The resolver alone over a recorded full corpus, every listing's decision written out — the fast loop for
 * a resolver change: seconds per country, no pipeline boot, against the same recording the CI measure
 * replays, and the identity takes check's resolve of its base and its head (`scripts/ci/identity-takes-check.sh`):
 *
 * {{{
 *   scripts/it-local.sh "worker/IntegrationTest/runMain integration.IdentityResolveDump --out target/dump --full pl \
 *     --corpus-dir test/resources/fixtures/corpus"
 * }}}
 *
 * `decisions-<cc>.jsonl` holds one line per listing (its serialised key, venue, raw title, the film its cluster takes,
 * the decision's explanation and what [[SuspectTakes]] finds against the take); with `KINOWO_IDENTITY_FOCUS` the
 * focused titles' explanations and candidates are written beside it (`focus-<cc>.txt`), as the CI measure's focus mode
 * prints them; with `KINOWO_IDENTITY_AGREEMENT_CACHE` the agreement stage runs over the no-matches too (`agreed-`,
 * `answers-`, `final-`, `catalogue-<cc>.jsonl`). With `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY` what the recording cannot
 * answer is asked of TMDB and IMDb live — the loop for a change asking new questions. `MONGODB_URI` names the throwaway
 * Mongo the corpus is seeded into. Nothing is judged here — the CI measure judges; this says what moved.
 *
 * Exit: 0 every country named written; 1 a country's corpus is not in `--corpus-dir`, or its resolve failed; 2 usage.
 */
object IdentityResolveDump {

  import IdentityShadow._

  val commandLine: CommandLine = CommandLine("IdentityResolveDump", "Writes every listing's decision of the resolver over recorded full corpora.",
    CommandLine.Value("out", "dir", "where decisions-<cc>.jsonl is written (required)"),
    CommandLine.Value("full", "codes", "the countries whose recorded corpora are resolved, comma-separated (required)"),
    CommandLine.Value("corpus-dir", "dir", "the directory holding cinema-scrapes-<cc>.json.gz (required)"),
    CommandLine.Value("fixtures", "dir", "the directory holding enrichment-<cc>/ (default: KINOWO_FIXTURE_ROOT, else the repository's)"))

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val countries = arguments.required("full").split(",").toSeq.map(_.trim).filter(_.nonEmpty).map(code =>
      Country.byCode(code).getOrElse(arguments.fail(s"--full: no country $code (one of ${Country.all.map(_.code).mkString(", ")})")))
    val out       = arguments.requiredPath("out")
    val corpusDir = arguments.requiredPath("corpus-dir")
    val configuration = settings.ProcessConfiguration.resolve()
    val root      = arguments.path("fixtures").map(settings.FixtureRoot(_)).getOrElse(configuration.fixtureRoot)
    val target    = IntegrationMongoTarget.from(configuration).getOrElse(arguments.fail("MONGODB_URI names no throwaway Mongo to seed the corpora into"))
    target.requireThrowaway()
    val corpora   = IdentityShadow.full(countries.toSet, corpusDir, root, configuration.identityLiveGaps)
    val missing   = countries.filterNot(c => corpora.exists(_.country == c))
    missing.foreach(c => System.err.println(s"[IdentityResolveDump] no cinema-scrapes-${c.code}.json.gz in $corpusDir"))
    val storages  = mutable.ListBuffer.empty[ConvergenceStorage]
    val failed    = try corpora.filterNot { c =>
      Try(dump(configuration, target, root, c, out, storages)).fold({ e =>
        System.err.println(s"[${c.label}] FAILED: $e"); e.printStackTrace(); false }, _ => true)
    } finally storages.foreach(s => Try(s.close()))
    if (missing.nonEmpty || failed.nonEmpty) sys.exit(1)
  }

  /** Every listing of `c` resolved and its decision written to `<out>/decisions-<cc>.jsonl` (and the rest, above). */
  private def dump(configuration: settings.ProcessConfiguration, target: IntegrationMongoTarget, root: settings.FixtureRoot, c: Corpus, dir: Path,
                   storages: mutable.ListBuffer[ConvergenceStorage]): Unit = {
    val w        = wiring(target, c, storages, root, configuration.env)
    val listings = listingsOf(w, c.normalizer)
    val lookups  = new Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)),
      language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers,
      c.gaps))
    val (resolution, seconds) = timed(IdentityResolver.resolve(listings, lookups, c.normalizer, IdentityCalibration.resolver))
    val clusterOf = resolution.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
    // what argues against each take (SuspectTakes), which the takes check asks a person to judge before a gain passes
    val suspects  = {
      val corpus = SuspectTakes.corpus(resolution, listings, lookups, _ => None)
      val year   = listings.flatMap(_.screenings.first).map(_.getYear).maxOption.getOrElse(SpecClock.PinnedDay.getYear)
      SuspectTakes.suspected(corpus.takes, year, corpus.billings).collect { case (t, found) if found.nonEmpty => t.listing.key -> found.map(_.render) }.toMap
    }
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
        "suspect"     -> JsArray(suspects.getOrElse(l.key, Nil).map(JsString(_))),
        "rules"       -> JsArray((d.trace.rulesOf(l.key) ++ c.normalizer.firedRules(l.cinema, l.rawTitle)).map(JsString(_))),
        // why not: what stopped a listing left with no film, what it searched, what it weighed
        "blocker"     -> (if (d.film.isDefined) JsNull else JsString(d.trace.nodes.get(l.key).flatMap(_.blocker).getOrElse("pooled:no-film"))),
        "searched"    -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.searched).map(JsString(_))),
        "candidates"  -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.candidates).map(JsString(_))),
        "refusals"    -> JsArray(d.trace.nodes.get(l.key).toSeq.flatMap(_.refusals).map(r =>
          JsString(s"${r.rule}: ${r.why}${r.film.fold("")(id => s" [tmdb $id]")}${if (r.detail.nonEmpty) s" — ${r.detail}" else ""}"))))))
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
    configuration.identityAgreementCache.foreach(cache => agreed(configuration, c, w, lookups, listings, resolution, cache, dir))
    println(f"[${c.label}] ${listings.size} listings → ${resolution.decisions.size} clusters " +
      f"(${resolution.decisions.count(_.film.isDefined)} with a film) in $seconds%.1fs; unanswered queries ${resolution.unknownQueries}")
  }

  /** The agreement stage over the resolution's no-matches, the families answered from the experiment's cache, else live —
   *  asked until nothing is open — and every cluster it takes (agreed, by a poster, a broadcast day or a fill rule) written
   *  to `agreed-<cc>.jsonl`. */
  private def agreed(configuration: settings.ProcessConfiguration, c: Corpus, w: ArchiveReplayWiring, lookups: IdentityLookups, listings: Seq[Listing], resolution: Resolution,
                     cache: settings.IdentityAgreementCache, dir: java.nio.file.Path): Unit = {
    import services.identity.agreement.{AgreementStage, VoterFamily}
    val fetch    = new ExperimentCacheFetch(cache.value)
    val answered = new InMemoryTmdbDocuments
    val store    = new FamilyAnswerStore(answered, new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val families = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(Set("pl", "de", "es")(c.country.code))(VoterFamily.Filmweb)
    val sources  = LiveFamilySources.of(fetch, c.country, families)
    val tmdb  = new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language, retrySleep = (_: Long) => ())
    // the venue and TMDB posters' hashes, of the images KINOWO_IDENTITY_POSTER_CACHE keeps, else downloaded
    val posterStore = new PosterAnswerStore(store, new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")))
    val posters     = CachedPosters.of(configuration, c.country)
    val catalogue   = new CatalogueFill(store, new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z")), fetch)
    val stage = new AgreementStage(families.map(family => family -> store.answers(family)).toMap, lookups, c.normalizer,
      IdentityCalibration.resolver, tmdbOf = imdb => Answer.Known(Try(tmdb.findByImdbId(imdb).map(_.id)).toOption.flatten),
      stored = new services.identity.agreement.InMemoryAgreementVerdicts, clock = _root_.tools.SpecClock.Pinned, posters = posterStore, tmdb = Some(lookups),
      identities = modules.wiring.IdentityProjectionWiring.identities(c.country.code), catalogue = catalogue.answers,
      listedOn = modules.wiring.IdentityProjectionWiring.listedOn(c.country.code), correctionPostersAtOnce = Int.MaxValue, correctionsPerApply = Int.MaxValue)
    val byKey = listings.map(l => l.key -> l).toMap
    // each family's questions four at a time, as the experiment read the sites unblocked; one its host never answers given up
    val asked = new AgreementQuestionRounds(store, sources, s"${c.label} agreement").run(() => stage.apply(resolution, byKey.get, store.version))(
      _ => stage.wanted, pending = _ => stage.wantedPosters.nonEmpty || stage.wantedCatalogue.nonEmpty,
      beside = { _ => posters.file(posterStore, stage.wantedPosters.toSeq); catalogue.file(stage.wantedCatalogue) })
    val (taken, rounds) = (asked.outcome, asked.rounds)
    // every answer the stage read, in the family seed's format: what `integration.ModelTakeCorrectionCost` measures over
    Files.writeString(dir.resolve(s"answers-${c.country.code}.jsonl"), answered.get(TmdbKind.Family, answered.fetchedBefore(TmdbKind.Family, Long.MaxValue)
      .map(_._1)).toSeq.sortBy(_._1).map { case (id, d) =>
      val copy = d.clone(); copy.put("_id", new org.bson.BsonString(id)); copy.toJson }.mkString("", "\n", "\n"))
    val taking = Set(ResolverDecision.Basis.Agreed, ResolverDecision.Basis.Poster, ResolverDecision.Basis.Broadcast, ResolverDecision.Basis.Filled,
      ResolverDecision.Basis.Catalogue, ResolverDecision.Basis.Withdrawn, ResolverDecision.Basis.Corrected)
    val lines = taken.decisions.filter(d => taking(d.basis)).flatMap { d =>
      d.members.flatMap(byKey.get).map(l => Json.stringify(JsObject(Seq(
        "venue" -> JsString(l.key.venue), "rawTitle" -> JsString(l.key.rawTitle), "basis" -> JsString(d.basis.toString),
        "film" -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
        "fallback" -> d.fallback.fold[play.api.libs.json.JsValue](JsNull)(f => JsString(f.id)),
        "agreement" -> JsString(d.explanation.lastOption.getOrElse(""))))))
    }
    Files.writeString(dir.resolve(s"agreed-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
    println(s"[${c.label}] agreement: ${taken.decisions.count(_.basis == ResolverDecision.Basis.Agreed)} cluster(s) agreed, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Poster)} taken by their poster, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Broadcast)} by their broadcast day, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Filled)} by a fill rule, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Catalogue)} by their catalogue ids; " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Withdrawn)} model take(s) withdrawn, " +
      s"${taken.decisions.count(_.basis == ResolverDecision.Basis.Corrected)} corrected after $rounds round(s); " +
      s"${stage.wanted.size} question(s), ${stage.wantedPosters.size} poster(s) still open")
    // every listing's film after the stage — what a change compares against its base: a film switched, gained or lost
    Files.writeString(dir.resolve(s"final-${c.country.code}.jsonl"), taken.decisions.flatMap { d =>
      d.members.flatMap(byKey.get).map(l => Json.stringify(JsObject(Seq(
        "venue" -> JsString(l.key.venue), "rawTitle" -> JsString(l.key.rawTitle), "key" -> JsString(ListingKey.serialised(l.key)),
        "basis" -> JsString(d.basis.toString), "film" -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
        "fallback" -> d.fallback.fold[play.api.libs.json.JsValue](JsNull)(f => JsString(s"${f.source}:${f.id}"))))))
    }.sorted.mkString("", "\n", "\n"))
    catalogueReport(c, taken, byKey, catalogue, imdb => Try(tmdb.findByImdbId(imdb).map(_.id)).toOption.flatten, dir)
  }

  /** What every cluster's catalogue ids name — the matched ones too — beside the film it took: `catalogue-<cc>.jsonl`, one
   *  line a cluster carrying a mappable id, `verdict` `agrees`, `contradicts` (the exact id names another TMDB film: a
   *  likely wrong match, to list, never to switch), `untaken` (no film either way) or `unmapped` (its ids map to none). */
  private def catalogueReport(c: Corpus, taken: Resolution, byKey: Map[ListingKey, Listing], catalogue: CatalogueFill, find: String => Option[Int],
                              dir: java.nio.file.Path): Unit = {
    import services.identity.agreement.Catalogue
    val clusters = taken.decisions.map(d => d -> d.members.flatMap(byKey.get)).filter(_._2.nonEmpty)
    var rounds = 0
    var open: Set[CatalogueQuestion] = Set.empty
    while ({ open = clusters.flatMap { case (_, ls) => val asked = mutable.Set.empty[CatalogueQuestion]; Catalogue.named(ls, catalogue.answers, asked); asked }
             .toSet; open.nonEmpty && rounds < 4 }) {
      rounds += 1
      println(s"[${c.label}] catalogue report round $rounds: ${open.size} question(s)")
      catalogue.file(open)
    }
    val lines = clusters.flatMap { case (d, ls) =>
      Catalogue.named(ls, catalogue.answers, mutable.Set.empty).toOption.flatten.map(Some(_)).map { named =>
        val mapped = named.flatMap(n => n.imdb.fold(n.tmdb)(find))
        val verdict = (d.film, named, mapped) match {
          case (_, None, _)                          => "unmapped"
          case (Some(film), _, Some(id)) if film == id => "agrees"
          case (Some(_), _, Some(_))                 => "contradicts"
          case (None, _, Some(_)) if d.basis == ResolverDecision.Basis.Catalogue => "taken"
          case (None, _, Some(_))                    => "untaken"
          case (_, _, None)                          => "no-tmdb"
        }
        Json.stringify(JsObject(Seq(
          "verdict" -> JsString(verdict), "basis" -> JsString(d.basis.toString),
          "film" -> d.film.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
          "filmTitle" -> d.film.flatMap(taken.films.get).fold[play.api.libs.json.JsValue](JsNull)(f => JsString(s"${f.title} (${f.year.getOrElse("?")})")),
          "fallback" -> d.fallback.fold[play.api.libs.json.JsValue](JsNull)(f => JsString(s"${f.source}:${f.id}")),
          "catalogue" -> mapped.fold[play.api.libs.json.JsValue](JsNull)(JsNumber(_)),
          "id" -> named.fold[play.api.libs.json.JsValue](JsNull)(n => JsString(s"${n.id.source}:${n.id.id}")),
          "linked" -> JsString(named.fold("")(n => if (n.linked) "page" else "listing")),
          "item" -> named.flatMap(_.item).fold[play.api.libs.json.JsValue](JsNull)(JsString(_)),
          "imdb" -> named.flatMap(_.imdb).fold[play.api.libs.json.JsValue](JsNull)(JsString(_)),
          "via" -> named.fold[play.api.libs.json.JsValue](JsNull)(n => JsString(n.hit.via)),
          "listings" -> JsNumber(ls.size),
          "members" -> JsArray(ls.take(6).map(l => JsString(s"${l.key.venue} | ${l.key.rawTitle}"))))))
      }
    }
    Files.writeString(dir.resolve(s"catalogue-${c.country.code}.jsonl"), lines.mkString("", "\n", "\n"))
    println(s"[${c.label}] catalogue report: ${lines.size} cluster(s) with a mappable id after $rounds round(s)")
  }
}
