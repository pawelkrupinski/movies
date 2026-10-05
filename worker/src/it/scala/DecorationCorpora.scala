package integration

import scripts.IdentityCalibrationData.{TmdbAnswers, languageOf}
import services.identity._
import services.movies.ListingKey
import tools.{ConvergenceStorage, IntegrationMongoTarget}

import java.nio.file.{Files, Paths}
import scala.collection.mutable

/** The recorded full corpora the decoration experiments resolve (`IdentityDecorationCandidates`,
 *  `IdentityDecorationTokens`): every country's listings with memoised lookups (the recording, its gaps asked live with
 *  the live-gaps key), the recorded film records' titles, and a resolve of them all under a decoration set. */
final class DecorationCorpora(configuration: settings.ProcessConfiguration, storages: mutable.ListBuffer[ConvergenceStorage]) {
  import DecorationCorpora._

  private val target = IntegrationMongoTarget.from(configuration).getOrElse(sys.error("MONGODB_URI names no throwaway Mongo"))
  target.requireThrowaway()
  private val corpusDir = configuration.identityCorpusDirectory.getOrElse(sys.error("KINOWO_IDENTITY_CORPUS_DIR")).value

  val loaded: Seq[Loaded] = IdentityShadow.full(configuration.identityFullCorpora.value, corpusDir, configuration.fixtureRoot, configuration.identityLiveGaps)
    .map { c =>
      val w = IdentityShadow.wiring(target, c, storages, configuration.fixtureRoot, configuration.env)
      val lookups = new IdentityShadow.Memo(new TmdbIdentityLookups(new clients.TmdbClient(c.fetch, apiKey = Some(settings.TmdbApiKey(IdentityShadow.StubTmdbKey)),
        language = c.country.language, retrySleep = (_: Long) => ()), new services.enrichment.ImdbClient(c.fetch), w.detailEnrichers, c.gaps))
      Loaded(c, IdentityShadow.listingsOf(w, c.normalizer), lookups)
    }

  /** Each country's recorded TMDB answers (the recording alone: nothing is asked live). */
  lazy val answers: Map[String, TmdbAnswers] = loaded.map { l =>
    l.c.country.code -> new TmdbAnswers(Seq(Paths.get(configuration.fixtureRoot.of(s"enrichment-${l.c.country.code}"))).filter(Files.isDirectory(_)),
      Map.empty, languageOf(l.c.country))
  }.toMap

  /** Every recorded film record's titles: title, original title, alternative titles. */
  lazy val records: Seq[String] = answers.values.toSeq.flatMap(_.films.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles))

  val byKey: Map[(String, ListingKey), Listing] = loaded.flatMap(l => l.listings.map(x => (l.c.country.code, x.key) -> x)).toMap

  /** Every listing's film under `decorations`, by country and key. */
  def resolveAll(decorations: TitleDecorations): Map[(String, ListingKey), Taken] = loaded.flatMap { l =>
    val started = System.nanoTime()
    val r = IdentityResolver.resolve(l.listings, l.lookups, l.c.normalizer, IdentityCalibration.resolver, decorations = decorations)
    println(f"[${l.c.label}] resolved ${l.listings.size} listings in ${(System.nanoTime() - started) / 1e9}%.1fs; unanswered ${r.unknownQueries}")
    val clusterOf = r.decisions.zipWithIndex.flatMap { case (d, i) => d.members.map(_ -> i) }.toMap
    l.listings.map { x =>
      val d = r.decisionOf(x.key)
      val film = d.film.map(id => s"tmdb:$id").orElse(d.fallback.filter(_.source == "imdb").map(f => s"imdb:${f.id}")).getOrElse("")
      val record = d.film.flatMap(r.films.get)
      (l.c.country.code, x.key) -> Taken(film, clusterOf.getOrElse(x.key, -1), record.fold("")(f => s"${f.title} (${f.year.getOrElse("?")})"),
        record.toSeq.flatMap(f => Seq(f.title) ++ f.originalTitle ++ f.alternativeTitles))
    }
  }.toMap
}

object DecorationCorpora {
  final case class Loaded(c: IdentityShadow.Corpus, listings: Seq[Listing], lookups: IdentityShadow.Memo)
  /** One listing's film under a decoration set: `tmdb:<id>`, `imdb:<tt>` for a fallback, `""` for none; its cluster, and
   *  the TMDB record's display line and titles. */
  final case class Taken(film: String, cluster: Int, title: String, titles: Seq[String] = Nil)
}
