package scripts

import models.Country
import org.bson.BsonDocument
import org.mongodb.scala.{MongoClient, MongoDatabase, ObservableFuture}
import services.identity._
import services.identity.adapters.{HistoryEntry, IdentityHistoryReplay, MongoIdentityHistory, MongoTmdbDocuments, TakeExplanation}
import services.movies.{ListingKey, TitleNormalizer}

import java.time.{Clock, Instant}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The identity history (`identity_history`) read off the LOCAL prod mirror (`mongodb://127.0.0.1:28017`, read-only):
 *
 *  - `why <cc> --listing "venue|title" [--at T]` — why the listing changed film at T (default now), from the history
 *    alone ([[TakeExplanation]]): its take there, the take before, the listings that joined or left its decision and
 *    every input it read that moved between the two (`scripts/identity-why`);
 *  - `replay <cc> --at T [--listing "venue|title"]` — the model's take at T, resolved again over the answers, venue pages
 *    and listings as they stood then ([[IdentityHistoryReplay]]; `scripts/identity-replay`). The model's: the agreement
 *    stage after it reads answers the history keeps too, but is not replayed here.
 *
 * `venue|title` matches each listing whose venue and billed title contain those (case aside).
 */
object IdentityHistoryTools {
  def main(args: Array[String]): Unit = {
    val clock           = Clock.systemUTC()
    val (command, rest) = (args.headOption.getOrElse(""), args.drop(1).toList)
    val cc      = rest.headOption.getOrElse(sys.error("a country code"))
    val country = Country.all.find(_.code == cc).getOrElse(sys.error(s"no country $cc"))
    def opt(name: String) = rest.sliding(2).collectFirst { case List(`name`, value) => value }
    val at      = opt("--at").map(Instant.parse(_).toEpochMilli).getOrElse(clock.millis())
    val wanted  = opt("--listing").map(_.split('|').map(_.trim.toLowerCase(java.util.Locale.ROOT)).toList)
    val uri     = opt("--mongo").getOrElse("mongodb://127.0.0.1:28017/?directConnection=true")
    val client  = MongoClient(uri)
    try {
      val db     = client.getDatabase(services.MongoConnection.mirrorDbFor(country.mongoDb))
      val reader = new MongoIdentityHistory(db, new services.TtlIndexMismatches)
      def matches(key: ListingKey) = wanted.forall {
        case venue :: title :: _ => key.venue.toLowerCase(java.util.Locale.ROOT).contains(venue) && key.rawTitle.toLowerCase(java.util.Locale.ROOT).contains(title)
        case other               => key.rawTitle.toLowerCase(java.util.Locale.ROOT).contains(other.mkString("|"))
      }
      command match {
        case "why" =>
          val keys = reader.since(Seq(HistoryEntry.Take), 0L).map(_.id).distinct.filter(id => ListingKey.parse(id).exists(matches))
          if (keys.isEmpty) println(s"no take of ${opt("--listing").getOrElse("?")} in $cc's history")
          keys.flatMap(TakeExplanation.explain(reader, _, at)).foreach(e => println(e.render.mkString("\n")))
        case "replay" => replay(db, reader, country, at, matches, clock)
        case other    => sys.error(s"unknown command '$other': why | replay")
      }
    } finally client.close()
  }

  private def replay(db: MongoDatabase, reader: MongoIdentityHistory, country: Country, at: Long, matches: ListingKey => Boolean,
                     clock: Clock): Unit = {
    val normalizer = TitleNormalizer.forCountry(country)
    val replay     = IdentityHistoryReplay.from(reader, at)
    val rows       = tools.ProdListings.read(db, tools.CountryScrapeCorpus.cinemasOf(country).toSet)
    val listings   = replay.listings(ModelRead.corpus(rows.map(r => r.cinema -> r.films), normalizer))
    val pages      = replay.pages(Await.result(db.getCollection[BsonDocument](services.venuepages.MongoVenuePageStore.Collection).find()
      .batchSize(tools.MongoReplies.Default).toFuture(), 5.minutes))
    val lookups    = tools.ProdLookupInputs.lookups(replay.documents(new MongoTmdbDocuments(db)), pages,
      tools.ProdLookupInputs.enrichersOf(country, clock), Refused, country, clock)
    val core       = listings.filter(l => matches(l.key))
    val pins       = new MongoPinStore(Some(db)).all()
    println(s"replaying $country at ${Instant.ofEpochMilli(at)}: ${listings.size} listings, ${core.size} selected")
    val decisions  = services.identity.IdentitySlicing.resolve(if (core.isEmpty) listings else core, listings, lookups, normalizer,
      services.identity.OlderRefs.resolveInputs(pins, country.code))
    decisions.filter(_.members.exists(matches)).foreach { d =>
      d.members.filter(matches).foreach(k => println(s"${k.venue} | ${k.rawTitle}: ${d.film.fold("no film")(id => s"tmdb $id")} ${d.basis} " +
        s"(${ResolverDecision.percent(d.confidence)}) — ${d.explanation.headOption.getOrElse("")}"))
    }
  }

  /** Every live ask refused: a replay reads what the store held, never TMDB now. */
  private object Refused extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]       = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }
}
