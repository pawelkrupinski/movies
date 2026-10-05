package integration

import clients.TmdbClient
import play.api.libs.json.{JsArray, Json}
import services.identity.{PosterAnswerStore, PosterHash, PosterHashing}
import services.identity.agreement.AgreementStage.PosterQuestion
import services.sharecards.{HttpPosterDownload, PosterDownload, PosterFailure}

import java.nio.file.{Files, Path, StandardCopyOption}

/**
 * Posters for the unmatched-cluster capture's poster evidence, from a cache of real downloads (`KINOWO_IDENTITY_POSTER_CACHE`:
 * `img/<sha1 of the URL>`, an `img/<sha1>.err` for one that failed for good; `tmdb/<id>.json`, a film's TMDB poster list
 * as `{"posters": [{"p": path, "l": language|null, "v": votes}]}`), else fetched live and filed into it — so a re-capture
 * asks nothing twice. Real images only: what the capture files is their hashes.
 */
final class CachedPosters(dir: Path, live: Option[TmdbClient], language: String) {
  private val images = dir.resolve("img")
  private val films  = dir.resolve("tmdb")
  private val fetch  = new HttpPosterDownload()

  private def sha1(text: String) =
    java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-1").digest(text.getBytes("UTF-8")))

  /** Each poster from the cache — a copy, which the hashing deletes — else downloaded and kept. */
  val download: PosterDownload = new PosterDownload {
    def fetch(url: String): Either[String, Path] = {
      val kept = images.resolve(sha1(url))
      val err  = images.resolve(sha1(url) + ".err")
      def copy(from: Path) = { val to = Files.createTempFile("poster-", ".img"); Files.copy(from, to, StandardCopyOption.REPLACE_EXISTING); to }
      if (Files.exists(kept)) Right(copy(kept))
      else if (Files.exists(err)) Left(PosterFailure.Http4xx)
      else CachedPosters.this.fetch.fetch(url) match {
        case Right(file) => Files.createDirectories(images); Files.copy(file, kept, StandardCopyOption.REPLACE_EXISTING); Right(file)
        case Left(reason) =>
          if (!PosterHashing.Passing(reason)) { Files.createDirectories(images); Files.writeString(err, reason) }
          Left(reason)
      }
    }
  }

  /** A film's TMDB posters in the languages production asks for (its country's, English, none), from the cache else live. */
  def posters(tmdbId: Int): Seq[TmdbClient.PosterImage] = {
    val file = films.resolve(s"$tmdbId.json")
    val all = if (Files.exists(file))
      (Json.parse(Files.readString(file)) \ "posters").asOpt[JsArray].toSeq.flatMap(_.value).map { p =>
        TmdbClient.PosterImage((p \ "p").as[String], (p \ "l").asOpt[String], 0.667, 0.0, 0, (p \ "v").asOpt[Int].getOrElse(0))
      }
    else live.fold(Seq.empty[TmdbClient.PosterImage]) { client =>
      val found = client.posters(tmdbId, also = Seq("en"))
      Files.createDirectories(films)
      Files.writeString(file, Json.stringify(Json.obj("id" -> tmdbId, "posters" -> found.map(p =>
        Json.obj("p" -> p.filePath, "l" -> p.language, "v" -> p.voteCount)))))
      found
    }
    all.filter(_.language.forall(l => l == language || l == "en"))
  }

  private val hashing = new PosterHashing(download, new services.sharecards.VipsPosterShrinker(binary = None), posters, language)

  /** Each poster of `questions` hashed and filed in `store`, eight at a time — one that fails twice filed as no poster. */
  def file(store: PosterAnswerStore, questions: Seq[PosterQuestion]): Unit = {
    def hashed(question: PosterQuestion): Seq[PosterHash] = {
      def once() = question match {
        case PosterQuestion.Venue(url)   => hashing.venue(url).toSeq
        case PosterQuestion.Film(tmdbId) => hashing.film(tmdbId)
      }
      scala.util.Try(once()).orElse(scala.util.Try(once())).getOrElse(Nil)
    }
    questions.grouped(8).foreach(_.map(question => java.util.concurrent.CompletableFuture.runAsync(() => store.file(question, hashed(question))))
      .foreach(_.join()))
  }
}

object CachedPosters {
  /** The posters `KINOWO_IDENTITY_POSTER_CACHE` keeps (else `target/identity-posters`), the rest fetched live — TMDB's lists
   *  with `KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY`. */
  def of(configuration: settings.ProcessConfiguration, country: models.Country): CachedPosters =
    new CachedPosters(configuration.identityPosterCache.fold(Path.of("target", "identity-posters"))(_.value),
      configuration.identityLiveGaps.map(key => new TmdbClient(new tools.RealHttpFetch(), apiKey = Some(settings.TmdbApiKey(key.tmdbKey)),
        language = country.language, retrySleep = (_: Long) => ())), country.language.getLanguage)
}
