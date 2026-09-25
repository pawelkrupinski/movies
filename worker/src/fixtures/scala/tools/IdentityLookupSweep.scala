package tools

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.{ScrapeListing, TitleNormalizer}

import scala.util.Try

/**
 * The identity resolver's QUERY SET over one corpus, issued once, so a recording pass captures
 * every answer the resolver will ask a replay for (docs/design/identity-resolver.md, "Recording
 * the resolver's queries").
 *
 * The pipeline resolves a MERGED group's evidence; the resolver resolves each listing's OWN
 * evidence, under query shapes the pipeline never makes. Against recorder run 36153174348's
 * trees that left PL 200, UK 140 (123 of them Cineworld's detail API), US 18 and DE 2 requests
 * unanswered, each replayed as a remembered 404 — a definitive no-match the live answer might
 * not have been. Run through a recording leg's fetch chain, this sweep fills exactly those.
 *
 * The set, a function of the listing SET alone (never of arrival order or of a memo):
 *
 *  1. every raw listing — `ScrapeListing.prepare`'s per-title fold NOT applied, because the
 *     resolver reads the rows it erases ("Sinn und Sinnlichkeit" 1995 beside 2026) — in the total
 *     order of [[Listing.sortKey]];
 *  2. the venue's detail page of each listing that has one at a venue with a `DetailEnricher`,
 *     once per (venue, page);
 *  3. `MovieService.resolveStagingRecord` of each DISTINCT [[Evidence]] (listing merged with its
 *     own detail, listing values winning), in [[Evidence.key]] order, on a slot of ONE fixed
 *     catalogue venue so the answer is a function of the evidence and not of the venue.
 *
 * `resolve` must not memoise across calls (pass a `MovieService` built with
 * `ResolutionCache.passthrough`): a memo hit skips the very requests a memo-free replay makes.
 */
object IdentityLookupSweep {

  /** One raw listing, as the resolver reads it. */
  final case class Listing(cinema: Cinema, rawTitle: String, cleanTitle: String, year: Option[Int],
                           directors: Seq[String], runtime: Option[Int], page: Option[String],
                           originalTitle: Option[String]) {
    /** A TOTAL order: every field, so two different listings never tie. */
    lazy val sortKey: String =
      Seq(cinema.displayName, rawTitle, cleanTitle, year.fold("")(_.toString), directors.mkString(","),
        runtime.fold("")(_.toString), page.getOrElse(""), originalTitle.getOrElse("")).mkString("\u0000")
  }

  /** A listing's evidence once its own detail page is merged in. Venue-free on purpose: two
   *  venues publishing the same evidence ask the same question. */
  final case class Evidence(cleanTitle: String, rawTitle: String, year: Option[Int], directors: Seq[String],
                            runtime: Option[Int], originalTitle: Option[String]) {
    lazy val key: String =
      Seq(cleanTitle, rawTitle, year.fold("")(_.toString), directors.sorted.mkString(","),
        runtime.fold("")(_.toString), originalTitle.getOrElse("")).mkString("\u0000")

    /** The staging record the resolve reads: one slot of `cinema` carrying this evidence. */
    def record(cinema: Cinema, normalizer: TitleNormalizer): MovieRecord =
      MovieRecord(data = Map[Source, SourceData](CinemaShowing.keyFor(cinema, cleanTitle, normalizer) ->
        SourceData(title = Some(cleanTitle), rawTitle = Some(rawTitle), originalTitle = originalTitle,
          director = directors, runtimeMinutes = runtime, releaseYear = year)))
  }

  object Evidence {
    def of(listing: Listing, detail: Option[FilmDetail]): Evidence = Evidence(
      cleanTitle    = listing.cleanTitle,
      rawTitle      = listing.rawTitle,
      year          = listing.year.orElse(detail.flatMap(_.releaseYear)),
      directors     = if (listing.directors.nonEmpty) listing.directors else detail.map(_.director).getOrElse(Nil),
      runtime       = listing.runtime.filter(_ > 0).orElse(detail.flatMap(_.runtimeMinutes).filter(_ > 0)),
      originalTitle = listing.originalTitle.orElse(detail.flatMap(_.originalTitle)))
  }

  final case class Summary(listings: Int, detailLookups: Int, detailFailures: Int, resolves: Int, resolveFailures: Int,
                           resolved: Int) {
    override def toString: String =
      s"$listings listing(s): $detailLookups detail lookup(s) ($detailFailures failed), " +
        s"$resolves resolve(s) ($resolveFailures failed, $resolved matched a film)"
  }

  /** Set to `true` to run the sweep in a convergence leg: a RECORDING leg records every answer
   *  the tree lacks, a HERMETIC one fails on each by name (`CountryConvergenceBehaviour`). */
  val EnvVar = "KINOWO_IDENTITY_LOOKUPS"

  def enabledFromEnv: Boolean = Env.fromProcess().get(EnvVar).exists(_.trim.equalsIgnoreCase("true"))

  /** The sweep over a booted replay wiring: its archived listings, its venues' detail
   *  enrichers, and a `MovieService` over its own TMDB client — built WITHOUT the wiring's
   *  resolution memo (the constructor's passthrough default), so every evidence issues the
   *  requests a memo-free resolver replay will. Both clients fetch through the wiring's
   *  recording chain, which is what files the answers into the leg's tree. */
  def over(w: ArchiveReplayWiring, country: models.Country): Summary = {
    val service = new services.movies.MovieService(w.movieCache, w.eventBus, w.tmdbClient, clock = w.clock,
      letterboxdIdResolver = Some(w.letterboxdIdResolver), wikidata = Some(w.wikidataClient))
    try run(w.archivedListings, w.detailEnrichers, service.resolveStagingRecord,
      CountryScrapeCorpus.cinemasOf(country).minBy(_.displayName), w.movieCache.normalizer)
    finally service.stop()
  }

  /** Every raw listing of `archived`, distinct, in the total order. */
  def listings(archived: Map[Cinema, Seq[CinemaMovie]], normalizer: TitleNormalizer): Seq[Listing] =
    archived.toSeq.flatMap { case (cinema, films) =>
      films.map { cm =>
        Listing(cinema, cm.movie.rawTitle.getOrElse(cm.movie.title), ScrapeListing.cleanTitle(cinema, cm.movie.title, normalizer)._1,
          cm.movie.releaseYear, cm.director, cm.movie.runtimeMinutes, cm.filmUrl, cm.movie.originalTitle)
      }
    }.distinctBy(_.sortKey).sortBy(_.sortKey)

  /** Issue the whole query set. `slotCinema` is the fixed venue every resolve's slot sits on —
   *  pick it by a rule of the catalogue (the country's first venue by name), never of the corpus. */
  def run(archived: Map[Cinema, Seq[CinemaMovie]], enrichers: Seq[DetailEnricher],
          resolve: (String, Option[Int], MovieRecord) => Option[MovieRecord], slotCinema: Cinema,
          normalizer: TitleNormalizer): Summary = {
    val all       = listings(archived, normalizer)
    val enricher  = enrichers.map(e => e.cinema -> e).toMap
    val detailed  = scala.collection.mutable.LinkedHashMap.empty[(String, String), Try[Option[FilmDetail]]]
    def detailOf(l: Listing): Option[FilmDetail] = (l.page, enricher.get(l.cinema)) match {
      case (Some(page), Some(e)) => detailed.getOrElseUpdate((l.cinema.displayName, page), Try(e.fetchFilmDetail(page))).toOption.flatten
      case _                     => None
    }
    val evidences = all.map(l => Evidence.of(l, detailOf(l))).distinctBy(_.key).sortBy(_.key)
    val answers   = evidences.map(e => Try(resolve(e.cleanTitle, e.year, e.record(slotCinema, normalizer))))
    Summary(all.size, detailed.size, detailed.values.count(_.isFailure), evidences.size, answers.count(_.isFailure),
      answers.count(_.toOption.flatten.exists(_.tmdbId.isDefined)))
  }
}
