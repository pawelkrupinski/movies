package controllers

import models.{CityScreening, Country, ResolvedMovie}
import play.api.mvc.RequestHeader
import services.MongoConnection
import services.readmodel.ReadModelReader

import java.util.Locale

/** One country's read model as `/debug/readmodel` reads it, and where that is (shown on the page). */
final case class DebugReadModel(reader: ReadModelReader, source: String,
                                 freshness: services.MirrorFreshness = services.MirrorFreshness.notMirrored)

/**
 * Which read model `/debug/readmodel` lists, per request: `?country=xx` when there is one
 * for it, else the serving country's.
 */
final class DebugReadModels private (byCountry: Map[Country, DebugReadModel], default: Country) {
  /** The countries the page can switch between, in [[Country.all]]'s order. */
  def countries: Seq[Country] = Country.all.filter(byCountry.contains)

  def resolve(request: RequestHeader): Country =
    request.getQueryString("country").flatMap(Country.byCode).filter(byCountry.contains).getOrElse(default)

  def apply(country: Country): DebugReadModel = byCountry(country)
}

object DebugReadModels {
  /** With the local mirror (`MONGODB_MOVIES_MIRROR_URI`, `mirror` opening a reader on one of its
   *  databases) every switchable country, the serving one included, reads its `<db>_prod_mirror`:
   *  a dev server serves `kinowo_local`, which nothing fills since the local worker went, so its
   *  own read model is empty. Without it, only the serving country, from the serving database.
   *  `freshnessOf` says how far behind a mirror database is: the debug navbar's age badge. */
  def of(serving: Country, servingReader: ReadModelReader, mirror: Option[String => ReadModelReader],
         freshnessOf: String => services.MirrorFreshness = _ => services.MirrorFreshness.notMirrored): DebugReadModels =
    mirror match {
      case Some(open) =>
        new DebugReadModels((Country.switchable :+ serving).distinct.map { country =>
          val db = MongoConnection.mirrorDbFor(country.mongoDb)
          country -> DebugReadModel(open(db), s"local mirror $db", freshnessOf(db))
        }.toMap, serving)
      case None =>
        new DebugReadModels(Map(serving -> DebugReadModel(servingReader, "the serving database")), serving)
    }
}

/** One film's screening documents in the read model, summarised for its row. */
final case class ScreeningCounts(docs: Int, cinemas: Int, showtimes: Int)

/** The read model as `/debug/readmodel` lists it: every `web_movies` doc sorted by
 *  title, and per film how many `web_screenings` docs, cinemas and showtimes it has. */
final case class ReadModelDump(movies: Seq[ResolvedMovie], counts: Map[String, ScreeningCounts]) {
  def screeningCount: Int = counts.valuesIterator.map(_.docs).sum
  def showtimeCount:  Int = counts.valuesIterator.map(_.showtimes).sum
}

object ReadModelDump {
  /** Both collections read whole, or `None`: a partial read is not listed as a smaller read model.
   *  The screenings are counted while streamed, never held: the US read model is ~100k docs. */
  def read(reader: ReadModelReader): Option[ReadModelDump] =
    reader.findAllMoviesChecked().answered.flatMap { movies =>
      val docs      = scala.collection.mutable.HashMap.empty[String, Int]
      val cinemas   = scala.collection.mutable.HashMap.empty[String, Set[String]]
      val showtimes = scala.collection.mutable.HashMap.empty[String, Int]
      val scan = reader.foreachServedScreening { (s: CityScreening) =>
        docs.updateWith(s.filmId)(n => Some(n.getOrElse(0) + 1))
        cinemas.updateWith(s.filmId)(c => Some(c.getOrElse(Set.empty) + s.cinema))
        showtimes.updateWith(s.filmId)(n => Some(n.getOrElse(0) + s.showtimes.size))
      }
      Option.when(scan.isComplete) {
        val counts = docs.iterator.map { case (film, n) => film -> ScreeningCounts(n, cinemas(film).size, showtimes(film)) }.toMap
        ReadModelDump(movies.sortBy(_.title.toLowerCase(Locale.ROOT)), counts)
      }
    }
}
