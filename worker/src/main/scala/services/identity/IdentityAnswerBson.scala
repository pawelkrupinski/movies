package services.identity

import org.mongodb.scala.bson.BsonArray
import org.bson.{BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/** A film's record as native BSON — explicit, like [[ResolverDecisionBson]]: an absent field is
 *  absent (directors unknown ≠ none), "TMDB knows no such film" is `null`. */
object IdentityAnswerBson {
  def film(film: Option[IdentityMeasures.Film]): BsonValue = film.fold[BsonValue](BsonNull()) { f =>
    val d = new BsonDocument().append("title", BsonString(f.title)).append("alternativeTitles", strings(f.alternativeTitles))
    f.originalTitle.foreach(t => d.append("originalTitle", BsonString(t)))
    f.year.foreach(y => d.append("year", BsonInt32(y)))
    f.runtime.foreach(r => d.append("runtime", BsonInt32(r)))
    f.directors.foreach(ds => d.append("directors", strings(ds)))
    f.countries.foreach(cs => d.append("countries", strings(cs)))
    f.popularity.foreach(p => d.append("popularity", BsonDouble(p)))
    if (f.imdbNumber > 0) d.append("imdbNumber", BsonInt32(f.imdbNumber))
    d
  }

  def filmOf(value: BsonValue): Option[IdentityMeasures.Film] = Option.when(!value.isNull)(value.asDocument).map { d =>
    IdentityMeasures.Film(d.getString("title").getValue, string(d, "originalTitle"), stringsOf(d.get("alternativeTitles")),
      int(d, "year"), int(d, "runtime"), Option(d.get("directors")).map(stringsOf), Option(d.get("countries")).map(stringsOf),
      Option(d.get("popularity")).map(_.asDouble.getValue), int(d, "imdbNumber").getOrElse(0))
  }

  private def strings(values: Seq[String]): BsonArray = BsonArray.fromIterable(values.map(BsonString(_)))
  private def stringsOf(value: BsonValue): Seq[String] = value.asArray.getValues.asScala.toSeq.map(_.asString.getValue)
  private def string(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).map(_.asString.getValue)
  private def int(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).map(_.asInt32.getValue)
}
