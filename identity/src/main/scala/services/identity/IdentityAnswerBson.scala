package services.identity

import org.mongodb.scala.bson.BsonArray
import org.bson.{BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}

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
    f.released.foreach(day => d.append("released", BsonString(day.toString)))
    f.releaseCountries.foreach(codes => d.append("releaseCountries", BsonString(codes)))
    f.releases.foreach(codes => d.append("releases", BsonString(codes)))
    if (f.alternativeRuntimes.nonEmpty) d.append("alternativeRuntimes", BsonArray.fromIterable(f.alternativeRuntimes.map(BsonInt32(_))))
    if (f.taglineKey != 0) d.append("taglineKey", BsonInt32(f.taglineKey))
    f.certificates.foreach(codes => d.append("certificates", BsonString(codes)))
    f.originalLanguage.foreach(code => d.append("originalLanguage", BsonString(code)))
    if (f.genreBits != 0) d.append("genreBits", BsonInt32(f.genreBits))
    f.cast.foreach(cast => d.append("castPairs", BsonArray.fromIterable(cast.pairs.map(name => BsonArray.fromIterable(name.map(BsonInt32(_)))))))
    d
  }

  def filmOf(value: BsonValue): Option[IdentityMeasures.Film] = Option.when(!value.isNull)(value.asDocument).map { d =>
    IdentityMeasures.Film(d.getString("title").getValue, string(d, "originalTitle"), BsonFields.stringsOf(d.get("alternativeTitles")),
      int(d, "year"), int(d, "runtime"), Option(d.get("directors")).map(BsonFields.stringsOf(_)), Option(d.get("countries")).map(BsonFields.stringsOf(_)),
      Option(d.get("popularity")).map(_.asDouble.getValue), int(d, "imdbNumber").getOrElse(0),
      string(d, "released").flatMap(day => scala.util.Try(java.time.LocalDate.parse(day)).toOption), string(d, "releaseCountries"),
      Option(d.get("alternativeRuntimes")).fold(Seq.empty[Int])(BsonFields.intsOf),
      string(d, "releases"), int(d, "taglineKey").getOrElse(0), string(d, "certificates"), string(d, "originalLanguage").map(_.intern),
      int(d, "genreBits").getOrElse(0),
      Option(d.get("castPairs")).flatMap(names => FilmCast.ofPairs({ import scala.jdk.CollectionConverters._; names.asArray.getValues.asScala.toSeq.map(BsonFields.intsOf) })))
  }

  private def strings(values: Seq[String]): BsonArray = BsonArray.fromIterable(values.map(BsonString(_)))
  private def string(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).map(_.asString.getValue)
  private def int(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).map(_.asInt32.getValue)
}
