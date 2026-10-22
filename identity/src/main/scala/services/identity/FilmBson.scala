package services.identity

import org.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/**
 * A film record ([[IdentityMeasures.Film]]) as a stored document, EVERY fact of it: what a store keeps of a record a
 * decision reads again after a restart must be all it read the first time. The agreement's verdicts kept an agreed film
 * as its title and year alone, and the stage a deploy brought up took films its first decision had refused on the
 * record's original title (DE "… Eröffnungskonzert - Raíces", identity-slice --drift). A field left unset is left out.
 */
object FilmBson {
  def encode(film: IdentityMeasures.Film): BsonDocument = {
    val d = new BsonDocument("title", BsonString(film.title))
    def put(name: String, value: Option[BsonValue]): Unit = value.foreach(d.append(name, _))
    def strings(values: Seq[String]) = new BsonArray(values.map(v => BsonString(v): BsonValue).asJava)
    def ints(values: Seq[Int])       = new BsonArray(values.map(v => BsonInt32(v): BsonValue).asJava)
    put("originalTitle", film.originalTitle.map(BsonString(_)))
    put("alternativeTitles", Option.when(film.alternativeTitles.nonEmpty)(strings(film.alternativeTitles)))
    put("year", film.year.map(BsonInt32(_)))
    put("runtime", film.runtime.map(BsonInt32(_)))
    put("directors", film.directors.map(strings))
    put("countries", film.countries.map(strings))
    put("popularity", film.popularity.map(BsonDouble(_)))
    put("imdbNumber", Option.when(film.imdbNumber != 0)(BsonInt32(film.imdbNumber)))
    put("released", film.released.map(day => BsonString(day.toString)))
    put("releaseCountries", film.releaseCountries.map(BsonString(_)))
    put("alternativeRuntimes", Option.when(film.alternativeRuntimes.nonEmpty)(ints(film.alternativeRuntimes)))
    put("releases", film.releases.map(BsonString(_)))
    put("taglineKey", Option.when(film.taglineKey != 0)(BsonInt32(film.taglineKey)))
    put("certificates", film.certificates.map(BsonString(_)))
    put("originalLanguage", film.originalLanguage.map(BsonString(_)))
    put("genreBits", Option.when(film.genreBits != 0)(BsonInt32(film.genreBits)))
    put("cast", film.cast.map(cast => new BsonArray(cast.pairs.map(pair => ints(pair): BsonValue).asJava)))
    d
  }

  def decode(d: BsonDocument): IdentityMeasures.Film = {
    def opt(name: String): Option[BsonValue] = Option(d.get(name)).filterNot(_.isNull)
    def string(name: String)  = opt(name).map(_.asString.getValue)
    def int(name: String)     = opt(name).map(_.asInt32.getValue)
    def strings(name: String) = opt(name).map(BsonFields.stringsOf)
    IdentityMeasures.Film(d.getString("title").getValue, string("originalTitle"), strings("alternativeTitles").getOrElse(Nil),
      int("year"), int("runtime"), strings("directors"), strings("countries"), opt("popularity").map(_.asNumber.doubleValue),
      int("imdbNumber").getOrElse(0), string("released").map(java.time.LocalDate.parse), string("releaseCountries"),
      opt("alternativeRuntimes").map(BsonFields.intsOf).getOrElse(Nil), string("releases"), int("taglineKey").getOrElse(0),
      string("certificates"), string("originalLanguage").map(_.intern), int("genreBits").getOrElse(0),
      opt("cast").flatMap(cast => FilmCast.ofPairs(cast.asArray.getValues.asScala.toSeq.map(BsonFields.intsOf))))
  }
}
