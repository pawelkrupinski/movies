package services.identity

import scala.jdk.CollectionConverters.*

import org.bson.{BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.bson.BsonArray
import services.identity.measures.Film
import services.identity.vocabulary.{BsonFields, FilmCast, Hit}

/** A film's record ([[Film]]) as native BSON, EVERY fact of it — the one format every store writes a film in
 *  (`tmdb_films`' and the TMDB queries' `record`, `identity_family_answers`' records, `identity_agreements`' agreed film,
 *  the capture fixtures): explicit, like [[ResolverDecisionBson]], an absent field is absent (directors unknown ≠ none),
 *  "TMDB knows no such film" is `null`. What a store keeps of a record a decision reads again after a restart must be
 *  all it read the first time (an agreed film kept as its title and year alone took, after a deploy, a film its first
 *  decision had refused on the record's original title: DE "… Eröffnungskonzert - Raíces", identity-slice --drift). */
object IdentityAnswerBson {
  def film(film: Option[Film]): BsonValue = film.fold[BsonValue](BsonNull())(record)

  def record(f: Film): BsonDocument = {
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
    f.cast.foreach(cast => d.append(CastField, BsonArray.fromIterable(cast.pairs.map(name => BsonArray.fromIterable(name.map(BsonInt32(_)))))))
    d
  }

  private val CastField = "castPairs"
  /** The cast's field in the format agreed films were written in before every store wrote this one. */
  private val LegacyCastField = "cast"

  def filmOf(value: BsonValue): Option[Film] = Option.when(!value.isNull)(value.asDocument).map(recordOf)

  /** A record this format wrote, or one in the agreed films' earlier format (`cast` for the cast, no `alternativeTitles`
   *  when there were none, null for an unknown field): a field absent or null is
   *  unknown, a day that does not parse is none, a popularity stored as any number reads as its double. */
  def recordOf(d: BsonDocument): Film =
    Film(d.getString("title").getValue, string(d, "originalTitle"), field(d, "alternativeTitles").fold(Seq.empty[String])(BsonFields.stringsOf),
      int(d, "year"), int(d, "runtime"), field(d, "directors").map(BsonFields.stringsOf(_)), field(d, "countries").map(BsonFields.stringsOf(_)),
      field(d, "popularity").map(_.asNumber.doubleValue), int(d, "imdbNumber").getOrElse(0),
      string(d, "released").flatMap(day => scala.util.Try(java.time.LocalDate.parse(day)).toOption), string(d, "releaseCountries"),
      field(d, "alternativeRuntimes").fold(Seq.empty[Int])(BsonFields.intsOf),
      string(d, "releases"), int(d, "taglineKey").getOrElse(0), string(d, "certificates"), string(d, "originalLanguage").map(_.intern),
      int(d, "genreBits").getOrElse(0),
      field(d, CastField).orElse(field(d, LegacyCastField)).flatMap(names => FilmCast.ofPairs(names.asArray.getValues.asScala.toSeq.map(BsonFields.intsOf))))

  /** The hit a record gives -- its title, original title, year and popularity (`unknownPopularity` when it states none),
   *  read as [[filmOf]] reads them, and nothing else of it: every film a search names is asked for its hit, and decoding
   *  each one's whole record for four fields was most of what a take-up's hits allocated (docs/design/
   *  corpus-context-persistence.md). */
  def hitOf(id: Int, record: BsonDocument, unknownPopularity: Double): Hit =
    Hit(id, record.getString("title").getValue, string(record, "originalTitle"), int(record, "year"),
      field(record, "popularity").fold(unknownPopularity)(_.asNumber.doubleValue))

  /** `d`'s field `name`, none when it is absent or null. */
  private def field(d: BsonDocument, name: String): Option[BsonValue] = Option(d.get(name)).filterNot(_.isNull)
  private def string(d: BsonDocument, name: String): Option[String] = field(d, name).map(_.asString.getValue)
  private def int(d: BsonDocument, name: String): Option[Int]       = field(d, name).map(_.asInt32.getValue)

  private def strings(values: Seq[String]): BsonArray = BsonArray.fromIterable(values.map(BsonString(_)))
}
