package services.identity

import models.Source
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString, BsonValue}
import services.movies.ListingKey

import scala.jdk.CollectionConverters._

/** A listing as BSON, every field the model and the stages after it read: what the unmatched-cluster capture keeps of
 *  its listings, and what the identity history keeps of each listing the model takes in (`IdentityHistory`) — read back
 *  equal, so a replay resolves the listing the worker resolved. */
object ListingBson {
  def doc(l: Listing): BsonDocument = {
    val d = new BsonDocument("cinema", new BsonString(l.cinema.displayName)).append("key", new BsonString(ListingKey.serialised(l.key)))
      .append("rawTitle", new BsonString(l.rawTitle)).append("title", new BsonString(l.title)).append("cleanTitle", new BsonString(l.cleanTitle))
      .append("directors", strings(l.directors)).append("countries", strings(l.countries))
      .append("catalogueIds", array(l.catalogueIds.map(c => new BsonDocument("source", new BsonString(c.source)).append("id", new BsonString(c.id)))))
    putInt(d, "year", l.year); putInt(d, "runtime", l.runtime); put(d, "page", l.page); put(d, "originalTitle", l.originalTitle)
    if (!l.screenings.isEmpty) d.append("screenings", strings(l.screenings.days.map(_.toString)))
    // a lean read's season alone (`ModelRead`): no day, but the season the rules read, and the year of its first day,
    // which a rule dating a film against its screenings reads (`ScreeningDays.firstYear`): read back as the season's
    // opening year, a listing first screening in January 2027 replayed as one of 2026 -- current for a 2025 film it was
    // not current for live (UK Regal Stowmarket "Song Sung Blue- The Music of Neil Diamond", Identity capture #2084, #2104)
    else l.screenings.season.foreach { season =>
      d.append("season", new BsonInt32(season)); putInt(d, "firstYear", l.screenings.firstYear)
    }
    if (!l.names.isEmpty) d.append("names", array(l.names.hashes.map(new BsonInt32(_))))
    if (!l.words.isEmpty) d.append("words", array(l.words.hashes.map(new BsonInt32(_))))
    if (!l.listedCast.isEmpty) d.append("listedCast", array(l.listedCast.hashes.map(new BsonInt32(_))))
    put(d, "searchTitle", l.searchTitle); put(d, "properTitle", l.properTitle); put(d, "poster", l.poster); put(d, "certificate", l.certificate)
    putInt(d, "yearUpperBound", l.yearUpperBound)
    putInt(d, "performanceYear", l.performanceYear)
    if (l.kept) d.append("kept", org.bson.BsonBoolean.TRUE)
    d
  }

  def of(d: BsonDocument): Listing = {
    val name   = d.getString("cinema").getValue
    val cinema = Source.byDisplayName.get(name).flatMap(Source.cinemaOf).getOrElse(sys.error(s"no venue named $name"))
    Listing(cinema, ListingKey.parse(d.getString("key").getValue).getOrElse(sys.error(s"bad key in ${d.toJson}")), d.getString("rawTitle").getValue,
      d.getString("title").getValue, d.getString("cleanTitle").getValue, optInt(d, "year"), stringsOf(d, "directors"), optInt(d, "runtime"),
      opt(d, "page"), opt(d, "originalTitle"), stringsOf(d, "countries"),
      d.getArray("catalogueIds").getValues.asScala.toSeq.map(_.asDocument).map(c => CatalogueId(c.getString("source").getValue, c.getString("id").getValue)),
      opt(d, "searchTitle"), opt(d, "poster"), optInt(d, "season").fold(ScreeningDays.of(stringsOf(d, "screenings").map(java.time.LocalDate.parse)))(season =>
        ScreeningDays.ofSeason(seasonDay(season, optInt(d, "firstYear")))),
      VenueNames.ofHashes(ints(d, "names")), properTitle = opt(d, "properTitle"),
      certificate = opt(d, "certificate"), listedCast = ListedCast.ofHashes(ints(d, "listedCast")), yearUpperBound = optInt(d, "yearUpperBound"),
      performanceYear = optInt(d, "performanceYear"), words = ProseWords.ofHashes(ints(d, "words")),
      kept = Option(d.get("kept")).exists(v => v.isBoolean && v.asBoolean.getValue))
  }

  /** A day of `season` in `firstYear` (its opening year when the document names none, as one written before it kept the
   *  year): the season runs from July into the next year's June. */
  def seasonDay(season: Int, firstYear: Option[Int]): java.time.LocalDate =
    firstYear.filter(_ > season).fold(java.time.LocalDate.of(season, ScreeningDays.SeasonOpens, 1))(java.time.LocalDate.of(_, 1, 1))

  private def array(values: Seq[BsonValue]): BsonArray = new BsonArray(values.asJava)
  private def strings(values: Seq[String]): BsonArray = array(values.map(new BsonString(_)))
  private def stringsOf(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))
  private def ints(d: BsonDocument, name: String): Seq[Int] = Option(d.get(name)).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asNumber.intValue))
  private def opt(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).filterNot(_.isNull).map(_.asString.getValue)
  private def optInt(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filterNot(_.isNull).map(_.asNumber.intValue)
  private def put(d: BsonDocument, name: String, value: Option[String]): BsonDocument = { value.foreach(v => d.append(name, new BsonString(v))); d }
  private def putInt(d: BsonDocument, name: String, value: Option[Int]): BsonDocument = { value.foreach(v => d.append(name, new BsonInt32(v))); d }
}
