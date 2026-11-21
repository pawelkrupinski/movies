package tools

import scala.jdk.CollectionConverters.*

import org.bson.{BsonArray, BsonDocument, BsonString, BsonValue}

/** The aggregation-expression pieces server-side update pipelines and `$expr` filters are built from. User text
 *  always travels inside [[literal]], since a string starting with `$` would otherwise read as a field path. */
object MongoExpressions {
  def op(name: String, args: BsonValue*): BsonDocument = new BsonDocument(name, new BsonArray(args.toList.asJava))
  def literal(value: BsonValue): BsonDocument = new BsonDocument("$literal", value)
  def field(path: String): BsonString = new BsonString("$" + path)
}
