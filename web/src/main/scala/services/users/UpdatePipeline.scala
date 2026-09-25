package services.users

import org.bson.{BsonArray, BsonDocument, BsonString, BsonValue}

import scala.jdk.CollectionConverters._

/** The aggregation-expression pieces the user stores' atomic update pipelines are
 *  built from. User text always travels inside [[literal]], since a string starting
 *  with `$` would otherwise read as a field path. */
private[users] object UpdatePipeline {
  def op(name: String, args: BsonValue*): BsonDocument = new BsonDocument(name, new BsonArray(args.toList.asJava))
  def literal(value: BsonValue): BsonDocument = new BsonDocument("$literal", value)
  def field(path: String): BsonString = new BsonString("$" + path)
}
