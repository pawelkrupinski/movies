package services.identitymodel

import scala.util.Try

import org.bson.BsonDocument
import services.identity.ListingBson
import services.identity.measures.Listing

/** A listing as the identity stores keep it ([[ListingBson]]), read back: None for one that no longer decodes (a field
 *  its writer had that this reader refuses), said out loud -- the store then holds less than it was given. */
private[services] object StoredListing extends play.api.Logging {
  def decoded(doc: BsonDocument, store: String): Option[Listing] =
    Try(ListingBson.of(doc)).fold(e => { logger.warn(s"$store: a stored listing no longer decodes, left out: $e"); None }, Some(_))
}
