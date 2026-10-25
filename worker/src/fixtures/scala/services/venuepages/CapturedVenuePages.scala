package services.venuepages

import org.bson.BsonDocument
import org.mongodb.scala.Document

/** venue_pages documents as a recording captured them (`tools.ProdLookupInputs`), read as [[MongoVenuePageStore]] reads
 *  its own: in this package, so the capture's pages decode by the store's own rules, not a copy of them. */
object CapturedVenuePages {

  /** A store holding `docs`, each decoded as venue_pages' reader decodes it; one it cannot decode is left out, as there. */
  def store(docs: Seq[BsonDocument]): VenuePageStore = {
    val pages = new InMemoryVenuePageStore
    docs.flatMap(d => MongoVenuePageStore.pageOf(Document(d))).foreach(pages.put)
    pages
  }
}
