package services.review

import org.mongodb.scala.MongoDatabase

/**
 * One country's review reads, against ITS database on the local read-mirror — never prod: the wiring
 * builds these only from `MONGODB_MOVIES_MIRROR_URI`. Whole-collection reads are keyset-paged and
 * projected to what a page shows; every other read is an `$in` over the keys of the cards on screen.
 * Each collection's reads are their own role: the listings' ([[MongoListingReads]]), the decisions'
 * ([[MongoDecisionReads]]), the corpus's films' ([[MongoFilmReads]]) and the stored TMDB records'
 * ([[MongoFilmRecordReads]]).
 */
final class MongoReviewSource(protected val db: MongoDatabase) extends ReviewSource
  with MongoListingReads with MongoDecisionReads with MongoFilmReads with MongoFilmRecordReads
