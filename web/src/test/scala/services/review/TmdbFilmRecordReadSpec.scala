package services.review

import org.bson.BsonDocument
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A `tmdb_films` document as the review pages read it — the worker's record shape (`IdentityAnswerBson.film`). */
class TmdbFilmRecordReadSpec extends AnyFlatSpec with Matchers {

  "a stored TMDB film" should "read from its record, else the search hit standing in for it, else not at all" in {
    MongoReviewSource.filmRecord(BsonDocument.parse("""{"_id": "1599768", "record": {"title": "Ghost School",
      "originalTitle": "Ghost School", "alternativeTitles": [], "year": 2026, "runtime": 95, "directors": ["Seemab Gul"],
      "countries": ["PK"], "imdbNumber": 31234567}, "local": {"imdb_id": "tt31234567"}}""")) shouldBe
      Some(1599768 -> FilmCard(1599768, Some("tt31234567"), Some("Ghost School"), Some("Ghost School"), Some(2026),
        Seq("Seemab Gul"), Some(95), None, None))
    MongoReviewSource.filmRecord(BsonDocument.parse("""{"_id": "603", "hit": {"title": "The Matrix", "year": 1999, "popularity": 3}}""")) shouldBe
      Some(603 -> FilmCard(603, None, Some("The Matrix"), None, Some(1999), Nil, None, None, None))
    MongoReviewSource.filmRecord(BsonDocument.parse("""{"_id": "604", "record": null}""")) shouldBe None
  }
}
