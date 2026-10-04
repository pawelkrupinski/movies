package services.readmodel

import models.{ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.deriving.Mirror

/** The projector skips a card's write when its [[CardHash]] is unchanged, so a `ResolvedMovie`
 *  field the hash leaves out is a field whose change never reaches `web_movies`: the card keeps
 *  serving the old value until something the hash does read moves. Every field but `_id` (the
 *  card's identity, the hash's key) must move the hash — a field added to the card without a
 *  `CardHash` part fails here, by name. */
class CardHashSpec extends AnyFlatSpec with Matchers {

  private val card = ResolvedMovie(
    _id = "film", title = "Film", originalTitle = Some("Original"), posterUrl = Some("https://poster.jpg"),
    fallbackPosterUrls = Seq("https://alt.jpg"), runtimeMinutes = Some(100), releaseYear = Some(2026),
    genres = Seq("Drama"), countries = Seq("PL"), directors = Seq("Someone"), cast = Seq("Actor"),
    synopsis = Some("Synopsis."), trailerUrls = Seq("https://trailer"),
    ratings = ResolvedRatings(Some(7.0), None, None, "https://mc", None, "https://rt", None, "https://fw"),
    weightedRating = 7.0, synopsisByCity = Map("poznan" -> "Local."), ageRating = Some("15"),
    shareCard = Some("film.jpg?v=1"), shareCardPending = false)

  /** `value` changed, whatever its type: the field's every other value is another card. */
  private def changed(value: Any): Any = value match {
    case s: String          => s + "·"
    case Some(_)            => None
    case None               => Some("·")
    case m: Map[?, ?]       => m.asInstanceOf[Map[Any, Any]] + ("·" -> "·")
    case s: Seq[?]          => s :+ "·"
    case d: Double          => d + 1
    case i: Int             => i + 1
    case b: Boolean         => !b
    case r: ResolvedRatings => r.copy(imdb = r.imdb.fold(Option(1.0))(_ => None))
    case other              => fail(s"no change defined for a ${other.getClass.getName}: extend `changed`")
  }

  "CardHash" should "move with every field of the card but its id" in {
    val mirror = summon[Mirror.ProductOf[ResolvedMovie]]
    val fields = card.productElementNames.toIndexedSeq
    val unhashed = fields.indices.filter(fields(_) != "_id").filter { index =>
      val values = card.productIterator.toArray
      values(index) = changed(values(index))
      CardHash.of(mirror.fromProduct(Tuple.fromArray(values))) == CardHash.of(card)
    }.map(fields)
    withClue(s"ResolvedMovie fields CardHash.of does not read (a change to them is never written): $unhashed") {
      unhashed shouldBe empty
    }
  }
}
