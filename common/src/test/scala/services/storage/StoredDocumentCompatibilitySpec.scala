package services.storage

import models.{CityScreening, MovieRecord, ResolvedMovie, ResolvedRatings, Showtime, Source, SourceData, Tmdb}
import org.bson.codecs.Codec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MovieCodecs, StoredMovieDto, StoredScreeningsDto, StoredSlotDto}
import services.readmodel.ReadModelCodecs

import java.time.{Instant, LocalDateTime}

/**
 * Every persisted document type, checked field by field: can it still be decoded when a
 * field is ABSENT?
 *
 * The collections outlive the case classes that describe them. A field gets added, or
 * `$unset` by a migration, or was never written by the build that created the row — and a
 * missing non-`Option` field makes the macro codec throw `Missing field: x`, which fails
 * the whole keyset batch and so takes the entire corpus scan down with one document.
 * Round-trip specs cannot see this: they decode exactly what they just encoded.
 *
 * Two outages came from this one word. `web_movies.ratings` 404'd the served corpus, and
 * `movies.sourceData` — `$unset` by the 2026-07-27 slot migration — broke the corpus scan
 * and silently aborted every staging fold for hours.
 *
 * Enforced as a RATCHET (see `check`): the exposure that exists today is written down per
 * document type, nothing may be added to it, and anything paid off must stay paid off. So
 * the next field added to any stored document has to be `Option[...]`, which is the act
 * that generated both outages — while the standing debt stays visible instead of being
 * quietly re-accumulated.
 */
class StoredDocumentCompatibilitySpec extends AnyFlatSpec with Matchers {

  /** `_id` is the document key — a BSON document without one is not a row at all, and
   *  Mongo will not store it. That is the only universally safe requirement. */
  private val IdOnly = Set("_id")

  /**
   * A RATCHET, not a pass/fail line.
   *
   * `knownRequired` is the exposure that already exists: fields whose absence breaks the
   * decode today. It is debt, listed rather than hidden, and the test enforces two things
   * about it — nothing may be ADDED to it, and anything removed must stay removed.
   *
   * Ratchet rather than a straight assertion because the two directions have very
   * different risk. Retro-fitting `Option` onto a dozen live fields is a schema change to
   * collections that are being served right now, for a failure that has not fired on them;
   * refusing the NEXT required field costs one `Option` in a code review. The generator of
   * both outages was a field being introduced (or, for `sourceData`, newly `$unset`) while
   * old-shaped rows were still in the collection — so the ratchet blocks exactly the act
   * that causes the failure, and the standing debt is written down instead of forgotten.
   *
   * To pay a line off: make the field `Option[...]` on the DTO, default it on read, and
   * delete it here. The test will hold you to it.
   */
  private def check[A](label: String, codec: Codec[A], sample: A, knownRequired: Set[String]): Unit = {
    val breaks   = StoredDocumentCompatibility.fieldsThatBreakDecode(codec, sample).map(_.field).toSet
    val added    = breaks -- knownRequired
    val paidOff  = knownRequired -- breaks -- IdOnly

    withClue(
      s"$label gained a field that cannot be decoded when ABSENT. Every stored document " +
      s"written before this field existed now fails to decode, and that failure kills the " +
      s"whole keyset batch — one old row takes the entire collection scan with it. This is " +
      s"the shape of both the `web_movies.ratings` 404 and the `movies.sourceData` outage. " +
      s"Make it `Option[...]` on the DTO and default it on read:\n" +
      added.toSeq.sorted.map(f => s"  $f").mkString("\n") + "\n") {
      added shouldBe empty
    }

    withClue(
      s"$label no longer requires ${paidOff.toSeq.sorted.mkString(", ")} — debt paid off. " +
      s"Delete it from this spec's `knownRequired` so the ratchet holds the new ground.\n") {
      paidOff shouldBe empty
    }
  }

  private val showtime = Showtime(LocalDateTime.parse("2026-08-01T20:00"), bookingUrl = Some("https://b"),
    room = Some("Sala 1"), format = List("2D"))

  "a `movies` document" should "decode with any field absent" in {
    val dto = StoredMovieDto.fromDomain("film|2026",
      MovieRecord(
        imdbId = Some("tt1"), imdbRating = Some(7.0), metascore = Some(70),
        filmwebUrl = Some("https://fw"), filmwebRating = Some(7.1), rottenTomatoes = Some(80),
        tmdbId = Some(1), wikidataId = Some("Q1"), metacriticUrl = Some("https://mc"),
        rottenTomatoesUrl = Some("https://rt"), searchTitle = Some("film"),
        tmdbNoMatch = true, detailPending = true,
        data = Map[Source, SourceData](Tmdb -> SourceData(title = Some("Film"), showtimes = Seq(showtime))),
        retainedSynopses = Map[Source, String](Tmdb -> "kept")),
      Instant.parse("2026-07-27T12:00:00Z"))

    // `sourceData` is THE field the 2026-07-27 migration `$unset`. It must decode absent.
    StoredDocumentCompatibility.encodedFields(MovieCodecs.registry.get(classOf[StoredMovieDto]), dto) should
      contain ("sourceData")

    check("StoredMovieDto", MovieCodecs.registry.get(classOf[StoredMovieDto]), dto,
      // `updatedAt` is written by every code path that has ever created a row, and the
      // freshness/ordering logic treats its absence as a bug rather than a default.
      // `movies` is the collection the 2026-07-27 outage ran through, so it is already
      // paid off down to the key and the timestamp. `sourceData` decoding ABSENT is what
      // that fix bought; the ratchet is what keeps it.
      knownRequired = IdOnly + "updatedAt")
  }

  "a `screenings` document" should "decode with any field absent" in {
    val dto = StoredScreeningsDto("film|2026Multikino", "film|2026", "Multikino",
      Seq(showtime), Instant.parse("2026-07-27T12:00:00Z"))
    check("StoredScreeningsDto", MovieCodecs.registry.get(classOf[StoredScreeningsDto]), dto,
      // DEBT: `showtimes` is the row's entire payload, so an absent one has never been
      // written by any build — but nothing enforces that, and a migration could.
      knownRequired = IdOnly + "updatedAt" + "filmId" + "slotKey" + "showtimes")
  }

  "a `movie_slots` document" should "decode with any field absent" in {
    val dto = StoredSlotDto("film|2026Multikino", "film|2026", "Multikino",
      SourceData(title = Some("Film")), Instant.parse("2026-07-27T12:00:00Z"))
    check("StoredSlotDto", MovieCodecs.registry.get(classOf[StoredSlotDto]), dto,
      // DEBT: same shape as `screenings` — `slot` is the payload.
      knownRequired = IdOnly + "updatedAt" + "filmId" + "slotKey" + "slot")
  }

  "a `web_movies` document" should "decode with any field absent" in {
    val movie = ResolvedMovie(
      _id = "film|2026", title = "Film", originalTitle = Some("Film"), posterUrl = Some("https://p"),
      fallbackPosterUrls = Seq("https://p2"), runtimeMinutes = Some(100), releaseYear = Some(2026),
      genres = Seq("Dramat"), countries = Seq("USA"), directors = Seq("D"), cast = Seq("A"),
      synopsis = Some("s"), synopsisByCity = Map("poznan" -> "s"), trailerUrls = Seq("https://t"),
      ratings = ResolvedRatings(Some(7.0), Some("https://imdb"), Some(70), "https://mc",
        Some(80), "https://rt", Some(7.1), "https://fw"),
      weightedRating = 7.2)
    // `ratings` is the field whose absence 404'd the served corpus the FIRST time this
    // class of bug landed. It must decode absent.
    StoredDocumentCompatibility.encodedFields(ReadModelCodecs.registry.get(classOf[ResolvedMovie]), movie) should
      contain ("ratings")

    // DEBT, and the sharpest of it. `web_movies` rows are written only by the projector,
    // and the full re-projection was RETIRED — so a row is rewritten when its film changes
    // and not otherwise. Adding a required field here therefore breaks every quiescent row
    // until something touches it, which is exactly how `ratings` 404'd the served corpus.
    // Any NEW field on ResolvedMovie must be `Option[...]`.
    check("ResolvedMovie", ReadModelCodecs.registry.get(classOf[ResolvedMovie]), movie,
      knownRequired = IdOnly ++ Set("title", "ratings", "weightedRating", "genres", "countries",
        "directors", "cast", "trailerUrls", "fallbackPosterUrls"))
  }

  "a `web_screenings` document" should "decode with any field absent" in {
    val screening = CityScreening("film|2026|poznan|Multikino", "film|2026", "poznan", "Multikino",
      filmUrl = Some("https://f"), showtimes = Seq(showtime))
    check("CityScreening", ReadModelCodecs.registry.get(classOf[CityScreening]), screening,
      // The read model is keyed and queried on these; a row lacking them cannot be served
      // or pruned, so an absent one is corruption rather than an older shape. `cinema` and
      // `showtimes` are DEBT on the same footing as the rest of the read model.
      knownRequired = IdOnly ++ Set("filmId", "city", "cinema", "showtimes"))
  }
}
