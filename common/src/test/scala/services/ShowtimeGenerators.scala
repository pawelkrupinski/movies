package services

import models.Showtime
import org.scalacheck.Gen

import java.time.LocalDateTime

/** Generated showtimes for the codec round-trip properties — booking URLs drawn so a row's
 *  URLs often share a prefix (the shape the row codecs store split), sometimes ARE that
 *  prefix, and reach past multi-byte characters. Shared by the `movies`-side `screenings`
 *  and the read-model `web_screenings` properties. */
object ShowtimeGenerators {

  val genText: Gen[String] =
    Gen.oneOf(Gen.alphaNumStr, Gen.oneOf("", "Łódź", "Zażółć gęślą", "東京", "naïve 🎬", "a.b$c"))

  val genUrl: Gen[String] = for {
    site   <- Gen.oneOf("https://kino.example/buy?show=", "https://kino.example/buy?show=1", "https://bilety.pl/ż/", "")
    suffix <- Gen.oneOf(Gen.numStr, Gen.oneOf("", "ą1", "🎬", "%C5%BC", "&x=1"))
  } yield site + suffix

  val genShowtime: Gen[Showtime] = for {
    minutes <- Gen.choose(0, 60 * 24 * 14)
    url     <- Gen.option(genUrl)
    room    <- Gen.option(genText)
    format  <- Gen.listOf(Gen.oneOf("2D", "3D", "IMAX", "NAP", "ATMOS"))
  } yield Showtime(LocalDateTime.of(2026, 10, 3, 9, 0).plusMinutes(minutes.toLong), url, room, format)
}
