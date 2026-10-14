package services.identity

import services.movies.ListingKey

import java.time.Instant

/**
 * TEST FIXTURE ONLY — pins an admin could write for the cases the proof found undecidable or
 * wrong-but-stable (docs/design/identity-resolver.md §7, §7a). Never written to production:
 * pins are an emergency escape hatch, and no film-specific rule lives in production code.
 *
 * The listings are spelled as the proof's corpora recorded them. TMDB ids are illustrative —
 * the specs exercise the constraint SHAPE a pin produces, not TMDB's catalogue.
 */
object KnownCasePins {

  val At: Instant = Instant.parse("2026-09-26T12:00:00Z")

  private def pin(listings: Seq[ListingKey], claim: PinClaim, reason: String) =
    Pin(listings, claim, author = "fixture", reason = reason, createdAt = At)

  /** Kino 1410's "Opętanie | klasyka w 4k": the title names two films and the listing
   *  publishes nothing else — undecidable from the evidence, so a human says which. */
  val Possession1981 = 21484
  val opetanie: ListingKey = ListingKey.Published("Kino 1410", "Opętanie | klasyka w 4k", None, Nil)
  val opetaniePin: Pin = pin(Seq(opetanie), PinClaim.IsFilm(Possession1981), "the 4K classics strand is Żuławski's 1981 film")

  /** The Met's 2026 "Samson i Dalila" broadcast is never DeMille's 1949 film. */
  val DeMilleSamson = 22683
  val metSamson: ListingKey = ListingKey.Native("Helios Blue City", "https://helios.pl/met-opera/samson-i-dalila",
    "The Met: Live in HD – Samson i Dalila")
  val metSamsonPin: Pin = pin(Seq(metSamson), PinClaim.NeverFilm(DeMilleSamson), "an opera broadcast, not the 1949 film")

  /** UK "(2026)" rerelease of Mockingjay – Part 2: a bracketed rerelease year cannot-links it
   *  from the 2015 film, so the resolver splits it off. */
  val MockingjayPart2 = 131634
  val mockingjay2026: ListingKey = ListingKey.Published("Vue Leeds Kirkstall", "The Hunger Games: Mockingjay – Part 2 (2026)", Some(2026), Nil)
  val mockingjay2015: ListingKey = ListingKey.Published("Vue Leeds Kirkstall", "The Hunger Games: Mockingjay – Part 2", Some(2015), Seq("Francis Lawrence"))
  val mockingjayPin: Pin = pin(Seq(mockingjay2015, mockingjay2026), PinClaim.SameFilm, "a rerelease, not a new film")

  /** Three of the 18 decorated "Lalka" spellings that no must-link joins to the plain one. */
  val lalka: Seq[ListingKey] = Seq(
    ListingKey.Published("Kino Muranów", "Lalka", Some(2025), Nil),
    ListingKey.Published("Kino Muranów", "Lalka – pokaz specjalny z prelekcją", None, Nil),
    ListingKey.Native("Kino Pod Baranami", "https://kinopodbaranami.pl/film/lalka", "LALKA | Kino Seniora"))
  val lalkaPin: Pin = pin(lalka, PinClaim.SameFilm, "decorated spellings of one film")

  val all: Seq[Pin] = Seq(opetaniePin, metSamsonPin, mockingjayPin, lalkaPin)
}
