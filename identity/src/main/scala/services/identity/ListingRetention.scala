package services.identity

import services.movies.TitleNormalizer

/**
 * How long a listing's IDENTITY FACTS live (the owner, 2026-10-30, identity design: "Drop showings, keep movie details
 * from all cinemas data until there are no showtimes in any cinema in the country", "and update the details when it
 * comes back"). A listing that leaves -- its venue's page or its showings dropped -- leaves the cinema data the site
 * serves at once, but its facts (year, runtime, director, original title, poster, the days it screened on, the links that
 * decided its film) stay in the model's input while a present listing of the country bills its title. A listing that
 * comes back replaces what was kept of it: its current data wins, never a frozen copy.
 *
 * A PURE FUNCTION of the inputs (the owner's R1, no stickiness; flip-unification design §7, retention as a pure
 * function). The input is every listing that left -- [[Departed]]: its last-seen facts and when it left -- and the
 * listings billed now. What is kept is [[retained]] of the two, read afresh on every resolve, incremental or whole: never
 * a set decided at the moment a listing left. A listing that left while nothing related to it is kept again the moment a
 * related one is billed, so "A leaves, then B arrives" and "B arrives, then A leaves" keep the same set (before
 * 2026-11-03 the first dropped A for good: G1 of the flip audit). The departed listings are persisted
 * (`identity_retained`): the worker's take-up reads them, and the nightly recording captures them, so every replay of a
 * recording reads the model's input as production holds it. What bounds them is a pure rule too: a listing departed
 * more than the retention window ago is no input ([[live]]), whatever came before or after -- the window is the worker's
 * typed config (`settings.IdentityRetentionWindow`, `KINOWO_IDENTITY_RETENTION_DAYS`), [[DefaultMaxAge]] unless set,
 * counted from the departure against the model's clock (docs/design/identity-listing-retention.md). It is why DE Metropol's bare
 * "Madoka Magica - Der Film: Rebellion" keeps Filmhaus (Langerringen)'s dated facts while Filmhaus is away, and stays on
 * the 2013 film (drift slices: Madoka, CinemaxX "Titans", Hasetor "Scott & Lila").
 *
 * "Its film" is read by its TITLE, by the one title relation the model groups and backs by ([[TitleRelation.related]]):
 * known before any resolve, so the rule needs no take to decide what a take reads, and whatever the model joins a present
 * listing to by title, or backs its film by, is kept while that listing is billed (the flip-unification design's stage 2).
 */
object ListingRetention {

  /** A listing that left: its facts as last billed, and when it left (epoch millis; [[Unknown]] for one persisted before
   *  departures were dated, which the take-up dates to itself). An INPUT, never a decision: whether it is kept is read
   *  afresh from the listings billed ([[retained]]). */
  final case class Departed(listing: Listing, at: Long)

  /** The departure time of a listing persisted before departures were dated (`identity_retained` before 2026-11-03). */
  val Unknown: Long = 0L

  /** How long a departed listing stays an input unless the worker's config sets another window: past it, its facts go
   *  whatever is billed. A pure bound on the store, by the departure's age alone -- never by the order of leaves and
   *  arrivals. Two months covers a film's run and a venue's later repeat of it; a longer window was measured to cure no
   *  flip (docs/design/identity-listing-retention.md). */
  val DefaultMaxAge: java.time.Duration = java.time.Duration.ofDays(60)

  /** Is `departed` an input at `now`: it left no more than `window` before. */
  def live(departed: Departed, now: Long, window: java.time.Duration = DefaultMaxAge): Boolean = now - departed.at <= window.toMillis

  /** A listing as kept: whole. The model's input holds no showing (time, screen, booking), only the DAYS a listing
   *  screens on, and those are facts of it: they date the broadcast a relay screens (UK "Royal Ballet and Opera: Alice's
   *  Adventures in Wonderland" is the 2026/27 staging by its day; kept without its days -- or with its season alone -- it
   *  joined the 2024/25 one and came back: 11 flips in the drift rows, 2026-10-30). The showings drop from the cinema data
   *  the site serves, not from the facts the model reads. Marked kept ([[Listing.kept]]): an input fact the resolve reads,
   *  so a kept listing never overrules, nor joins, what the present listings' own sources decide (DE "Eternity"). */
  def kept(listing: Listing): Listing = listing.copy(kept = true)

  /** The listings kept of `departed` (those that left) against `present` (those billed now), each marked [[kept]]: every
   *  departed listing no present one is under whose title a present listing relates to ([[TitleRelation.related]]). A
   *  pure function of the two sets: whole resolves, the worker's drain, its take-up and every replay read it alike. DE
   *  Filmhaus Kino Koeln's "BLACK WATER (2025)" is kept while Kinocenter Giessen bills "Black Water (GLOBALE)", UK Picture
   *  House Keighley's "The Ladykillers (1955)" while Nerve Centre bills "The Ladykillers | Cinematic Breakfast": the model
   *  joins each pair by title (drift slices kinocenter-giessen-black-water-globale, nerve-centre-londonderry-the-ladykillers). */
  def retained(departed: Seq[Listing], present: Seq[Listing], normalizer: TitleNormalizer): Seq[Listing] =
    retained(departed, present, normalizer, (listing: Listing) => TitleRelation.of(listing, normalizer))

  /** [[retained]], each listing's title read by `forms` (the worker's cache of them). */
  def retained(departed: Seq[Listing], present: Seq[Listing], normalizer: TitleNormalizer, forms: Listing => TitleRelation.Forms): Seq[Listing] = {
    val billed = present.iterator.map(_.key).toSet
    val gone   = departed.filterNot(listing => billed(listing.key))
    if (gone.isEmpty || present.isEmpty) Nil
    else {
      val showing = new TitleRelation.Showing(present.map(forms), gone.map(forms), normalizer)
      gone.filter(listing => showing.relates(forms(listing))).map(kept)
    }
  }

  /** [[retained]] of the departed listings that are inputs at `now` within `window` ([[live]]). */
  def retainedAt(departed: Iterable[Departed], present: Seq[Listing], now: Long, normalizer: TitleNormalizer,
                 forms: Listing => TitleRelation.Forms, window: java.time.Duration = DefaultMaxAge): Seq[Listing] =
    retained(departed.iterator.filter(live(_, now, window)).map(_.listing).toSeq, present, normalizer, forms)
}
