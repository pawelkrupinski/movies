package services.identity

import services.movies.{ListingKey, TitleNormalizer}

/**
 * How long a listing's IDENTITY FACTS live (the owner, 2026-10-30, identity design: "Drop showings, keep movie details
 * from all cinemas data until there are no showtimes in any cinema in the country", "and update the details when it
 * comes back"). A listing that leaves -- its venue's page or its showings dropped -- loses its showings at once, but its
 * facts (year, runtime, director, original title, poster, the links that decided its film) stay in the model's input
 * while a present listing of the country still bills its title; they go once none does. A listing that comes back
 * replaces what was kept of it: its current data wins, never a frozen copy.
 *
 * The rule is data, not stickiness: a pure function of the listings that left, the ones present and the ones kept
 * before, applied where listings are fed to the model (the worker's drain, the drift replay, the equivalence spec's
 * histories), so a whole resolve of the inputs and the incremental model read the same set. It is why DE Metropol's bare
 * "Madoka Magica - Der Film: Rebellion" keeps Filmhaus (Langerringen)'s dated facts while Filmhaus is away, and stays on
 * the 2013 film (drift slices: Madoka, CinemaxX "Titans", Hasetor "Scott & Lila").
 *
 * "Its film" is read by its title (the model's `t:` key, [[TitleNormalizer.sanitize]]): known before any resolve, so the
 * rule needs no take to decide what a take reads.
 */
object ListingRetention {

  /** What a leave comes to: the listings kept, their showings dropped, and the keys that go -- the ones that left and are
   *  not kept, and the ones kept before whose title no present listing bills any more. */
  final case class Left(kept: Seq[Listing], dropped: Seq[ListingKey])

  /** The title a listing is kept by. */
  def titleOf(listing: Listing, normalizer: TitleNormalizer): String = normalizer.sanitize(listing.title)

  /** A listing as kept: its identity facts, no showings. */
  def kept(listing: Listing): Listing = listing.copy(screenings = ScreeningDays.None)

  /** `gone`, listings that just left, against `present` (those still showing after the leave) and `retained` (those kept
   *  before): each gone listing whose title a present one bills is kept, showings dropped; every other goes, and so does
   *  every retained one whose title no present listing bills any more. */
  def leave(gone: Seq[Listing], present: Iterable[Listing], retained: Iterable[Listing], normalizer: TitleNormalizer): Left = {
    val title = (listing: Listing) => titleOf(listing, normalizer)
    leave(gone, retained, present.iterator.map(title).toSet, title)
  }

  /** [[leave]] over the titles `showing` (those a present listing bills), each listing's title read by `title`. */
  def leave(gone: Seq[Listing], retained: Iterable[Listing], showing: String => Boolean, title: Listing => String): Left = {
    val (stay, go) = gone.partition(listing => showing(title(listing)))
    val expired    = retained.iterator.filterNot(listing => showing(title(listing))).map(_.key).toSeq
    Left(stay.map(kept), go.map(_.key) ++ expired)
  }
}
