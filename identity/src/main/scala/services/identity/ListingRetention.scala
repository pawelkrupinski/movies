package services.identity

import services.movies.{ListingKey, TitleNormalizer}
import services.resolution.SearchTitles

/**
 * How long a listing's IDENTITY FACTS live (the owner, 2026-10-30, identity design: "Drop showings, keep movie details
 * from all cinemas data until there are no showtimes in any cinema in the country", "and update the details when it
 * comes back"). A listing that leaves -- its venue's page or its showings dropped -- leaves the cinema data the site
 * serves at once, but its facts (year, runtime, director, original title, poster, the days it screened on, the links that
 * decided its film) stay in the model's input
 * while a present listing of the country still bills its title; they go once none does. A listing that comes back
 * replaces what was kept of it: its current data wins, never a frozen copy.
 *
 * The rule is data, not stickiness: a pure function of the listings that left, the ones present and the ones kept
 * before, applied where listings are fed to the model (the worker's drain, the drift replay, the equivalence spec's
 * histories), so a whole resolve of the inputs and the incremental model read the same set. It is why DE Metropol's bare
 * "Madoka Magica - Der Film: Rebellion" keeps Filmhaus (Langerringen)'s dated facts while Filmhaus is away, and stays on
 * the 2013 film (drift slices: Madoka, CinemaxX "Titans", Hasetor "Scott & Lila").
 *
 * "Its film" is read by its normalized title ([[titleOf]]): known before any resolve, so the rule needs no take to decide
 * what a take reads.
 */
object ListingRetention {

  /** What a leave comes to: the listings kept, their showings dropped, and the keys that go -- the ones that left and are
   *  not kept, and the ones kept before whose title no present listing bills any more. */
  final case class Left(kept: Seq[Listing], dropped: Seq[ListingKey])

  /** The title a listing is kept by: its normalized title, read off the listing alone -- the title it is searched under
   *  (`TitleNormalizer.searchQuery`: case, programme banners, format and edition tags such as OmU, OV, 3D, IMAX), without
   *  a trailing parenthetical (`SearchTitles.withoutTrailingParenthesis`: a year, "(GLOBALE)", "(OmU)"), sanitized. DE
   *  Filmhaus Kino Koeln's "BLACK WATER (2025)" is kept while Kinocenter Giessen bills "Black Water (GLOBALE)": kept by its
   *  sanitized title alone it went and came back with the order Giessen's answers arrived in (drift slice, seed 29). Never
   *  by a take: what a take reads cannot be decided by a take. */
  def titleOf(listing: Listing, normalizer: TitleNormalizer): String = {
    val normalized = normalizer.sanitize(SearchTitles.withoutTrailingParenthesis(normalizer.searchQuery(listing.title)))
    if (normalized.nonEmpty) normalized else normalizer.sanitize(listing.title)
  }

  /** A listing as kept: whole. The model's input holds no showing (time, screen, booking), only the DAYS a listing
   *  screens on, and those are facts of it: they date the broadcast a relay screens (UK "Royal Ballet and Opera: Alice's
   *  Adventures in Wonderland" is the 2026/27 staging by its day; kept without its days -- or with its season alone -- it
   *  joined the 2024/25 one and came back: 11 flips in the drift rows, 2026-10-30). The showings drop from the cinema data
   *  the site serves, not from the facts the model reads. */
  def kept(listing: Listing): Listing = listing

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
