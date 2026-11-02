package services.identity

import services.movies.{ListingKey, TitleNormalizer}

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
 * histories), so a whole resolve of the inputs and the incremental model read the same set. The set kept is persisted
 * (`identity_retained`): the worker's take-up resumes it ([[restored]]) rather than losing it at a restart, and the nightly
 * recording captures it, so every replay of a recording -- the convergence rows, the takes check -- reads the model's
 * input as production holds it: the listings billed and the ones kept. It is why DE Metropol's bare
 * "Madoka Magica - Der Film: Rebellion" keeps Filmhaus (Langerringen)'s dated facts while Filmhaus is away, and stays on
 * the 2013 film (drift slices: Madoka, CinemaxX "Titans", Hasetor "Scott & Lila").
 *
 * "Its film" is read by its TITLE, by the one title relation the model groups and backs by ([[TitleRelation.related]]):
 * known before any resolve, so the rule needs no take to decide what a take reads, and whatever the model joins a present
 * listing to by title, or backs its film by, is kept while that listing is billed (the flip-unification design's stage 2).
 */
object ListingRetention {

  /** What a leave comes to: the listings kept, their showings dropped, and the keys that go -- the ones that left and are
   *  not kept, and the ones kept before whose title no present listing bills any more. */
  final case class Left(kept: Seq[Listing], dropped: Seq[ListingKey])

  /** A listing as kept: whole. The model's input holds no showing (time, screen, booking), only the DAYS a listing
   *  screens on, and those are facts of it: they date the broadcast a relay screens (UK "Royal Ballet and Opera: Alice's
   *  Adventures in Wonderland" is the 2026/27 staging by its day; kept without its days -- or with its season alone -- it
   *  joined the 2024/25 one and came back: 11 flips in the drift rows, 2026-10-30). The showings drop from the cinema data
   *  the site serves, not from the facts the model reads. Marked kept ([[Listing.kept]]): an input fact the resolve reads,
   *  so a kept listing never overrules, nor joins, what the present listings' own sources decide (DE "Eternity"). */
  def kept(listing: Listing): Listing = listing.copy(kept = true)

  /** `gone`, listings that just left, against `present` (those still showing after the leave) and `retained` (those kept
   *  before): each gone listing a present one relates to by title ([[TitleRelation.related]]) is kept, showings dropped;
   *  every other goes, and so does every retained one no present listing relates to any more. DE Filmhaus Kino Koeln's
   *  "BLACK WATER (2025)" is kept while Kinocenter Giessen bills "Black Water (GLOBALE)", UK Picture House Keighley's
   *  "The Ladykillers (1955)" while Nerve Centre bills "The Ladykillers | Cinematic Breakfast": the model joins each pair
   *  by title, and each went and came back with the order the present one's answers arrived in when retention read a
   *  title key of its own (drift slices kinocenter-giessen-black-water-globale, nerve-centre-londonderry-the-ladykillers). */
  def leave(gone: Seq[Listing], present: Iterable[Listing], retained: Iterable[Listing], normalizer: TitleNormalizer): Left = {
    val forms = (listing: Listing) => TitleRelation.of(listing, normalizer)
    leave(gone, retained, new TitleRelation.Showing(present.toSeq.map(forms), (gone ++ retained).map(forms), normalizer), forms)
  }

  /** What was kept before (`stored`: the worker's persisted set, or a recording's capture of it) as it stands against
   *  `present`, the listings billed now: [[leave]] of every stored listing no present one replaced, as if each had just
   *  left. Kept, those whose title a present listing bills; dropped, the rest. A listing back replaces what was kept
   *  of it: a stored listing under a present key is neither. How the worker's take-up resumes the set it kept, and how
   *  every replay of a recording reads the model's input beyond the listings billed (the convergence rows, the takes
   *  check), so all of them hold the same set. */
  def restored(stored: Seq[Listing], present: Seq[Listing], normalizer: TitleNormalizer): Left = {
    val billed = present.iterator.map(_.key).toSet
    leave(stored.filterNot(listing => billed(listing.key)), present, Nil, normalizer)
  }

  /** [[leave]] over the titles `showing` (the present listings'), each listing's title read by `forms`. */
  def leave(gone: Seq[Listing], retained: Iterable[Listing], showing: TitleRelation.Showing, forms: Listing => TitleRelation.Forms): Left = {
    val (stay, go) = gone.partition(listing => showing.relates(forms(listing)))
    val expired    = retained.iterator.filterNot(listing => showing.relates(forms(listing))).map(_.key).toSeq
    Left(stay.map(kept), go.map(_.key) ++ expired)
  }
}
