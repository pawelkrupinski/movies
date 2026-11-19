package services.venuepages

import models.{MovieRecord, Source}
import services.cinemas.common.DetailEnricher
import services.titles.TitleNormalizer

/** Which slots of a film row a venue page lands on. */
object DetailTargets {

  /** The slots of `row` (`None`: not cached) that `ref`'s page of `enricher`'s venue lands on, for the row's BASE title
   *  `title`.
   *
   *  For a 1:1 venue the listing slot is keyed per shown title (`CinemaShowing`), so target THAT slot — a bare-cinema
   *  target would create a separate empty slot and the detail would never merge into the film's showtimes. A chain
   *  redirects detail to its shared network source (not per-title), which is left as-is.
   *
   *  `title` is the film's BASE (row) title, but a DECORATED edition ("Plenerowe Pałacowe: X", folded onto the base
   *  row) has its listing slot keyed by the decorated shown title — so `keyFor(cinema, base)` derives a slot the scrape
   *  never wrote, fabricating a phantom bare slot the next scrape prunes (per-tick churn + the detail never reaches the
   *  real slot). One cinema can run the same film as SEVERAL programme editions ("Kino bez barier: X", "Pora dla
   *  seniora: X", "X przedpremierowo") — each its own card/slot; the detail is the SAME film's, so land it on EVERY one
   *  of this cinema's existing slots, never fabricating a bare phantom. Only when NONE exists (a chain's network source,
   *  or a not-yet-scraped row) create the derived key.
   *
   *  "The SAME film's" detail holds only for an edition that has no page of its own. Kinoteka lists "Rozważna i
   *  romantyczna | Kino dla rodzica" and "… | Kino przy herbatce" on one row, each with its OWN page (112 vs 131
   *  minutes, a different synopsis and billing), and this task reads just one of them — the representative slot's.
   *  Landing it on the sibling made the re-read, which is authoritative, overwrite that edition with a page that is not
   *  its own, so the row changed between two days of identical listings. A slot pointing at a DIFFERENT page is that
   *  page's to fill, not this one's. */
  def of(enricher: DetailEnricher, row: => Option[MovieRecord], normalizer: TitleNormalizer, title: String, ref: String): Seq[Source] =
    if (enricher.detailTarget != enricher.cinema) Seq(enricher.detailTarget)
    // A market-wide page (`pagesSharedAcrossVenues`): the enricher held is any one of the group's,
    // so the page lands on every slot of the row that names it, whichever venue's — and creates none.
    else if (enricher.pagesSharedAcrossVenues) sharedPageSlots(row, ref)
    else venueSlots(enricher, row, normalizer.slotKey(enricher.cinema, title), ref)

  /** Every venue slot of `row` naming the page `ref`, whichever venue's — none created. */
  private def sharedPageSlots(row: Option[MovieRecord], ref: String): Seq[Source] =
    row.toList.flatMap(_.data.collect {
      case (s, sd) if Source.cinemaOf(s).isDefined && DetailEnricher.nativeRefOf(sd).contains(ref) => s
    })

  /** `enricher`'s own venue slots of `row` that `ref`'s page is for, or the `derived` base-title slot. */
  private def venueSlots(enricher: DetailEnricher, row: Option[MovieRecord], derived: Source, ref: String): Seq[Source] = {
    val venueSlots  = row.toList.flatMap(_.data.filter { case (s, _) => Source.cinemaOf(s).contains(enricher.cinema) })
    val cinemaSlots = venueSlots.collect { case (s, sd) if DetailEnricher.nativeRefOf(sd).forall(_ == ref) => s }
    if (venueSlots.isEmpty) Seq(derived)             // chain / not-yet-scraped → create it
    else if (cinemaSlots.contains(derived)) Seq(derived)  // scrape wrote the base title too
    else cinemaSlots                                  // decorated edition(s) → merge into the real slot(s)
  }
}
