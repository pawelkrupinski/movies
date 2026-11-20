package services.identity

import services.identity.measures.Film
import scala.annotation.unused
import services.identity.vocabulary.{Answer, CandidateQuery, DetailFacts, Hit, ProseWords}

/**
 * The only non-pure input: the observations the resolver reads. Each answer must be a function of
 * its argument alone — no argument carries a venue set, a group or a previous answer — which is
 * what lets the resolver enumerate every question from the listing set up front.
 */
trait IdentityLookups {
  /** Whether the listing's venue publishes a detail page the source can answer for. */
  def hasDetail(listing: Listing): Boolean
  /** The questions a caller is about to ask, one by one — a source that can answer them together
   *  (in parallel, over the network) may, and serve the asks that follow from what it fetched.
   *  Every read phase begins with one, so nothing fetched for an earlier phase is served to a later. */
  def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = ()
  /** The asks a [[prefetch]] was for have been asked: what it fetched to answer them may go. */
  def prefetchAnswered(): Unit = ()
  /** Questions no listing held asks any more — a source tracking what each question read may forget them. */
  def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[models.ListingKey]): Unit = ()
  /** The venue's own detail page for the listing. */
  def detail(listing: Listing): Answer[Option[DetailFacts]]
  /** Every film a query names. */
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]
  /** A film's own record (`TmdbFilmRecord`). */
  def film(tmdbId: Int): Answer[Option[Film]]
  /** Does this source number its films by their TMDB ids — the number a TMDB-keyed feed's link names ([[FeedLink]])? A
   *  film-database family's answers number theirs as one resolve meets them (`agreement.FamilyLookups`): there a link's
   *  TMDB id names none of them, and read as a film number it denied the very film the link meant. */
  def numbersFilmsByTmdbId: Boolean = true
  /** The day a film was released, as its record states it: `Unknown` while the record is unknown — or, in a store, while
   *  it was filed before records kept the whole day and holds only the year TMDB dated it in
   *  ([[agreement.Broadcast]] reads the day; nothing else does). */
  def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId).map(_.flatMap(_.released))
  /** A film's top-billed cast as TMDB credits it ([[TmdbFilmRecord.cast]]): what the cast evidence reads
   *  ([[CastEvidence]]), never the model. Read apart from its record ([[film]]), as the release day is: every candidate
   *  of every family holds a record, and only an unmatched cluster's fill reads a cast. `Known(None)` where the source
   *  does not know it — a record filed before records kept the cast — which the evidence reads as nothing to go on. */
  def cast(@unused tmdbId: Int): Answer[Option[Seq[String]]] = Answer.Known(None)
  /** The words a film's TMDB descriptions capitalise — its localized and English overviews ([[TmdbFilmRecord.overview]]):
   *  what the synopsis evidence reads ([[agreement.SynopsisEvidence]]), never the model. Read apart from its record, as
   *  the cast is. `Known(None)` where the source does not know it — a record filed before records kept the overview. */
  def overview(@unused tmdbId: Int): Answer[Option[ProseWords]] = Answer.Known(None)
}
