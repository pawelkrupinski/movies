package services.identity

/**
 * `inner`'s answers, every question forwarded as asked: the base of a source that changes a few of `inner`'s answers
 * (blinds a page, notes a gap, asks a second source on a gap) and leaves the rest as they were. A question
 * [[IdentityLookups]] gains is forwarded here once, never forgotten by one wrapper of several.
 */
trait ForwardingIdentityLookups(inner: IdentityLookups) extends IdentityLookups {
  def hasDetail(listing: Listing): Boolean = inner.hasDetail(listing)
  override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit =
    inner.prefetch(queries, films, details)
  override def prefetchAnswered(): Unit = inner.prefetchAnswered()
  override def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[services.movies.ListingKey]): Unit =
    inner.released(queries, films, details)
  def detail(listing: Listing): Answer[Option[DetailFacts]]          = inner.detail(listing)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]            = inner.candidates(query)
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]       = inner.film(tmdbId)
  override def numbersFilmsByTmdbId: Boolean                         = inner.numbersFilmsByTmdbId
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = inner.releaseDay(tmdbId)
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]]        = inner.cast(tmdbId)
  override def overview(tmdbId: Int): Answer[Option[ProseWords]]     = inner.overview(tmdbId)
}
