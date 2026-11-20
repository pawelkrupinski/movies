package services.identity.vocabulary

/** What a venue's own detail page adds to its listing: the identity fields the model reads, and the page's synopsis and
 *  cast, which only the cast evidence reads ([[names]]) — the page's own strings, as the venue page index holds them,
 *  never copied. */
final case class DetailFacts(year: Option[Int], directors: Seq[String], runtime: Option[Int], originalTitle: Option[String],
                             countries: Seq[String] = Nil, synopsis: Option[String] = None, cast: Seq[String] = Nil) {
  /** The people the page's synopsis and cast name: worked out when the cast evidence asks, never held. */
  def names: VenueNames = if (synopsis.isEmpty && cast.isEmpty) VenueNames.None else VenueNames.of(synopsis, cast)
  /** The words the page's synopsis capitalises ([[ProseWords.ofProse]]): worked out when the synopsis evidence asks, never held. */
  def words: ProseWords = synopsis.fold(ProseWords.None)(text => ProseWords.ofProse(Seq(text)))
}
