package services.identity

import models.{Helios, KinoApollo, Multikino, Rialto}
import services.movies.{ListingKey, TitleNormalizer}

/** Corpora whose families' decisions hang on what OTHER families hold — the corpus-wide facts
 *  (`CorpusContext`) an incremental model must keep current. */
object CrossFamilyCorpora {
  import FilmTable.{F, listing}

  /** `decides`: the other families' facts change what this corpus's families DECIDE, not only the context. */
  final case class Corpus(label: String, listings: Seq[Listing], lookups: IdentityLookups, decides: Boolean)

  private def translated(venue: models.Cinema, title: String, original: String): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, None, Nil), title, title, title, None, Nil, None, None, Some(original))

  def all(normalizer: TitleNormalizer): Seq[Corpus] = Seq(
    // A banner's house is learned from its OTHER works' records: UK venues bill the Royal Ballet &
    // Opera season "RBO Cinema Season 2026-27: …"; TMDB files RBO's Swan Lake and Alice, its Manon
    // only under the Met (`IdentityResolverCasesSpec`).
    {
      def rbo(work: String) = Seq(Helios, KinoApollo).map(listing(_, s"RBO Cinema Season 2026-27: $work"))
      def met(work: String) = Seq(Multikino, Rialto).map(listing(_, s"Met Opera 2026-27: $work"))
      Corpus("a banner's house learned from its other works (RBO's Manon)",
        rbo("Swan Lake") ++ rbo("Alice's Adventures in Wonderland") ++ rbo("Manon") ++ met("Manon") ++ met("Macbeth"),
        new FilmTable(Seq(F(1702782, "Royal Ballet & Opera 2026/27: Swan Lake", 2027, "", 0, 3),
          F(1702778, "Royal Ballet & Opera 2026/27: Alice's Adventures in Wonderland", 2027, "", 0, 3),
          F(1703631, "The Metropolitan Opera 2026/27: Manon", 2027, "", 0, 5),
          F(1703622, "The Metropolitan Opera 2026/27: Macbeth", 2026, "", 0, 5)), normalizer), decides = true)
    },
    // A venue title holds while its original names ONE record: "Niebo nad Normandią", originally
    // "Pressure", is a title of the one "Pressure" — until another family's listing (a director's
    // other film) brings a second "Pressure" into the corpus.
    Corpus("a venue title learned from one family, withdrawn by another's record",
      Seq(Helios, KinoApollo).map(translated(_, "Niebo nad Normandią", "Pressure")) ++
        Seq(listing(Multikino, "Arrival", Some(2016), Some("Denis Villeneuve"))),
      new FilmTable(Seq(F(1, "Pressure", 2023, "Anthony Maras", 118), F(2, "Arrival", 2016, "Denis Villeneuve", 116),
        F(3, "Pressure", 2011, "Denis Villeneuve", 90, searched = false)), normalizer), decides = false),
    // A record's alternative titles bill its work under another banner: TMDB files the National
    // Theatre's broadcasts "National Theatre Live: …" and lists "National Theatre at Home: …" beside
    // them (1693710 "At Home"). The alternatives arrive with the RECORD, not the search hit, so a
    // late record re-bills every node reaching it, and the banner's contenders move.
    Corpus("a banner's contenders moved by a record's alternative titles (NT Live at Home)",
      Seq(Helios, KinoApollo).map(listing(_, "NT Live: Dr. Strangelove")) ++
        Seq(Multikino, Rialto).map(listing(_, "NT Live: The Importance of Being Earnest")),
      new FilmTable(Seq(F(1401957, "National Theatre Live: Dr. Strangelove", 2025, "", 0, 2,
          alternatives = Seq("National Theatre at Home: Dr. Strangelove")),
        F(1352026, "National Theatre Live: The Importance of Being Earnest", 2025, "", 0, 2,
          alternatives = Seq("National Theatre at Home: The Importance of Being Earnest"))), normalizer), decides = false)
  )
}
