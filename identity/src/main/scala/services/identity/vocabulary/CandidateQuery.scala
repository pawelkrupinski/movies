package services.identity.vocabulary

/** The questions the resolver asks. A function of one evidence: which is what makes the whole
 *  query set a function of the listing SET (assumption A1). */
enum CandidateQuery {
  /** A yearless title search (`IdentityMeasures.searchQueries`). */
  case Title(query: String)
  /** Every film a person of this name directed (or, with no directing credit, wrote). */
  case Director(name: String)
  /** Every film IMDb lists under this very title, found in TMDB by its IMDb id: a path to a record
   *  TMDB's own search does not return ("Caligula: The Ultimate Cut", whose record only an IMDb id
   *  reaches). A path, never a rank: its films are scored on the listing's facts alone. */
  case Imdb(title: String)
  /** Every film IMDb lists under this very title in ANY language — its own title, its original or one of its
   *  AKAs — found in TMDB by its IMDb id: the record a local title names when TMDB carries no translation of it
   *  ("Camino dla opornych" is IMDb's Polish title of TMDB's "Compostelle", "Kuźma" IMDb's "Kuzma"). Each film
   *  found reads the title as one of its own ([[CorpusContext.titlesByFacts]]); a path, never a rank. The ones TMDB holds
   *  no record of are answered too, under their FALLBACK ids ([[FallbackIds]]): never a candidate TMDB's rules weigh,
   *  only what a cluster no TMDB film was taken for may fall back to ([[ResolverDecision.fallback]]). */
  case ImdbTitled(title: String)
  /** Every film the director a title CLOSES on directed ([[TitleCredit]]): the name its closing bracket holds, when a
   *  search for the very title suggests a director of that name — DE "ES (ULRICH SCHAMONI)" reaches Schamoni's 1966
   *  "Es", which no search of "Es" ranks. Asked only of a title closing on such a bracket, crediting nobody. A path,
   *  never a rank: its films are scored on the listing's facts, the credit among them ([[IdentityMeasures.bannerNamesItsDirector]]). */
  case TitleCredit(title: String)

  def sortKey: String = this match {
    case Title(q)       => s"t\u0000$q"
    case Director(n)    => s"d\u0000$n"
    case Imdb(t)        => s"i\u0000$t"
    case ImdbTitled(t)  => s"a\u0000$t"
    case TitleCredit(t) => s"c\u0000$t"
  }

}

object CandidateQuery {
  implicit val ordering: Ordering[CandidateQuery] = Ordering.by(_.sortKey)

  /** The query a [[CandidateQuery.sortKey]] names. */
  def fromSortKey(key: String): Option[CandidateQuery] = key.split("\u0000", 2) match {
    case Array("t", text) => Some(Title(text))
    case Array("d", name) => Some(Director(name))
    case Array("i", text) => Some(Imdb(text))
    case Array("a", text) => Some(ImdbTitled(text))
    case Array("c", text) => Some(TitleCredit(text))
    case _                => None
  }

  /** A director credit as TMDB's person search knows it: without a trailing IMDb disambiguator —
   *  " (I)", " (II)", " (III)", … Case-sensitive and limited to well-formed numerals below C, because
   *  that is all IMDb ever writes: a lowercase "(mix)" or "(vi)", a malformed "(IIII)", or a word-like
   *  "(MIX)" is part of the credit. */
  def personName(name: String): String = ImdbDisambiguatorSuffix.replaceFirstIn(name, "").trim

  private val ImdbDisambiguatorSuffix: scala.util.matching.Regex =
    """\s+\((?=[IVXL])(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})\)$""".r
}
