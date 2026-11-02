package services.identity

import IdentityMeasures.*
import services.movies.TitleContainment

/** How a listing's director credits relate to a film's: the director relation, and a credit read by search or by title.
 *  Part of [[IdentityMeasures]], which exports it. */
private[identity] object CreditMeasures {
  /** How two credit lists relate: one person in common — the same words or letters in any order,
   *  case or splitting (`same_person`) — a shared name word only (a surname, a transliteration), or
   *  nobody in common. Credits in different scripts (a venue's "Bi Gan", TMDB's "毕赣") are compared in Latin letters
   *  ([[CreditRelation.latinized]]); nobody in common THERE is `different_script`, its own category, since a
   *  transliteration can miss a person a Latin spelling names (a Japanese reading of Kanji is not
   *  its pinyin). `incomparable` only when a side has no name left to compare in Latin letters. */
  def directorRelation(a: Seq[String], b: Seq[String]): Measure = CreditRelation.of(new CreditRelation.Credits(a), new CreditRelation.Credits(b))

  /** `measures`, the director read as the listing's own when its credits and the record's are in different
   *  scripts and no transliteration joins them, but the person TMDB finds by the listing's OWN spelling directed
   *  (or wrote) the film: "Wong Kar Wai" is TMDB's 王家衛, whose pinyin reads "Wang Jiawei" (film 843). */
  def creditedBySearch(measures: Map[String, Measure], directedBySpelling: Boolean): Map[String, Measure] =
    if (directedBySpelling && measures.get("director").exists(m => m == Category("different_script") || m == Category("incomparable")))
      measures + ("director" -> SamePersonMeasure)
    else measures

  /** `measures`, the director read as credited when the listing credits nobody but its title bills the film under a
   *  banner naming one of its directors ([[bannerNamesItsDirector]]). */
  def creditedByTitle(l: Listing, f: Film, measures: Map[String, Measure]): Map[String, Measure] =
    if (!measures.get("director").contains(MissingListing) || !bannerNamesItsDirector(l, f)) measures
    else measures + ("director" -> SamePersonMeasure)

  /** `measures`, the director read from the venue's OWN text ([[VenueNames]]: its synopsis, its cast field) where the
   *  listing credits nobody: the text naming one of the film's directors WHOLE (`named`: [[textNamedDirectors]]) is a
   *  credit (`same_person`) — ES taquilla's "Hope", "Basada en la propia historia de su directora Maria Sødahl", is her
   *  2019 film; UK Chichester's "The Ties That Bind Us", "writer/director Carine Tardieu", is L'Attachement. One
   *  measure, so its learned weight scores the film and [[Corroboration]] and the vetoes read it as any credit.
   *  Never a credit AGAINST a film: a synopsis names stars and crew who direct too, so the text naming another film's
   *  director says nothing of this one — measured 2026-11-02 (docs/identity-experiments/2026-11-02-text-director-against.md),
   *  it denied UK Riverside Woodbridge's "Rocky 50th Anniversary" its 1976 film for naming Stallone, Rocky II's director. */
  def creditedByText(f: Film, named: Set[String], measures: Map[String, Measure]): Map[String, Measure] =
    if (measures.get("director").contains(MissingListing) && f.directors.exists(_.exists(named))) measures + ("director" -> SamePersonMeasure)
    else measures

  /** Does the venue's own text (`names`: [[VenueNames.names]], two words at least, never a surname alone) name one of the
   *  film's directors WHOLE? */
  def textNamesItsDirector(names: String => Boolean, f: Film): Boolean = f.directors.exists(_.exists(names))

  /** The directors of `films` the venue's own text (`names`) names WHOLE, where the listing `l` credits nobody — what
   *  [[creditedByText]] reads each of them against. Not a person the billing's title names: the title's subject, whom the
   *  text names as such, not as its director — UK Forum Northallerton's "NURSE GEORGIE CARROLL: INFECTIOUS" describes the
   *  comedian, whom IMDb credits as the director of an earlier special of hers, not of this one. */
  def textNamedDirectors(l: Listing, names: String => Boolean, films: Iterable[Film]): Set[String] =
    if (l.directors.nonEmpty) Set.empty
    else {
      val billed = (l.title +: l.rawTitle.toSeq).flatMap(TitleContainment.tokens).toSet
      films.iterator.flatMap(_.directors.getOrElse(Nil)).filter(director => names(director) && !TitleContainment.tokens(director).forall(billed)).toSet
    }

  /** Does the listing's title bill one of the film's titles whole under a BANNER naming one of its directors — the
   *  surname ([[Film.directorSurnames]]) among the words before it? "Konwicki Salto", "Konwicki: Salto" (PL Kino Fenix's
   *  retrospective of Tadeusz Konwicki), and a series' banner naming two: DE Babylon (Mitte)'s "Hitchcock & Chabrol:
   *  Spellbound" is Hitchcock's 1945 film. A banner the resolver learned and peels off ([[TitleDecorations]]) still says
   *  so: the peeled title is searched, the banner stays evidence. So does a credit CLOSING the title — one of its
   *  directors named whole, two words at least, after one of its titles whole: DE Caligari's "ES (ULRICH SCHAMONI)" is
   *  Schamoni's 1966 "Es", not Muschietti's 2017 "It" its German title shares. Only for a listing crediting nobody. */
  private[identity] def bannerNamesItsDirector(l: Listing, f: Film): Boolean =
    l.directors.isEmpty && (l.bannerCuts.exists { case (titled, banner) => f.titleKeys.contains(titled) && banner.exists(f.directorSurnames) } ||
      l.closingCuts.exists { case (titled, credit) => f.directorNameKeys.contains(credit) && f.titleKeys.contains(titled) })
}
