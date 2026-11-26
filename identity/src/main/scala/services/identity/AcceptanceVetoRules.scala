package services.identity

import services.identity.Acceptance.Refused
import services.identity.AcceptanceRule.Weighing
import services.identity.measures.{Film, IdentityMeasures, ListingShape, Scored}
import services.titles.{NonMovieEventClassifier, YearWindow}
import services.titles.billing.Credits

/** What the vetoes of [[AcceptanceRuleTable.Vetoes]] ask of any rule's take: whether the billing is something else than
 *  the film it found — a performance beside it, an author's namesake, a staged show's own record, an event no film, a
 *  page reused for another evening, a name alone, another film of its director beside it — each answering with what it
 *  held against the film (the refusal's detail), if anything.
 *  Functions of the candidates and the listing they score; [[AcceptanceWiring]] makes the one the rules ask. */
private[identity] final class AcceptanceVetoRules {
  import AcceptanceDirectorRules.BilledRuntime
  /** A film the billing names by one piece while ANOTHER piece, sharing no word with the record's titles, bills a
   *  performance ([[NonMovieEventClassifier.isPerformance]]: a tribute, a concert, a stage show): the evening is the show,
   *  named after the film's title, and whatever record the venue's system attached to it is the namesake its search
   *  found. UK Electric Palace Hastings' "Straight Talk: A Tribute to Dolly Parton" [1992] {Barnet Kellman} is a tribute
   *  show named after her song, not the 1992 film she starred in. Only the event the billing bills is read
   *  ([[FilmNess.eventBilled]]): a billing attaching a screening ("+ film", "pokaz", a festival's "Closing
   *  Film + Awards") keeps its film, and a performance "joined with" the film is its companion ("DYRYGENT - POŁĄCZONY Z
   *  KONCERTEM MUZYKI NA ŻYWO", Wajda's film). A record titled by the performance itself ("Kabaret", "Die Tribute von
   *  Panem") is the film its event words name. A film of the silent era is screened to a performance — live music is
   *  its accompaniment (PL Kino Wisła's "PANTEON - Gabinet Doktora Caligari | muzyka na żywo", Wiene's 1920 film). */
  private[identity] def performedBeside(scored: Scored): Option[String] = {
    val film      = scored.candidate.film
    val pieces    = if (film.year.exists(_ <= ListingShape.SilentEraEnd)) Nil else scored.listing.performancePieces
    lazy val filmWords = film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val performed =
      if (pieces.isEmpty || !pieces.exists(_._2.forall(filmWords))) Nil
      else pieces.collect { case (piece, words, true) if !words.exists(filmWords) => piece }
    Option.when(performed.nonEmpty && !film.titles.exists(NonMovieEventClassifier.isPerformance(_)))(
      s"${Refused.named(scored)} beside: ${performed.mkString(", ")}")
  }

  /** The staged show's OWN record for a billing naming a staged show ([[ListingShape.showsOwnRecord]]): a pantomime is an
   *  event, whatever facts its billing shares with the record TMDB files of it. Held here, where a listing takes a film
   *  ALONE, and not only after the assignment ([[BilledPiece]]): a listing holding the record would join no other spelling
   *  of the event (`ConstraintEdges`' same-event must-link joins two holding no film). UK, prod 2026-11-13: 15 "Cbeebies
   *  Panto: Treasure Island" billings (Lonsdale Alhambra Penrith, BEAM Hertford, Showcase's sensory-friendly ones) took
   *  1785679 by favoured-calibrated, then lost it to the billed-as-one-piece veto alone. */
  private[identity] def stagedShowsOwn(scored: Scored): Boolean =
    ListingShape.showsOwnRecord(scored.listing.rawTitle.getOrElse(scored.listing.title), scored.candidate.film)

  /** The older namesake of a work a billing credits to its author ([[ListingShape.creditDenies]]): DE Cineplex's "Fallen
   *  Angels by Noël Coward", the 2026 capture of Coward's play, is not Wong Kar-wai's 1995 "Fallen Angels". A refusal, not
   *  a denial: the billing's siblings stay free to join the namesake's listings (a denial's cannot-link split UK's
   *  92-venue cluster of the capture, losing 16 of its takes). Asked of every accepted take, so a billing with no
   *  authorship word allocates nothing ([[ResolverAllocationSpec]]'s dated-title budget). */
  private[identity] def authorsNamesake(scored: Scored): Option[String] = {
    val listing = scored.listing
    Credits.authorCredited(listing.rawTitle.getOrElse(listing.title))
      .filter(credit => ListingShape.creditDenies(credit, scored.candidate.film, listing.dating.screeningsSeason))
      .map(credit => s"${Refused.named(scored)}: ${credit.work} by ${credit.person}")
  }

  /** A billing that reads as no film at all ([[FilmNess]]: event words — a gig, a tour, a festival, a show — and no relay
   *  or screening attached) whose record shares none of its other words: the namesake a search for the event's words
   *  found, not what the venue screens. The agreement stage already asks no family about such a billing
   *  ([[NonFilmEvents]]); this holds the model's own takes to the same reading. Asked of every accepted take,
   *  so a billing with no event word answers from the listing's one-off [[MeasuredListing.eventWords]] and
   *  allocates nothing ([[ResolverAllocationSpec]]'s dated-title budget). */
  private[identity] def readsAsNoFilm(scored: Scored): Boolean = {
    val listing = scored.listing
    val film    = scored.candidate.film
    // V2: an event-worded billing whose record shares none of its other words — found by the event's words alone
    // V3: V2 alone (V1's NonFilmEvents reading dropped), and only where the billing has words beside the event's
    listing.eventWords.nonEmpty && listing.nonEventWords.nonEmpty &&
      // V3d: the record's own and original title only — an alternative title TMDB files under the gig's own billing
      // (1419814 carries "Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer") is the reused record's echo, not its name
      !(film.title +: film.originalTitle.toSeq).exists(t => IdentityMeasures.countedWords(t).exists(listing.nonEventWords)) &&
      // V3e: a record in another script is named by its alternatives (Cinema City's "ENHYPEN WORLD TOUR 'BLOOD SAGA'"), and a
      // year the billing states that the record shares is a film's fact ("Silent Night, Bloody Night (1972)")
      !(film.title.exists(c => Character.isLetter(c) && Character.UnicodeScript.of(c) != Character.UnicodeScript.LATIN) &&
        film.titles.exists(t => IdentityMeasures.countedWords(t).exists(listing.nonEventWords))) &&
      !listing.statedYear.exists(year => film.year.exists(filmYear => math.abs(filmYear - year) <= 1))
  }
  private[identity] def onlyTheEventsWords(scored: Scored): String =
    s"${Refused.named(scored)}: its record shares only the event's words (${scored.listing.eventWords.mkString(", ")})"

  /** Any film while the listing's page reads as REUSED: a record its ORIGINAL title and the director it credits name
   *  stands in its pool ([[copiedBeyondItsBilling]]) though the billing says more than that record's titles. The copied
   *  title is what a search for the billing hit, copied onto a ticket page with the record's director, running time and
   *  year: they agree with the record by construction and confirm nothing, and the billing's other words name something
   *  else — a party, a band, a show the page was reused for. DE kinoheld: Kaminwerk's "Teenie Fasching" on Schweikart's
   *  "Fasching" (1939), "Dark New Wave Night" on Ai's "New Wave", Lagerhalle's "Three for Silver" (a folk band) on Kamal's
   *  "Tea For Three", KAP-Torgau's "VOODOO LOUNGE" (a Rolling Stones tribute) on Costabile's "VooDoo". Nor does such a
   *  page lend another film its take: only one the billing names whole (an exact, original or alternative title) may
   *  still be taken — Landsberg's "Helmut Schleich" (a Kabarett evening, on Kuzmic's "Helmut") is no more the 2018
   *  recording of his "Kauf, du Sau" the bare name leads. */
  private[identity] def reusedPage(weighing: Weighing, scored: Scored): Option[String] = {
    def namedWhole = scored.category("title").exists(Set("exact", "original", "alternative"))
    weighing.eligible.find(copiedBeyondItsBilling).filter(copied => (copied eq scored) || !namedWhole)
      .map(copied => s"${Refused.named(copied)} billed beyond: ${billedBeyond(copied).mkString(", ")}")
  }

  /** Is `scored` named only by the listing's ORIGINAL title and the director it credits, that title a mere part of the
   *  billing ([[MeasuredListing.originalStandsApart]] false), with the billing saying more than any of the
   *  record's titles ([[billedBeyond]]) and stating no year of it? A billing the record's titles hold word for word
   *  ("Matrix" of "The Matrix", or a translation TMDB holds as an alternative title) is its own, and a year the listing
   *  states that the record shares makes the billing another record's spelling of it, as the copied facts' other reader
   *  holds (`CopiedTitleDenial.namedOnlyByCopiedTitle`: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003]). The title
   *  relation cannot tell: it reads the original title as one of the listing's own shapes, a `segment` of every record
   *  so titled. */
  private[identity] def copiedBeyondItsBilling(scored: Scored): Boolean = {
    val listing = scored.listing
    scored.category("originalTitle").contains("match") && IdentityMeasures.sameDirector(scored.measures) && !listing.originalStandsApart &&
      !listing.year.exists(year => scored.candidate.film.year.exists(film => math.abs(film - year) <= 1)) && billedBeyond(scored).nonEmpty
  }
  /** The billing's words none of the record's titles carries — none when a whole piece of the billing, or the search its
   *  title rules leave of it, is the record's word for word ("Twisters" of "(4DX Rewind) Twisters", "Endgame" of
   *  "Vengadores: Endgame (Reestreno)", "Moje własne Idaho" of "… + DEBATA [POKAZ BEZPŁATNY]"): the rest decorates it. */
  private[identity] def billedBeyond(scored: Scored): Seq[String] = {
    val listing   = scored.listing
    val filmWords = scored.candidate.film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val pieces    = IdentityMeasures.shapes(listing.ownTitles ++ listing.searchTitles, listing.decorations).map(IdentityMeasures.countedWords).filter(_.nonEmpty)
    if (pieces.exists(_.forall(filmWords))) Nil else IdentityMeasures.countedWords(listing.title).filterNot(filmWords)
  }

  /** Does the listing bill a NAME alone ([[services.titles.billing.Credits.billsOnlyAName]]) that the record's longer title merely
   *  leads, with no fact the venue states agreeing — its year, its running time, its director, an original title naming
   *  the record whole? A performer billed bare is the performer's evening, not a recording of one programme: DE Filmforum
   *  Landsberg's "Helmut Schleich" (the Kabarettist live) is not "Helmut Schleich: Kauf, du Sau" (2018), UK Pavilions
   *  Teignmouth's "Patrick Monahan" not "Patrick Monahan: Live" (2011). Dated alike, Lichtburg's "Berliner Philharmoniker"
   *  [2026] is their "Silvesterkonzert 2026". Only a record titled as a programme after the name ([[titledAsAProgramme]]):
   *  PL SDK's "Left-Handed Girl" is the film TMDB titles "Left-Handed Girl. To była ręka… diabła!", a title and its
   *  distributor's subtitle. */
  private[identity] def aNameAlone(scored: Scored): Boolean = {
    val listing = scored.listing
    val billed  = listing.rawTitle.getOrElse(listing.title)
    scored.category("title").contains("fragment") && services.titles.billing.Credits.billsOnlyAName(billed) &&
      titledAsAProgramme(billed, scored.candidate.film) &&
      !scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency) &&
      !IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime) &&
      !IdentityMeasures.sameDirector(scored.measures) && !scored.category("originalTitle").contains("match")
  }

  /** Does one of the record's titles continue `name` as a programme of its: "<name>: …" or "<name> - …" ("Helmut Schleich:
   *  Kauf, du Sau", "Patrick Monahan: Live"), case and spacing aside — not a sentence after it, a film's subtitle. */
  private[identity] def titledAsAProgramme(name: String, film: Film): Boolean = {
    val words     = name.trim.split("\\s+").map(java.util.regex.Pattern.quote).mkString("\\s+")
    val programme = ("(?iu)^" + words + """\s*(?::|\s[-–—]\s)""").r
    (film.title +: (film.originalTitle.toSeq ++ film.alternativeTitles)).exists(title => programme.findPrefixOf(title.trim).isDefined)
  }

  /** Another eligible film of `best`'s credited director that the title names, or that the listing's year dates as
   *  closely, when the title names nothing of `best` (`title=none`): walking a director's credit reaches his films, and a
   *  title naming none of two his facts fit alike picks neither — DE "München Mord: Leben und Sterben in Schwabing" [2019]
   *  {Sascha Bigler}, a TV episode, is neither of his 2019 films; UK "Gremlins Special Event" {Joe Dante} is not his
   *  "Innerspace", however its 120 minutes fit, beside the Gremlins it names. A translation the facts single out stands
   *  (PL "Fach" is Rossi's "Il Mestiere"). */
  private[identity] def anotherOfItsDirector(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    if (!best.category("title").contains("none") || !IdentityMeasures.sameDirector(best.measures) || inflects(best)) None
    else eligible.find(other => (other ne best) && IdentityMeasures.sameDirector(other.measures) &&
      (other.titleNamesIt || (best.number("year.delta").isDefined && other.number("year.delta") == best.number("year.delta"))))

  /** Does a word of the billing inflect a word of the record's title ("Osobliwy świat Hieronymusa Boscha" of "The
   *  Curious World of Hieronymus Bosch"): a title in another language naming the film, though it shares no word whole. */
  private[identity] def inflects(scored: Scored): Boolean = {
    import services.titles.TitleContainment.tokens
    val billed = scored.listing.ownTitles.flatMap(tokens)
    scored.candidate.film.titles.flatMap(tokens).filter(_.length >= IdentityMeasures.StemLetters).exists(word => billed.exists(_.startsWith(word)))
  }
}
