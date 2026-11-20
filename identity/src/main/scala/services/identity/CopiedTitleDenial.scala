package services.identity

import services.identity.measures.{IdentityMeasures, MeasuredListing}
import services.identity.vocabulary.Denial

/** The denial a feed's copied original title makes in a family ([[FamilyScope]]): a film only the title a feed copied
 *  names, never the billed title, is the copy's pull, not the venue's film. */
private[identity] final class CopiedTitleDenial(pool: FamilyPool, links: FeedLinkOverrule, recorded: Map[Int, Candidate]) {

  /** Why `node`'s original title, a FEED's copy ([[SourceReliability.copiesOriginalTitle]]), denies it `candidate`: only that
   *  copied title names it, never the billed title. The copied title is the feed entry's, restated — the entry's claim of
   *  which film it is ([[FeedLink]]) — so a namesake it alone names is the copy's pull, not the venue's film; and the year,
   *  director and running time the feed copied beside it agree with that entry by construction, so they confirm nothing
   *  (one origin, counted once). A TMDB-keyed feed's link names the entry's film, which stays the node's to take. DE
   *  Mathäser's "Rodychi (UKRAJINA CINEMA)": Cineamo links Chereshnya's "Родичі" (1658840m) and copies its English title
   *  "Kin", which took Baker's 2018 "Kin"; "Karavan" took the 1946 "Caravan", "Akten des Missbrauchs" Zobel's "The Hunt".
   *  Measured on convergence run 38529260149's DE model takes: 7 listings took a film only the copied title of another
   *  linked film named, all 7 wrong. A page feed names no film, its copy no less one when it shares nothing with the
   *  billing ([[MeasuredListing.originalSharesNothingBilled]]): DE Cinecitta's "Über Unterbiberger - Vom Woid in
   *  die Welt" links kinoheld's page of another event, which copied Eisenstein's "Октябрь", his credit and its 142
   *  minutes, and took "Oktober 1917" at 61.9% while thirteen venues billed Ditscherlein's 2025 documentary. */
  def namedOnlyByCopiedTitle(node: EvidenceNode, candidate: Candidate): Option[Denial] =
    copiedBy(node).filter(_._2.forall(_ != candidate.tmdbId)).flatMap { case (feed, linked) =>
      copiedTitleDenial(links.evidenceOf(node).measured, candidate, feed, linked) }

  /** The feed whose copy `node`'s original title is, with the film it links, if any: a TMDB-keyed feed's link, else a
   *  page feed's copy, which names no film: on a node no TMDB-keyed feed links — whose link, where this resolve's
   *  films are a family's own numbers, is read as nothing, its copied title the linked film's own (DE "Der Film 444
   *  und ein Treffen …") — and only a copy sharing nothing with the billing: one spelling the billed title, or
   *  translating it word for word, is the venue's work restated (DE "Mein Opa der Bankräuber" copied as "Mijn Opa de
   *  Bankrover", "Kardes Türküler ile 30 yil" as "Kardeş Türküler ile 30 Yıl"). */
  private def copiedBy(node: EvidenceNode): Option[(String, Option[Int])] =
    links.copyingLink(node).map { case (feed, linked) => (feed, Some(linked)) }
      .orElse(Option.when(FeedLink.linkedFilm(node.listings).isEmpty && node.listings.forall(SourceReliability.copiesOriginalTitle) &&
        links.evidenceOf(node).measured.originalSharesNothingBilled && !links.evidenceOf(node).measured.originalTranslatesBilled)(
        SourceReliability.claimOf(node.listings.head) -> None))

  private def copiedTitleDenial(listing: MeasuredListing, candidate: Candidate, feed: String, linked: Option[Int]): Option[Denial] =
    // a SHORT ([[ShortFilms.isShort]]) beside a TMDB-keyed feed's link to another film is the copy's pull whatever the
    // billed title names, and whatever year: the feed's pick is a programme's film, the year and an English title a short
    // carries too the pick's restated (DE "L`Estive" ×3: Cineamo links Lenoir's 2026 documentary, copying its English
    // title "Summer Meadow", which a 3-minute 2024 short carries beside "L'Estive"; "HEIMSUCHUNG - MittagsKINO" took the
    // 18-minute "Visitation", the English title of Schlöndorff's "Heimsuchung"; Kino Gelnhausen's "FILMKUNST: Ein einfacher
    // Unfall", Panahi's 2025 film beside a 2025 short). Only where the feed copied a title the venue does not bill
    // ([[FeedLink.copiesAnotherTitle]]): a short the venue's own original title is, is its namesake
    if (linked.isDefined && FeedLink.copiesAnotherTitle(listing) && ShortFilms.isShort(candidate.film))
      Some(Denial(s"a short, its feed ($feed) linking ${linked.fold("")(_.toString)}", Denial.Kind.NotTheWork(creditOverrules = false)))
    else Option.when(listing.originalTitle.nonEmpty && !sameYear(listing, candidate) && !billsItsWords(listing, candidate, linked) &&
      IdentityMeasures.names(pool.titleRelation(listing, candidate).value, listing, candidate.film) && !namedByItsBilling(listing, candidate))(
      Denial(s"named only by the title its feed ($feed) copied" + linked.fold("")(film => s" from $film")))

  /** A page feed's copy names no film, so the record must not carry the billing's words either: one sharing two of
   *  them, which a mere spelling keeps from naming it, is the billing's ("Madoka Magica - The Movie: Rebellion" is
   *  "Mahou Shoujo Madoka Magica the Movie (Part 3): The Story of the Rebellion"); one word is a namesake's chance
   *  ("Über Unterbiberger - Vom Woid in die Welt" and "Oktober 1917 – Zehn Tage, die die Welt erschütterten"). */
  private def billsItsWords(listing: MeasuredListing, candidate: Candidate, linked: Option[Int]): Boolean =
    linked.isEmpty && (Seq(candidate.film.title) ++ candidate.film.originalTitle).exists(title => listing.wordsBilledOf(title) >= 2)

  /** A year the listing states that the film shares — the linked film's, copied — makes the two one film under two
   *  records: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003] links 1486412m and is the 2003 film 68030. */
  private def sameYear(listing: MeasuredListing, candidate: Candidate): Boolean =
    listing.year.exists(year => candidate.film.year.exists(film => math.abs(film - year) <= 1))

  /** Whether the billed titles — without the copied original, nor a search title that is only it again — name the record
   *  as its database holds it: the venue titles the corpus learned for it by that very original
   *  ([[IdentityMeasures.venueTitles]]) are the copy's pull again. Nor by one word a first name runs into, no delimited
   *  piece of the billing: DE Centraltheater Brake's "RAY WILSON", a singer's concert Cineamo links to the 1944 "Wilson"
   *  and copies that title from, named "Wilson - Der Weltverbesserer" (2017) only by the surname (prod 2026-10-14). */
  private def namedByItsBilling(listing: MeasuredListing, candidate: Candidate): Boolean = {
    val copied = listing.originalTitle.map(IdentityMeasures.key)
    val billed = listing.copy(originalTitle = None, searchTitles = listing.searchTitles.filterNot(title => copied.contains(IdentityMeasures.key(title))))
    val record = recorded.get(candidate.tmdbId).fold(candidate.film)(_.film)
    val pieces = IdentityMeasures.namingPieces(billed, record)
    lazy val byOneWordOnly = pieces.nonEmpty && pieces.forall(_.sizeIs == 1) && !billed.shapeKeys.exists(key => record.forms.exists(_.key == key))
    IdentityMeasures.names(pool.relate(billed, record).value, billed, record) && !byOneWordOnly
  }
}
