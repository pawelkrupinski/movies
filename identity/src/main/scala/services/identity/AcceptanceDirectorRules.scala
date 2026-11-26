package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.measures.{IdentityMeasures, MultiFilmBill, Scored, SearchMeasures, StageRelays}
import services.titles.YearWindow

/** What the acceptance rules that take a film by the director the listing credits ask: a director's work, billed under
 *  another subtitle or alone, and a director's title ([[AcceptanceFactRows]]), each asking `factRules` whether a published
 *  fact contradicts the record. Functions of the candidates and the listing they score; [[AcceptanceWiring]] makes the
 *  one the rules ask. */
private[identity] final class AcceptanceDirectorRules(factRules: AcceptanceFactRules) {
  import factRules.contradicted
  import AcceptanceDirectorRules.BilledRuntime

  /** The candidates whose WORK the listing bills under another subtitle, or publishes alone, by the director it credits,
   *  that no published year or runtime contradicts (`IdentityMeasures.sharesWork`, `titleIsWorkOf`, or the original title
   *  the venue publishes, `originalTitleIsWorkOf`: DE "Ein Hund namens Quill", published as "Quill" by Yōichi Sai, 2004,
   *  is his "Quill - Ein Freund für´s Leben"; three venues' "Leonas" is Cotelo's "Leonas, el instinto más salvaje", fifth
   *  in TMDB's search for the word): [[AcceptanceRuleTable.DirectorsWork]]'s to pick one of.
   *
   *  Both titles bill the work under a banner ("Royal Shakespeare Company: Macbeth", "RSC Live: Macbeth") and the running
   *  times agree: a staging the credited director made, not one of the word's many films — when it is the only record
   *  billing that work, whatever its running time: two seasons of one staging (Oliver Mears's Tosca, 2025/26 and
   *  2026/27, the later with no credits or running time yet) are the season's to tell apart, not this rule's — counting
   *  every record billing it that credits no OTHER director: a season's record often credits nobody yet. */
  private[identity] def directorsWorks(weighing: Weighing): Seq[Scored] = {
    lazy val billedAlike = weighing.eligible.count(scored => billsAStaging(scored) && !scored.category("director").contains("different"))
    def billedWork(scored: Scored) = scored.ownCredit && billsAStaging(scored) && billedAlike == 1 &&
      IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime)
    weighing.eligible.filter(scored => scored.ownCredit &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored) || billedWork(scored)) && !contradicted(scored))
  }

  private def bareWork(scored: Scored) =
    (IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film) orElse IdentityMeasures.originalTitleIsWorkOf(scored.listing, scored.candidate.film))
      .exists(words => words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))

  private def billsAStaging(scored: Scored) = StageRelays.billings(scored.listing, scored.candidate.film).nonEmpty

  /** The eligible candidates the listing's title — or the original title its venue publishes — names EXACTLY that credit
   *  the director the listing credits, when no published year or runtime contradicts them: PL Kinoteka's "Podróżnik",
   *  published as Kiarostami's original, is his film ([[AcceptanceRuleTable.DirectorsTitle]]'s to pick one of).
   *
   *  The original title names it only when it stands apart from the billing ([[MeasuredListing.originalStandsApart]])
   *  and the billed title names no eligible record exactly: a ticket page that copies the original title and director of
   *  the record a search for its billing hit states nothing that record does not, so the two cannot confirm it (DE
   *  kinoheld's "Tino Bomelino" as Tino {Anna Lucía Alcalá Pérez}, "Ganes" as Ganges; Fontane Klub's "Ich bin Dein",
   *  Iram Haq's film by its title and runtime, credited to another "Jeg er din"'s director — run 38578038277).
   *
   *  A whole delimited piece of the billing names it as its bare title does, the rest a banner, as `segment-top-hit` reads
   *  it ([[byPiece]]) — one word too, the credited director telling the word's films apart: UK Keswick Alhambra's "KFC
   *  Presents: Hen" {György Pálfi} is his 2026 "Hen", not the Afrikaans 2025 one TMDB ranks first. */
  private[identity] def directorsTitles(weighing: Weighing): Seq[Scored] = {
    val eligible = weighing.eligible
    lazy val titled = eligible.exists(_.category("title").contains("exact"))
    def byOriginal(candidate: Scored) =
      candidate.category("originalTitle").contains("match") && candidate.listing.originalStandsApart && !titled
    eligible.filter(candidate => (candidate.category("title").contains("exact") || byOriginal(candidate) || byPiece(candidate)) &&
      candidate.ownCredit && !contradicted(candidate))
  }

  /** Does a whole delimited piece of the listing's OWN billing (a banner's rest, after its colon, dash or pipe:
   *  `SearchTitles.candidates` of its title alone) name the candidate, standing for the whole
   *  ([[SearchMeasures.pieceStandsForTheWhole]], one word enough) in a billing of one work? Not a shape its published
   *  original title or its search titles give it: a ticket page copying the original title and director of the record a
   *  search hit (DE kinoheld's "DAS PACK" as Pack {Peiqi Peng}) states nothing that record does not. */
  private[identity] def byPiece(candidate: Scored): Boolean =
    candidate.category("title").contains("segment") && billedAsAPiece(candidate) && !MultiFilmBill.billsTwoWorks(candidate.listing) &&
      !IdentityMeasures.joinsAnotherWork(candidate.listing, candidate.candidate.film) &&
      SearchMeasures.pieceStandsForTheWhole(candidate.listing, candidate.candidate.film, words = 1)

  private def billedAsAPiece(candidate: Scored): Boolean = {
    val listing = candidate.listing
    val whole   = listing.ownTitles.map(IdentityMeasures.key).toSet
    val pieces  = listing.ownTitles.flatMap(services.titles.SearchTitles.candidates(_, None)).map(IdentityMeasures.key)
      .filter(key => key.nonEmpty && !whole(key)).toSet
    candidate.candidate.film.ownTitles.exists(title => pieces(IdentityMeasures.key(title)))
  }
}

private[identity] object AcceptanceDirectorRules {
  /** How far a billed work's running time may stand from the venue's: a broadcast's interval and introduction. */
  val BilledRuntime = 15
}
