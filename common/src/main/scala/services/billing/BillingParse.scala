package services.billing

import services.movies.FormatTags

/**
 * A billing text read ONCE into the parts a venue bills around a work: the one structured parse every billing reader
 * reads its part from instead of re-reading the raw string with regexes of its own (proposal #2,
 * docs/proposals/2026-10-21-matching-fundamentals.md §4.2). Each part is a field, read lazily, so a reader pays only for
 * the part it reads; the grammar behind a part lives in this package ([[VersionTags]], [[NumberedTitles]], [[DecorationWords]],
 * [[Credits]], [[BillingPieces]], [[BillJoins]]) or, for the ingest's peel shared
 * with the scrapers, in [[FormatTags]].
 *
 * It parses any billing text, not only a listing's raw billing: the identity measures read a listing's original title
 * and a film's titles through it too ("The Lord of the Rings: The Two Towers – Extended").
 *
 * Phase 1 moves readers here without changing what any of them reads: `BillingParseEquivalenceSpec` holds every distinct
 * billing of the identity recordings with what the readers made of it before the move. Parts so far: the format and
 * language notes with the certificate and running time ([[peeled]], [[formatWords]]), the certificate alone
 * ([[uncertified]]), the closing version tag ([[versionless]]), the seasons and bracketed years ([[yearless]]), the
 * instalment, subtitle and parts a title numbers ([[instalment]], [[subtitle]], [[titledParts]]), the edition words
 * ([[editionWords]]), the credits ([[credit]], [[author]]), the pieces, segments and banner ([[pieces]], [[segments]],
 * [[banner]]) with the programme and festival shapes, and a bill's pieces and markers ([[billPieces]], [[slashPieces]],
 * [[marker]], [[billedOne]]). Still read elsewhere: a relay's house and season (`RelayStage`, `StageRelays`), the work a
 * billing names beside its decorations.
 */
final class BillingParse private (val text: String) {

  /** The screen-format and language-version notes, a box office's certificate and running time, peeled off the text
   *  ([[FormatTags.peel]]): "Digger (15) 105 mins (OV)" is "Digger", OV, 15, 105 minutes. */
  lazy val peeled: FormatTags.Peeled = FormatTags.peel(text)

  /** The text without its format and language notes ([[peeled]]'s title). */
  def formatless: String = peeled.title

  /** The text without the BBFC certificate note a box office bills behind it, and the running time or short year it
   *  runs on into ([[FormatTags.uncertified]]): "Digger - Cert 15" is "Digger". Nothing else of the text is peeled. */
  lazy val uncertified: String = FormatTags.uncertified(text)

  /** The text without the closing VERSION tag it bills, if it closes on one ([[VersionTags.versionless]]): "Der Herr der
   *  Ringe 2 (Extended Version)" is "Der Herr der Ringe 2"; "The Final Cut" has none. */
  lazy val versionless: Option[String] = VersionTags.versionless(text)

  /** The text without its seasons and bracketed years ([[NumberedTitles.withoutYears]]): how a house bills a work,
   *  whatever it dates it by. "Tosca (2026)" is "Tosca ". */
  lazy val yearless: String = NumberedTitles.withoutYears(text)

  /** The instalment the title numbers ([[NumberedTitles.instalmentNumber]]): 2 of "Der Herr der Ringe 2 (Extended
   *  Version)" and of "Dune Part Two"; none of "Blade Runner 2049" or "2001: A Space Odyssey". */
  lazy val instalment: Option[Int] = NumberedTitles.instalmentNumber(text)

  /** The subtitle the title gives its series' instalment ([[NumberedTitles.subtitleOf]]): "two", "towers" of "The Lord of
   *  the Rings: The Two Towers – Extended", the version's words left out. */
  lazy val subtitle: Option[NumberedTitles.Subtitle] = NumberedTitles.subtitleOf(text)

  /** The parts the title numbers and subtitles ([[NumberedTitles.titledParts]]): part 1, "sendung", "yoghi" of "DAS
   *  INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)"; a part's words may be none. */
  lazy val titledParts: Seq[(Option[String], Seq[String])] = NumberedTitles.titledParts(text)

  // ── the words billed around a work ─────────────────────────────────────────────────────────

  /** The edition words of its folded words ([[DecorationWords.EditionWords]]): "extended", "cut" of "Das Boot - Director's
   *  Cut". */
  lazy val editionWords: Seq[String] = DecorationWords.editionWordsOf(text)

  /** The screen-format and language-version tokens it names anywhere ([[FormatTags.formatTokensIn]]): a scraper's format
   *  cell, "2D napisy". */
  lazy val formatWords: List[String] = FormatTags.formatTokensIn(text)

  // ── credits ────────────────────────────────────────────────────────────────────────────────

  /** The name its closing bracket holds and nothing else ([[Credits.bracketedName]]): "Ulrich Schamoni" of "ES (ULRICH
   *  SCHAMONI)". */
  lazy val credit: Option[String] = Credits.bracketedName(text)

  /** The work and the author an authorship word closing it credits ([[Credits.authorCredited]]): "Fallen Angels" by "Noël
   *  Coward". */
  lazy val author: Option[Credits.AuthorCredit] = Credits.authorCredited(text)

  // ── pieces, banners, strands ───────────────────────────────────────────────────────────────

  /** Its delimited pieces, seasons and bracketed years out ([[BillingPieces.pieces]]): what its searches split it by. */
  lazy val pieces: Seq[String] = BillingPieces.pieces(text)

  /** Its segments and the breaks between them, a bracket read as a pipe ([[BillingPieces.segments]]): what a venue bills
   *  beside its work. */
  lazy val segments: BillingPieces.Segments = BillingPieces.segments(text)

  /** Its banner, the leading piece a colon or a pipe closes ([[BillingPieces.banner]]): "Somali Film Festival" of
   *  "Somali Film Festival: Lifeline". */
  lazy val banner: Option[String] = BillingPieces.banner(text)

  /** Its WORK: the text less a leading piece naming a strand, a programme or a club and a closing piece opening on one
   *  ([[BillingPieces.work]]): "Drugie życie" of "Kino Konesera: Drugie życie". What the lookup query asks for. */
  lazy val work: String = BillingPieces.work(text)

  /** Where its work starts, when a strand's piece leads it: the end of that banner, delimiter included. */
  lazy val workStart: Option[Int] = Some(BillingPieces.workSpan(text)._1).filter(_ > 0)

  /** Is its banner a film festival's ([[BillingPieces.underAFilmFestival]])? */
  def underAFilmFestival: Boolean = BillingPieces.underAFilmFestival(text)

  /** Does it bill a film festival by its own name and nothing else ([[BillingPieces.billsAFilmFestival]])? */
  def billsAFilmFestival: Boolean = BillingPieces.billsAFilmFestival(text)

  /** The programme slot it bills in place of a film under a festival's or season's banner
   *  ([[BillingPieces.programmeSlot]]): "Opening Night". */
  def programmeSlot: Option[String] = BillingPieces.programmeSlot(text)

  // ── several works ──────────────────────────────────────────────────────────────────────────

  /** The pieces a bill's "+" joins ([[BillJoins.billPieces]]); itself alone when it joins none. */
  def billPieces: Seq[String] = BillJoins.billPieces(text)

  /** Does it join works by a bill's "+" ([[BillJoins.joinsWorks]])? */
  def joinsWorks: Boolean = BillJoins.joinsWorks(text)

  /** The pieces a spaced "//" joins ([[BillJoins.slashPieces]]); itself alone when it joins none. */
  def slashPieces: Seq[String] = BillJoins.slashPieces(text)

  /** The "Triple:" or "Double:" banner heading it ([[BillJoins.headingBanner]]). */
  def headingBanner: Option[String] = BillJoins.headingBanner(text)

  /** The word by which its folded words bill several films beside other words ([[BillJoins.foldedMarker]]). */
  def foldedMarker: Option[String] = BillJoins.foldedMarker(text)

  /** The word billing several films: its heading banner, else its folded marker. */
  def marker: Option[String] = headingBanner.orElse(foldedMarker)

  /** The one title a programme word annotates after a dash ([[BillJoins.billedOne]]): "Inna Mamusia" of "Inna Mamusia -
   *  maraton". */
  def billedOne: Option[String] = BillJoins.billedOne(text)

  /** Does it bill a set ("zestaw") of works ([[BillingPieces.billsASet]])? */
  def billsASet: Boolean = BillingPieces.billsASet(text)

  /** Does it quote two works or more ([[BillingPieces.quotesSeveral]])? */
  def quotesSeveral: Boolean = BillingPieces.quotesSeveral(text)

  /** Does it quote two dated works or more ([[BillingPieces.quotesSeveralDated]])? */
  def quotesSeveralDated: Boolean = BillingPieces.quotesSeveralDated(text)

  /** Does it close on two quoted works a conjunction joins ([[BillingPieces.quotesTwoJoined]])? */
  def quotesTwoJoined: Boolean = BillingPieces.quotesTwoJoined(text)

  /** Is it two segments a dash joins, and nothing else ([[BillingPieces.dashedPair]])? */
  def dashedPair: Boolean = BillingPieces.dashedPair(text)

  /** Does a segment of it list three works or more by name ([[BillingPieces.listsWorks]])? */
  def listsWorks: Boolean = BillingPieces.listsWorks(text)
}

object BillingParse {
  def apply(text: String): BillingParse = new BillingParse(text)
}
