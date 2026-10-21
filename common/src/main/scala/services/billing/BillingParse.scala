package services.billing

import services.movies.FormatTags

/**
 * A billing text read ONCE into the parts a venue bills around a work: the one structured parse every billing reader
 * reads its part from instead of re-reading the raw string with regexes of its own (proposal #2,
 * docs/proposals/2026-10-21-matching-fundamentals.md §4.2). Each part is a field, read lazily, so a reader pays only for
 * the part it reads; the grammar behind a part lives in this package ([[VersionTags]], [[NumberedTitles]]) or, for the ingest's peel shared
 * with the scrapers, in [[FormatTags]].
 *
 * It parses any billing text, not only a listing's raw billing: the identity measures read a listing's original title
 * and a film's titles through it too ("The Lord of the Rings: The Two Towers – Extended").
 *
 * Phase 1 moves readers here without changing what any of them reads: `BillingParseEquivalenceSpec` holds every distinct
 * billing of the identity recordings with what the readers made of it before the move. Parts so far: the format and
 * language notes with the certificate and running time ([[peeled]]), the certificate alone ([[uncertified]]), the
 * closing version tag ([[versionless]]), the seasons and bracketed years ([[yearless]]), and the instalment, subtitle
 * and parts a title numbers ([[instalment]], [[subtitle]], [[titledParts]]). Still read elsewhere: a banner or strand,
 * the work, a bill's pieces.
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
   *  Version)" and of "Dune: Part Two"; none of "Blade Runner 2049" or "2001: A Space Odyssey". */
  lazy val instalment: Option[Int] = NumberedTitles.instalmentNumber(text)

  /** The subtitle the title gives its series' instalment ([[NumberedTitles.subtitleOf]]): "two", "towers" of "The Lord of
   *  the Rings: The Two Towers – Extended", the version's words left out. */
  lazy val subtitle: Option[NumberedTitles.Subtitle] = NumberedTitles.subtitleOf(text)

  /** The parts the title numbers and subtitles ([[NumberedTitles.titledParts]]): part 1, "sendung", "yoghi" of "DAS
   *  INDISCHE GRABMAL (TEIL 1, DIE SENDUNG DES YOGHI)"; a part's words may be none. */
  lazy val titledParts: Seq[(Option[String], Seq[String])] = NumberedTitles.titledParts(text)
}

object BillingParse {
  def apply(text: String): BillingParse = new BillingParse(text)
}
