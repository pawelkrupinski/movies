package services.billing

import services.movies.FormatTags

/**
 * A billing text read ONCE into the parts a venue bills around a work: the one structured parse every billing reader
 * reads its part from instead of re-reading the raw string with regexes of its own (proposal #2,
 * docs/proposals/2026-10-21-matching-fundamentals.md §4.2). Each part is a field, read lazily, so a reader pays only for
 * the part it reads; the grammar behind a part lives in this package ([[VersionTags]]) or, for the ingest's peel shared
 * with the scrapers, in [[FormatTags]].
 *
 * It parses any billing text, not only a listing's raw billing: the identity measures read a listing's original title
 * and a film's titles through it too ("The Lord of the Rings: The Two Towers – Extended").
 *
 * Phase 1 moves readers here without changing what any of them reads: `BillingParseEquivalenceSpec` holds every distinct
 * billing of the identity recordings with what the readers made of it before the move. Parts so far: the format and
 * language notes with the certificate and running time ([[peeled]]), the certificate alone ([[uncertified]]), and the
 * closing version tag ([[versionless]]). Still read elsewhere: a banner or strand, the work, a bill's pieces, a numbered
 * subtitle.
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
}

object BillingParse {
  def apply(text: String): BillingParse = new BillingParse(text)
}
