package tools

/**
 * Loose title key for cross-source film matching in [[FilmwebDiff]]: the same
 * screening must produce the same key whether it comes from our scraper or from
 * Filmweb, so language/format variants and truncation don't double-count a film
 * as both `ours-only` and `fw-only`.
 *
 * Normalisation, in order:
 *   1. lower-case, drop parenthetical/bracket suffixes (original titles, year);
 *   2. cut at the first ellipsis (`…` / `...`) — a truncated listing keeps only
 *      its common prefix, so "skarpetek… podróż na kraj kosmosu" and the full
 *      title both reduce to the same prefix key (no truncation mismatch);
 *   3. strip trailing language/format tags (hindi, telugu, eng, napisy, dubbing,
 *      lektor, 2D/3D, IMAX, 4DX, VR, …) wherever they trail the base title, so
 *      "Peddi Hindi" / "Peddi Telugu" / "Peddi" all key as "peddi";
 *   4. Arabic→Roman for sequel numerals ("Avatar 3" ↔ "Avatar III");
 *   5. collapse whitespace + punctuation noise.
 *
 * Pure + public so the spec can pin the cross-source equivalences directly.
 */
object FilmwebDiffTitleNormalizer {

  private val ArabicToRoman: Map[String, String] = Map(
    "1" -> "i", "2" -> "ii", "3" -> "iii", "4" -> "iv", "5" -> "v",
    "6" -> "vi", "7" -> "vii", "8" -> "viii", "9" -> "ix", "10" -> "x",
    "11" -> "xi", "12" -> "xii", "13" -> "xiii", "14" -> "xiv", "15" -> "xv",
    "16" -> "xvi", "17" -> "xvii", "18" -> "xviii", "19" -> "xix", "20" -> "xx"
  )

  /** Language-version + screening-format tags that ride on a base title across
   *  sources. Stripped only when they TRAIL the title (so a film genuinely named
   *  e.g. "2D" — none exist — isn't reduced to nothing). */
  private val TrailingTags: Set[String] = Set(
    // language
    "hindi", "telugu", "tamil", "kannada", "malayalam", "eng", "english",
    "napisy", "napisami", "nap", "dubbing", "dubbed", "dub", "dubb",
    "lektor", "lek", "pl", "org", "oryginalna", "subtitles", "subtitled",
    // format
    "2d", "3d", "imax", "4dx", "4d", "vr", "screenx", "dolby", "atmos", "hfr"
  )

  def normalize(rawTitle: String): String = {
    val lowered = rawTitle.trim.toLowerCase

    // 2. drop trailing parenthetical/bracket annotations, then cut at ellipsis.
    val noBrackets = lowered.replaceAll("[\\(\\[].*$", "")
    val deTruncated = noBrackets.split("(…|\\.\\.\\.)", 2).head

    // 5. tokenise on non-alphanumeric runs (drops dots, dashes, slashes, etc.).
    val words = deTruncated.split("[^\\p{L}\\p{N}]+").iterator.filter(_.nonEmpty).toList

    // 3. strip trailing format/language tags (one or several stacked, e.g.
    //    "… napisy 2d").
    val base = dropTrailingTags(words)

    // 4. Arabic → Roman sequel numerals, then collapse to a single key.
    base.map(w => ArabicToRoman.getOrElse(w, w)).mkString(" ").trim
  }

  @annotation.tailrec
  private def dropTrailingTags(words: List[String]): List[String] = words match {
    // Keep at least one word — don't strip a title down to empty.
    case init :+ last if init.nonEmpty && TrailingTags(last) => dropTrailingTags(init)
    case _ => words
  }
}
