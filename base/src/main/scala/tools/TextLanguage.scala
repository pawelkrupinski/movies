package tools

import java.util.Locale

/**
 * Stopword-ratio guesser over the four deployment languages (Polish, English,
 * German, Spanish) — NOT a general-purpose detector. Exists so
 * `MovieRecord.bestSynopsis` can tell a candidate synopsis is in the WRONG
 * language and rank it below a correctly-localized one instead of picking it
 * purely on paragraph/length, and so the identity resolver can tell a film
 * title in the venue's own language from a foreign one ([[readsAs]]).
 *
 * Confirmed prod case: Cinema City's own `cinema-city.pl` detail page served
 * an English synopsis for "Marsupilami" on its Polish-domain pages — an
 * upstream CMS bug, not a scraper locale mistake. The English blurb was
 * longer than TMDB's correct Polish one and won `bestSynopsis` outright for
 * every city Cinema City serves.
 *
 * Deliberately crude: exact-match against small hand-picked stopword lists,
 * no external corpus/model, so it stays hermetic and dependency-free. It is a
 * SOFT ranking signal, not a hard gate — `detect` returns `None` (no penalty)
 * whenever the text is too short or ambiguous to clear [[MinHits]], so a
 * borderline call never risks hiding a real synopsis.
 */
object TextLanguage {

  private val Word = "\\p{L}+".r

  // Hand-picked to avoid cross-language collisions where practical (deburred,
  // lowercase). Kept short and unambiguous rather than exhaustive — this only
  // has to catch "this blurb is obviously in the wrong language", not
  // classify text in general.
  private val Stopwords: Vector[(String, Set[String])] = Vector(
    "pl" -> Set(
      "i", "w", "na", "z", "do", "sie", "nie", "jest", "ktory", "ktora", "ktore",
      "jak", "po", "za", "od", "przez", "aby", "ze", "ale", "gdy", "byc", "ona",
      "on", "jego", "jej", "tym", "tego", "dla", "tez"
    ),
    "en" -> Set(
      "the", "and", "of", "to", "in", "is", "that", "for", "on", "with", "his",
      "her", "they", "who", "but", "when", "after", "before", "into", "from",
      "was", "were", "this", "their"
    ),
    "de" -> Set(
      "der", "die", "das", "und", "ist", "nicht", "ein", "eine", "mit", "sich",
      "auf", "fur", "von", "zu", "dass", "als", "aber", "wenn", "nach", "wird",
      "einen", "einem", "sein", "seine", "ihre"
    ),
    "es" -> Set(
      "el", "la", "los", "las", "de", "que", "y", "en", "un", "una", "por",
      "con", "para", "su", "se", "no", "pero", "cuando", "despues", "sus", "mas"
    )
  )

  /** Minimum stopword hits before a guess is trusted — below this the text is
   *  too short or too ambiguous, and `detect` returns `None` rather than risk
   *  a false mismatch. */
  private val MinHits = 3

  /** Best-guess ISO 639-1 tag among pl/en/de/es for `text`, or `None` when no
   *  language clears [[MinHits]]. Deburred before matching so a diacritic
   *  doesn't split a stopword from its ASCII spelling (`się` → `sie`,
   *  `für` → `fur`). Ties keep the first language in [[Stopwords]] order. */
  def detect(text: String): Option[String] = {
    val counted = hits(text)
    if (counted.isEmpty) None
    else {
      val (bestLang, bestScore) = counted.maxBy(_._2)
      Option.when(bestScore >= MinHits)(bestLang)
    }
  }

  /** Does `text` read as `language` (a tag of [[Stopwords]]): one of its stopwords at least, and more of them than of
   *  any other language's? For a title's few words, where [[detect]]'s [[MinHits]] is never cleared: "Gegen das
   *  Vergessen" reads as German, "Ekmek parası" as none of the four. */
  def readsAs(text: String, language: String): Boolean = {
    val counted = hits(text)
    counted.collectFirst { case (`language`, own) => own }.exists(own => own >= 1 && counted.forall { case (lang, n) => lang == language || n < own })
  }

  /** Each language's stopword count in `text`, in [[Stopwords]] order; empty when it holds no word. */
  private def hits(text: String): Vector[(String, Int)] = {
    val tokens = Word.findAllIn(TextNormalization.deburr(text).toLowerCase(Locale.ROOT)).toVector
    if (tokens.isEmpty) Vector.empty else Stopwords.map { case (lang, words) => lang -> tokens.count(words.contains) }
  }
}
