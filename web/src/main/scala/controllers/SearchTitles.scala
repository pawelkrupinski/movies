package controllers

/**
 * What the listing's search matches besides a film's title: its original title and, for one in
 * another script, that title in Latin letters ("Москва слезам не верит" → "Moskva slezam ne
 * verit"), so a search typed either way finds it. Computed here once for every client: the web
 * cards carry it as `data-search-titles`, the apps get it as `/api/repertoire`'s `searchTitles`,
 * and neither Swift nor Kotlin needs a transliterator of its own.
 */
object SearchTitles {

  /** ICU's general Any-Latin transliteration, then to ASCII: ISO-style for Cyrillic and Greek,
   *  pinyin for Han. One instance per thread (an ICU transliterator is not documented as safe to
   *  share). The same transform as identity's `CreditRelation.latinized`, kept separate because a
   *  change under identity/ moves the resolver's source digest. */
  private val toLatin = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance("Any-Latin; Latin-ASCII"))

  /** The original title and its romanisation, each only when it adds a spelling the title lacks. */
  def of(title: String, originalTitle: Option[String]): Seq[String] = {
    val original = originalTitle.map(_.trim).filter(_.nonEmpty).filterNot(_.equalsIgnoreCase(title.trim))
    original.toSeq.flatMap { text =>
      val latin = if (text.forall(_ < 0x80)) text else toLatin.get.transliterate(text).trim
      Seq(text, latin)
    }.distinct
  }
}
