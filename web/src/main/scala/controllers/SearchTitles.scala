package controllers

/**
 * What the listing's search matches besides a film's title: the read model's curated spellings
 * of it (`ResolvedMovie.searchTitles`: each source's original title, IMDb's and Filmweb's usually
 * romanised the way the language does, and TMDB's English title), each also without its accents
 * so a search typed without them finds it. Computed here once for every client: the web cards
 * carry it as `data-search-titles`, the apps get it as `/api/repertoire`'s `searchTitles`.
 *
 * A machine romanisation (ICU's Any-Latin) is the fallback, only for an original in another
 * script that no source spells in Latin letters: it reads Japanese kanji as Mandarin and Hong Kong
 * titles in Mandarin rather than Cantonese, and an abjad (Arabic, Persian, Hebrew), which writes
 * no short vowels, it does not romanise at all -- "khrs nyst" is no spelling anybody types.
 */
object SearchTitles {

  /** One instance per thread (an ICU transliterator is not documented as safe to share). The
   *  general transform is the one identity's `CreditRelation.latinized` uses, kept separate because
   *  a change under identity/ moves the resolver's source digest. */
  private def transliterator(id: String) = ThreadLocal.withInitial(() => com.ibm.icu.text.Transliterator.getInstance(id))
  private val toLatin    = transliterator("Any-Latin; Latin-ASCII")
  private val withoutAccents = transliterator("Latin-ASCII")

  /** Scripts that write no short vowels: a machine romanisation of one spells nothing. */
  private val Abjads: Set[Character.UnicodeScript] =
    Set(Character.UnicodeScript.ARABIC, Character.UnicodeScript.HEBREW, Character.UnicodeScript.SYRIAC, Character.UnicodeScript.THAANA)

  private def scripts(text: String): Set[Character.UnicodeScript] =
    text.codePoints().toArray.iterator.filter(Character.isLetter).map(Character.UnicodeScript.of).toSet

  private def isLatin(text: String): Boolean = {
    val found = scripts(text)
    found.nonEmpty && found.subsetOf(Set(Character.UnicodeScript.LATIN))
  }

  /** The spellings, in order, each once (by case), none `title` again (with or without accents). `curated` is
   *  `ResolvedMovie.searchTitles`; a document projected before it existed has only `originalTitle`. */
  def of(title: String, originalTitle: Option[String], curated: Seq[String]): Seq[String] = {
    val spellings = (originalTitle.toSeq ++ curated).map(_.trim).filter(_.nonEmpty)
    val anyLatin  = spellings.exists(isLatin)
    val forms = spellings.flatMap { spelling =>
      if (isLatin(spelling)) Seq(spelling, withoutAccents.get.transliterate(spelling))
      else if (!anyLatin && scripts(spelling).intersect(Abjads).isEmpty) Seq(spelling, toLatin.get.transliterate(spelling).trim)
      else Seq(spelling)
    }
    // Not the title again, nor the title without its accents: what the clients match the title
    // itself by is theirs to decide, not a spelling of some other title.
    val key   = (text: String) => text.toLowerCase(java.util.Locale.ROOT)
    val shown = Set(title.trim, withoutAccents.get.transliterate(title.trim)).map(key)
    forms.filterNot(form => shown(key(form))).distinctBy(key)
  }
}
