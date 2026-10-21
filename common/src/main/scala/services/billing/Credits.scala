package services.billing

import services.movies.{FormatTags, TitleContainment}

/**
 * The CREDITS a billing names around its work, read off its text alone ([[BillingParse.credit]], [[BillingParse.author]]):
 * the name a closing bracket holds and nothing else ("ES (ULRICH SCHAMONI)", read by `services.identity.TitleCredit`), and
 * the author an authorship word closing the title credits ("Fallen Angels by Noël Coward", read by
 * `services.identity.ListingShape`). Whether a film is the credited person's is the identity's question, not this one.
 */
object Credits {

  /** A billing's WORK and the author it credits it to, after an authorship word closing its title — "Fallen Angels by
   *  Noël Coward", "Die Physiker von Friedrich Dürrenmatt", "Wesele autorstwa Stanisława Wyspiańskiego": a playwright's
   *  or an author's whole name (two words or more; "Vertigo by Hitchcock" credits nobody). */
  final case class AuthorCredit(work: String, person: String)

  private val ClosingName = """^.*\p{L}.*?\s*\(\s*(\p{L}[\p{L}.'’-]*(?:\s+\p{L}[\p{L}.'’-]*){1,3})\s*\)\s*$""".r

  /** The name `title`'s closing bracket holds, if it holds nothing but what may be one: two to four words of letters,
   *  none a note's word, each capitalised or a name's particle. A bracket of one word, or with a number, is a format, a
   *  language or a year ("(OmU)", "(1966)"); one of note words a cut, a score, an event or a version ("(Director's Cut)",
   *  "(Live Score)") or subtitles ("(mit Untertiteln)"). */
  def bracketedName(title: String): Option[String] = title.trim match {
    case ClosingName(name) if namesAPerson(name) => Some(name)
    case _                                        => None
  }

  /** Could the bracket's words be a person's name — none a note's word, each capitalised or a name's particle? */
  private def namesAPerson(name: String): Boolean = {
    val words = name.split("\\s+").toSeq
    words.forall(word => word.head.isUpper || Particles(word)) &&
      !TitleContainment.tokens(name).exists(word => DecorationWords.EventWords(word) || DecorationWords.EditionWords(word) || NoteWords(word)) &&
      FormatTags.formatTokensIn(name).isEmpty
  }
  /** The lower-case words a name holds: "Wim van der Wenders", "Manoel de Oliveira". */
  private val Particles = Set("van", "von", "der", "den", "de", "di", "da", "del", "della", "du", "le", "la", "ten", "ter", "zu", "y", "al", "bin")
  /** Words of a subtitles' or a language's note ("mit Untertiteln", "with subtitles", "z napisami"). */
  private val NoteWords = Set("mit", "with", "untertitel", "untertiteln", "subtitles", "subtitled", "napisami", "napisy", "lektor", "sous", "titres",
    "subtitulada", "subtitulado", "subtitulos")

  /** The [[AuthorCredit]] `title` closes on, if any. Allocates nothing on a title holding no authorship word: asked of
   *  every accepted take. */
  def authorCredited(title: String): Option[AuthorCredit] =
    if (!holdsAuthorshipWord(title)) None
    else AuthorCredited.findFirstMatchIn(title).map(m => AuthorCredit(m.group("work").trim, m.group("person").trim))

  private val AuthorshipWords = Array(" by ", " von ", " de ", " autorstwa ")
  /** Does `title` hold an authorship word ([[AuthorshipWords]]), case aside — by index alone, no allocation. */
  private def holdsAuthorshipWord(title: String): Boolean = {
    var w = 0
    while (w < AuthorshipWords.length) {
      val word = AuthorshipWords(w)
      var at   = 0
      while (at <= title.length - word.length) { if (title.regionMatches(true, at, word, 0, word.length)) return true; at += 1 }
      w += 1
    }
    false
  }
  private val AuthorCredited = {
    val part     = """\p{Lu}[\p{L}'’.-]*"""
    val particle = """(?:(?:van|von)(?:\s+(?:der|den))?|de(?:\s+la)?|da|del|della|di|du|la|le)"""
    (s"""(?U)^(?<work>.+?)\\s+(?i:by|von|de|autorstwa)\\s+(?<person>$part(?:\\s+(?:$particle\\s+)?$part)+)""" +
      """(?:\s*[(\[][^)\]]*[)\]])*\s*$""").r
  }
}
