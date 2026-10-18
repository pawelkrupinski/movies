package services.movies

import scala.io.Source

/**
 * The words venues bill AROUND a film — an anniversary, a premiere, a talk, a sneak, an audience, a double bill, a
 * house's banner — by concept and language, in ONE checked-in table (`title-words.tsv`), read by every rule that
 * recognises them. Before it, each rule kept its own list, in the languages its author met first: a German "35-jähriges
 * Jubiläum" bill read as a different film from the same film's English "35th Anniversary" because one list knew only
 * English and Spanish. `TitleWordsSpec` now fails when a concept lacks a word in any language a country bills in.
 *
 * Three ways to read a concept, as its readers need:
 *  - [[tokens]]: the concept's words as [[TitleContainment.tokens]] folds them (case and accents off) — a word set;
 *  - [[foldedPattern]]: a regex over a title's folded words joined by single spaces (`TitleContainment.tokens(t).mkString(" ")`);
 *  - [[rawPattern]]: a regex fragment over the title as billed, to be used case-insensitively, each accented letter
 *    matching its bare letter too.
 */
object TitleWords {
  val Resource = "title-words.tsv"

  /** A concept and its readers (who reads it is what a word added to it changes). */
  enum Concept(val id: String) {
    /** `IdentityMeasures.AnniversaryYears`, `DecorationWords.EditionWords`. */
    case Anniversary     extends Concept("anniversary")
    /** `DecorationWords.EditionWords`. */
    case Edition         extends Concept("edition")
    /** `DecorationWords.EventWords` (and `MultiFilmBill.EventTailWords`). */
    case Premiere        extends Concept("premiere")
    case Talk            extends Concept("talk")
    case Screening       extends Concept("screening")
    case Festival        extends Concept("festival")
    case Performance     extends Concept("performance")
    case Marathon        extends Concept("marathon")
    /** `ListingShape.billsAFilmFestival`: a film festival billed by its own name alone is its programme, no film. */
    case FilmFestival    extends Concept("film-festival")
    /** `ListingShape.billsAFilmFestival`: a festival's touring edition, billed after its name ("Cinema Edition"). */
    case FestivalEdition extends Concept("festival-edition")
    /** `DecorationWords.EventWords` and `ListingShape.HouseWords`. */
    case Relay           extends Concept("relay")
    /** `MultiFilmBill.EventTailWords`, beside the event words. */
    case SideEvent       extends Concept("side-event")
    /** `SuspectTakes.OldFilmWords`. */
    case OldFilm         extends Concept("old-film")
    /** `NonMovieEventClassifier.isMysteryScreening` — the scrape filter: a listing billing one is dropped. */
    case Sneak           extends Concept("sneak")
    /** `agreement.NonFilmEvents` — kept as a listing, never a film. */
    case SecretScreening extends Concept("secret-screening")
    /** `MultiFilmBill`'s markers. */
    case DoubleBill      extends Concept("double-bill")
    /** `ExtraTitleRules`' screening-note strip (`BareAudience`). */
    case Audience        extends Concept("audience")
    /** `StageRelays.StageHouseWords` and `ListingShape.HouseWords`. */
    case StageHouse      extends Concept("stage-house")
    /** `ListingShape.HouseWords` and `ListingShape.StageWords` (`IdentityResolver.billedAsPiece`: a staging beside a film). */
    case Stage           extends Concept("stage")
    case HouseBanner     extends Concept("house-banner")
    /** `services.identity.ShortFilms.billsAShort`: a listing billing a short may take one. */
    case ShortFilm       extends Concept("short-film")
    /** `TitleFacts`: a director credited inside a title, read into the listing's director at intake. */
    case DirectorCredit  extends Concept("director-credit")
    /** `TitleFacts`: an original title labelled inside a title, read into the listing's original title at intake. */
    case OriginalTitle   extends Concept("original-title")
  }
  object Concept {
    val byId: Map[String, Concept] = values.map(c => c.id -> c).toMap
  }

  final case class Entry(concept: Concept, language: String, phrase: String, note: Option[String])

  /** Lines `concept<TAB>language<TAB>phrase[<TAB>note]`; `#` comments and blank lines skipped. An unknown concept or a
   *  short line fails: a typo must not drop a word silently. */
  def parse(lines: Iterator[String]): Seq[Entry] =
    lines.zipWithIndex.filter { case (line, _) => line.trim.nonEmpty && !line.startsWith("#") }.map { case (line, i) =>
      line.split('\t').toSeq match {
        case Seq(concept, language, phrase, rest*) if phrase.trim.nonEmpty =>
          val c = Concept.byId.getOrElse(concept.trim, throw new IllegalArgumentException(s"$Resource:${i + 1}: unknown concept '$concept'"))
          Entry(c, language.trim, phrase.trim, rest.headOption.map(_.trim).filter(_.nonEmpty))
        case _ => throw new IllegalArgumentException(s"$Resource:${i + 1}: not concept<TAB>language<TAB>phrase: '$line'")
      }
    }.toSeq

  lazy val entries: Seq[Entry] = {
    val in = Option(getClass.getClassLoader.getResourceAsStream(Resource)).getOrElse(throw new IllegalStateException(s"no $Resource on the classpath"))
    try parse(Source.fromInputStream(in, "UTF-8").getLines()) finally in.close()
  }

  def phrases(concepts: Concept*): Seq[String] = entries.collect { case e if concepts.contains(e.concept) => e.phrase }.distinct
  /** The concepts' phrases in `languages` only: what one country's venues write. */
  def phrasesIn(languages: Set[String], concepts: Concept*): Seq[String] =
    entries.collect { case e if concepts.contains(e.concept) && languages(e.language) => e.phrase }.distinct

  /** The languages a concept holds a word in. */
  def languages(concept: Concept): Set[String] = entries.collect { case e if e.concept == concept => e.language }.toSet

  /** Every word of the concepts' phrases, folded as [[TitleContainment.tokens]] folds a title's, of two letters or more:
   *  a phrase's single letter is no word of the concept ("Q&A" folds to "q", "a" — the article of every English title).
   *  A stem has no word to hold, so a concept read this way may not have one. */
  def tokens(concepts: Concept*): Set[String] = {
    val all = phrases(concepts*)
    all.find(_.contains('*')).foreach(p => throw new IllegalStateException(s"'$p' is a stem: ${concepts.map(_.id).mkString(", ")} are read as words"))
    all.flatMap(TitleContainment.tokens).filter(_.lengthIs > 1).toSet
  }

  /** The concepts' phrases as one alternation over FOLDED text (a title's [[TitleContainment.tokens]] joined by single
   *  spaces), each phrase from a word boundary — unless it opens on `*`, read inside a word too ("ukrytyseanswciemno") —
   *  its words one optional space apart ("seanswciemno" is "seans w ciemno"), a stem open-ended, any other word ending
   *  on a boundary. Longest phrase first. */
  def foldedPattern(concepts: Concept*): String =
    s"(?:${alternation(concepts) { phrase =>
      val words = phrase.split("\\s+").toSeq.map { word =>
        val body = TitleContainment.tokens(word.stripPrefix("*").stripSuffix("*")).map(java.util.regex.Pattern.quote).mkString("\\s?")
        if (word.endsWith("*")) s"$body\\w*" else body
      }
      (if (phrase.startsWith("*")) "" else "\\b") + words.mkString("\\s?") + (if (phrase.endsWith("*")) "" else "\\b")
    }})"

  /** The concepts' phrases as one alternation over the title AS BILLED, for a case-insensitive (`(?iu)`) pattern: an
   *  accented letter also matches its bare letter ("sesión" = "sesion"), words are spaces or hyphens apart
   *  ("dementia-friendly"), a stem is open-ended. Longest phrase first; no boundary of its own — the reader's. */
  def rawPattern(concepts: Concept*): String = rawAlternation(phrases(concepts*))
  /** [[rawPattern]] over the phrases of `languages` only ([[phrasesIn]]). */
  def rawPatternIn(languages: Set[String], concepts: Concept*): String = rawAlternation(phrasesIn(languages, concepts*))
  private def rawAlternation(phrases: Seq[String]): String =
    s"(?:${alternationOf(phrases) { phrase =>
      phrase.split("\\s+").toSeq.map { word =>
        val body = word.stripPrefix("*").stripSuffix("*").map { ch =>
          val bare = tools.TextNormalization.deburr(ch.toString)
          if (bare != ch.toString && bare.length == 1) s"[$ch$bare]"
          else if (ch.isLetterOrDigit) ch.toString
          else if (ch == '-') "[\\s-]*"
          else java.util.regex.Pattern.quote(ch.toString)
        }.mkString
        if (word.endsWith("*")) s"$body\\p{L}*" else body
      }.mkString("[\\s-]*")
    }})"

  private def alternation(concepts: Seq[Concept])(compile: String => String): String = alternationOf(phrases(concepts*))(compile)
  private def alternationOf(phrases: Seq[String])(compile: String => String): String =
    phrases.sortBy(p => (-p.length, p)).map(compile).mkString("|")
}
