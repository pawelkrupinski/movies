package services.billing

import services.movies.{TitleContainment, TitleText, TitleWords}

/**
 * The PIECES a billing's own delimiters cut it into, its BANNER and the strand and programme shapes they make, read off
 * its text alone ([[BillingParse.pieces]], [[BillingParse.segments]], [[BillingParse.banner]],
 * [[BillingParse.programmeSlot]] and the shape tests beside them). Which piece names a film is the identity's question
 * (`services.identity.ListingShape.billedBeside`).
 */
object BillingPieces {

  // ── pieces ─────────────────────────────────────────────────────────────────────────────────

  /** Where a title's own delimiters cut it into the pieces it is searched by: a colon or pipe, or a dash or slash spaced
   *  on both sides. */
  val Delimiter: java.util.regex.Pattern = java.util.regex.Pattern.compile("""\s*[:|]\s*|\s+[-–—/]\s+""")
  /** A title's delimited pieces ([[Delimiter]]), its seasons and bracketed years out. */
  def pieces(title: String): Seq[String] = Delimiter.split(NumberedTitles.withoutYears(title)).toSeq.map(_.trim).filter(_.nonEmpty)

  /** Where a billing breaks into the SEGMENTS a venue bills beside its work: a colon, pipe, bullet or plus; a dash spaced
   *  after it, as the title's searches split one ("Song Sung Blue- The Music of Neil Diamond"), never a hyphen within a
   *  word; a quotation mark. */
  val SegmentBreak: scala.util.matching.Regex = """\s*[:|•+]\s*|\s*[-–—]\s+|\s*[„”"“«»]\s*""".r
  private val Bracketed = """\([^)]*\)|\[[^\]]*]""".r

  /** A billing's segments ([[SegmentBreak]]) and the breaks between them, trimmed, a bracket read as a pipe: one break
   *  fewer than segments. */
  final case class Segments(parts: IndexedSeq[String], breaks: IndexedSeq[String])
  def segments(raw: String): Segments = {
    val piped = Bracketed.replaceAllIn(raw, " | ")
    Segments(SegmentBreak.split(piped).toIndexedSeq, SegmentBreak.findAllIn(piped).toIndexedSeq.map(_.trim))
  }

  // ── the work ───────────────────────────────────────────────────────────────────────────────

  private def wordsOf(concepts: TitleWords.Concept*) = TitleWords.rawPattern(concepts*)
  /** A strand's word anywhere in a piece ([[TitleWords.Concept.Strand]]). */
  private val StrandWord = s"(?iu)(?<![\\p{L}\\p{N}])(?:${wordsOf(TitleWords.Concept.Strand)})(?![\\p{L}\\p{N}])".r
  /** A piece opening on a strand's word, a quotation mark aside. */
  private val OpensOnStrand = ("(?iu)^[\\s\"'„“”]*(?:" + wordsOf(TitleWords.Concept.Strand) + ")(?![\\p{L}\\p{N}])").r
  /** A broadcaster's or a stage's piece ("Royal Ballet & Opera im Kino", "MET live im Kino") or a season's ("RBO Kino
   *  Saison 2026-27"): the relay's house, which its own readers read off the billing, never a strand of a venue's. */
  private val HouseWord = s"(?iu)(?<![\\p{L}\\p{N}])(?:${wordsOf(TitleWords.Concept.StageHouse, TitleWords.Concept.HouseBanner,
    TitleWords.Concept.Relay, TitleWords.Concept.Stage)})(?![\\p{L}\\p{N}])".r
  private val SeasonSpan = """\d{2,4}\s*[-/–]\s*\d{2}(?!\d)""".r

  /** Does `piece`, its brackets aside ("Godzilla Minus Zero (pokaz nocny)" names no strand), name a strand? */
  private def namesAStrand(raw: String, strand: scala.util.matching.Regex): Boolean = {
    val piece = Bracketed.replaceAllIn(raw, " ")
    strand.pattern.matcher(piece).find() && !HouseWord.pattern.matcher(piece).find() && !SeasonSpan.pattern.matcher(piece).find()
  }
  /** What is left is a work's title: a word of three letters or more ("UA" is a language's note), not a day or a running
   *  time ("19th October", "4 Stunden"), nor a programme's slot ("Opening Night", [[programmeSlot]]) or a festival's own
   *  part ("Purbeck Film Festival - Shorts Social": its shorts, competition, showcase, talk, awards). */
  private def namesAWork(rest: String): Boolean =
    AWord.pattern.matcher(rest).find() && !DateOrCount.pattern.matcher(rest.trim).matches() &&
      !ProgrammeSlot.pattern.matcher(rest.trim).matches() && !FestivalPart.pattern.matcher(rest).find()
  private val FestivalPart =
    """(?iu)(?<![\p{L}\p{N}])(?:shorts?|competition|programmes?|programs?|showcase|forum|talk|edition|selection|\p{L}*preis|awards?)(?![\p{L}\p{N}])""".r
  private val AWord = """\p{L}{3}""".r
  /** A day or a running time billed in a work's place: "19th October", "4 Stunden", "12 grudnia". */
  private val DateOrCount = ("""(?iu)\d{1,4}(?:st|nd|rd|th|\.)?\s+(?:jan|feb|m[aä]r|apr|ma[iy]|jun|jul|aug|sep|o[ck]t|nov|de[cz]|""" +
    """stycz|lut|kwie|czerw|lip|sierp|wrze|pa[źz]dz|listop|grud|ene|abr|ago|dic|stund|std|hour|hr|min|godz|hora)\p{L}*\.?""").r

  /** Does a closing piece name the work it bills — quoted ("premiera animacji „Kocie obserwacje”"), or a screening's or a
   *  premiere's word opening more than two words, the rest naming what is shown ("Pokaz przedpremierowy PRZEKLEŃSTW
   *  NIEWINNOŚCI", "premiera pierwszego odcinka 3. Sezonu")? Then it is the work's piece, not a strand's: "Premiera
   *  Krajowa", "Seans wyprzedany" are not. */
  private def namesItsOwnWork(piece: String, raw: String): Boolean =
    QuoteMark.pattern.matcher(piece).find() ||
      OpensOnEvent.pattern.matcher(piece).find() && piece.trim.split("\\s+").count(_.exists(_.isLetter)) > 2
  private val QuoteMark    = "[\"„“”«»]".r
  private val OpensOnEvent = """(?iu)^[\s"'„“”]*(?:pokaz|seans|premiera|przedpremiera|screening|preview|vorpremiere)(?![\p{L}\p{N}])""".r

  /** The [[Delimiter]]s that cut the billing itself: none inside a bracket or a quotation, nor a clock's colon ("19:45"). */
  private def cuts(raw: String): IndexedSeq[(Int, Int)] = {
    val found = Delimiter.matcher(raw)
    val out   = IndexedSeq.newBuilder[(Int, Int)]
    var depth = 0; var quoted = false; var at = 0
    while (found.find()) {
      while (at < found.start) {
        raw(at) match {
          case '(' | '[' => depth += 1
          case ')' | ']' => depth = math.max(0, depth - 1)
          case '"' | '„' | '“' | '”' => quoted = raw(at) match { case '„' => true; case '”' => false; case _ => !quoted }
          case _ =>
        }
        at += 1
      }
      val clock = found.group().trim == ":" && found.start > 0 && raw(found.start - 1).isDigit && found.end < raw.length && raw(found.end).isDigit
      if (depth == 0 && !quoted && !clock) out += ((found.start, found.end))
    }
    out.result()
  }

  /** Where the billing's WORK lies in it: the billing less a leading piece naming a strand, a programme or a club
   *  ([[TitleWords.Concept.Strand]]: "Kino Konesera: Drugie życie", "Japanese Film Club: Your Name") and a closing piece
   *  opening on one ("Tony | Kino Dostępne", "Lalka - seans dla grupy zorg."), each cut at the billing's own delimiter
   *  ([[Delimiter]]) and only while a work's title is left. A house's or a season's piece is the relay's, never cut. */
  def workSpan(raw: String): (Int, Int) = {
    val at = cuts(raw)
    var (from, to) = (0, raw.length)
    var (first, last) = (0, at.size - 1)
    while (first <= last && namesAStrand(raw.substring(from, at(first)._1), StrandWord) && namesAWork(raw.substring(at(first)._2, to))) {
      from = at(first)._2; first += 1
    }
    while (last >= first && namesAStrand(raw.substring(at(last)._2, to), OpensOnStrand) && !namesItsOwnWork(raw.substring(at(last)._2, to), raw) &&
           namesAWork(raw.substring(from, at(last)._1))) {
      to = at(last)._1; last -= 1
    }
    (from, to)
  }

  /** The billing's work ([[workSpan]]), trimmed: itself when it bills no strand. */
  def work(raw: String): String = { val (from, to) = workSpan(raw); if (from == 0 && to == raw.length) raw else raw.substring(from, to).trim }

  // ── the banner ─────────────────────────────────────────────────────────────────────────────

  private val BannerSlot = """^([^:|]+?)\s*[:|]\s*\S""".r
  /** The BANNER: the leading piece of the billing a colon or a pipe closes, a title after it ("Somali Film Festival:
   *  Lifeline"). */
  def banner(raw: String): Option[String] = BannerSlot.findFirstMatchIn(raw).map(_.group(1))

  private val FilmFestival    = TitleWords.foldedPattern(TitleWords.Concept.FilmFestival).r
  private val FestivalEdition = TitleWords.foldedPattern(TitleWords.Concept.FestivalEdition).r

  /** Is the billing's banner ([[banner]]) a film festival's ("Pornfilmfestival: …")? */
  def underAFilmFestival(raw: String): Boolean =
    banner(raw).exists(banner => FilmFestival.pattern.matcher(TitleContainment.tokens(banner).mkString(" ")).find())

  /** Does the title bill a FILM FESTIVAL by its own name and nothing else — a piece naming a film festival
   *  ([[TitleWords.Concept.FilmFestival]]), its edition's year or number and its touring edition's name
   *  ([[TitleWords.Concept.FestivalEdition]]: "Cinema Edition") aside? DE "Alpen Film Festival 2026", "Freeride
   *  Filmfestival 2026 - Cinema Edition". A work billed under the festival's banner ("Alpen Film Festival 2026: Mont
   *  Blanc") or beside it ("Fargo - Purbeck Film Festival") is the work, and a festival that is no film festival may be a
   *  film's own name ("Festival Express"). */
  def billsAFilmFestival(raw: String): Boolean = {
    def folded(text: String) = TitleContainment.tokens(text).mkString(" ")
    // what an edition adds: its year or number, its touring edition's name
    def editionOnly(words: String) = FestivalEdition.replaceAllIn(words, " ").trim.split(' ').forall(_.forall(_.isDigit))
    pieces(raw).filter(_.exists(_.isLetter)) match {
      case Seq(festival, rest*) if rest.forall(piece => editionOnly(folded(piece))) =>
        val words = folded(festival)
        val found = FilmFestival.pattern.matcher(words)
        found.find() && {
          val (before, after) = (words.substring(0, found.start), words.substring(found.end))
          // the festival's name before its phrase ("Alpen Film Festival 2026"), or the phrase first and the name after it
          // ("Festiwal Filmowy Kino Lato", "Festival de Cine de Sitges"); a sentence about a festival ("How I Started A Film
          // Festival", a short) bills an article, which no festival's name does
          (editionOnly(after) || editionOnly(before)) && !before.trim.split(' ').exists(TitleText.Articles)
        }
      case _ => false
    }
  }

  // ── a programme slot ───────────────────────────────────────────────────────────────────────

  /** A programme SLOT a festival or season bills by its place in the programme rather than by a film's title. */
  private val ProgrammeSlot = ("""(?iu)(?:opening|closing)\s+(?:night|gala|film)|gala|shorts?\s+programme|shorts?\s+program|""" +
    """secret\s+screening|surprise\s+(?:film|screening)|mystery\s+(?:film|screening)|""" +
    """gala\s+(?:otwarcia|zamknięcia)|(?:film|pokaz)\s+(?:otwarcia|zamknięcia|niespodzianka)|seans\s+niespodzianka|""" +
    """eröffnungs(?:film|gala|abend)|abschluss(?:film|gala|abend)|überraschungs(?:film|vorstellung)|""" +
    """sesión\s+(?:inaugural|de\s+clausura)|gala\s+(?:inaugural|de\s+inauguración|de\s+clausura)|película\s+sorpresa""").r
  /** A banner naming a festival, a season, a series or an edition — what a slot is billed under. */
  private val ProgrammeBanner =
    """(?iu)(?<!\p{L})(?:festival|festivals|fest|festiwal\p{L}*|filmfest\p{L}*|festspiele|season|sezon\p{L}*|series|ciclo|muestra|semana|week|tydzień|edition|edycja|edycji)(?!\p{L})""".r

  /** The PROGRAMME SLOT the billing names in place of a film — a festival's or season's banner and a slot of its
   *  programme, and nothing else ("Unrestricted View Horror Film Festival 2026: Opening Night"). `None` unless every
   *  other piece of the title is such a banner. */
  def programmeSlot(raw: String): Option[String] =
    // no banner, no slot: read for every node of every resolve, so the title is split only when one is billed
    if (!ProgrammeBanner.pattern.matcher(raw).find()) None
    else {
      val all = pieces(raw)
      all.find(piece => ProgrammeSlot.pattern.matcher(piece).matches()).filter(slot =>
        all.sizeIs >= 2 && all.forall(piece => (piece eq slot) || ProgrammeBanner.pattern.matcher(piece).find()))
    }

  // ── several works by shape ─────────────────────────────────────────────────────────────────

  /** A quoted title. */
  val Quoted: scala.util.matching.Regex = """[„"“]([^"”„]+)["”]""".r
  /** A set ("zestaw") of works. */
  private val SetOfWorks = """(?i)\bzestaw\b""".r
  /** Does the billing bill a set ("zestaw") of works? */
  def billsASet(raw: String): Boolean = SetOfWorks.findFirstIn(raw).isDefined

  /** Does the billing quote two works or more ("…: „Bestia”, reż. Hertz (1917), „Cud nad Wisłą”, …")? */
  def quotesSeveral(raw: String): Boolean = Quoted.findAllIn(raw).size >= 2
  /** [[quotesSeveral]] where two of the quoted works are dated ("(1917)", "(1921)"). */
  def quotesSeveralDated(raw: String): Boolean = quotesSeveral(raw) && DatedWork.findAllIn(raw).size >= 2
  private val DatedWork = """\((?:18|19|20)\d\d\)""".r
  /** Does the billing join two quoted works by a conjunction, the two closing it (PL "„Mieszkanie” i „Owoce rajskich drzew
   *  spożywamy”", 'MAF: "Obcy" oraz "Milcząca Przyjaciółka"')? */
  def quotesTwoJoined(raw: String): Boolean = JoinedQuotes.findFirstIn(raw).isDefined
  private val JoinedQuotes = """(?iu)[„"“][^"”„]+["”]\s*(?:i|oraz|und|and|&|et|y|e)\s+[„"“][^"”„]+["”]\W*$""".r

  /** Is the billing two segments a dash joins, and nothing else ("LÍDA BAAROVÁ – DOOMED BEAUTY")? */
  def dashedPair(raw: String): Boolean = {
    val breaks = SegmentBreak.findAllIn(raw).toSeq.map(_.trim)
    breaks.sizeIs == 1 && Dash.matches(breaks.head) && SegmentBreak.split(raw).count(_.trim.nonEmpty) == 2
  }
  private val Dash = "[-–—]".r

  /** Does a segment of the billing list three works or more by name, a comma between each, every one capitalised
   *  ("Retro - An Undiscovered World, Running High, Carrying Dreams: The Weight Between", a programme of shorts)? */
  def listsWorks(raw: String): Boolean = SegmentBreak.split(raw).exists { piece =>
    val works = piece.split(",").map(_.trim).toSeq
    works.sizeIs >= ListedWorks && works.forall(work => work.headOption.exists(_.isUpper) && TitleContainment.tokens(work).sizeIs >= 2)
  }
  private val ListedWorks = 3
}
