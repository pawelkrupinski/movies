package services.identity

import services.identity.measures.{Film, IdentityMeasures}
import services.titles.billing.{BillingPieces, DecorationWords}
import services.identity.measures.IdentityMeasures.withoutLeadingArticle
import services.titles.{TitleContainment, TitleWords}

/**
 * One billing's segments read against `films` for [[ListingShape.billedBeside]]: which of its whole pieces runs as one of
 * the films' titles, and which piece beside that run bills another thing. Each question the rule asks of the billing is a
 * method of its own; read once per call, never kept.
 *
 * `raw`: the billing, its certificate note off; `venues` and `bannerSlot` as [[ListingShape.billedBeside]] takes them.
 */
private[identity] final class BilledBeside(raw: String, listing: Listing, films: Seq[Film],
                                           venues: Seq[String] => Boolean, bannerSlot: Boolean) {
  import BilledBeside.*

  private val segments = BillingPieces.segments(raw)
  private val breaks   = segments.breaks
  private val words    = segments.parts.map(TitleContainment.tokens)
  private val titles   = films.flatMap(_.titles).map(t => withoutLeadingArticle(TitleContainment.tokens(t))).filter(_.nonEmpty).distinct
  // each title as its letters run together: a piece spelling it without its stops ("AI" of Spielberg's "A.I.") is it
  private val keys     = films.flatMap(_.titles).map(IdentityMeasures.key).filter(_.nonEmpty).toSet
  private val theirs   = films.flatMap(f => f.titles ++ f.directors.getOrElse(Nil)).flatMap(TitleContainment.tokens).toSet
  private val original = listing.originalTitle.map(t => withoutLeadingArticle(TitleContainment.tokens(t))).filter(_.nonEmpty)
  // a piece crediting the film's director, as an inflected language bills the name ("Poniedziałki z Konwickim: pisarz –
  // scenarzysta – reżyser"): the whole billing is the venue's programme of the director's films
  private val names    = films.flatMap(_.directors.getOrElse(Nil)).flatMap(TitleContainment.tokens).filter(_.length >= ListingShape.DirectorStem)
  /** The film's run of whole pieces, the first one: `(from, to)`, `to` exclusive. */
  private val run      = (for { from <- words.indices.iterator; to <- (from + 1 to words.size).iterator if runsAsATitle(from, to) }
                            yield (from, to)).nextOption()
  /** The banner segment each piece is in: a colon or a pipe closes one. */
  private val segment  = segments.parts.indices.map(k => breaks.take(k).count(closesABanner))

  /** The piece beside the film's run that bills another thing, said as why; `None` when there is none. */
  def piece: Option[String] = noted orElse run.filterNot(readsAsTheFilms).flatMap(besideTheRun)

  private def runsAsATitle(from: Int, to: Int) = {
    val run = words.slice(from, to).flatten
    titles.contains(withoutLeadingArticle(run)) || keys.contains(run.mkString)
  }
  private def outside(k: Int, from: Int, to: Int) = k < from || k >= to

  private def own(word: String) = word.length >= 3 && !word.exists(_.isDigit) && !theirs.exists(spells(word, _)) && !DecorationWords.billedAround(word)
  private def credits(word: String) = names.exists(word.startsWith)
  private def programme(piece: Seq[String]) = piece.exists(word => ScreeningProgramme.findFirstIn(word).isDefined)
  private def programmeWords(piece: Seq[String]) = programme(piece) || piece.exists(DecorationWords.billedAround) || DecorationWords.namesAnEvent(piece)
  // the film's own tagline, its subtitle ("Frauen bildet Banden - Eine Spurensuche zur Geschichte der Roten Zora")
  private def tagline(piece: Seq[String]) = piece.nonEmpty && films.exists(_.taglineKey == Film.taglineKeyOf(piece.mkString(" ")))

  // a film of the silent era under a screening programme: the billing is the programme's, its accompanist's (DE
  // "Film+Musikfest: Greta Garbo: The Temptress", 1926), as one billing it silent is
  private def silent = films.forall(_.year.exists(_ <= ListingShape.SilentEraEnd)) && words.exists(programme)

  // one of the film's own titles of three pieces or more, piece for piece, one piece re-worded: the venue's spelling of
  // that piece ("Exhibition on Screen: Vermeer - Die Blockbuster-Ausstellung" of its "Exhibition on Screen: Vermeer -
  // The Greatest Exhibition"); two pieces alike are no coincidence a namesake's two-piece title can be
  private def reworded = films.flatMap(_.titles).exists { title =>
    val theirs = BillingPieces.SegmentBreak.split(title).toIndexedSeq.map(piece => withoutLeadingArticle(TitleContainment.tokens(piece)))
    theirs.sizeIs >= 3 && theirs.size == words.size && theirs.indices.count(k => theirs(k) != withoutLeadingArticle(words(k))) == 1
  }

  // the billing opens with one of the film's own titles of two pieces or more, word for word, its last word maybe glued
  // to what follows ("LA BOUM - Die FeteZum 60. Geb. von Sophie Marceau!" of "La Boum - Die Fete"): the rest is a note
  private def opens = films.flatMap(_.titles).exists { title =>
    val theirs = TitleContainment.tokens(title)
    val ours   = words.flatten
    BillingPieces.SegmentBreak.split(title).count(_.trim.nonEmpty) >= 2 && theirs.nonEmpty && ours.sizeIs >= theirs.size &&
      ours.take(theirs.size - 1) == theirs.init && ours(theirs.size - 1).startsWith(theirs.last)
  }

  /** Is every film titled only by a screening note's words ("Q & A", "Talk")? */
  private def titledAsANote = films.forall { film =>
    val title = TitleContainment.tokens(film.title)
    title.nonEmpty && (title.forall(NoteWords) || NotePhrase.matches(title.mkString(" ")))
  }

  // a film titled only by a screening note's words: the billing's note, not a film, beside the billing's other pieces (UK
  // The Garden Cinema's "Köln Tracks: The Legend Of Keith Jarrett’s Köln Concert + Q&A")
  private def noted = run.filter { case (from, to) => titledAsANote && words.indices.exists(k => outside(k, from, to) && words(k).nonEmpty) }
    .map(_ => s"'${films.head.title}' is a screening note's words, billed beside the billing's own pieces")

  /** Is the whole billing the films' own wording, whatever is around the run: a silent film's programme, a title re-worded
   *  or opened, a piece crediting the director? */
  private def readsAsTheFilms(run: (Int, Int)) =
    silent || reworded || opens || words.indices.exists(k => outside(k, run._1, run._2) && words(k).exists(credits))

  private def besideTheRun(run: (Int, Int)): Option[String] = {
    val (from, to) = run
    aroundTheRun(from, to).map(words).find(billsAnotherThing)
      .map(piece => s"'${piece.mkString(" ")}' is billed beside '${films.head.title}', no word of it the venue's around a film")
  }

  /** The pieces around the run that are not the venue's slots: no strand's banner, no banner's slot, nothing a "+" adds
   *  after the film, no tag piped after it beside one naming a programme. */
  private def aroundTheRun(from: Int, to: Int): Iterator[Int] = {
    // the pieces piped after the film, one after another: the venue's tags, when one of them names a programme (PL
    // "Królestwo Kensuke | EKO-KINO | ZIELONE INSTYTUCJE KULTURY")
    val piped  = (to until words.size).takeWhile(k => breaks.lift(k - 1).contains("|"))
    val tagged = piped.exists(k => programme(words(k)))
    words.indices.iterator.filter(k => outside(k, from, to)).filter(k => words(k).nonEmpty).filterNot(strand(_, from))
      .filterNot(inTheBannerSlot(_, from))
      // what the screening adds after the film, a "+" closing it (PL "OPĘTANIE + TRASA PLAKATOWA", a poster tour)
      .filterNot(k => k >= to && breaks.lift(to - 1).contains("+"))
      .filterNot(k => tagged && piped.contains(k))
  }

  // a strand's banner: a segment closed ahead of the film's that names a screening programme or an event in one of its
  // pieces is the venue's, all of it (DE "SPECIAL: 2. Dangerous Encounters – Hong Kong im Filmrausch: MADE IN HONG KONG",
  // a numbered strand of the Filmrausch's)
  private def strand(k: Int, from: Int) =
    segment(k) < segment(from) && words.indices.exists(j => segment(j) == segment(k) && programmeWords(words(j)))

  // the banner's slot: the leading piece a colon or a pipe closes, the film after it
  private def inTheBannerSlot(k: Int, from: Int) = bannerSlot && k == 0 && from > 0 && breaks.headOption.exists(closesABanner)

  /** Does `piece` hold a word of its own, three letters at least, and none of the venue's wording around a film? */
  private def billsAnotherThing(piece: Seq[String]) =
    !original.contains(withoutLeadingArticle(piece)) && piece.exists(own) && !numberedNote(piece, own) &&
      !programmeWords(piece) && !piece.exists(ScreeningNote.matches) && !venues(piece) && !tagline(piece) && !StudioTags(piece.mkString(" "))
}

private object BilledBeside {
  private def closesABanner(break: String) = break == ":" || break == "|"

  /** An animation studio's name, as a venue tags its strand of the studio's films beside each title: DE Andreasstadel's
   *  "Kikis kleiner Lieferservice- Studio Ghibli" is Miyazaki's 1989 film, its studio no second thing billed beside it. */
  private val StudioTags: Set[String] = Set("studio ghibli", "ghibli", "pixar", "disney pixar", "dreamworks", "aardman")

  /** Is `word` the film's own `theirs`, or it misspelt by one letter dropped, added or changed — four letters and more
   *  ("Living - Einma wirklich leben" of "Living - Einmal wirklich leben")? */
  private def spells(word: String, theirs: String): Boolean =
    word == theirs || (word.length >= 4 && theirs.length >= 4 && math.abs(word.length - theirs.length) <= 1 && {
      val (short, long) = if (word.length <= theirs.length) (word, theirs) else (theirs, word)
      if (short.length == long.length) short.indices.count(i => short(i) != long(i)) <= 1
      else { val at = short.indices.find(i => short(i) != long(i)).getOrElse(short.length); short.substring(at) == long.substring(at + 1) }
    })

  /** A NOTE a number carries — a running time, a certificate, a subscription's or an anniversary's number — one word of
   *  its own beside the number at most: "129 mins", "Cert 18", "ABO 10", "50 Jahre". */
  private def numberedNote(piece: Seq[String], own: String => Boolean): Boolean =
    piece.exists(_.exists(_.isDigit)) && piece.count(own) <= 1

  /** The words of a screening's note — a talk, a Q&A, the screening as an event ([[TitleWords]]): a title of these alone
   *  names the note. */
  private val NoteWords = TitleWords.tokens(TitleWords.Concept.Talk, TitleWords.Concept.Screening)
  /** One whole phrase of a screening's note, folded — "Q & A", whose letters [[NoteWords]] holds no word of. */
  private val NotePhrase = TitleWords.foldedPattern(TitleWords.Concept.Talk, TitleWords.Concept.Screening).r
  /** A word naming a screening programme or its audience, as a stem a compound carries ("Filmmittwoch", "Kinderkino"), or
   *  a festival's ("SPECIAL: Jazzfest Community Week: SOUNDTRACK TO A COUP D’ETAT"). */
  private val ScreeningProgramme = """film|kin[oaie]|cine|movie|klub|club|dkf|kinder|kids|children|dzieci|familie|family|senior|fest$""".r
  /** A word of what a screening holds beside its film — a talk, a panel, a matinee, its audience's slot, its running time:
   *  UK "Judas and the Black Messiah + Panel Discussion", "The Incomer (12A) PARENT & BABY", "Wicker (15) 105 mins *MATINEE*", PL "Klątwa doliny węży – z autorską narracją
   *  Łony", "Zaproszenie | Zobacz przedpremierowo", DE "Live - Vortrag: Portugal - Der Wanderfilm", "BANKOMATT – Schweizer
   *  Filmwoche – mit Regisseur!". */
  private val ScreeningNote = """discussion|introduction|panel|matinee|parent|baby|relaxed|mins|minutes|narracj\w*|narration|przedpremier\w*|premierow\w*|deluxe|vortrag\w*|regisseur\w*""".r
}
