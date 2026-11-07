package services.titlerules

/** Hardcoded, reusable regex snippets that rule patterns reference as
 *  `{{NAME}}`. A rule writes `{{SEP}}` instead of spelling out a separator
 *  class, so the separator language is defined in ONE place and generalising it
 *  reshapes every rule that uses it. Expanded into each rule's pattern by
 *  [[PlaceholderExpander]] before the regex compiles (see
 *  [[TitleRuleSet.effectiveRules]]).
 *
 *  These are part of the rule language, not editable data — change them here and
 *  redeploy. The admin editor shows [[defined]] as a read-only, documented
 *  reference and expands the tokens when validating a pattern, but can't edit
 *  them. */
object TitleRulePlaceholders {

  /** The banner-separator characters: colon, pipe, slash, underscore, backslash,
   *  en-dash, em-dash, hyphen. (Hyphen last so it's literal in the class.) */
  private val SepChars = """:|/_\\–—-"""

  /** Like [[SepChars]] but without `_` — the guard set. A banner-name guard must
   *  eat an underscore-glued name ("„GAG"_SENIOR") yet stop at a hard separator. */
  private val GuardSepChars = """:|/\\–—-"""

  private val Certificate = services.movies.BbfcCertificate.alternation(
    services.movies.BbfcCertificate.Public :+ services.movies.BbfcCertificate.Pending)
  private val CertificateNote =
    s"""(?:[-–—]\\s*Cert(?:ificate)?\\.?\\s+$Certificate|\\(\\s*(?:Cert(?:ificate)?\\.?\\s*)?$Certificate(?:\\s+(?:TBC|CTBA))?\\s*\\)(?:\\s+CTBA)?)"""
  private val RuntimeMinutes = """\d{2,3}\s*mins?\.?(?:\s+TBC)?"""
  private val AsteriskNote   = """\*\s*\p{L}[\p{L}\s'&-]*\*"""

  /** A special audience a screening is billed for, as a venue words it — each one no film's title ends on, so it
   *  is a note even with no separator before it: the word table's audiences ([[services.movies.TitleWords]]), in every
   *  language a country bills in ("Relaxed Screening", "Sesión relajada", "Kinderwagenkino"). */
  private val BareAudience = services.movies.TitleWords.rawPattern(services.movies.TitleWords.Concept.Audience)
  /** A special audience a strand bills ahead of or after the film: any "<who> friendly" (dementia, autism, baby, sensory,
   *  family, dog…), a parent-and-baby or seniors' strand, and SEND (special educational needs) as a banner, a colon after
   *  it — never the word "send" a title opens on ("Send Help"). */
  private val AudienceStrand = """(?:Parents?\s*(?:&|and)\s*Bab(?:y|ies)|\p{L}+[-\s]Friendly|Silver\s+Screen|send(?=\s*:))"""
  /** What names a screening note outright: an audience (the seniors' "Silver Screen" too, a film's title only once
   *  set off), a Q&A, a restored print. */
  private val NoteMarker = s"""(?:$BareAudience|silver\\s+screen|q\\s*&\\s*a|4k\\s+(?:restoration|remaster(?:ed)?))"""
  /** Every word a screening note is made of — its markers (an audience whole, in any language) and the words a venue
   *  strings them together with. */
  private val NoteWord =
    s"""(?:$BareAudience|""" + """relaxed|dementia|autism|friendly|parents?|bab(?:y|ies)|toddlers?|silver|screen\p{L}*|club|q\s*&\s*a|""" +
      """intro(?:duction)?|panel|live|subtitled|dubbed|dub|subs?|english|captioned|captions?|4k|35mm|70mm|""" +
      """restoration|remaster(?:ed)?|with|and|plus|&|\+)"""
  /** A run of nothing but note words that names at least one note. */
  private val NoteSegment = s"""(?=(?:$NoteWord[\\s,/]*)*?$NoteMarker)$NoteWord(?:[\\s,/]+$NoteWord)*(?![\\p{L}\\p{N}])"""

  /** A Q&A as a venue bills it after the film, who gives it named or not: "Q&A", "a Live Q&A", "Director Q&A",
   *  "Writer/Director Q&A", "Cast & Crew Q&A". */
  private val GiverRole = """(?:writer|director|filmmaker|producer|actor|actress|cast|crew|guest)s?"""
  private val QaNight =
    s"""(?:an?\\s+)?(?:(?:live|in[-\\s]person|recorded|special|$GiverRole(?:['’]s)?(?:\\s*(?:/|&|and)\\s*$GiverRole)?)\\s+)*""" +
      """Q\s*&\s*A(?![\p{L}\p{N}])"""

  /** The touring "Federico Fellini: ciao a tutti!" retrospective (~12 cinemas) brands its films in many shapes — a
   *  "Federico Fellini:" or bare "Fellini." prefix, an optional "ciao a tutti!" / "Ciao a tutti:" subtitle, and a
   *  "| / – przegląd Federico Fellini …" suffix. The bare colon form ("Fellini: …") is deliberately NOT matched (only
   *  "Federico Fellini:" or the dot "Fellini."), so a real film whose title merely starts "Fellini:" survives. The
   *  separator-less "Federico Fellini <FILM>" form (KinoMuza's "Federico Fellini SŁODKIE ŻYCIE") is matched too, but
   *  only with a `(?=\s+\S)` look-ahead so a bare "Federico Fellini" (the standalone documentary) is left intact, not
   *  amputated to an empty key. */
  private val FelliniPrefix =
    """(?iu)^(?:Federico\s+Fellini\s*:|Federico\s+Fellini(?=\s+\S)|Fellini\s*\.)\s*(?:ciao\s+a?\s*tutti\s*!?)?\s*[:\-–—]?\s*"""
  private val FelliniSuffix =
    """(?i)\s*(?:\|\s*|[–—-]\s*przegl[ąa]d\s+)Federico\s+Fellini\b.*$"""

  /** A festival or film club's ACRONYM tag on its screening of an ordinary film, generic in the acronym. What keeps
   *  each one honest:
   *   - prefix: an edition ORDINAL, then 2–6 CAPITALS (case-sensitive) and a separator straight after them, so "1. FC
   *     Köln - Der Film" (the name runs on) and "32. MFSP A PART - Trixie" stay whole;
   *   - year prefix: an edition YEAR in place of the ordinal and no separator ("2026 LHFF Scream (1996)"), so the
   *     acronym must be 2–6 CAPITALS standing alone and a lower-case letter must follow — "2001 A Space Odyssey" (one
   *     letter), "2001 Maniacs" (a word), "2001 MANIACS" (seven capitals) and an all-capitals listing like "1776 THE
   *     MUSICAL" (nothing says a title follows) stay whole;
   *   - suffix: 3–6 CAPITALS ending the title after a pipe, with a lower-case letter BEFORE the pipe — in "KINO SENIORA
   *     | LALKA" the capitals after the pipe are the film — and never a signed, subtitled or language-version tag (PJM,
   *     SDH, ENG, UKR, ORG): that screening is its own audience's card. Screen-format tags ("| IMAX", "| DUB") are
   *     `FormatTags`' and are peeled at ingest before the rules see the title. */
  private val NumberedFestivalTag  = """^\d{1,3}\.\s*\p{Lu}{2,6}\s*[-–—:]\s*"""
  private val YearFestivalTag      = """^(?:19|20)\d{2}\s+\p{Lu}{2,6}\s+(?=.*\p{Ll})"""
  private val FestivalTagAfterPipe = """\s*\|\s*(?!(?:PJM|SDH|ENG|UKR|ORG)\s*$)\p{Lu}{3,6}\s*$"""

  /** A placeholder: the `name` referenced as `{{name}}`, its regex `expansion`,
   *  and a human `doc` line shown in the admin editor's reference panel. */
  final case class Placeholder(name: String, expansion: String, doc: String)

  /** Every placeholder, in display order. [[all]] is derived from this. */
  val defined: Seq[Placeholder] = Seq(
    Placeholder("SEP", s"""\\s*[$SepChars]\\s*""",
      "A banner separator — colon, pipe, hyphen, en/em dash, slash, underscore or backslash, " +
      "with optional surrounding whitespace. Use it wherever a rule separates a banner from the " +
      "film title; one token matches every separator the old hand-spelled classes did, so a rule " +
      "written once works for ': ', ' | ', ' - ', '–', '/', '_' alike."),
    Placeholder("SEPD", s"""\\s*[.$SepChars]\\s*""",
      "Like SEP, but also matches a period. For the few suffixes that historically allowed a '.' " +
      "delimiter — anniversary / restored / 'wersja' / the przedpremiera prefix " +
      "('przedpremiera. Film')."),
    Placeholder("NSEP", s"""[^$GuardSepChars]""",
      "One character that is NOT a separator — the building block for a banner-name guard like " +
      "^DKF\\s+{{NSEP}}+{{SEP}} (\"DKF <name>: <film>\"), so the guard stops at the first separator " +
      "instead of backtracking across one into the film title. Narrower than SEP: it excludes the " +
      "hard separators but keeps '_', so an underscore-glued cycle name is still consumed."),
    Placeholder("VENUE", """(?-i:\p{Lu})\p{L}*""",
      "A venue's or a place's name, one word — 'Kadr', 'Muzie', 'KINOTEKA' — for a strand billed under any venue's name. " +
      "Its first letter is a capital even under the rule's (?i), where a bare \\p{Lu} matches lower case too and the " +
      "rule took any word for the venue ('Film: kino moralne' searched as 'Film')."),
    Placeholder("CERT", Certificate,
      "A BBFC certificate as a UK box office bills it in a title — U, PG, 12, 12A, 15, 18, or TBC while it " +
      "awaits one. The bare value only: each rule writes the bracket or the 'Cert' around it. FormatTags " +
      "peels a trailing one off every listing with the same value set."),
    // The notes a box office bills behind the title. FormatTags peels each off every listing at ingest; the search
    // strips in ExtraTitleRules take the same shape off a query where another strip EXPOSED it (the identity model's
    // search title runs no re-peel). One spelling each, so the two never disagree on where a note ends.
    Placeholder("CERT_NOTE", CertificateNote,
      "A BBFC certificate note as a box office bills it — '- Cert 12A', '- Certificate 15', '(15)', '( 15 )', " +
      "'(Cert. TBC)', '(12A TBC)', Watershed's '(18 CTBA)' / '(18) CTBA' (certificate to be announced). No leading space and no anchor: a rule writes its own context around it. The " +
      "shape FormatTags' certificate peel reads."),
    Placeholder("RUNTIME_MINS", RuntimeMinutes,
      "A box office's running time — '100 mins', '95 min.', '160 mins TBC' — without its brackets: FormatTags " +
      "peels only a bracketed one (or one behind a certificate), the search strip a bare one too."),
    Placeholder("ASTERISK_NOTE", AsteriskNote,
      "A showing's note between asterisks — '*MATINEE*', '*SUBTITLED MATINEE*': letters only, so a title's own " +
      "asterisks ('M*A*S*H', 'Monet*', 'B****') never read as one. The shape FormatTags' asterisk peel reads."),
    Placeholder("AUDIENCE", BareAudience,
      "A special audience a screening is billed for, in every language a country bills in — the word table's " +
      "audience phrases (title-words.tsv), each one no film's title ends on."),
    Placeholder("AUDIENCE_STRAND", AudienceStrand,
      "A special-audience strand billed beside the film: '<who> Friendly', 'Parent & Baby', 'Silver Screen', 'SEND:'."),
    Placeholder("NOTE_SEGMENT", NoteSegment,
      "A run of nothing but screening-note words (audiences, Q&A, intro, subtitled, 4K, 35mm…) naming at least one note."),
    Placeholder("QA_NIGHT", QaNight,
      "A Q&A as a venue bills it after the film, who gives it named or not — 'Q&A', 'a Live Q&A', 'Director Q&A', " +
      "'Writer/Director Q&A', 'Cast & Crew Q&A'. No connector: each rule writes the '+', 'plus' or 'with' before it."),
    Placeholder("FELLINI_PREFIX", FelliniPrefix,
      "The touring 'Federico Fellini: ciao a tutti!' retrospective's prefix ('Federico Fellini:', 'Fellini.', a bare " +
      "'Federico Fellini <FILM>'), whole: a rule pattern of its own, in the lookup and merge-key tiers alike."),
    Placeholder("FELLINI_SUFFIX", FelliniSuffix,
      "The Fellini retrospective's suffix ('| Federico Fellini …', '– przegląd Federico Fellini …'), whole."),
    Placeholder("FESTIVAL_TAG_NUMBERED", NumberedFestivalTag,
      "A numbered festival or club acronym before the film ('11. UFF - ', '19. FGA: '), from the start."),
    Placeholder("FESTIVAL_TAG_YEAR", YearFestivalTag,
      "A year-numbered festival acronym before the film ('2026 LHFF '), from the start, a lower-case letter after it."),
    Placeholder("FESTIVAL_TAG_AFTER_PIPE", FestivalTagAfterPipe,
      "A festival or club acronym after a pipe closing the title ('| UFF', '| DKF'), never PJM/SDH/ENG/UKR/ORG.")
  )

  /** name → expansion, consumed by [[PlaceholderExpander]] / [[TitleRuleSet]]. */
  val all: Map[String, String] = defined.map(p => p.name -> p.expansion).toMap
}
