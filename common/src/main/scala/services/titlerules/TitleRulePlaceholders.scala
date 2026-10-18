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
      "asterisks ('M*A*S*H', 'Monet*', 'B****') never read as one. The shape FormatTags' asterisk peel reads.")
  )

  /** name → expansion, consumed by [[PlaceholderExpander]] / [[TitleRuleSet]]. */
  val all: Map[String, String] = defined.map(p => p.name -> p.expansion).toMap
}
