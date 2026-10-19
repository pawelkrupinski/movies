package services.cinemas.pl

import java.util.Locale

import models.CinemaMovie
import services.movies.{TitleContainment, TitleWords}

import scala.util.matching.Regex
/** Decides whether a scraped listing is a live STAGE/MUSIC event rather than a
 *  film. Small municipal & arthouse venues sell tickets to their own concerts,
 *  stand-up nights, kabaret shows, recitals and theatre plays through the same
 *  ticketing/listing surface their film repertoire comes from, so those rows
 *  leak into the scrape (e.g. "Koncert Joscho Stephan Trio", "Piotr Bałtroczyk
 *  Stand-up", "Edyta Geppert - recital", a tribute gig "…- pamięci Amy
 *  Winehouse", a Silesian "…biesiada z TVS", a gift "Bon podarunkowy", a blind
 *  "Seans w ciemno", or a festival "…– spotkanie z podróżnikiem…").
 *
 *  This is deliberately HIGH-PRECISION, not high-recall: a stray event slipping
 *  through is cosmetic, but dropping a real film is a regression. So two whole
 *  classes of cinema content that merely *contain* event vocabulary are kept:
 *
 *   - "Event cinema" BROADCASTS — opera/ballet/theatre transmissions and
 *     concert films shown on the screen (Royal Ballet & Opera season, National
 *     Theatre Live / "Teatr w Kinie", Met Opera, André Rieu, Pavarotti). These
 *     carry a broadcast marker ([[BroadcastMarkers]]) that vetoes the event
 *     verdict.
 *   - Art DOCUMENTARIES — the "Exhibition on Screen" / "Wielka Sztuka" series
 *     ("…Przełomowa wystawa", "Wielka Sztuka w Kinoteatrze Rialto - Hopper…").
 *     These never trip a marker in the first place: the event vocabulary here is
 *     `wystawa`/art words, which are NOT treated as event markers, and the
 *     venue's "Kino**teatr**ze" survives because [[EventMarkers]] anchors on a
 *     word boundary (`\bteatr`, not a bare substring).
 *
 *  Applied only by the clients that opt in — by mixing in [[OnlyMovieEventsFilter]]
 *  or calling [[isLiveEvent]] themselves; nothing applies it to every scrape
 *  (cf. the per-client filters in
 *  `KinoSfinksClient` / `CharlieMonroeClient`, which this complements rather
 *  than replaces — those discriminate on a structured category the venue
 *  exposes; this one works off the title for venues that expose none). */
object NonMovieEventClassifier {

  /** Mystery / blind screenings — sold as an event, never resolving to a title: "Seans w
   *  ciemno", "Seans niespodzianka" (collapsed "seanswciemno"), and Germany's "Sneak
   *  Preview" / "OV Sneak-Preview" / CineStar's "CineSneak". Not a bare "sneak": "Sneakers"
   *  and the 2025 "Sneaks" are films. Language-neutral enough to apply outside Poland, unlike
   *  the stage vocabulary below ("tribute" is a word of "Die Tribute von Panem"). The word table's sneaks
   *  ([[services.movies.TitleWords]]), read on the title's folded words. */
  private val MysteryScreeningMarkers = TitleWords.foldedPattern(TitleWords.Concept.Sneak).r

  /** Whether `title` is a mystery screening ([[MysteryScreeningMarkers]], or a title made of nothing but a
   *  sneak's own words — [[BareSneak]]). */
  def isMysteryScreening(title: String): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    hasMysteryMarker(title) || isBareSneak(t)
  }
  private def hasMysteryMarker(title: String): Boolean =
    MysteryScreeningMarkers.findFirstIn(TitleContainment.tokens(title).mkString(" ")).isDefined

  /** A sneak billed with no film: "Sneak", "Kult-Sneak", "Horror Sneak FSK 18", "OV-Sneak", "Sneak 1343", "Horror
   *  Classics Sneak Vol. 83", "Cult Sneak of Horror", "Grusel Sneak", "Cine Sneak", "SNEAK Extreme", "KLASSIKER SNEAK
   *  REVIEW - Special - Live" (German venues, prod 2026-10-12: 48 cards titled just "Sneak").
   *  Every word must be one a sneak bills itself with — its week ("Sneak KW 43 26") or month ("Sneak_nov", "Sneak
   *  Dezember"), or that it is an undisclosed "Original Sneak - Mystery Movie" / "OV Sneak: The surprise preview" —
   *  so a sneak that NAMES its film ("Hope Sneak") is that film's preview and stays, as do "Sneakers" and "Sneaks".
   *  A word may fuse a strand to the sneak ("Kultsneak", "CineSneak"), read as the two words it joins. */
  private val SneakWords = Set("sneak", "ov", "omu", "omeu", "kult", "cult", "horror", "classic", "classics",
    "klassiker", "retro", "fsk", "of", "vol", "preview", "die", "des", "jahres", "the", "kw", "original", "mystery",
    "movie", "surprise", "grusel", "cine", "extreme", "review", "special", "live",
    // …its night and its strand ("Friday Night Sneak", "Montags-Sneak", "Arthaus Sneak Vol. 43" — DE 2026-10-13).
    "night", "late", "nacht", "arthaus", "arthouse", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday",
    "sunday", "montag", "dienstag", "mittwoch", "donnerstag", "freitag", "samstag", "sonntag", "montags", "dienstags",
    "mittwochs", "donnerstags", "freitags", "samstags", "sonntags", "originals",
    // …and its month, in German and English, whole or cut short ("Sneak_nov" — Filmwerk Gütersloh, DE 2026-10-14).
    "januar", "january", "jan", "februar", "february", "feb", "märz", "maerz", "march", "mar", "april", "apr", "mai",
    "may", "juni", "june", "jun", "juli", "july", "jul", "august", "aug", "september", "sep", "sept", "oktober",
    "october", "okt", "oct", "november", "nov", "dezember", "december", "dez", "dec")
  private def isBareSneak(lower: String): Boolean = {
    val words = lower.split("""[^\p{L}\p{N}]+""").filter(_.nonEmpty).flatMap(unfuseSneak)
    words.contains("sneak") && words.forall(w => SneakWords.contains(w) || w.forall(_.isDigit))
  }
  /** "kultsneak" → kult, sneak: a word ending in "sneak" whose front is itself a sneak's word. */
  private def unfuseSneak(word: String): Seq[String] = {
    val front = word.stripSuffix("sneak")
    if (front.nonEmpty && front != word && SneakWords.contains(front)) Seq(front, "sneak") else Seq(word)
  }

  /** Non-screenings whose vocabulary no film title in ANY of our languages shares, so a feed in any language may
   *  read them — unlike [[EventMarkers]], whose Polish words ("tribute", "jazz", "teatr") are words of German and
   *  English film titles ("Die Tribute von Panem", "All that Jazz"). Found in the German feeds (Cineamo, kinoheld,
   *  Filmstarts — prod 2026-10-12), which sell a venue's readings, gigs, comedy nights and day passes beside its
   *  films:
   *   - performed: a concert BILLED as one ("Konzert der JazzKur BigBand", "KONZERT: Acoustic Rock Night" — a
   *     leading "Konzert", not "Konzert-Special: Ozzy & Black Sabbath", the concert film), cabaret, improv,
   *     comedy shows, karaoke, a church service, a tournament, a day/festival pass, a closed day;
   *   - talked: a reading, a lecture, a quiz, a slam — only standing alone, since a venue bolts them onto a film
   *     as often ("Becoming Kim + Filmgespräch", "Zeise Shortfilm Slam", "Saw (Horror-Filmnacht Special-Quiz)"),
   *     the same rule as [[isStandaloneDiscussion]];
   *   - billed: a venue's own stage act named by its performer, read only where the billing says so (DE recording
   *     of 2026-10-12) — a comedian's "neues Programm", a "LIVE:" night, a "Tour 2027", a trio, duo or "feat.", a
   *     magic show, a puppet theatre, a "Buchlesung", "Lesebühne" or "Multivisionsvortrag" (compounds end in the
   *     word). A name with nothing beside it ("Andy Strauß") stays: the title cannot tell an act from a film. */
  private val NeutralPerformanceMarkers = List(
    """(?:^|[:|]\s*)konzert(?:lesung)?\b(?!-)""".r,
    """(?:benefiz|jubiläums|advents|tango)konzert""".r,
    """konzerte?\s+mit\b""".r,
    """kabarett""".r,                       // "Musikkabarett", "Frauenkabarett": compounds too
    """\bimprotheater\b""".r,
    """\bimpro[\s-]?(?:show|festival)""".r,
    """\btheatersport\b""".r,
    """(?:kasperl|figuren|kinder|puppen|schatten|laienspiel-?)theater|puppenspiel|theaterveranstaltung""".r,
    """\bcomedy[\s-]*(?:show|night|slam|live|abend)\b""".r,
    // Stand-up and karaoke as the EVENT bills them — hyphenated or glued, or with its night: a bare "Stand Up" or
    // "Karaoke" is a film's own title ("Stand Up Guys", "Karaoke" (2022), a sing-along "Vaiana: wersja karaoke").
    """\bstand-?up\b|\bstand\s+up\s+comedy\b""".r,
    """\bkaraoke[\s-]*(?:live|night|nacht|abend|party|show)\b""".r,
    """^gottesdienst\b""".r,
    """andacht\b""".r,                      // "Musikalische Abendandacht"
    """turnier\b""".r,                      // "Tischkickerturnier": compounds end in it
    """\btages(?:ticket|karte)\b""".r,
    """\bfestival[\s-]?pass\b""".r,
    """\bruhetag\b""".r,
    // An act billed by its programme or its band rather than by a film:
    """\bneue[nms]?\s+programm""".r,        // "Philipp Weber mit seinem neuen Programm „ABMARSCH!…“"
    """\bjahresrückblick""".r,              // "Holger Paetz … der satirische Jahresrückblick"
    """^live:\s""".r,                       // "LIVE: SBB – SMALL BIG BAND" — not "Berliner Philharmoniker LIVE: …", a relay
    """\btribute[\s-]+(?:band|show|abend|by)\b""".r, // not "Die Tribute von Panem"
    """\bbig\s?band\b|houseband\b|\bfeat\.\s""".r, // not a bare "& Band": "Dieter Thomas Kuhn & Band" is a documentary
    // A trio or duo as a performer's band — the word after a name ("Hanne Kah Duo"), never the bare word nor an
    // article's: "Trio", "Duo" and "Das Trio" (1998) are films' own titles, as is a "Quartett" (2013).
    """(?<=\p{L}\s)(?<!\b(?:das|die|der|the|ein|a)\s)\b(?:trio|duo)\b""".r,
    """zauber(?:kunst|show|keller|ei\b)|travestie""".r
  )
  private val NeutralTalkMarkers = List(
    """lesung\b""".r,                       // "Buchlesung", "Märchenlesung", "Premierenlesung"
    """\bleseshow""".r,
    """lese(?:bühne|buehne)""".r,           // a reading stage: "LILI - Lesebühne im Lichtspielhaus", "Sax Royal die Lesebühne"
    """\bliest\s+(?:aus|was|vor|die|der|das|den)\b""".r, // "Melanie Raabe liest aus: Daisy"
    """vortrag\b""".r,                      // "Fachvortrag", "Multivisionsvortrag"
    """\bmultivision|live-reportage""".r,   // a travel photographer's slide show, presented live
    """\btour\s+20\d\d\b""".r,              // "Moritz Hohl: Musst du wissen – Tour 2027" (a "Kurz Film Tour 2026" names its films)
    // A workshop as the class bills itself — "WORKSHOP: SOUND DESIGN", a compound ("Schreibworkshop"), the plural, or
    // who it is for or with — never the bare word, a film's own: "The Workshop", "Workshop".
    """(?<=[\p{L}-])workshop|\bworkshops\b|^workshop\s*[:–-]|\bworkshop\s+(?:für|mit|for|with)\s""".r,
    // A quiz or a slam as the event bills it — a compound ("QuizKino Live", "Quizlabor Leipzig", "PubUp-Kino-Quiz"), a
    // quiz night, a poetry slam — never the bare word, a film's own: "Quiz Show" (1994, DE corpus 2026-10-12), "Slam";
    // nor the Spanish "quizá(s)", which only begins like one.
    """(?<=[\p{L}-])quiz|quiz(?=\p{L})(?!á)|\bquiz[\s-]+(?:night|nacht|abend)\b""".r,
    """\b(?:poetry|science|lyrik|dichter)[\s-]*slam\b|\bslam[\s-]+poetry\b""".r
  )
  private val FilmMention = """film""".r

  /** The broadcast veto without its bare " live": a comedy night bills itself "Comedy LIVE!", a karaoke night
   *  "Karaoke LIVE", and neither is a broadcast. The named broadcasts (Met, Royal Ballet & Opera, NT Live, André
   *  Rieu) still veto. Lazy: [[BroadcastMarkers]] is defined further down this object. */
  private lazy val NeutralBroadcastVeto = BroadcastMarkers.filterNot(_ == " live") :+ "live viewing"

  /** [[isScreenedBroadcast]] by a broadcast's NAME alone — its bare " live" aside, which a performer's own show bills
   *  too ("Jan Logemann LIVE: …"). */
  def isNamedBroadcast(title: String): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    NeutralBroadcastVeto.exists(t.contains)
  }

  /** Is `piece` — a delimited piece of a title — nothing but a broadcast's name or screening label ([[isNamedBroadcast]]'s
   *  vocabulary, whole: "LIVE VIEWING", "NT Live"), no work's? Words alone, case and punctuation aside: "The
   *  Metamorphosis" carries "the met" and is a work. */
  def isBroadcastLabel(piece: String): Boolean = {
    def words(s: String) = s.toLowerCase(Locale.ROOT).split("""[^\p{L}\p{N}]+""").filter(_.nonEmpty).toSeq
    val own = words(piece)
    own.nonEmpty && NeutralBroadcastVeto.exists(marker => words(marker) == own)
  }

  /** The listings box-office staff create to try their own site, billed as such: BFI Southbank's "Z Test Show
   *  please ignore" (2026-10-12), Spektrix's "Website Test Event 2". A test NAMED as a show, event or screening —
   *  not "Screen Test" or "The Test", which are films — or a plea not to book it. */
  private val TestListingMarkers = List(
    """\btest\s+(?:show|event|screening|performance)s?\b""".r,
    """\bplease\s+ignore\b|\bdo\s+not\s+book\b""".r
  )
  private def isTestListing(t: String): Boolean = TestListingMarkers.exists(_.findFirstIn(t).isDefined)

  /** Whether `title` is no screening at all, by what a feed in any language may read: a mystery screening, a box
   *  office's test listing, or a [[NeutralPerformanceMarkers]] / standalone [[NeutralTalkMarkers]] event — never a
   *  screened broadcast. What the feeds that cannot use the Polish stage vocabulary filter on (Cineamo, kinoheld,
   *  kinoprogramm.com, Webedia). */
  def isNonScreening(title: String): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    isMysteryScreening(title) || isTestListing(t) || (!isNamedBroadcast(t) && (
      NeutralPerformanceMarkers.exists(_.findFirstIn(t).isDefined) ||
      (NeutralTalkMarkers.exists(_.findFirstIn(t).isDefined) && !hasBoltedOnFilm(t) && FilmMention.findFirstIn(t).isEmpty)))
  }

  /** Word-anchored markers of a live stage/music event. Anchored on `\b` so
   *  `\bteatr` matches "Teatr Skene", "Teatralne popołudnie", "teatru capitol"
   *  but NOT "Kinoteatrze Rialto"; `spektakl` (not followed by a Polish letter)
   *  matches "spektakl komediowy", "Spektakl- Pomoc Domowa", "Spektakl_kubuś…"
   *  but NOT "Społeczeństwo spektaklu" (an inflected form — a real film).
   *  `\bgala\b` ignores "Galaxy"/"Galaktyki" (no boundary after "gala").
   *  `koncert`/`gala`/`balet` are Polish-only spellings, so the English
   *  "Royal Ballet" / "…Opera" of the kept broadcasts never match. */
  private val EventMarkers = List(
    """\bstand[\s-]?up\b""".r,
    """\bkabaret""".r,
    """\brecital""".r,
    """\bspektakl(?![a-ząćęłńóśźż])""".r,  // nominative "spektakl", not "spektakl-u/-em"
    """\boperetk""".r,    // operetka / operetki
    """\bkoncert""".r,    // koncert / koncertu
    """\bteatr""".r,      // teatr / teatru / teatrem / teatralne (NOT kinoteatr…)
    """\bbalet""".r,      // baletowa (PL); English "Ballet" stays
    // Music recitals/tributes beyond a bare "koncert". `symfoniczn` matches the
    // adverb of a pops-symphonic gig ("…symfonicznie") but NOT the real silent
    // film "Nosferatu - symfonia grozy" (no "-czn"); `\btribute`/`\bpamięci`
    // ("…- pamięci Amy Winehouse") tag memorial gigs; `\bjazz`/`\borkiestr`/
    // `\bfilharmoni` are stage-music venues/ensembles. `na żywo` = a live music
    // performance (broadcasts say "transmisja/retransmisja na żywo" → vetoed).
    """symfoniczn""".r,
    """\btribute\b""".r,
    """\bpamięci\b""".r,
    """\bjazz\b""".r,
    """\borkiestr""".r,
    """\bfilharmoni""".r,
    """na żywo""".r,
    // Silesian folk/variety shows. `\bszlagier` covers "Szlagierowa uczta…" and
    // "Szlagierowy zawrót głowy" without a bare `uczta`/`zawrót głowy` that would
    // wrongly drop the films "Uczta Babette" / "Zawrót głowy" (PL Vertigo).
    """\bbiesiad""".r,
    """\bszlagier""".r,
    // Gift vouchers sold through the same ticketing surface ("Bon podarunkowy",
    // collapsed "Bonpodarunkowy"). `\s?` absorbs the lost space.
    """bon\s?podarunkow""".r,
    """bon\s?upominkow""".r,
    """\bvoucher""".r,
    // Activity classes sold through the same ticketing surface — a yoga session
    // ("Joga w chmurach – Studio Jogi…") is never a screening. Covers "joga",
    // "jogi", "jogą", "jogę" without touching any film title; closed by a
    // lookahead rather than `\b`, since Java's ASCII-only `\b` sees no boundary
    // after "ą"/"ę" (the same trap `spektakl` above avoids).
    """\bjog[aiąę](?![\p{L}\d])""".r,
    // Silent-disco nights ("Zamkowe Silent Disco"). Deliberately NOT a bare
    // `disco`, which would drop the film "Disco Polo".
    """silent\s?disco""".r
  )

  /** Festival panels / author meetings ("…– spotkanie z podróżnikiem…", "panel
   *  dyskusyjny", "…– debata z udziałem gości…"). Unlike the markers above these
   *  ALSO appear as an addendum to a real film screening — "The Room + spotkanie
   *  z Gregiem Sestero", "Salto - pokaz + prelekcja", "…+ Q&A z…" — so they only
   *  flag an event when NO film-attachment signal ([[FilmAttachmentSignals]]) is
   *  present. A standalone "spotkanie z X" with no film is dropped; a film with a
   *  bolted-on talk is kept. */
  private val DiscussionMarkers = List(
    """spotkanie ze?\b""".r,
    """panel\s?dyskusyjn""".r,
    """\bdebat""".r,
    """\bprelekcj""".r,
    """\bdyskusj""".r,
    """q&a""".r
  )

  /** Substrings that mark a real film the discussion is bolted onto: a `+`/`|`
   *  joining film and extra (or the spelled-out " plus " of "Pollock Plus Q&A with
   *  Carol Cruickshanks"), or a screening word, Polish or English ("…AND
   *  post-screening Q&A with writers/directors"). Their presence vetoes the
   *  [[DiscussionMarkers]] verdict (but NOT the concert/folk markers above —
   *  a "Koncert + support" is still an event). */
  private val FilmAttachmentSignals = List("+", "|", " plus ", "pokaz", "seans", "przedpremiera", "screening")
  private def hasBoltedOnFilm(t: String): Boolean = FilmAttachmentSignals.exists(t.contains)
  private def isStandaloneDiscussion(t: String): Boolean =
    DiscussionMarkers.exists(_.findFirstIn(t).isDefined) && !hasBoltedOnFilm(t)

  /** Workshops sold through the film ticketing ("Warsztaty ceramiczne listopad/ grudzień 2026" at Kino w Ratuszu,
   *  which the resolver matched to the film "Grudzień" by its month) — the PLURAL only, as they are sold: a film may be
   *  called "Warsztat". Like a talk, a workshop is often a screening's companion ("Akademia Pana Kleksa - warsztaty i
   *  film", "Filmy non-camerowe + warsztaty filmowe"), so it is an event only with no film attached — no
   *  [[FilmAttachmentSignals]] and no "film" in the title. */
  private val WorkshopMarker = """\bwarsztat(?:y|ów|ach|ami)(?![\p{L}\d])""".r
  private val FilmWord       = """\bfilm""".r
  private def isStandaloneWorkshop(t: String): Boolean =
    WorkshopMarker.findFirstIn(t).isDefined && !hasBoltedOnFilm(t) && FilmWord.findFirstIn(t).isEmpty

  /** A magic show ("Pokaz magii dla dzieci", "Wieczór iluzji"), named by what
   *  it shows. Deliberately not a bare `magii`/`iluzj`: "Drzewo magii" and
   *  "Iluzja" are films, and Warsaw's Kino Iluzjon names itself in titles. */
  private val MagicShowMarkers = List(
    """\bpokaz\s+(magii|iluzji)""".r,
    """\bwiecz[oó]r\s+(magii|iluzji)""".r,
    """\bmagic\s+show""".r
  )

  /** A magic show named by its PERFORMER: "Iluzjonista Adrian Mitoraj". The
   *  job word alone is a film ("Iluzjonista", 2006), so it counts only when a
   *  capitalised first name and surname follow — matched on the title as
   *  published, since lowercasing erases exactly that cue. */
  private val PerformerBilling =
    """(?:\b[Ii]luzjonist\p{L}*|\bILUZJONIST\p{L}*|\b[Mm]agik|\bMAGIK)\s+\p{Lu}\p{L}+\s+\p{Lu}\p{L}+""".r
  private def isMagicShow(title: String, lower: String): Boolean =
    MagicShowMarkers.exists(_.findFirstIn(lower).isDefined) || PerformerBilling.findFirstIn(title).isDefined

  /** "gala" is handled separately from [[EventMarkers]]: it marks a live event
   *  (Gala Baletowa, Światowa gala muzyczna, Gala Rozdania Nagród) EXCEPT a film
   *  screened under a gala banner, which uses a "Banner | Film" title — e.g.
   *  "Gala Wręczenia Nagrody Wolności | Pieśni lasu" (a real documentary). The
   *  pipe distinguishes the two; `\bgala\b` also ignores "Galaxy"/"Galaktyki". */
  private val GalaWord = """\bgala\b""".r
  private def isStandaloneGala(t: String): Boolean =
    GalaWord.findFirstIn(t).isDefined && !t.contains("|")

  /** Markers that this is screened cinema content (a broadcast or a film
   *  screening), which VETO an event verdict even when an event marker is
   *  present — "André Rieu … letni koncert", "…| TEATR W KINIE | National
   *  Theatre Live", "Pavarotti … Koncert gwiazd z Arena di Verona". */
  private val BroadcastMarkers = List(
    "andre rieu", "andré rieu",
    "pavarotti", "arena di verona",
    "na ekranie",                       // "Sztuka na ekranie - Koncert…"
    "retransmisja", "transmisja",
    "national theatre", "teatr w kinie", "nt live", " live",
    "royal opera", "royal ballet", "sezon kinowy",
    "met opera", "the met",
    "w kinie",                          // "André Rieu W KINIE…"
    "pokaz filmu"                       // "Pokaz filmu „…" i koncert
  )

  /** True when `title` carries a marker of screened "event cinema" — an
   *  opera/ballet/theatre transmission or concert film shown ON the screen
   *  (André Rieu, NT Live, Met Opera, a `retransmisja`). This is legitimate
   *  cinema content that must be KEPT even when other signals (an event-y title,
   *  or a venue's `MusicEvent`/`TheaterEvent` structured type) would otherwise
   *  flag it as a live event — see [[BiletynaClient]], which filters on the
   *  schema.org `@type` and reuses this as its veto. */
  def isScreenedBroadcast(title: String): Boolean =
    BroadcastMarkers.exists(title.toLowerCase(Locale.ROOT).contains)

  /** True when `title` names a live stage/music event rather than a film — or a box office's test listing,
   *  which is no film either.
   *
   *  `venueMarkers` lets ONE venue add its own programme's names — a castle's
   *  terrace season, a club night — without those names becoming national rules.
   *  They join the shared markers here rather than being applied by the caller so
   *  that the surrounding rule holds for them too: the broadcast veto still wins,
   *  and matching is still done on the lowercased title. See
   *  `OnlyMovieEventsFilter.venueEventMarkers` for how a client supplies them. */
  def isLiveEvent(title: String, venueMarkers: Seq[Regex] = Nil): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    isPerformance(title, venueMarkers) || isTestListing(t) ||
      (!isScreenedBroadcast(t) && (isStandaloneDiscussion(t) || isStandaloneWorkshop(t)))
  }

  /** [[isLiveEvent]] by what is PERFORMED alone — a concert, a stage play, a gala, a magic show, a voucher — without the
   *  talks and workshops, which a venue bills as a film's companion under the film's own title as often as on their own
   *  ("SKRZYŻOWANIE - 50 na 51 - spotkanie z Janem Englertem" is the film, its guest beside it). What the identity
   *  model's non-film events read ([[services.identity.agreement.NonFilmEvents]]), where skipping a film costs its take. */
  def isPerformance(title: String, venueMarkers: Seq[Regex] = Nil): Boolean = {
    val t = title.toLowerCase(Locale.ROOT)
    !isScreenedBroadcast(t) && (
      hasMysteryMarker(title) ||
      EventMarkers.exists(_.findFirstIn(t).isDefined) ||
      venueMarkers.exists(_.findFirstIn(t).isDefined) ||
      isStandaloneGala(t) ||
      isMagicShow(title, t))
  }

  /** [[isLiveEvent]] on a whole listing: a listing that states a film's record —
   *  a director credit AND a production year — is a film whatever its title
   *  says. The title alone can't tell Fosse's "Kabaret" (1972) or "Koncert"
   *  (2009) from a cabaret night or a gig, since the event vocabulary IS the
   *  film's name. Either half alone is not enough: a theatre play credits its
   *  director, and an event row can carry a catalogue year. Every client that
   *  filters a built listing goes through this, so the veto holds for all. */
  def isLiveEvent(listing: CinemaMovie, venueMarkers: Seq[Regex]): Boolean =
    !carriesFilmRecord(listing) && isLiveEvent(listing.movie.title, venueMarkers)

  def isLiveEvent(listing: CinemaMovie): Boolean = isLiveEvent(listing, Nil)

  private def carriesFilmRecord(listing: CinemaMovie): Boolean =
    listing.director.exists(_.trim.nonEmpty) && listing.movie.releaseYear.isDefined
}
