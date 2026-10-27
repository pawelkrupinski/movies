package services.identity

import services.movies.ListingKey

/**
 * One thing a decision depends on, as its RECEIPT names it: every external read is a [[Question]], read in one of the
 * ways the decision reads it — a family's answer, whether the question memory holds an attempt of it, a lookup through
 * the venues' store or TMDB's lookups (a film's record read for its facts, its release day, its cast or its
 * description) — and the one input no question answers, the year. A receipt is `Map[Dependency, Long]`: each dependency
 * with the digest of what the decision read of it, nothing there yet too (docs/design/identity-no-drift.md).
 *
 * It names its dependency only: what the outcome MEANS is the reader's. The one codec ([[Dependency.encode]],
 * [[Dependency.parse]]) is what a receipt is stored and fed by — total both ways, a stored id no dependency has kept as
 * [[Dependency.Unparsed]], which never stands.
 */
enum Dependency {
  /** A question's answer, as the store filing it holds it: a family's question ([[Question.Family]]). */
  case Answer(question: Question.Family)
  /** Whether the question memory holds an attempt of `question` (the settle rule reads it: an attempt is evidence of no
   *  answer). */
  case Attempted(question: Question)
  /** `question` as a lookup through `via` reads it: a search ([[Question.Candidate]]), a venue's detail page
   *  ([[Question.VenuePage]]), or one `aspect` of a film's TMDB record ([[Question.FilmRecord]]). */
  case Lookup(via: Dependency.Via, aspect: Dependency.Aspect, question: Question)
  /** A question asked, read again when its answer is filed — a correction's posters, finds and family questions. */
  case Asked(question: Question)
  /** The year a release is read as current by. */
  case Year
  /** A stored id no dependency has (a receipt from before its form): read as moved, so what holds it never stands. */
  case Unparsed(id: String)

  /** The id a filing of what this dependency reads comes on the answers' changes as: a family's answer and a question
   *  asked by the question's own id ([[Question.id]]), a TMDB record (by any lookup of it) as its record's id, any TMDB
   *  search as [[Dependency.QueryFiled]]. What nothing files is named by its encoding, which no filing matches. */
  def filing: String = this match {
    case Answer(question) => question.id.get
    case Asked(question)  => question.id.getOrElse(Dependency.encode(this))
    case Lookup(_, _, Question.FilmRecord(film)) => Question.Record(film).id.get
    case Lookup(_, _, Question.Candidate(_))     => Dependency.QueryFiled
    case _                                       => Dependency.encode(this)
  }

  /** Is it a read of TMDB's that a filing of a record or a search comes as ([[filing]])? */
  def readsTmdb: Boolean = this match {
    case Lookup(_, _, Question.FilmRecord(_) | Question.Candidate(_)) => true
    case _                                                           => false
  }
}

object Dependency {
  /** Which lookups a [[Dependency.Lookup]] went through: the venues' store, or TMDB's (the store, else live). */
  enum Via(val label: String) {
    case Venues extends Via("venues")
    case Tmdb   extends Via("tmdb")
  }
  /** Which of a question's answers a lookup read: the answer itself (a search's hits, a detail page, a film's record), or
   *  a film record's release day, cast or description. */
  enum Aspect {
    case Answer, Release, Cast, Overview
  }

  /** THE digest of what a read of any dependency saw, recorded and compared alike: every kind is read as an answer — a
   *  lookup's or a family's as it came, an attempt as [[attemptRead]], the year as [[yearRead]], nothing there yet as
   *  `Unknown` — and digested by its text. One function, so the form a receipt records is the form a re-read compares
   *  (docs/design/identity-no-drift.md, "One dependency index": the year recorded as its bare text never read equal).
   *  64 bits, two MurmurHash3 seeds: stable across runs, as a stored digest must be. */
  def digest(answer: services.identity.Answer[?]): Long = {
    val text = answer.toString
    (scala.util.hashing.MurmurHash3.stringHash(text, 0x2f1d7a3b).toLong << 32) | (scala.util.hashing.MurmurHash3.stringHash(text, 0x6c8e9cf5).toLong & 0xffffffffL)
  }
  /** Whether a question was attempted, as an [[Attempted]] read reads it. */
  def attemptRead(attempted: Boolean): services.identity.Answer[String] = services.identity.Answer.Known(attempted.toString)
  /** The year, as a [[Year]] read reads it. */
  def yearRead(year: Int): services.identity.Answer[String] = services.identity.Answer.Known(year.toString)
  /** What a read of a question none answered digests to: an answer filed no longer matches it. */
  val Unanswered: Long = digest(services.identity.Answer.Unknown)

  /** What any TMDB search is filed as among the answers' changes: one id for all — a receipt that read any search is read
   *  again on a search's filing. */
  val QueryFiled = "tmdb|query"

  /** A search as a receipt names it: its sort key with the kind's NUL a colon, readable in a stored receipt. */
  def queryId(query: CandidateQuery): String = query.sortKey.replaceFirst("\u0000", ":")
  /** The search a [[queryId]] names. */
  def queryOf(id: String): Option[CandidateQuery] = CandidateQuery.fromSortKey(id.replaceFirst(":", "\u0000"))

  private val YearId    = "clock"
  private val TriedId   = "tried"
  private val AskedId   = "asked"
  private val Kinds     = Map("film" -> Aspect.Answer, "release" -> Aspect.Release, "cast" -> Aspect.Cast, "overview" -> Aspect.Overview)
  private val KindOf    = Kinds.map(_.swap)

  /** The one encoding of a dependency: a family's answer as its question's id (`<family>|<kind>|<text>`), an attempt as
   *  `tried|<question id>`, a question asked as `asked|<question id>`, a lookup as `<via>|<kind>|<argument>` (`detail`
   *  by the listing's stored key, `candidates` by [[queryId]], `film`/`release`/`cast`/`overview` by the TMDB id), the
   *  year as `clock`. The form receipts were stored under before they were typed, but a detail page's: by its listing's
   *  key, not its place among the cluster's. */
  def encode(dependency: Dependency): String = dependency match {
    case Answer(question)    => question.id.get
    case Attempted(question) => s"$TriedId|${question.id.getOrElse("?")}"
    case Asked(question)     => s"$AskedId|${question.id.getOrElse("?")}"
    case Year                => YearId
    case Unparsed(id)        => id
    case Lookup(via, aspect, question) => question match {
      case Question.VenuePage(listing) => s"${via.label}|detail|${ListingKey.serialised(listing)}"
      case Question.Candidate(query)   => s"${via.label}|candidates|${queryId(query)}"
      case Question.FilmRecord(film)   => s"${via.label}|${KindOf(aspect)}|$film"
      case other                       => s"${via.label}|?|${other.id.getOrElse("?")}"
    }
  }

  /** The dependency `id` encodes ([[encode]]); one it does not, [[Unparsed]]. */
  def parse(id: String): Dependency = id.split("\\|", 3) match {
    case Array(YearId) => Year
    case Array(TriedId, rest*)  => Question.parse(rest.mkString("|")).fold(Unparsed(id))(Attempted(_))
    case Array(AskedId, rest*)  => Question.parse(rest.mkString("|")).fold(Unparsed(id))(Asked(_))
    case Array(label, kind, argument) if Via.values.exists(_.label == label) =>
      val via = Via.values.find(_.label == label).get
      kind match {
        case "detail"     => ListingKey.parse(argument).fold(Unparsed(id))(key => Lookup(via, Aspect.Answer, Question.VenuePage(key)))
        case "candidates" => queryOf(argument).fold(Unparsed(id))(query => Lookup(via, Aspect.Answer, Question.Candidate(query)))
        case _            => (Kinds.get(kind), argument.toIntOption) match {
          case (Some(aspect), Some(film)) => Lookup(via, aspect, Question.FilmRecord(film))
          case _                          => Unparsed(id)
        }
      }
    case _ => Question.parse(id).collect { case family: Question.Family => Answer(family) }.getOrElse(Unparsed(id))
  }
}
