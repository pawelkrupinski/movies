package services.identity

import services.identity.TitleDecorations.{LastBillJoin, Learned, MinCandidateRemainders, Tail, TailRun}
import services.movies.TitleContainment

import scala.collection.mutable

/**
 * The OFFLINE learner of the venue decorations the resolver strips ([[TitleDecorations]]): what
 * `scripts.IdentityDecorationsLearn` learns into `identity-decorations.json` ([[learn]], [[accumulate]]), and the
 * candidates `integration.IdentityDecorationCandidates` and `scripts.DecorationDiscovery` measure before one is kept
 * ([[candidates]], [[aligned]]). The resolver reads only the artefact; nothing here runs in production.
 */
object TitleDecorationsLearner {

  /** "Recurs across films": two different films at the least. The smallest count that says it,
   *  not a tuned number — a run with one remainder is indistinguishable from a film's own title. */
  val MinFilms = 2

  /** "Recurs" for a run seen around ONE film: two venues carrying it. */
  val MinVenues = 2

  /** The remainders a [[Learned]] decoration keeps as its provenance. */
  val ProvenanceExamples = 5

  /** The longest candidate run proposed: "weekend seniora z kultura" is four words. */
  val CandidateRun = 5

  /** Each run seen (keyed by its side and words, or a tail's words) → the remainders it was seen around → the venues
   *  billing the decorated title. */
  private type Seen[K] = mutable.HashMap[K, Map[Seq[String], Set[String]]]

  private def note[K](seen: Seen[K], key: K, rest: Seq[String], venues: Set[String]): Unit =
    seen.updateWith(key)(m => Some(m.getOrElse(Map.empty).updatedWith(rest)(v => Some(v.getOrElse(Set.empty) ++ venues))))

  /** The two edge cuts of `tokens` at `k` words: the leading run and what follows it, the trailing run and what leads it. */
  private def edges(tokens: Seq[String], k: Int): Seq[(String, (Seq[String], Seq[String]))] =
    Seq("prefix" -> (tokens.take(k), tokens.drop(k)), "suffix" -> (tokens.takeRight(k), tokens.dropRight(k)))

  /** A run worth proposing beside `known`: not known on its side already, and some letter in it. */
  private def proposable(known: TitleDecorations)(side: String, run: Seq[String]): Boolean =
    !(if (side == "prefix") known.prefixes else known.suffixes)(run) && run.exists(_.exists(_.isLetter))

  /** A decoration as learned: `run` on `side`, seen around the remainders `byRest` (to the venues billing each), of which
   *  `films` are different films. */
  private def learned(side: String, run: Seq[String], byRest: Map[Seq[String], Set[String]], films: Set[Seq[String]]): Learned =
    Learned(side, run.mkString(" "), films.size, byRest.values.flatten.toSet.size, byRest.size,
      films.toSeq.map(_.mkString(" ")).sorted.take(ProvenanceExamples))

  /** Strongest first: most films, then the side and the text — the order every list here is in. */
  private def strongestFirst(decorations: Seq[Learned]): Seq[Learned] = decorations.sortBy(d => (-d.films, d.side, d.decoration))

  /** The decorations `listings` (each a venue and one of its titles) carry that no title of
   *  `recordTitles` does, strongest first (most films, then the text). A function of the two SETS:
   *  any order of either learns the same list. */
  def learn(listings: Iterable[(String, String)], recordTitles: Iterable[String],
            searches: Iterable[(String, Boolean)] = Nil): Seq[Learned] = {
    val byTokens: Map[Seq[String], Set[String]] = listings.iterator
      .map { case (venue, title) => TitleContainment.tokens(title) -> venue }.filter(_._1.nonEmpty).toSeq
      .groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    val seen: Seen[(String, Seq[String])] = mutable.HashMap.empty
    byTokens.foreach { case (tokens, venues) =>
      (1 until tokens.size).foreach { k =>
        edges(tokens, k).foreach { case (side, (run, rest)) => if (byTokens.contains(rest)) note(seen, (side, run), rest, venues) }
      }
    }
    val recordKeys = recordTitles.iterator.map(TitleContainment.tokens).filter(_.nonEmpty).toSet
    // A title's own searches answered empty, by its words: every spelling of them.
    val searchedEmpty: Set[Seq[String]] = searches.iterator.map { case (t, empty) => TitleContainment.tokens(t) -> empty }.toSeq
      .groupMapReduce(_._1)(_._2)(_ && _).collect { case (ws, true) if ws.nonEmpty => ws }.toSet
    /** One film only: its rest is a record's title, two venues carry it, the decorated title's own
     *  searches found nothing, and no record's title crosses from the rest into the run or starts
     *  the run where the rest ends — the title goes on with another film's ("Basia. Humor w paski
     *  mam + Kocia Szajka", "Toddler Club: Tabby McTat + Room on the Broom"): a programme. */
    def aroundOneFilm(side: String, run: Seq[String], byRest: Map[Seq[String], Set[String]], rest: Seq[String]): Boolean = {
      val decorated = if (side == "prefix") run ++ rest else rest ++ run
      val runsOn = (rest.size + 1 to decorated.size).exists(n => recordKeys(if (side == "prefix") decorated.takeRight(n) else decorated.take(n)))
      val nextFilm = (1 to run.size).exists(n => recordKeys(if (side == "prefix") run.takeRight(n) else run.take(n)))
      recordKeys(rest) && searchedEmpty(decorated) && byRest.values.flatten.toSet.size >= MinVenues && !runsOn && !nextFilm
    }
    val recurring = seen.toSeq.map { case (key, byRest) => key -> (byRest, distinctFilms(byRest.keySet)) }
      .filter { case ((side, run), (byRest, films)) =>
        films.size >= MinFilms || (films.size == 1 && aroundOneFilm(side, run, byRest, films.head)) }
    val recurringTails = tailsOf(listings, byTokens.keySet)
    val inRecords = carried(recordTitles, (recurring.map(_._1._2) ++ recurringTails.map(_._1)).toSet)
    val edgeRuns = recurring.collect { case ((side, run), (byRest, films)) if !inRecords(run) => learned(side, run, byRest, films) }
    val billed   = recurringTails.collect { case (run, (byFirst, films)) if !inRecords(run) => learned(Tail, run, byFirst, films) }
    strongestFirst(edgeRuns ++ billed)
  }

  /** CANDIDATE decorations — what [[learn]] cannot see yet: an edge run of up to [[CandidateRun]] words recurring around
   *  at least `minRemainders` different remainders ([[distinctFilms]]) where the remainders need NOT be another
   *  listing's whole title. A programme banner's films often play nowhere else ("Edukacja Młode Horyzonty" around
   *  "Binti", "Fritzi – przyjaźń bez granic"), and a venue's own format tag around films that venue alone bills
   *  ("2D PL LOLO"). No record title may carry the run (as [[learn]]), and `known` decorations are not proposed again.
   *  Never decorations by themselves: each is MEASURED (`scripts.IdentityDecorationCandidates`) and only those whose
   *  stripping takes more listings right and none wrong are kept. Strongest first, as [[learn]] orders. */
  def candidates(listings: Iterable[(String, String)], recordTitles: Iterable[String], known: TitleDecorations,
                 minRemainders: Int = MinCandidateRemainders): Seq[Learned] = {
    val seen: Seen[(String, Seq[String])] = mutable.HashMap.empty
    listings.iterator.map { case (venue, title) => TitleContainment.tokens(title) -> venue }.filter(_._1.sizeIs > 1).distinct.foreach { case (tokens, venue) =>
      (1 to math.min(CandidateRun, tokens.size - 1)).foreach { k =>
        edges(tokens, k).foreach { case (side, (run, rest)) => note(seen, (side, run), rest, Set(venue)) }
      }
    }
    val recurring = seen.iterator.filter { case ((side, run), byRest) => byRest.sizeIs >= minRemainders && proposable(known)(side, run) }
      .map { case (key, byRest) => key -> (byRest, distinctFilms(byRest.keySet)) }.filter(_._2._2.sizeIs >= minRemainders).toSeq
    val inRecords = carried(recordTitles, recurring.map(_._1._2).toSet)
    strongestFirst(recurring.collect { case ((side, run), (byRest, films)) if !inRecords(run) => learned(side, run, byRest, films) })
  }

  /** CANDIDATE decorations learned SUPERVISED, from listings the model already holds to be one film: within each of
   *  `clusters` (each listing's venue and title), a title that is another member's title plus an edge run ("Fritzi –
   *  przyjaźń bez granic Edukacja Młode Horyzonty" beside "Fritzi – przyjaźń bez granic") shows that run decorating
   *  the film. Runs seen so around at least `minFilms` different inner titles and not `known`, strongest first. Unlike
   *  [[learn]] and [[candidates]], a run a record title carries is proposed too: the model already holds both titles one
   *  film, so the run decorates these films whatever other films it names — "lektor", "dubbing", "pl", which films are
   *  titled too, and which every learned run so far was blocked on. The measure decides. A run INSIDE a title (an
   *  infix) is not stripped by [[TitleDecorations.strip]], so it is not proposed. */
  def aligned(clusters: Iterable[Seq[(String, String)]], known: TitleDecorations, minFilms: Int = MinFilms): Seq[Learned] = {
    val seen: Seen[(String, Seq[String])] = mutable.HashMap.empty
    clusters.foreach { members =>
      val titled = members.map { case (venue, title) => TitleContainment.tokens(title) -> venue }.filter(_._1.nonEmpty).distinct
      val plain  = titled.map(_._1).toSet
      titled.foreach { case (tokens, venue) =>
        (1 until math.min(tokens.size, CandidateRun + 1)).foreach { k =>
          edges(tokens, k).foreach { case (side, (run, rest)) => if (plain(rest)) note(seen, (side, run), rest, Set(venue)) }
        }
      }
    }
    val recurring = seen.toSeq.filter { case ((side, run), _) => proposable(known)(side, run) }
      .map { case (key, byRest) => key -> (byRest, distinctFilms(byRest.keySet)) }.filter(_._2._2.sizeIs >= minFilms)
    strongestFirst(recurring.map { case ((side, run), (byRest, films)) => learned(side, run, byRest, films) })
  }

  /** An EVENT TAIL: a run of up to `TailRun` words that STARTS what titles bill last after a spaced "+", behind at
   *  least [[MinFilms]] different works, each another listing's whole title — "spotkanie z" after "Punku" and "Kalafior
   *  przeznaczenia", "PJM" after "Lalka" and "Obcy". A second WORK is billed after one first work ("Basia… + Kocia
   *  Szajka", "We're Going on a Bear Hunt + The Tiger Who Came to Tea"), never a run beside many. Each run → the works
   *  it follows → the venues billing them, and those works counted as [[distinctFilms]]. */
  private def tailsOf(listings: Iterable[(String, String)], titles: Set[Seq[String]]): Seq[(Seq[String], (Map[Seq[String], Set[String]], Set[Seq[String]]))] = {
    val seen: Seen[Seq[String]] = mutable.HashMap.empty
    listings.foreach { case (venue, title) =>
      LastBillJoin.findFirstMatchIn(title).foreach { m =>
        val first = TitleContainment.tokens(title.substring(0, m.start))
        val piece = TitleContainment.tokens(title.substring(m.end))
        if (first.nonEmpty && titles(first)) (1 to math.min(piece.size, TailRun)).foreach(n => note(seen, piece.take(n), first, Set(venue)))
      }
    }
    seen.toSeq.map { case (run, byFirst) => run -> (byFirst, distinctFilms(byFirst.keySet)) }.filter(_._2._2.size >= MinFilms)
  }

  /** `learned` — this recording's decorations — with every `earlier` one it did not learn again, unless a title of
   *  `recordTitles` now carries it. A recording sees only the programmes billed that week: a banner learned around two
   *  films stays a banner after its season ends ("WAJDA: re-wizje"), and comes back with the next one. What both learned
   *  is this recording's; the list is ordered as [[learn]] orders it. */
  def accumulate(earlier: Seq[Learned], learned: Seq[Learned], recordTitles: Iterable[String]): Seq[Learned] = {
    val relearned = learned.map(d => (d.side, d.decoration)).toSet
    val kept      = earlier.filterNot(d => relearned((d.side, d.decoration)))
    val inRecords = carried(recordTitles, kept.map(d => d.decoration.split(" ").toSeq).toSet)
    strongestFirst(learned ++ kept.filterNot(d => inRecords(d.decoration.split(" ").toSeq)))
  }

  /** `decorations` without `withheld` edge runs (side, words) — what a hold-out measure of the detector resolves with,
   *  to see whether it finds the learned decorations again. */
  def without(decorations: TitleDecorations, withheld: Set[(String, Seq[String])]): TitleDecorations =
    decorations.copy(prefixes = decorations.prefixes -- withheld.collect { case ("prefix", run) => run },
      suffixes = decorations.suffixes -- withheld.collect { case ("suffix", run) => run })

  /** The remainders that are not a decorated spelling of another remainder: "lalka" and "lalka 2d"
   *  are one film. */
  private def distinctFilms(rests: Set[Seq[String]]): Set[Seq[String]] =
    rests.filterNot(r => rests.exists(o => TitleContainment.isTokenRun(o, r)))

  /** The runs of `wanted` some title of `titles` carries anywhere, as a contiguous token run. */
  private def carried(titles: Iterable[String], wanted: Set[Seq[String]]): Set[Seq[String]] = {
    val longest = wanted.map(_.size).maxOption.getOrElse(0)
    titles.iterator.map(TitleContainment.tokens).flatMap(ts =>
      ts.indices.iterator.flatMap(i => (1 to math.min(longest, ts.size - i)).iterator.map(n => ts.slice(i, i + n)).filter(wanted))).toSet
  }
}
