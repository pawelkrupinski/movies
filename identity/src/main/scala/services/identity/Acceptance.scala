package services.identity

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.IdentityMeasures.ListingFilm
import services.identity.Scored.Accepted
import services.resolution.YearWindow

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it
 *  TAKES, and at what confidence: [[aloneNamed]] for a node on its own evidence, [[pooled]] for a
 *  cluster's pooled evidence. Each is an ordered list of RULES, every one a function of the ranked
 *  candidates alone; the first to accept decides, and the listing's season production, when a
 *  record names it, pre-empts them all ([[seasonProduction]]). A new rule is a new method here and
 *  one entry in a list. */
private[identity] final class Acceptance(calibration: IdentityCalibration) {

  val weights = new EvidenceWeights(calibration)
  import weights.{against, factsAnswered, favours, facts, fitsBetter, own, priorsLent, speaksAgainst}

  /** Neither denied nor reached only by IMDb's suggestion ([[Scored.suggestedOnly]]): a candidate the rules may take. */
  private def isEligible(scored: Scored): Boolean = !scored.denied && !scored.suggestedOnly
  private def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filter(isEligible)

  /** An acceptance rule, by the name an explanation gives it. Whichever condition took the film, no rule takes one whose
   *  title numbers its series OTHERWISE than the listing's ([[ownInstalment]]), nor one its title names nothing of beside
   *  another film of its director the title names, or the listing's year dates as closely ([[anotherOfItsDirector]]). */
  private final case class Rule(name: String, why: Seq[Scored] => Verdict) {
    def accepts(ranked: Seq[Scored]): Verdict =
      ownInstalment(why(ranked)).flatMap(pickedByItsTitle(ranked)).flatMap(takeable).flatMap(onNoReusedPage(ranked)).flatMap(notPerformedBeside)
  }
  /** Refuses a film the billing names by one piece while ANOTHER piece, sharing no word with the record's titles, bills a
   *  performance ([[NonMovieEventClassifier.isPerformance]]: a tribute, a concert, a stage show): the evening is the show,
   *  named after the film's title, and whatever record the venue's system attached to it is the namesake its search
   *  found. UK Electric Palace Hastings' "Straight Talk: A Tribute to Dolly Parton" [1992] {Barnet Kellman} is a tribute
   *  show named after her song, not the 1992 film she starred in. Only the event the billing bills is read
   *  ([[agreement.NonFilmEvents.eventBilled]]): a billing attaching a screening ("+ film", "pokaz", a festival's "Closing
   *  Film + Awards") keeps its film, and a performance "joined with" the film is its companion ("DYRYGENT - POŁĄCZONY Z
   *  KONCERTEM MUZYKI NA ŻYWO", Wajda's film). A record titled by the performance itself ("Kabaret", "Die Tribute von
   *  Panem") is the film its event words name. */
  private def notPerformedBeside(accepted: Accepted): Verdict = {
    val scored    = accepted._1
    val film      = scored.candidate.film
    val pieces    = scored.listing.performancePieces
    lazy val filmWords = film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val performed =
      if (pieces.isEmpty || !pieces.exists(_._2.forall(filmWords))) Nil
      else pieces.collect { case (piece, words, true) if !words.exists(filmWords) => piece }
    if (performed.isEmpty || film.titles.exists(NonMovieEventClassifier.isPerformance(_))) Right(accepted)
    else Left(Acceptance.Refused("another piece of its billing is a performance the film is not", Some(scored.candidate.tmdbId),
      s"${Acceptance.named(scored)} beside: ${performed.mkString(", ")}"))
  }
  /** Refuses any film while the listing's page reads as REUSED: a record its ORIGINAL title and the director it credits
   *  name stands in its pool ([[copiedBeyondItsBilling]]) though the billing says more than that record's titles. The
   *  copied title is what a search for the billing hit, copied onto a ticket page with the record's director, running
   *  time and year: they agree with the record by construction and confirm nothing, and the billing's other words name
   *  something else — a party, a band, a show the page was reused for. DE kinoheld: Kaminwerk's "Teenie Fasching" on
   *  Schweikart's "Fasching" (1939), "Dark New Wave Night" on Ai's "New Wave", Lagerhalle's "Three for Silver" (a folk
   *  band) on Kamal's "Tea For Three", KAP-Torgau's "VOODOO LOUNGE" (a Rolling Stones tribute) on Costabile's "VooDoo".
   *  Nor does such a page lend another film its take: only one the billing names whole (an exact, original or
   *  alternative title) may still be taken — Landsberg's "Helmut Schleich" (a Kabarett evening, on Kuzmic's "Helmut")
   *  is no more the 2018 recording of his "Kauf, du Sau" the bare name leads. */
  private def onNoReusedPage(ranked: Seq[Scored])(accepted: Accepted): Verdict = {
    val scored = accepted._1
    def namedWhole = scored.category("title").exists(Set("exact", "original", "alternative"))
    eligibleOf(ranked).find(copiedBeyondItsBilling).filter(copied => (copied eq scored) || !namedWhole).fold[Verdict](Right(accepted)) { copied =>
      Left(Acceptance.Refused("its page reads as reused: only a copied original title names a film of it, and the billing says more",
        Some(scored.candidate.tmdbId), s"${Acceptance.named(copied)} billed beyond: ${billedBeyond(copied).mkString(", ")}"))
    }
  }
  /** Is `scored` named only by the listing's ORIGINAL title and the director it credits, that title a mere part of the
   *  billing ([[IdentityMeasures.Listing.originalStandsApart]] false), with the billing saying more than any of the
   *  record's titles ([[billedBeyond]]) and stating no year of it? A billing the record's titles hold word for word
   *  ("Matrix" of "The Matrix", or a translation TMDB holds as an alternative title) is its own, and a year the listing
   *  states that the record shares makes the billing another record's spelling of it, as the copied facts' other reader
   *  holds (`FamilyScope.namedOnlyByCopiedTitle`: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003]). The title
   *  relation cannot tell: it reads the original title as one of the listing's own shapes, a `segment` of every record
   *  so titled. */
  private def copiedBeyondItsBilling(scored: Scored): Boolean = {
    val listing = scored.listing
    scored.category("originalTitle").contains("match") && IdentityMeasures.sameDirector(scored.measures) && !listing.originalStandsApart &&
      !listing.year.exists(year => scored.candidate.film.year.exists(film => math.abs(film - year) <= 1)) && billedBeyond(scored).nonEmpty
  }
  /** The billing's words none of the record's titles carries — none when a whole piece of the billing, or the search its
   *  title rules leave of it, is the record's word for word ("Twisters" of "(4DX Rewind) Twisters", "Endgame" of
   *  "Vengadores: Endgame (Reestreno)", "Moje własne Idaho" of "… + DEBATA [POKAZ BEZPŁATNY]"): the rest decorates it. */
  private def billedBeyond(scored: Scored): Seq[String] = {
    val listing   = scored.listing
    val filmWords = scored.candidate.film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val pieces    = IdentityMeasures.shapes(listing.ownTitles ++ listing.searchTitles, listing.decorations).map(IdentityMeasures.countedWords).filter(_.nonEmpty)
    if (pieces.exists(_.forall(filmWords))) Nil else IdentityMeasures.countedWords(listing.title).filterNot(filmWords)
  }
  private def pickedByItsTitle(ranked: Seq[Scored])(accepted: Accepted): Verdict =
    noneOf(anotherOfItsDirector(accepted._1, eligibleOf(ranked)), "its title names nothing of it, and another film of its director it names or dates as closely stands beside it",
      accepted._1).map(_ => accepted)
  /** Refuses an UNTAKEN film ([[Scored.untaken]]) whichever rule took it: it stays a rival, never a take. */
  private def takeable(accepted: Accepted): Verdict =
    accepted._1.untaken.fold[Verdict](Right(accepted))(why => Left(Acceptance.Refused("untaken", Some(accepted._1.candidate.tmdbId), why)))
  /** Refuses a film both titles number, differently: PL Cinema City's and UK Odeon's "League of Legends Worlds 26 |
   *  Finals in Cinema" is not TMDB's "League of Legends Worlds25 - Finals in Cinema", its only search hit. A number only
   *  one side carries is weaker — TMDB files a two-part film's halves by their subtitles ("De Gaulle: Liberté" is UK
   *  "De Gaulle: Liberté Part 2") — and stays the rules' own to weigh ([[imdbSuggestedWhy]], [[soleResultWhy]]). */
  private def ownInstalment(verdict: Verdict): Verdict = verdict.flatMap { accepted =>
    val scored = accepted._1
    needOf(!scored.category("numeral").contains("different"), "the title numbers another instalment", scored,
      s"numeral=${scored.category("numeral").getOrElse("")}").map(_ => accepted)
  }
  private def otherInstalment(scored: Scored) = scored.category("numeral").exists(IdentityMeasures.OtherInstalment)
  /** Refuses a film the title numbers as ANOTHER instalment than its own ([[IdentityMeasures.OtherInstalment]]). */
  private def notAnotherInstalment(scored: Scored): Either[Acceptance.Refused, Unit] =
    needOf(!otherInstalment(scored), "the title numbers another instalment", scored, s"numeral=${scored.category("numeral").getOrElse("")}")

  /** A rule's verdict: the film it accepts, or the first of its conditions that refused — the identity trace's
   *  `refused:<rule>:<why>`, so a listing no rule took says which condition stopped each — with the candidate the
   *  rule was weighing when it did and what that candidate's evidence said ([[Acceptance.Refused]]). */
  private type Verdict = Either[Acceptance.Refused, Accepted]
  import Acceptance.Refused
  private def need(holds: Boolean, refusal: => String): Either[Refused, Unit] = if (holds) Right(()) else Left(Refused(refusal))
  /** [[need]], naming the candidate it refused and why its evidence did not hold. */
  private def needOf(holds: Boolean, refusal: => String, about: Scored, detail: => String = ""): Either[Refused, Unit] =
    if (holds) Right(()) else Left(Refused(refusal, Some(about.candidate.tmdbId), detail))
  /** No candidate of `found` may stand: the first that does refuses `about`, named in the detail. */
  private def noneOf(found: Option[Scored], refusal: => String, about: Scored): Either[Refused, Unit] =
    found.fold[Either[Refused, Unit]](Right(()))(other => Left(Refused(refusal, Some(about.candidate.tmdbId), Acceptance.named(other))))
  /** `found` without its HOLLOW records — no year, no running time — when it holds a full one beside them: TMDB's empty
   *  duplicate of a film ("Solo", 1450958, beside Sophie Dupuis's 2023 "Solo") is no second film of its director. */
  private def withoutHollow(found: Seq[Scored]): Seq[Scored] = {
    def hollow(scored: Scored) = scored.candidate.film.year.isEmpty && scored.candidate.film.runtime.forall(_ <= 0)
    val full = found.filterNot(hollow)
    if (full.nonEmpty && full.size < found.size) full else found
  }
  private def one(found: Seq[Scored], none: => String, many: => String, noneDetail: => String = ""): Either[Refused, Scored] = found match {
    case Seq(only) => Right(only)
    case Seq()     => Left(Refused(none, None, noneDetail))
    case _         => Left(Refused(many, None, found.map(Acceptance.named).mkString("; ")))
  }
  /** What a listing's facts leave a director's rule to go on: its credit, or that it credits nobody. */
  private def credited(ranked: Seq[Scored]): String =
    ranked.headOption.map(_.listing.directors).filter(_.nonEmpty).fold("the listing credits no director")(names => s"credits ${names.mkString(", ")}")
  private def firstHit(ranked: Seq[Scored]): String = ranked.find(_.rank.contains(1)).fold("its search returned nothing")(hit => s"first hit ${Acceptance.named(hit)}")
  /** The first-ranked candidate, the listing every rule reads off it — refused when there is none. */
  private def firstCandidate(ranked: Seq[Scored]): Either[Refused, Scored] = ranked.headOption.toRight(Refused("no candidate"))
  /** Refuses a listing billing two works ([[MultiFilmBill.billsTwoWorks]]): a double bill is neither film. */
  private def notADoubleBill(listing: IdentityMeasures.Listing): Either[Refused, Unit] = need(!MultiFilmBill.billsTwoWorks(listing), "a double bill")
  /** Refuses a film the listing's title joins another work to by a slash ([[IdentityMeasures.joinsAnotherWork]]): a bill
   *  of two works is neither, as a "+" one is. */
  private def notJoinedToAnotherWork(listing: IdentityMeasures.Listing, scored: Scored): Either[Refused, Unit] =
    needOf(!IdentityMeasures.joinsAnotherWork(listing, scored.candidate.film), "the title joins another work to it", scored)
  /** `scored` accepted at its own probability. */
  private def atOwn(scored: Scored): Accepted = scored -> scored.probability
  private def cut(probability: Double) = f"${probability * 100}%.1f%% < ${calibration.ratingCut * 100}%.1f%%"
  /** `scored` at what its evidence CLASS measured (`IdentityCalibration.classProbability`), lending and never withdrawing —
   *  its class read with the namesakes the release veto took away counted as no rivals ([[ReleaseVeto.unvetoed]]). */
  private def classAccepted(scored: Scored, measures: Map[String, IdentityMeasures.Measure]): Verdict =
    calibration.classProbability(ListingFilm, ReleaseVeto.unvetoed(scored, measures)).toRight(Refused("no evidence class measured for it", Some(scored.candidate.tmdbId)))
      .flatMap { classProbability =>
        val lent = math.max(scored.probability, classProbability)
        Either.cond(calibration.showsRatings(lent), scored -> lent, Refused("below the rating cut", Some(scored.candidate.tmdbId), cut(lent)))
      }

  /** What a rule taking `scored` asks last: nothing it publishes speaks against it as `weighed` (the candidate read as
   *  the rule reads its title), no other of `eligible` fits `weighed`'s facts better, and its evidence class clears the
   *  cut ([[classAccepted]]). */
  private def unopposed(scored: Scored, weighed: Scored, eligible: Seq[Scored]): Verdict =
    for {
      _     <- needOf(!speaksAgainst(weighed), "a published fact weighs against it", scored, against(weighed).mkString(" "))
      _     <- noneOf(eligible.find(rival => (rival ne scored) && fitsBetter(rival, weighed)), "a rival fits its facts better", scored)
      taken <- classAccepted(scored, weighed.measures)
    } yield taken

  /** The first of `rules` to accept, with its name. */
  private def firstOf(ranked: Seq[Scored], rules: Seq[Rule]): Option[(Accepted, String)] =
    rules.iterator.map(rule => rule.accepts(ranked).map(_ -> rule.name)).collectFirst { case Right(taken) => taken }

  /** Why each rule a node may be taken by alone refused it: `(rule, the first condition that stopped it)`. */
  def refusals(ranked: Seq[Scored]): Seq[DecisionTrace.Refusal] =
    if (billsSeveralWorks(ranked)) Seq(DecisionTrace.Refusal("alone", "bills several works"))
    else aloneRules.flatMap(rule => rule.accepts(ranked).left.toOption.map(refused =>
      DecisionTrace.Refusal(rule.name, refused.why, refused.film, refused.detail)))

  private val aloneRules = Seq(Rule("sole-work", soleWorkWhy), Rule("favoured-calibrated", favouredCalibratedWhy), Rule("exact-top-hit", topHitWhy),
    Rule("segment-top-hit", segmentTopHitWhy), Rule("sole-result", soleResultWhy), Rule("imdb-suggested", imdbSuggestedWhy),
    Rule("directors-work", directorsWorkWhy), Rule("directors-title", directorsTitleWhy), Rule("dated-title", datedTitleWhy),
    Rule("house-production", houseProductionWhy), Rule("stage-production", stageProductionWhy), Rule("season-record", seasonRecordWhy))

  private val pooledRules = Seq(Rule("unrivalled-calibrated", unrivalledCalibratedWhy), Rule("exact-top-hit", topHitWhy),
    Rule("imdb-suggested", imdbSuggestedWhy))

  /** The rules a cluster is taken by on its pooled evidence, by name in the order they are tried — after
   *  `season-production`, which pre-empts them: what `identity-resolver.md` §21 lists, a spec holding the two together
   *  (the alone rules' order it reads off [[refusals]]). */
  private[identity] def pooledOrder: Seq[String] = "season-production" +: pooledRules.map(_.name)

  /** What the rule called `name` alone takes of `ranked` — for a spec holding one rule to its cases. */
  private[identity] def byRule(name: String)(ranked: Seq[Scored]): Option[Accepted] =
    (aloneRules ++ pooledRules).find(_.name == name).getOrElse(throw new NoSuchElementException(s"no rule $name")).accepts(ranked).toOption

  /** What a node accepts ON ITS OWN, with the rule that took it — only when its own facts favour it over the runner-up:
   *  a bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not decided alone — it
   *  follows the film its title's credited siblings chose (the cluster's), or the pooled vote — unless it is the
   *  listing's exact top hit, which is measured as a class. Every own match names the rule that took it, however far
   *  under the calibrated cut its probability stands ([[Acceptance.ruleTaking]]). A family's scope asks it once per
   *  node ([[FamilyScope.takenAlone]]). */
  def aloneNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    takenBy(ranked, keep = { case ((taken, _), rule) => rule == "imdb-suggested" || !outweighedBySuggestion(ranked, taken) })(
      datedOrBroadcastSeasonRecord(ranked).orElse(firstOf(ranked, aloneRules)))

  /** Does an undenied film only IMDb's suggestion reached ([[Scored.suggestedOnly]], which only the IMDb rule may take),
   *  that another venue's listing of the title corroborates, weigh at least as much on the evidence as `taken`? Then the
   *  evidence does not single `taken` out, and no other rule takes it: PL Kino Kryterium's "DKF: Ktoś całkiem obcy" took
   *  TMDB's Polish title of Foley's 2007 "Perfect Stranger" at 46.3% beside Andersen's 2025 "I Was a Stranger" at 47.8%
   *  — which TMDB holds under its Arabic title, and Kino Chatka Żaka's listing crediting him corroborates — the film
   *  Kryterium shows. An uncorroborated suggestion is no such rival: IMDb suggests a title's best-known films, and DE
   *  "Béni`s Noise", "Escape to Life" and a dozen more lost their own films to one. */
  private def outweighedBySuggestion(ranked: Seq[Scored], taken: Scored): Boolean =
    ranked.exists(other => (other ne taken) && other.suggestedOnly && !other.denied && other.number("venues.corroborating").exists(_ > 0) &&
      other.probability >= taken.probability)

  /** A DOUBLE PROGRAMME: two quoted works a conjunction joins ([[ListingShape.quotesTwoJoined]], PL "„Mieszkanie” i „Owoce
   *  rajskich drzew spożywamy”"), or a "+" joining two whole works ([[MultiFilmBill.billsTwoWholeWorks]]), whatever is held
   *  of them, is neither film (user rule) — UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna
   *  Harrison, Robin Shaw} ×133, its two films scored 91–93% and popularity's coin picking one; PL Kino Pałacowe's
   *  "Historia kina w Popielawach + Pruska kultura", a 1908 short TMDB does not hold. A talk joined to one film
   *  ("+ spotkanie z reżyserem") is no second work, and a film whose own whole title it is ("Romeo + Juliet") is no
   *  bill. So is a programme a word bills as several films ([[MultiFilmBill]]: "Triple Feature: Lord of the Rings",
   *  "The Dark Knight Trilogy", "Maraton Horrorów") — unless an eligible film's own title carries that word ("Marathon
   *  Man"), and then only that film may be taken ([[takesNoBill]]). */
  def billsSeveralWorks(ranked: Seq[Scored]): Boolean = ranked.headOption.exists { any =>
    any.listing.rawTitle.exists(ListingShape.quotesSeveralDated) || (any.listing.title +: any.listing.rawTitle.toSeq).exists(ListingShape.quotesTwoJoined) ||
    slashesTwoWorks(any.listing, ranked) ||
    (MultiFilmBill.billsTwoWorks(any.listing) && !eligibleOf(ranked).exists(_.category("title").contains("exact")) &&
      MultiFilmBill.billsTwoWholeWorks(any.listing)) ||
      markerOf(ranked).exists(marker => !eligibleOf(ranked).exists(scored => MultiFilmBill.namedBy(marker, scored.candidate.film)) &&
        billedAlone(ranked).isEmpty)
  }

  /** Does a SLASH join two works — every piece of the title a slash parts the whole title of one candidate, a leading
   *  article aside, and no candidate titled as two pieces? UK Savoy Cinema Corby's "We're Going on a Bear Hunt / Tiger
   *  Who Came to Tea" bills the two shorts as one programme, as the "+" bills do; one film's titles either side of the
   *  slash ("Le Fabuleux Destin d'Amélie Poulain / Die fabelhafte Welt der Amélie") are its own, and a piece naming no
   *  candidate ("/ Baby Friendly") no second work. */
  private def slashesTwoWorks(listing: IdentityMeasures.Listing, ranked: Seq[Scored]): Boolean = {
    def key(text: String) = {
      val words = services.movies.TitleContainment.tokens(text)
      if (words.sizeIs >= 2 && IdentityMeasures.LeadingArticles(words.head)) words.tail else words
    }
    def slashed(title: String) = {
      val parts  = SlashBreak.split(title).toSeq
      val pieces = parts.map(key).filter(_.nonEmpty)
      // a format, a screening note or an event after the slash is no second work, whatever film carries its name
      // ("Baranek Shaun i kudłata bestia / dubbing", '"LALKA" / Premiera')
      pieces.sizeIs >= 2 && parts.forall(MultiFilmBill.namesAWork) && {
        val named = pieces.map(piece => eligibleOf(ranked).filter(_.candidate.film.titles.exists(key(_) == piece)).map(_.candidate.tmdbId).toSet)
        named.forall(_.nonEmpty) && named.combinations(2).forall { case Seq(a, b) => (a intersect b).isEmpty }
      }
    }
    // read for every rule's every ranking: a title with no slash is split for nothing
    def read(title: String) = title.indexOf('/') >= 0 && slashed(title)
    read(listing.title) || listing.rawTitle.exists(raw => raw != listing.title && read(raw))
  }
  private val SlashBreak = """\s+/\s+""".r

  /** The one film a marathon billing one title names ([[MultiFilmBill.billedOne]]): the one eligible candidate titled
   *  exactly so, when no eligible candidate's title extends it — PL "Inna Mamusia - maraton" is Inna mamusia (2026);
   *  "Władca Pierścieni - maraton" bills Jackson's films, which extend Bakshi's "Władca Pierścieni". */
  private def billedAlone(ranked: Seq[Scored]): Option[Int] =
    ranked.headOption.flatMap(_.listing.billedOne).flatMap { title =>
      val named    = IdentityMeasures.key(title)
      val eligible = eligibleOf(ranked)
      def keys(scored: Scored) = scored.candidate.film.titles.map(IdentityMeasures.key)
      val extended = eligible.exists(scored => keys(scored).exists(key => key != named && key.startsWith(named)))
      eligible.filter(scored => keys(scored).contains(named)).map(_.candidate.tmdbId).distinct match {
        case Seq(one) if !extended && named.nonEmpty => Some(one)
        case _                                       => None
      }
    }

  /** The word the ranked listing bills several films by, if any ([[MultiFilmBill.marker]]). */
  private def markerOf(ranked: Seq[Scored]): Option[String] =
    ranked.headOption.flatMap(_.listing.billMarker)

  /** Is `taken` no film of a programme the listing bills by a word — the word its own title's, if the listing has one? */
  private def takesNoBill(ranked: Seq[Scored])(taken: (Accepted, String)): Boolean =
    markerOf(ranked).forall(marker => MultiFilmBill.namedBy(marker, taken._1._1.candidate.film) ||
      billedAlone(ranked).contains(taken._1._1.candidate.tmdbId))

  /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
   *  best eligible candidate the calibration accepts that no namesake out-fits ([[unrivalledCalibratedWhy]]).
   *  Each the edition of it the listing names, if any ([[editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] = pooledNamed(ranked).map(_._1)

  /** [[pooled]], with the rule that accepted — for the decision's trace. */
  def pooledNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    // a relay's broadcast-season record pre-empts the pooled rules as it does the alone ones: a cluster of "MET: Macbeth"
    // spellings took the Met's 2014 staging by a pooled rule after the alone path ruled it another broadcast. Never
    // stage-production pooled: one member's stated year is not the cluster's broadcast — it took UK Aldeburgh's
    // "Royal Ballet: The Nutcracker" [2025] as the 2025/26 record, and split Savoy Worksop's "MET:Manon" off the Met's
    takenBy(ranked)(broadcastSeasonRecord(ranked).map(_ -> "season-record").orElse(firstOf(ranked, pooledRules)))

  /** What both ways of taking a film share: nothing for a bill of several works; the season's production when there is
   *  one (it pre-empts every rule, and a season production refused takes nothing), else what `rules` take — either only
   *  if `keep` keeps it — each the edition of it the listing names, and never a film the bill's word refuses
   *  ([[takesNoBill]]). */
  private def takenBy(ranked: Seq[Scored], keep: ((Accepted, String)) => Boolean = _ => true)
                     (rules: => Option[(Accepted, String)]): Option[(Accepted, String)] =
    if (billsSeveralWorks(ranked)) None
    else seasonProduction(ranked).map(_.map(_ -> "season-production")).getOrElse(rules).filter(keep).filter(_._1._1.untaken.isEmpty)
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }.filter(takesNoBill(ranked))

  /** The probability that `film` is the listing's film — the decision's confidence, on the scale
   *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
   *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
   *  lent when the listing's facts accepted it ([[calibratedWhy]]). */
  def confidenceOf(ranked: Seq[Scored], film: Int): Double =
    topHit(ranked).filter(_._1.candidate.tmdbId == film).map(_._2)
      .orElse(calibratedWhy(ranked).toOption.filter(_._1.candidate.tmdbId == film).map(_._2))
      .orElse(pooled(ranked).filter(_._1.candidate.tmdbId == film).map(_._2))
      .getOrElse(eligibleOf(ranked).find(_.candidate.tmdbId == film).fold(0.0)(_.probability))

  /** Why an own match's confidence stands above its calibrated probability: its exact top hit's
   *  class, or the ranking priors lending ([[EvidenceWeights.priorsLent]]) — naming each rival the release veto took
   *  out of the class it was accepted by ([[ReleaseVeto]]). */
  def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double): String =
    if (confidence <= scored.probability) ""
    else (if (topHit(ranked).exists(_._1.candidate.tmdbId == scored.candidate.tmdbId)) " as its exact top hit"
      else " with the ranking priors lending, never withdrawing") +
      (if (scored.vetoedRivals == 0) "" else ranked.flatMap(rival => rival.denial.filter(ReleaseVeto.vetoes)
        .map(why => s"rival ${rival.candidate.tmdbId} $why")).mkString(" (", "; ", ")"))

  // ── the rules ──────────────────────────────────────────────────────────────────────────

  /** The listing's EXACT TOP HIT, accepted on what the calibration measured for its evidence as a
   *  CLASS (`IdentityCalibration.classProbability`): the one film its whole title names exactly
   *  that its own title search returned first, in TMDB's order ([[IdentityMeasures.exactTopHits]]),
   *  when the listing's own evidence rules it out on nothing, published nothing against it, and
   *  gives no rival a better fit ([[EvidenceWeights.fitsBetter]]). A bare title credits it with the
   *  naive-Bayes sum of missing facts, a low popularity and its same-titled rivals, which undersells
   *  what the class measured on the labels; here the search standing LENDS confidence and never
   *  withdraws it. */
  private def topHit(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(topHitWhy(ranked)).toOption
  private def topHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any  <- firstCandidate(ranked)
      id   <- IdentityMeasures.exactTopHits(any.listing, ranked.map(scored => (scored.candidate.tmdbId, scored.candidate.film, scored.rank))) match {
                case Seq(only) => Right(only)
                case Seq()     => Left(Refused("no film its whole title names is its search's first hit"))
                case ids       => Left(Refused("two exact top hits", None, ids.mkString("; ")))
              }
      best <- eligible.find(_.candidate.tmdbId == id).toRight(Refused("its exact top hit is denied", Some(id),
                ranked.find(_.candidate.tmdbId == id).flatMap(_.denial).fold("")(_.why)))
      taken <- unopposed(best, best, eligible)
    } yield taken
  }

  /** A programme listing's EXACT TOP HIT once its banner is off: the one candidate whose title is a whole
   *  delimited piece of the listing's (`segment`) and which the listing's own search returned FIRST, when
   *  the title names no other candidate at all (the rest is a banner, not a second film), it bills no two
   *  works, and nothing it publishes speaks against it or fits a rival better. The piece IS a bare title,
   *  so it is credited with what the exact-top-hit class measured for bare titles. PL's "DZIEŃ KINA POLSKIEGO:
   *  Przepraszam, czy tu biją" and ~30 programme listings like it sat at 28.9% with no rule to take them. */
  private def segmentTopHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any    <- firstCandidate(ranked)
      // a one-word piece names no film here, rival or taken: "Inna Mamusia - maraton" is no "Maraton"
      scored <- one(ranked.filter(scored => scored.titleNamesIt && (scored.category("title").exists(IdentityMeasures.Rivalling) ||
                  IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film))), "the title names no film", "the title names two films")
      _      <- needOf(!scored.denied, "the film it names is denied", scored, scored.denial.fold("")(_.why))
      _      <- needOf(scored.rank.contains(1), "not its search's first hit", scored, scored.rank.fold("not ranked")(rank => s"ranked $rank"))
      _      <- needOf(scored.category("title").contains("segment"), "not named by a piece of the title", scored,
                  s"title=${scored.category("title").getOrElse("absent")}")
      _      <- notADoubleBill(any.listing)
      _      <- needOf(IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film), "its piece cannot stand for the whole title", scored)
      _      <- notJoinedToAnotherWork(any.listing, scored)
      // a year the title states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      _      <- needOf(scored.number("titleYear.delta").forall(FactRelations.nearDelta), "the title dates another year",
                  scored, scored.number("titleYear.delta").fold("")(delta => f"titleYear.delta=$delta%.0f"))
      // Read as the bare title its piece is: the banner beside it is no evidence against the film.
      bare    = scored.copy(measures = scored.measures + ("title" -> IdentityMeasures.Category("exact")))
      taken  <- unopposed(scored, bare, eligible)
    } yield taken
  }

  /** The film IMDb's suggestions for the listing's own title name, on the old pipeline's three rungs — the route
   *  that reaches a film whose local title TMDB lacks, since IMDb matches a query against a film's
   *  other-language titles ("Superfutrzak i złośliwa wiewiórka" suggests only the Finnish "Supermarsu ja suuri
   *  huijaus"): IMDb's FIRST suggestion with the year the listing states; the ONE suggested film the listing's
   *  credited director directed; or the ONLY film IMDb suggests, when the title names it and TMDB ranks no
   *  other film of that very title above it ("Ziemia obiecana" is Wajda's, not the 1927 film IMDb alone spells
   *  so); or the ONE film IMDb lists under one of the listing's search titles in some language — an AKA TMDB does
   *  not carry ("Camino dla opornych" is IMDb's Polish title of "Compostelle"). Read as a title the film answers to,
   *  nothing the listing publishes may speak against it, no rival may fit better, and a double bill is neither film. */
  private def imdbSuggestedWhy(ranked: Seq[Scored]): Verdict = firstCandidate(ranked).flatMap { any =>
    val eligible  = ranked.filterNot(_.denied)
    val suggested = eligible.filter(scored => scored.imdb.isDefined || scored.imdbTitled.nonEmpty)
    def sameDirector(scored: Scored) = IdentityMeasures.sameDirector(scored.measures)
    // the film's own title or its original — not an alternative TMDB files it under ("Lumière" is not "Café Lumière")
    def exact(scored: Scored)        = scored.category("title").exists(Set("exact", "original"))
    def outranked(scored: Scored) = eligible.exists(rival => (rival ne scored) && rival.category("title").contains("exact") &&
      rival.rank.exists(r => scored.rank.forall(r < _)))
    // the ONE suggestion IMDb lists under the listing's own title in some language (an AKA TMDB does not carry):
    // "Camino dla opornych" is IMDb's Polish title of TMDB's French "Compostelle". Only while nothing else the
    // listing names stands beside it: no other record carrying its title, though denied (Renoir 2025 beside the 2012
    // film IMDb also lists as "Renoir"; the 1951 "Streetcar" beside the 1989 TV film), no year the title dates against
    // it ("Miłość 2024" is not Haneke's 2012 film), no numbered set ("Bolek i Lolek – zestaw IV"), a title of words,
    // not a number ("2026"), and no stage work broadcast as a non-season film (the Met's "Così fan tutte").
    def titledRung(scored: Scored): Boolean =
      IdentityMeasures.soleImdbTitled(ranked.filter(_.imdbTitled.nonEmpty).map(s => s.candidate.tmdbId -> s.imdbTitled).toMap)
        .exists(_._1 == scored.candidate.tmdbId) &&
        !ranked.exists(other => (other ne scored) && other.category("title").exists(IdentityMeasures.TitlesItsOwn)) &&
        IdentityMeasures.takesImdbTitle(scored.listing, scored.candidate.film) && titledByMoreThanAWord(scored)
    // the billing's whole title, a piece of two words at least, the word the billing opens with, or a DATED record: a
    // one-word piece after another names an undated record only as many films' title does — DE "Friday the 13th:
    // Legacy", a fan film, is not David Slade's undated "Legacy" IMDb lists under its last word, while "Spooked: Ringu"
    // is the 1998 "Ring" and "Gina (Kurzfilm)" is "Gina"
    def titledByMoreThanAWord(scored: Scored): Boolean = scored.candidate.film.year.isDefined || scored.imdbTitled.exists { title =>
      val words = IdentityMeasures.words(title)
      scored.listing.ownKeys(IdentityMeasures.key(title)) || words.sizeIs >= 2 ||
        scored.listing.ownForms.exists(_.words.startsWith(words))
    }
    def rung(scored: Scored): Boolean = titledRung(scored) || scored.imdb.exists { place =>
      (place.place == 1 && scored.number("year.delta").contains(0.0)) ||
        (sameDirector(scored) && suggested.count(sameDirector) == 1) ||
        (place.of == 1 && scored.titleNamesIt && !outranked(scored)) ||
        // the old pipeline's yearless rung: IMDb's FIRST suggestion, and the only suggested film the title is ("Kroll")
        (place.place == 1 && exact(scored) && suggested.count(exact) == 1 && !outranked(scored))
    }
    // A fallback, as the old pipeline's was: IMDb answers only when TMDB's own search ranks no OTHER film the title
    // names above it ("BTS 'ARIRANG' IN SÃO PAULO" is TMDB's São Paulo record, not IMDb's first 2026 suggestion, Busan;
    // "Kroll" is TMDB's first, the 1991 film IMDb suggests, above 1972's "Krõll"), and
    // never for an instalment the title numbers otherwise ("Recepta na szczęście 2" is not the first film).
    def searchNamesAnother(scored: Scored, rival: Scored) = (rival ne scored) && rival.titleNamesIt &&
      rival.rank.exists(r => scored.rank.forall(r < _))
    for {
      _      <- need(suggested.nonEmpty, "IMDb suggests none of its candidates")
      _      <- notADoubleBill(any.listing)
      scored <- one(suggested.filter(rung), "no IMDb suggestion is taken by its year, its director or as the only one", "two IMDb suggestions qualify")
      _      <- notJoinedToAnotherWork(any.listing, scored)
      // EXPERIMENT: a bare billing (no year, director, runtime or original title of its own) that reaches a film only by
      // IMDb's suggestion and only through a title the film is filed under beside its own: DE "Patagonien", a travel
      // lecture, is not Bozzelli's 2023 "Patagonia" by its German alternative title
      _      <- needOf(!(scored.suggestedOnly && !exact(scored) && statesNothing(scored.listing)),
                  "a bare billing reaches the film only by IMDb's suggestion of a title it is filed under, not its own", scored,
                  s"title=${scored.category("title").getOrElse("absent")}")
      _      <- noneOf(eligible.find(searchNamesAnother(scored, _)), "TMDB ranks another film the title names above it", scored)
      _      <- notAnotherInstalment(scored)
      taken  <- unopposed(scored, asItsTitlesTopHit(scored), eligible)
    } yield taken
  }

  private def statesNothing(l: IdentityMeasures.Listing): Boolean =
    l.year.isEmpty && l.directors.isEmpty && l.runtime.isEmpty && l.originalTitle.isEmpty

  /** Is every word of the listing's whole title one of the film's title's — two words at least, or the film's title
   *  before its dash or colon — as the old pipeline took a whole title's only search result: "Dzień Dziecka księdza
   *  Kaczkowskiego" is "Dzień Dziecka księdza Jana Kaczkowskiego", "TAFITI" is "Tafiti – Ab durch die Wüste". Two words
   *  or more must cover the film's title ([[coversItsTitle]]): an event billed in a few words is not every longer title
   *  that happens to hold them. */
  private def wordsOfIts(scored: Scored): Boolean = {
    val own = IdentityMeasures.yearlessTokens(scored.listing.title)
    // the billing's own pieces, when it has two or more ("About Time: Once Upon a Time")
    lazy val pieces = TitlePartBreak.split(scored.listing.title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty)
    own.nonEmpty && (Seq(scored.candidate.film.title) ++ scored.candidate.film.originalTitle).exists { title =>
      own.forall(IdentityMeasures.yearlessTokens(title).contains) && touchesPartsWhole(own, title, scored.listing.title) &&
        ((own.sizeIs >= 2 && coversItsTitle(own, pieces, title)) || IdentityMeasures.yearlessTokens(MainTitleBreak.split(title).head) == own)
    }
  }
  /** Does the billing touch every part of `title` ([[TitlePartBreak]]) it touches at all by half that part's words or more?
   *  Or by its leading words alone, a title the venue cut short ("Shaun The Sheep : The Beast"). A part touched otherwise
   *  holds the billing's words only by chance — a banner's that recur in it: UK Watershed's "About
   *  Time: Once Upon a Time", an episode of Berger's series, touches "All About My Mother" by one word in four, so it is not
   *  "Once Upon a Time… All About My Mother". A part the billing bills in its own order is still touched whole
   *  ("GÖTTERDÄMMERUNG Royal Opera" of "Royal Ballet & Opera 2026/27: Götterdämmerung"). A part touched by a whole part of
   *  the BILLING is no chance either: Leiston's "Ballet - Notre-Dame de Paris" [2013] bills "Ballet" as a part of its own,
   *  which "La Scala Ballet: Notre-Dame de Paris" holds in its first part — where "About Time" touches "All About My
   *  Mother" by half a part of the billing. */
  private def touchesPartsWhole(own: Seq[String], title: String, billing: String): Boolean = {
    val words  = own.toSet
    val billed = TitlePartBreak.split(billing).toSeq.map(IdentityMeasures.yearlessTokens(_).toSet).filter(_.nonEmpty)
    TitlePartBreak.split(title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty).forall { part =>
      val touched = part.filter(words)
      // its leading words alone: the venue cut the title short ("Shaun The Sheep : The Beast")
      touched.isEmpty || touched.size * 2 >= part.size || part.takeWhile(words).size == touched.size ||
        billed.exists(_.subsetOf(touched.toSet))
    }
  }
  /** Do the listing's words `own`, all of them the film's `title`'s, cover that title: more than half its words — each of
   *  the billing's own `pieces`, when it bills two or more, read by the title ([[readIn]]) — or every part of it
   *  ([[TitlePartBreak]]) they touch whole — "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)". Fewer are
   *  words scattered over a longer title (the wrong-match hunt, 10-14, 9 wrong to 1 right): DE "Simply Christmas", a
   *  band's Advent concert, is no "Steve's Simply Happy Christmas Cook-A-Long", "DAS GEHEIME FILMARCHIV", a secret-classic
   *  series, no "Das geheime Filmarchiv der Eva Braun"; nor is a piece whose words the title reads the other way round
   *  another's phrase: UK Watershed's "About Time: Once Upon a Time", an episode of Berger's series, is no "Once Upon a
   *  Time… All About My Mother" (prod 2026-10-14). */
  private def coversItsTitle(own: Seq[String], pieces: => Seq[Seq[String]], title: String): Boolean = {
    val words = own.toSet
    val ordered = IdentityMeasures.yearlessTokens(title)
    val theirs = ordered.toSet
    (words.size * 2 > theirs.size && (pieces.sizeIs < 2 || pieces.forall(readIn(_, title, ordered)))) || {
      val parts = TitlePartBreak.split(title).toSeq.map(IdentityMeasures.yearlessTokens).filter(_.nonEmpty)
      parts.filter(_.exists(words)).forall(_.forall(words))
    }
  }
  /** Does the film's `title` (its words `ordered`) read the billing's `piece`: its words in the piece's order ([[inOrder]]),
   *  or all within one part of the title ([[TitlePartBreak]]), where a translation may order them otherwise — DE Cinema
   *  Wolfhagen's "National Opera Paris: Schwanensee" is "Opéra national de Paris 2024: Schwanensee" (10-15). A piece whose
   *  words two of the title's parts hold the other way round is another's phrase: "About Time" in "Once Upon a Time… All
   *  About My Mother". */
  private def readIn(piece: Seq[String], title: String, ordered: Seq[String]): Boolean =
    inOrder(piece, ordered) || TitlePartBreak.split(title).iterator.map(IdentityMeasures.yearlessTokens).exists(part => piece.forall(part.contains))
  /** Are `piece`'s words `title`'s in its order, others between them at most? */
  private def inOrder(piece: Seq[String], title: Seq[String]): Boolean =
    piece.foldLeft(Option(title)) { (rest, word) => rest.map(_.dropWhile(_ != word)).filter(_.nonEmpty).map(_.tail) }.isDefined

  /** Denied only by the probability cut — no learned rule, no pin — while every word of the listing's title is the
   *  film's: the cut is then the title's reading of a reordered banner, not a fact against it. ES Ocine's "Manon
   *  (BALLET LIVE)" ×7 is TMDB's "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE", the one film its search returns, denied
   *  at 2.4% for its original title "Manon" reading as a fragment of that. */
  private def cutOnly(scored: Scored): Boolean =
    !scored.suggestedOnly && scored.deniedByCutOnly && wordsOfIts(scored)
  private val MainTitleBreak = java.util.regex.Pattern.compile("""\s+[-–—]\s+|:\s""")
  /** What parts a title: a spaced dash, a colon, a full stop, an ellipsis or a middle dot before a space. */
  private val TitlePartBreak = java.util.regex.Pattern.compile("""\s+[-–—·]\s+|[:.…]\s""")
  /** The ONLY film one of the listing's own title searches returned — the old pipeline's `searchUnique`, which
   *  took it unless a fact CONTRADICTED it: "Loving Karma" [78′] is TMDB's 85-minute record, the one "Loving
   *  Karma" finds, though seven minutes weigh against it in the probability. The title must name it — a piece
   *  standing for the whole ([[IdentityMeasures.standsForTheWhole]]: not "Bhutan" of "Bhutan – Trails of
   *  Happiness") — no other candidate the title names may stand beside it, and a double bill is neither film. */
  private def soleResultWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any   <- firstCandidate(ranked)
      sole  <- one(ranked.filter(scored => scored.soleResult && (isEligible(scored) || cutOnly(scored))),
                 "no search of its returned a single film", "its searches returned different single films",
                 s"${ranked.count(_.rank.isDefined)} film(s) found by its searches")
      _     <- needOf(sole.titleNamesIt || wordsOfIts(sole), "the title does not name its search's only film", sole,
                 s"title=${sole.category("title").getOrElse("absent")}")
      _     <- needOf(!contradicted(sole), "a published fact contradicts it", sole, contradiction(sole))
      // a director the venue credits is the listing's own fact: a search's only film directed by another, that the title
      // names only in part, is no answer (PL "Carmen" [2026] {Richard Eyre} and "Carmen: Salzburger Festspiele 2026") —
      // a film carrying the very title still is (Kino Parczew's "Tedi i magiczna lampa" credits its co-directors)
      _     <- needOf(!sole.category("director").contains("different") || sole.category("title").exists(IdentityMeasures.Rivalling),
                 "it credits another director", sole)
      _     <- notADoubleBill(any.listing)
      _     <- notJoinedToAnotherWork(any.listing, sole)
      _     <- needOf(sole.category("title").exists(IdentityMeasures.Rivalling) || wordsOfIts(sole) ||
                   IdentityMeasures.standsForTheWhole(any.listing, sole.candidate.film),
                 "its piece cannot stand for the whole title", sole)
      // a film the title names by fewer of its words is no rival to one carrying them all ("Manon" beside "BALLET LIVE.
      // MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)")
      _     <- noneOf(eligible.find(other => (other ne sole) && other.titleNamesIt && !(wordsOfIts(sole) && !wordsOfIts(other))),
                 "the title names another film", sole)
      _     <- notAnotherInstalment(sole)
      titled = asItsTitlesTopHit(sole)
      taken <- classAccepted(sole, titled.measures.filterNot { case (name, _) => name == "runtime.delta" || name == "year.delta" })
    } yield taken
  }

  /** The best eligible candidate, when its probability — the priors lending, never withdrawing
   *  ([[EvidenceWeights.priorsLent]]) — clears the calibration's cut. */
  private def calibratedWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    eligible.headOption.toRight(Refused("no eligible candidate", None, ranked.headOption.flatMap(_.denial).fold("")(denial => s"best denied: $denial")))
      .map(best => best -> priorsLent(best, eligible))
      .flatMap { case accepted @ (best, probability) =>
        Either.cond(calibration.showsRatings(probability), accepted, Refused("below the rating cut", Some(best.candidate.tmdbId), cut(probability))) }
  }

  /** [[calibratedWhy]], when the listing's own facts also favour it over the runner-up. */
  private def favouredCalibratedWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      taken <- calibratedWhy(ranked)
      _     <- noneOf(eligible.lift(1).filterNot(favours(taken._1, _)), "its own facts do not favour it over the runner-up", taken._1)
      _     <- noneOf(closerThan(taken._1, eligible), "its title sits inside a closer record", taken._1)
    } yield taken
  }

  /** Another eligible film of `best`'s credited director that the title names, or that the listing's year dates as
   *  closely, when the title names nothing of `best` (`title=none`): walking a director's credit reaches his films, and a
   *  title naming none of two his facts fit alike picks neither — DE "München Mord: Leben und Sterben in Schwabing" [2019]
   *  {Sascha Bigler}, a TV episode, is neither of his 2019 films; UK "Gremlins Special Event" {Joe Dante} is not his
   *  "Innerspace", however its 120 minutes fit, beside the Gremlins it names. A translation the facts single out stands
   *  (PL "Fach" is Rossi's "Il Mestiere"). */
  private def anotherOfItsDirector(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    if (!best.category("title").contains("none") || !IdentityMeasures.sameDirector(best.measures) || inflects(best)) None
    else eligible.find(other => (other ne best) && IdentityMeasures.sameDirector(other.measures) &&
      (other.titleNamesIt || (best.number("year.delta").isDefined && other.number("year.delta") == best.number("year.delta"))))
  /** Does a word of the billing inflect a word of the record's title ("Osobliwy świat Hieronymusa Boscha" of "The
   *  Curious World of Hieronymus Bosch"): a title in another language naming the film, though it shares no word whole. */
  private def inflects(scored: Scored): Boolean = {
    import services.movies.TitleContainment.tokens
    val billed = scored.listing.ownTitles.flatMap(tokens)
    scored.candidate.film.titles.flatMap(tokens).filter(_.length >= IdentityMeasures.StemLetters).exists(word => billed.exists(_.startsWith(word)))
  }

  /** The film a node's evidence LEANS to though no rule took it: the best eligible candidate on a listing billing one
   *  work, at [[LeanMargin]] times the runner-up's probability — every fact the listing publishes weighed in it, the
   *  priors lending — and no closer record. Not the runner-up rule's facts alone: a bare title's "facts" are the
   *  records' missing credits (US "Lady Frankenstein", 35.8% beside 2025's "Frankenstein" at 4.0%, lost on its record
   *  crediting a director the listing does not name). Not a match: the film a no-match's card may keep the ratings of, when an
   *  earlier answer gave the card that film's ([[IdentityProjectionPlan]]). PL "Tatarak" read 18.4% for Wajda's 2009
   *  film, its 1965 namesake 3.3%; "Lalka" leans to the 2026 film at 68.1%, not the 1968 one the old pipeline rated
   *  it as; "Bolek i Lolek" (28.9% beside "Reksio" at 28.9%) and the 1986 and 2025 "Caravaggio" lean to neither. */
  def leaning(ranked: Seq[Scored]): Option[Scored] =
    Option.when(ranked.headOption.forall(any => !MultiFilmBill.billsTwoWorks(any.listing)) && !billsSeveralWorks(ranked))(eligibleOf(ranked))
      .flatMap(eligible => eligible.headOption.filter(_.untaken.isEmpty).filter(best => eligible.lift(1).forall(runnerUp =>
        best.probability >= Acceptance.LeanMargin * runnerUp.probability) && closerThan(best, eligible).isEmpty))

  /** The eligible record the listing's title sits inside ([[titledCloser]]) whose answered facts
   *  `best`'s do not beat, if any: then `best`, which the title only overlaps, is not the listing's
   *  film on that evidence — on its own or pooled. */
  private def closerThan(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    eligible.find(closer => (closer ne best) && titledCloser(closer, best) && factsAnswered(best, closer) <= factsAnswered(closer, closer))

  /** Does `closer`'s record hold the listing's title, or name a whole piece of it
   *  ([[IdentityMeasures.ContainingRelations]]), while `other`'s only overlaps it, names it by no
   *  original or alternative title (PL "Following" is Nolan's "Śledząc" by its original title), and
   *  does not hold `closer`'s title itself ("English National Ballet presents The Sleeping Beauty"
   *  holds "The Sleeping Beauty")? Then `other` is taken over it only when the facts `closer`'s record
   *  answers favour `other` ([[EvidenceWeights.factsAnswered]]). ES "BTS World Tour 'ARIRANG' In
   *  Buenos Aires: Live" ×82 took the Busan concert, and PL Kinoteka's double bill "Basia. Humor w
   *  paski mam + Kocia Szajka…" took "Basia. Radzę sobie!", on a credit and a runtime the record their
   *  title names merely does not state. */
  private def titledCloser(closer: Scored, other: Scored): Boolean = {
    def containing(scored: Scored) = scored.category("title").exists(IdentityMeasures.ContainingRelations)
    def words(scored: Scored) = services.movies.TitleContainment.tokens(scored.candidate.film.title).toSet
    containing(closer) && !containing(other) && !other.titleNamesIt && !(words(closer) subsetOf words(other))
  }

  /** [[calibratedWhy]] — unless the title names another candidate by the very same pieces and the
   *  pooled FACTS ([[EvidenceWeights.facts]]) fit that one better: four "Camino dla opornych" whose
   *  original title "Santiago" names two films, and whose 113 minutes fit the fourth the search
   *  returned, do not take the 93-minute first. A candidate the title names less specifically
   *  ("Mad Max" inside "Mad Max 2: The Road Warrior") or not at all is no such rival, and
   *  namesakes the facts fit alike stay the calibration's to tell apart — its ranking priors and
   *  the family's venue count are measured evidence there (a bare "Resident Evil" at 148 venues).
   *  Two films the title names by disjoint pieces ([[IdentityMeasures.namedApart]]) are not
   *  namesakes: only the facts may pick one of "Lalka (Dolly)"'s two. */
  private def unrivalledCalibratedWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      taken <- calibratedWhy(ranked)
      best   = taken._1
      _     <- noneOf(closerThan(best, eligible), "its title sits inside a closer record", best)
      _     <- noneOf(eligible.find(rival => (rival ne best) && {
                 val pieces = IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)
                 val alike  = pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.candidate.film)
                 val apart  = IdentityMeasures.namedApart(best.listing, best.candidate.film, rival.candidate.film)
                 (alike && facts(rival) > facts(best)) || (apart && facts(rival) >= facts(best))
               }), "a film the title names alike fits its facts better", best)
      _     <- noneOf(Option.when(namedWithinAnother(best, eligible))(best), "the title names it only within another candidate's longer title", best)
    } yield taken
  }

  /** Does the venue's billing name `best` only WITHIN another eligible candidate's longer title — the agreement's same
   *  concept ([[agreement.Agreement.namedWithinAnother]]), read where pooled evidence alone would take it? PL "Kacper i
   *  Emma – najlepsi przyjaciele Edukacja Młode Horyzonty" names the 2026 short "Najlepsi przyjaciele" only as part of
   *  the 2013 "Kacper i Emma: Najlepsi przyjaciele". */
  private def namedWithinAnother(best: Scored, eligible: Seq[Scored]): Boolean = eligible.sizeIs > 1 && {
    val billed = Seq(services.movies.TitleContainment.tokens(best.listing.rawTitle.getOrElse(best.listing.title)))
    def spans(film: IdentityMeasures.Film) = agreement.Agreement.spansIn(billed, Seq(film))
    agreement.Agreement.namedWithinAnother(spans(best.candidate.film), eligible.filterNot(_ eq best).map(other => spans(other.candidate.film)))
  }

  /** The eligible candidate whose record names the listing's SEASON PRODUCTION — the season and
   *  the work its title names. `None`: no candidate does. `Some(Some(x))`: `x` is the listing's
   *  film on that identity, whatever the calibrated probability (the fitted weights do not read a
   *  season yet): a season names its production as a published year names a film, and the
   *  namesakes it rules out are already denied (`ListingConstraints.seasonsApart`).
   *  `Some(None)`: two records do (two houses' stagings of one work in one season) and the
   *  listing's own facts favour neither — ambiguity, so nothing is taken, on the season or on
   *  the database's ranking. The confidence stays the calibrated probability. */
  def seasonProduction(ranked: Seq[Scored]): Option[Option[Accepted]] =
    ranked.filter(scored => !scored.denied && scored.seasonProduction).sortBy(scored => (-own(scored), scored.candidate.tmdbId)) match {
      case Seq()       => None
      case Seq(one)    => Some(Some(atOwn(one)))
      case first +: second +: _ => Some(Option.when(own(first) > own(second))(atOwn(first)))
    }

  /** What [[contradicted]] read: the year distance and the runtime delta the listing published. */
  private def contradiction(scored: Scored): String =
    (scored.number("year.distance").map(d => f"year.distance=$d%.0f") ++ scored.number("runtime.delta").map(d => f"runtime.delta=$d%.0f")).mkString(" ")

  /** The film a cluster no TMDB film was taken for FALLS BACK to, from a fallback source's pool (a `fallback`
   *  [[FamilyScope]]): the best eligible film whose record carries the listing's title as its own — its title, original
   *  or an alternative title — CORROBORATED by a fact the listing publishes, its director or its very year, contradicted by
   *  none, on a listing billing one work, with no rival so titled that its facts do not favour it over. Stricter than any
   *  TMDB rule: a fallback record has no search rank or popularity to lend, and a wrong film is worse than none (measured
   *  2026-09-30: every correct rating page for a film TMDB lacks came from a row publishing a year or a director). */
  def fallback(ranked: Seq[Scored]): Option[Scored] = {
    // on the source's own record only — one parsed from IMDb's answer always holds its principal credits — never the
    // title and year a suggestion gave before the record was filed, which no credit or running time could rule out
    val titled = eligibleOf(ranked).filter(scored => scored.category("title").exists(IdentityMeasures.Rivalling) && scored.candidate.film.directors.isDefined)
    titled.headOption.filter(best => !MultiFilmBill.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(favours(best, _)))
  }

  /** A fact the listing publishes agrees: the director it credits, or the year it gives, exactly. */
  private def corroborated(scored: Scored): Boolean =
    IdentityMeasures.sameDirector(scored.measures) || scored.number("year.distance").contains(0.0)

  /** The candidate read as its title's exact top hit, unrivalled — how a rule taking a film by another title (an IMDb
   *  suggestion, a search's only result) has its own facts weighed, the title's evidence granted. */
  private def asItsTitlesTopHit(scored: Scored): Scored = scored.copy(measures = scored.measures ++ TopHitMeasures)
  private val TopHitMeasures = Map("title" -> IdentityMeasures.Category("exact"), "search.rank" -> IdentityMeasures.Number(1),
    "rivals" -> IdentityMeasures.Number(0))

  /** A published year more than one off, or a runtime 30 minutes or more off: the listing's own facts against it. */
  private def contradicted(scored: Scored): Boolean =
    scored.number("year.distance").exists(_ > YearWindow.PublishedAdjacency) || IdentityMeasures.runtimeContradicts(scored.measures)

  /** The film whose WORK the listing's whole title is, when TMDB ranks it first, it is the only such
   *  candidate, and no other candidate is one the title names — the rest only a director's
   *  filmography reached. US venues' "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES" (×1,915 with São
   *  Paulo) is "…: Live Viewing", not the 2022 Seoul concert film its director also made; neither
   *  pipeline matched them. A work of one word is too many films' title ("It" of "It: Chapter Two").
   *  The listing may bill that work under a shorter subtitle of its own, the whole title running along
   *  the record's (DE ×140 and ES ×82 "…In Buenos Aires: Live"), when no other candidate bills the
   *  work: a banner leading the title is no work ("The Metropolitan Opera: La Fanciulla del West
   *  Encore" is not its 2018 staging). */
  private def soleWorkWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    def isWork(scored: Scored) = IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def sharesItsWork(scored: Scored) = IdentityMeasures.billsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def billsWork(scored: Scored) = scored.category("title").contains("fragment") && sharesItsWork(scored)
    for {
      work <- one(eligible.filter(scored => scored.rank.contains(1) && (isWork(scored) || billsWork(scored)) && !contradicted(scored)),
                "no first hit is the title's work", "two first hits are the title's work", firstHit(ranked))
      _    <- noneOf(eligible.find(other => (other ne work) && (other.titleNamesIt || (!isWork(work) && sharesItsWork(other)))),
                "the title names another film", work)
    } yield atOwn(work)
  }

  /** How far a billed work's running time may stand from the venue's: a broadcast's interval and introduction. */
  private val BilledRuntime = 15
  /** The one candidate whose WORK the listing bills under another subtitle, or publishes alone, by
   *  the director it credits, that no published year or runtime contradicts
   *  (`IdentityMeasures.sharesWork`, `titleIsWorkOf`, or the original title the venue publishes, `originalTitleIsWorkOf`:
   *  DE "Ein Hund namens Quill", published as "Quill" by Yōichi Sai, 2004, is his "Quill - Ein Freund für´s Leben"; three venues' "Leonas" is Cotelo's "Leonas, el
   *  instinto más salvaje", fifth in TMDB's search for the word): Multikino's "Cirque du Soleil:
   *  Kurios - Gabinet osobliwości" by Michel Laprise is his "…: KURIOS - Cabinet des curiosités".
   *  The pipeline took such hits by their director on 3,247 listings with no wrong match; two
   *  candidates fitting alike (a director's sequels of one work) are no answer. A one-word work
   *  also needs the published year, a word being many films' title. */
  private def directorsWorkWhy(ranked: Seq[Scored]): Verdict = {
    def bareWork(scored: Scored) =
      (IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film) orElse IdentityMeasures.originalTitleIsWorkOf(scored.listing, scored.candidate.film))
        .exists(words => words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))
    // Both titles bill the work under a banner ("Royal Shakespeare Company: Macbeth", "RSC Live: Macbeth") and the
    // running times agree: a staging the credited director made, not one of the word's many films — when it is the
    // only record billing that work, whatever its running time: two seasons of one staging (Oliver Mears's Tosca,
    // 2025/26 and 2026/27, the later with no credits or running time yet) are the season's to tell apart, not this rule's
    // — counting every record billing it that credits no OTHER director: a season's record often credits nobody yet.
    def bills(scored: Scored) = StageRelays.billings(scored.listing, scored.candidate.film).nonEmpty
    lazy val billedAlike = eligibleOf(ranked).count(scored => bills(scored) && !scored.category("director").contains("different"))
    def billedWork(scored: Scored) = IdentityMeasures.sameDirector(scored.measures) && bills(scored) && billedAlike == 1 &&
      IdentityMeasures.runtimeGap(scored.measures).exists(_ <= BilledRuntime)
    val found = eligibleOf(ranked).filter(scored => IdentityMeasures.sameDirector(scored.measures) &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored) || billedWork(scored)) && !contradicted(scored))
    one(withoutHollow(found), "no film of its credited director shares its work", "two films of its director share its work", credited(ranked)).map(atOwn)
  }

  /** The one eligible candidate the listing's title — or the original title its venue publishes — names EXACTLY that
   *  credits the director the listing credits, when no published year or runtime contradicts it: a title and its director
   *  name one film together, however the database's search ranks it or however few venues list it
   *  (US "Man of Iron" {Andrzej Wajda}, seventh in TMDB's search behind "Iron Man"; PL Kinoteka's "Podróżnik", published
   *  as Kiarostami's original). Two such records are no answer.
   *
   *  The original title names it only when it stands apart from the billing ([[IdentityMeasures.Listing.originalStandsApart]])
   *  and the billed title names no eligible record exactly: a ticket page that copies the original title and director of
   *  the record a search for its billing hit states nothing that record does not, so the two cannot confirm it (DE
   *  kinoheld's "Tino Bomelino" as Tino {Anna Lucía Alcalá Pérez}, "Ganes" as Ganges; Fontane Klub's "Ich bin Dein",
   *  Iram Haq's film by its title and runtime, credited to another "Jeg er din"'s director — run 38578038277). */
  private def directorsTitleWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    lazy val titled = eligible.exists(_.category("title").contains("exact"))
    def byOriginal(candidate: Scored) =
      candidate.category("originalTitle").contains("match") && candidate.listing.originalStandsApart && !titled
    one(withoutHollow(eligible.filter(candidate =>
      (candidate.category("title").contains("exact") || byOriginal(candidate)) &&
      IdentityMeasures.sameDirector(candidate.measures) && !contradicted(candidate))),
      "no film of its exact title by its credited director", "two films of its title by its director", credited(ranked)).map(atOwn)
  }

  /** The one eligible candidate the listing's title names EXACTLY from the year its title dates it
   *  (a year off at most, as a release and a premiere differ), when nothing it publishes contradicts
   *  it — a credited director included: US "Troll (1986)" is the 1986 film, though TMDB ranks "Troll 2"
   *  and a 2022 "Troll" above it ([[namedButForItsYear]]: decorated, as "… Encore (2027)", too). Two
   *  such records are no answer. */
  private def datedTitleWhy(ranked: Seq[Scored]): Verdict =
    one(eligibleOf(ranked).filter(candidate => namedButForItsYear(candidate) &&
      candidate.number("titleYear.delta").exists(FactRelations.nearDelta) && !contradicted(candidate) &&
      !candidate.category("director").contains("different")),
      "no film of its title from the year its title dates", "two films of its title from that year",
      if (ranked.exists(_.number("titleYear.delta").isDefined)) "" else "the title dates no year").map(atOwn)

  /** Is the listing's title, years aside, the candidate's own title or original title — or that
   *  title decorated along one edge ("La Fanciulla del West Encore" of the Met's "… 2026/27: La
   *  Fanciulla del West")? Asked only beside a title year that agrees ([[datedTitleWhy]]). */
  private def namedButForItsYear(candidate: Scored): Boolean = {
    // Each title's tokens as the listing and the film hold them, worked out once (`ownYearlessTitles`, `billedTitles`,
    // `billedWorks`), not per candidate weighed: ~40 MB of a PL hard-cluster run's resolves (JFR 2026-10-06).
    val titles = candidate.candidate.film.ownYearlessTitles
    // One word is many films' title — but not as a whole piece beside the year the listing dates it by, which the
    // rule requires: "Akademia Kina Polskiego: Drogówka (2012)" is Smarzowski's 2013 film.
    // A one-word title must be a whole delimited PIECE of the listing's ("…: Drogówka (2012)"), not its last word
    // ("Fanciulla Encore (2027)" is no "Encore").
    lazy val pieces = candidate.listing.billedWorks   // the shapes' yearless tokens; a one-word title is never empty
    candidate.listing.billedTitles.exists(own => own.nonEmpty &&
      titles.exists(title => title == own || (title.sizeIs >= 2 && services.movies.TitleContainment.isTokenRun(title, own)) ||
        (title.sizeIs == 1 && pieces(title))))
  }

  /** The one eligible record billing the listing's work under the listing's OWN house
   *  ([[Scored.houseProduction]]) that no published year or runtime contradicts: Flicks' "NT Live:
   *  The Misanthrope" is the National Theatre's 2026 broadcast, "National Theatre Live: The
   *  Misanthrope", though TMDB's search ranks three bare records of Molière's play above it — a
   *  house's banner names its production as a season names one ([[seasonProduction]]). Two such
   *  records (a broadcast and its encore) are no answer — nor are two records carrying one title,
   *  however each reads as billed (US "NT Live: Hamlet": the National Theatre's 2010 and 2015
   *  broadcasts) — and neither is one that another candidate
   *  the title names fits at least as well on the listing's own facts: a banner is learned, not
   *  known, and Everyman's "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day",
   *  publishing nothing but its title, is Cameron's film, not TMDB's making-of documentary. */
  private def houseProductionWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      // nor one from another year than the title dates the production: US "MetOpera: Carmen (2009)" ×513 is Eyre's
      // 2009 staging, not the Met's 2024 record its house bills
      house <- { val billed = eligible.filter(candidate => candidate.houseProduction && !contradicted(candidate))
                 one(billed.filter(_.number("titleYear.delta").forall(FactRelations.nearDelta)),
                   if (billed.isEmpty) "no record bills its work under its house" else "its house's record is from another year than its title dates",
                   "two records bill its work under its house", billed.map(Acceptance.named).mkString("; ")) }
      _     <- noneOf(eligible.find(other => (other ne house) &&
                 (IdentityMeasures.key(other.candidate.film.title) == IdentityMeasures.key(house.candidate.film.title) ||
                   (other.titleNamesIt && own(other) >= own(house)))), "another record carries its title or fits as well", house)
    } yield atOwn(house)
  }

  /** A stage broadcast billed with no season but with its year — in its field or its title: the one eligible SEASON
   *  record naming the same stage work ([[StageWorks]], in any language), billing the listing's house
   *  ([[StageRelays.bannerNamesItsHouse]]), from the very year the venue publishes, that no running time contradicts. DE
   *  "Royal Ballet & Opera im Kino: Manon" [2026] {Kenneth MacMillan} ×59 is "Royal Ballet & Opera 2026/27: Manon"
   *  (October 2026), not the Met's 2026/27 Manon (April 2027), which "MET Opera Live im Kino: Manon" [2027] ×4 is; DE
   *  "RBO live 2027: Tosca" is the RBO's 2026/27 record, not the ROH's 2011 one IMDb suggests. It pre-empts every rule
   *  as [[seasonProduction]] does ([[datedOrBroadcastSeasonRecord]]). */
  private def stageProduction(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(stageProductionWhy(ranked)).toOption
  private def stageProductionWhy(ranked: Seq[Scored]): Verdict =
    for {
      any    <- firstCandidate(ranked)
      _      <- need(any.listing.seasonYear.isEmpty, "the title names its season")
      year   <- any.listing.statedYear.orElse(StageRelays.bannerYear(any.listing)).toRight(Refused("the listing publishes no year"))
      // a year before the season its screenings fall in dates the staging's premiere, not the broadcast
      // ([[Listing.screenedSeason]]): UK Aldeburgh's "Royal Ballet: The Nutcracker" [2025], screening in the 2026/27
      // season, is no 2025/26 record's, which the season rules then tell apart
      _      <- need(any.listing.screenedSeason.forall(season => year >= season), "its year predates the season it screens in")
      works   = StageRelays.stageWorks(any.listing)
      _      <- need(works.nonEmpty, "the title names no stage work")
      itsHouse = StageRelays.bannerNamesItsHouse(any.listing)
      record <- one(eligibleOf(ranked).filter(scored => scored.candidate.film.season.isDefined &&
                  StageRelays.stageWorks(scored.candidate.film).exists(works) && itsHouse(scored.candidate.film) &&
                  scored.candidate.film.year.contains(year) && !contradicted(scored)),
                  "no season record of its work from its year", "two season records of its work from its year", s"works ${works.toSeq.sorted.mkString(",")}, $year")
    } yield atOwn(record)

  /** The one SEASON record whose title is the listing's own once its season is taken out ("&" and "and" alike), when
   *  the listing names no season: UK "Royal Ballet and Opera: Romeo and Juliet" {Kenneth MacMillan} ×127, screening
   *  May–June 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet" — the cut alone denied it, the season marker
   *  reading as a title the listing does not carry. Two seasons of the title ("…: Tosca", 2025/26 and 2026/27) are told
   *  apart by the season the listing's screenings broadcast it in ([[ofItsBroadcastSeason]]), and are no answer without
   *  one; a fact contradicting it, or another director, refuses it.
   *
   *  So is the record of the very season the listing's screenings broadcast it in that names its stage work under its
   *  house ([[StageRelays.bannerNamesItsHouse]]), however the venue shortens the banner: prod UK 2026-10-13 "MET:
   *  Macbeth" (17 October 2026), "RBO: Tosca 27" and "Macbeth (Met Opera)" are the Met's and RBO's 2026/27 records. */
  private def seasonRecord(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(seasonRecordWhy(ranked)).toOption
  /** [[seasonRecord]]'s answer when it is a record of the season the listing's screenings broadcast it in: it pre-empts
   *  every rule, as [[seasonProduction]] does a season the title names — a house's other stagings of the work (the Met's
   *  2014 "Macbeth", the Royal Ballet's 2013 "Romeo and Juliet") are other broadcasts than the one the venue screens. */
  private def broadcastSeasonRecord(ranked: Seq[Scored]): Option[Accepted] = seasonRecord(ranked).filter { case (record, _) =>
    record.listing.screenedSeason.exists(record.candidate.film.season.contains)
  }
  /** The season record a relay's own year ([[stageProduction]]) or its screenings' season ([[broadcastSeasonRecord]])
   *  names, with the rule naming it: what pre-empts the rules after [[seasonProduction]]. */
  private def datedOrBroadcastSeasonRecord(ranked: Seq[Scored]): Option[(Accepted, String)] =
    stageProduction(ranked).map(_ -> "stage-production").orElse(broadcastSeasonRecord(ranked).map(_ -> "season-record"))
  private def seasonRecordWhy(ranked: Seq[Scored]): Verdict = {
    def plain(title: String) = IdentityMeasures.key(title.replace("&", " and "))
    for {
      any    <- firstCandidate(ranked)
      listing = any.listing
      _      <- need(listing.seasonYear.isEmpty, "the title names its season")
      own     = (Seq(listing.title) ++ listing.rawTitle).map(plain).toSet
      record <- one(ofItsBroadcastSeason(ranked.filter(scored => (isEligible(scored) || scored.deniedByCutOnly) &&
                  scored.candidate.film.season.isDefined &&
                  (scored.candidate.film.titles.exists(t => own(plain(IdentityMeasures.withoutSeasons(t)))) || StageRelays.screenedSeasonRecord(listing, scored.candidate.film)))),
                  "no season record carries its title or its work under its house", "two seasons' records carry its title")
      _      <- needOf(!contradicted(record), "a published fact contradicts it", record, contradiction(record))
      // a season spans two years: a year the listing states outside them is another season's broadcast (UK Aldeburgh's
      // "The Royal Ballet: Romeo & Juliet" [2025] is no "The Royal Ballet: Romeo and Juliet (2021/22)") — but for the
      // record of the season its screenings broadcast it in, whose year it predates: that year dates the staging's premiere
      // (Aldeburgh's, screening on 25 May 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet", aired that day)
      _      <- needOf(listing.year.forall(year => record.candidate.film.season.forall(season => year == season || year == season + 1)) ||
                  StageRelays.screenedSeasonRecord(listing, record.candidate.film), "the year it states is another season's", record)
      _      <- needOf(!record.category("director").exists(Set("different", "different_script")), "it credits another director", record)
    } yield atOwn(record)
  }

  /** Of several season records of one stage work, the one season the listing's screenings broadcast it in
   *  ([[IdentityMeasures.Listing.screenedSeason]]) when exactly that season's records are among them; all of them
   *  otherwise. A relay airs on its house's published dates: UK "Royal Ballet & Opera: Swan Lake" (recording
   *  38342304766, ~30 venues billing no season or year), screening on 22 April 2027, is the 2026/27 record, not the
   *  house's 2024/25 one. */
  private def ofItsBroadcastSeason(records: Seq[Scored]): Seq[Scored] =
    records.headOption.flatMap(_.listing.screenedSeason).filter(_ => records.sizeIs > 1)
      .map(season => records.filter(record => record.candidate.film.season.contains(season)))
      .filter(_.nonEmpty).getOrElse(records)

  /** The EDITION of the accepted film that the listing's whole title names, when there is exactly
   *  one: a later record carrying the film's title under a qualifier (`IdentityMeasures.editionOf`
   *  — "Radiohead X Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu"), which the listing
   *  names by its whole title MORE as its own than it names the film ([[Acceptance.namedAs]]). The
   *  listing's facts chose the work, and an edition carries its work's facts — the venue credits
   *  Murnau, TMDB the edition's maker — so they do not deny the edition; a pin still does. With the
   *  confidence of the work. The film itself when the listing names it as its own — by its title
   *  or its own original title — or no edition, or two.
   *
   *  A work the title names only by one of its ALTERNATIVE titles, while another record carries
   *  that title as its own, is named as closely by the title as that record: TMDB files "Caligula:
   *  The Ultimate Cut" among the 1979 "Caligula"'s alternatives beside the re-cut's own record, and
   *  "Nosferatu: A Symphony of Horror" among Murnau's beside David Lee Fisher's 2023 remake. Then
   *  the record is an edition by the work's OWN titles, and the listing's credit decides: a record
   *  crediting another person than the listing's is another film, one crediting nobody else is the
   *  work's edition — its running time is the cut's own. */
  def editionNamed(ranked: Seq[Scored])(accepted: Accepted): Accepted = {
    val (work, confidence) = accepted
    val named = Acceptance.namedAs(work.category("title"))
    // The listing's own original title naming the work whole names it too ("Die Puppe", "Lalka").
    if (named == Acceptance.NamedAsItsOwn || work.category("originalTitle").contains("match")) accepted
    else ranked.filter(edition => (edition ne work) && !edition.deniedByPin && Acceptance.namedAs(edition.category("title")) > named && (
      if (named == 0) IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film)
      else IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film.copy(alternativeTitles = Nil)) && !edition.category("director").contains("different"))) match {
      case Seq(edition) => edition -> confidence
      case _            => accepted
    }
  }
}

private[identity] object Acceptance {
  /** The rule a node's alone acceptance (`aloneNamed`'s answer) took `film` by, if it took that film. */
  def ruleTaking(taken: Option[(Accepted, String)], film: Int): Option[String] =
    taken.collect { case ((scored, _), rule) if scored.candidate.tmdbId == film => rule }
  /** The condition that stopped a rule (`why`, a fixed phrase: the trace's rule id), the candidate it was weighing
   *  when it did, and what that candidate's evidence said there — the facts against it, its probability against the
   *  cut, the rival it lost to. */
  final case class Refused(why: String, film: Option[Int] = None, detail: String = "")

  /** How many times its runner-up's probability the film a no-match leans to holds ([[Acceptance.leaning]]): a tie
   *  between namesakes is no lean. */
  val LeanMargin = 1.25
  /** A candidate as a refusal names it: "1365683 Primavera (2025)". */
  def named(scored: Scored): String =
    s"${scored.candidate.tmdbId} ${scored.candidate.film.title}${scored.candidate.film.year.fold("")(year => s" ($year)")}"

  /** How much as ITS OWN a title relation names a record: by the record's own title or original
   *  title ([[NamedAsItsOwn]]), by one of the alternative titles the database files beside them,
   *  or not whole (0). */
  val NamedAsItsOwn = 2
  def namedAs(titleRelation: Option[String]): Int = titleRelation match {
    case Some("exact") | Some("original") => NamedAsItsOwn
    case Some("alternative")              => 1
    case _                                => 0
  }
}
