package services.identity

import services.cinemas.pl.NonMovieEventClassifier
import services.identity.IdentityMeasures.ListingFilm
import services.identity.Scored.Accepted

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it
 *  TAKES, and at what confidence: [[aloneNamed]] for a node on its own evidence, [[pooled]] for a
 *  cluster's pooled evidence. Each is an ordered list of RULES, every one a function of the ranked
 *  candidates alone; the first to accept decides, and the listing's season production, when a
 *  record names it, pre-empts them all ([[seasonProduction]]). A new rule is a new method here and
 *  one entry in a list. */
private[identity] final class Acceptance(private[identity] val calibration: IdentityCalibration)
    extends AcceptanceTitleRules, AcceptanceCalibratedRules, AcceptanceFactRules, AcceptanceStageRules {

  val weights = new EvidenceWeights(calibration)
  import weights.{against, fitsBetter, own, speaksAgainst}

  /** Neither denied nor reached only by IMDb's suggestion ([[Scored.suggestedOnly]]): a candidate the rules may take. */
  private[identity] def isEligible(scored: Scored): Boolean = !scored.denied && !scored.suggestedOnly
  private[identity] def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filter(isEligible)

  /** An acceptance rule, by the name an explanation gives it. Whichever condition took the film, no rule takes one whose
   *  title numbers its series OTHERWISE than the listing's ([[ownInstalment]]), nor one its title names nothing of beside
   *  another film of its director the title names, or the listing's year dates as closely ([[anotherOfItsDirector]]). */
  private[identity] final case class Rule(name: String, why: Seq[Scored] => Verdict) {
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
  private[identity] def notPerformedBeside(accepted: Accepted): Verdict = {
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
  private[identity] def onNoReusedPage(ranked: Seq[Scored])(accepted: Accepted): Verdict = {
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
  private[identity] def copiedBeyondItsBilling(scored: Scored): Boolean = {
    val listing = scored.listing
    scored.category("originalTitle").contains("match") && IdentityMeasures.sameDirector(scored.measures) && !listing.originalStandsApart &&
      !listing.year.exists(year => scored.candidate.film.year.exists(film => math.abs(film - year) <= 1)) && billedBeyond(scored).nonEmpty
  }
  /** The billing's words none of the record's titles carries — none when a whole piece of the billing, or the search its
   *  title rules leave of it, is the record's word for word ("Twisters" of "(4DX Rewind) Twisters", "Endgame" of
   *  "Vengadores: Endgame (Reestreno)", "Moje własne Idaho" of "… + DEBATA [POKAZ BEZPŁATNY]"): the rest decorates it. */
  private[identity] def billedBeyond(scored: Scored): Seq[String] = {
    val listing   = scored.listing
    val filmWords = scored.candidate.film.titles.flatMap(IdentityMeasures.countedWords).toSet
    val pieces    = IdentityMeasures.shapes(listing.ownTitles ++ listing.searchTitles, listing.decorations).map(IdentityMeasures.countedWords).filter(_.nonEmpty)
    if (pieces.exists(_.forall(filmWords))) Nil else IdentityMeasures.countedWords(listing.title).filterNot(filmWords)
  }
  private[identity] def pickedByItsTitle(ranked: Seq[Scored])(accepted: Accepted): Verdict =
    noneOf(anotherOfItsDirector(accepted._1, eligibleOf(ranked)), "its title names nothing of it, and another film of its director it names or dates as closely stands beside it",
      accepted._1).map(_ => accepted)
  /** Refuses an UNTAKEN film ([[Scored.untaken]]) whichever rule took it: it stays a rival, never a take. */
  private[identity] def takeable(accepted: Accepted): Verdict =
    accepted._1.untaken.fold[Verdict](Right(accepted))(why => Left(Acceptance.Refused("untaken", Some(accepted._1.candidate.tmdbId), why)))
  /** Refuses a film both titles number, differently: PL Cinema City's and UK Odeon's "League of Legends Worlds 26 |
   *  Finals in Cinema" is not TMDB's "League of Legends Worlds25 - Finals in Cinema", its only search hit. A number only
   *  one side carries is weaker — TMDB files a two-part film's halves by their subtitles ("De Gaulle: Liberté" is UK
   *  "De Gaulle: Liberté Part 2") — and stays the rules' own to weigh ([[imdbSuggestedWhy]], [[soleResultWhy]]). */
  private[identity] def ownInstalment(verdict: Verdict): Verdict = verdict.flatMap { accepted =>
    val scored = accepted._1
    needOf(!scored.category("numeral").contains("different"), "the title numbers another instalment", scored,
      s"numeral=${scored.category("numeral").getOrElse("")}").map(_ => accepted)
  }
  private[identity] def otherInstalment(scored: Scored) = scored.category("numeral").exists(IdentityMeasures.OtherInstalment)
  /** Refuses a film the title numbers as ANOTHER instalment than its own ([[IdentityMeasures.OtherInstalment]]). */
  private[identity] def notAnotherInstalment(scored: Scored): Either[Acceptance.Refused, Unit] =
    needOf(!otherInstalment(scored), "the title numbers another instalment", scored, s"numeral=${scored.category("numeral").getOrElse("")}")

  /** A rule's verdict: the film it accepts, or the first of its conditions that refused — the identity trace's
   *  `refused:<rule>:<why>`, so a listing no rule took says which condition stopped each — with the candidate the
   *  rule was weighing when it did and what that candidate's evidence said ([[Acceptance.Refused]]). */
  private[identity] type Verdict = Either[Acceptance.Refused, Accepted]
  import Acceptance.Refused
  private[identity] def need(holds: Boolean, refusal: => String): Either[Refused, Unit] = if (holds) Right(()) else Left(Refused(refusal))
  /** [[need]], naming the candidate it refused and why its evidence did not hold. */
  private[identity] def needOf(holds: Boolean, refusal: => String, about: Scored, detail: => String = ""): Either[Refused, Unit] =
    if (holds) Right(()) else Left(Refused(refusal, Some(about.candidate.tmdbId), detail))
  /** No candidate of `found` may stand: the first that does refuses `about`, named in the detail. */
  private[identity] def noneOf(found: Option[Scored], refusal: => String, about: Scored): Either[Refused, Unit] =
    found.fold[Either[Refused, Unit]](Right(()))(other => Left(Refused(refusal, Some(about.candidate.tmdbId), Acceptance.named(other))))
  /** `found` without its HOLLOW records — no year, no running time — when it holds a full one beside them: TMDB's empty
   *  duplicate of a film ("Solo", 1450958, beside Sophie Dupuis's 2023 "Solo") is no second film of its director. */
  private[identity] def withoutHollow(found: Seq[Scored]): Seq[Scored] = {
    def hollow(scored: Scored) = scored.candidate.film.year.isEmpty && scored.candidate.film.runtime.forall(_ <= 0)
    val full = found.filterNot(hollow)
    if (full.nonEmpty && full.size < found.size) full else found
  }
  private[identity] def one(found: Seq[Scored], none: => String, many: => String, noneDetail: => String = ""): Either[Refused, Scored] = found match {
    case Seq(only) => Right(only)
    case Seq()     => Left(Refused(none, None, noneDetail))
    case _         => Left(Refused(many, None, found.map(Acceptance.named).mkString("; ")))
  }
  /** What a listing's facts leave a director's rule to go on: its credit, or that it credits nobody. */
  private[identity] def credited(ranked: Seq[Scored]): String =
    ranked.headOption.map(_.listing.directors).filter(_.nonEmpty).fold("the listing credits no director")(names => s"credits ${names.mkString(", ")}")
  private[identity] def firstHit(ranked: Seq[Scored]): String = ranked.find(_.rank.contains(1)).fold("its search returned nothing")(hit => s"first hit ${Acceptance.named(hit)}")
  /** The first-ranked candidate, the listing every rule reads off it — refused when there is none. */
  private[identity] def firstCandidate(ranked: Seq[Scored]): Either[Refused, Scored] = ranked.headOption.toRight(Refused("no candidate"))
  /** Refuses a listing billing two works ([[MultiFilmBill.billsTwoWorks]]): a double bill is neither film. */
  private[identity] def notADoubleBill(listing: IdentityMeasures.Listing): Either[Refused, Unit] = need(!MultiFilmBill.billsTwoWorks(listing), "a double bill")
  /** Refuses a film the listing's title joins another work to by a slash ([[IdentityMeasures.joinsAnotherWork]]): a bill
   *  of two works is neither, as a "+" one is. */
  private[identity] def notJoinedToAnotherWork(listing: IdentityMeasures.Listing, scored: Scored): Either[Refused, Unit] =
    needOf(!IdentityMeasures.joinsAnotherWork(listing, scored.candidate.film), "the title joins another work to it", scored)
  /** `scored` accepted at its own probability. */
  private[identity] def atOwn(scored: Scored): Accepted = scored -> scored.probability
  private[identity] def cut(probability: Double) = f"${probability * 100}%.1f%% < ${calibration.ratingCut * 100}%.1f%%"
  /** `scored` at what its evidence CLASS measured (`IdentityCalibration.classProbability`), lending and never withdrawing —
   *  its class read with the namesakes the release veto took away counted as no rivals ([[ReleaseVeto.unvetoed]]). */
  private[identity] def classAccepted(scored: Scored, measures: Map[String, IdentityMeasures.Measure]): Verdict =
    calibration.classProbability(ListingFilm, ReleaseVeto.unvetoed(scored, measures)).toRight(Refused("no evidence class measured for it", Some(scored.candidate.tmdbId)))
      .flatMap { classProbability =>
        val lent = math.max(scored.probability, classProbability)
        Either.cond(calibration.showsRatings(lent), scored -> lent, Refused("below the rating cut", Some(scored.candidate.tmdbId), cut(lent)))
      }

  /** What a rule taking `scored` asks last: nothing it publishes speaks against it as `weighed` (the candidate read as
   *  the rule reads its title), no other of `eligible` fits `weighed`'s facts better, and its evidence class clears the
   *  cut ([[classAccepted]]). */
  private[identity] def unopposed(scored: Scored, weighed: Scored, eligible: Seq[Scored]): Verdict =
    for {
      _     <- needOf(!speaksAgainst(weighed), "a published fact weighs against it", scored, against(weighed).mkString(" "))
      _     <- noneOf(eligible.find(rival => (rival ne scored) && fitsBetter(rival, weighed)), "a rival fits its facts better", scored)
      taken <- classAccepted(scored, weighed.measures)
    } yield taken

  /** The first of `rules` to accept, with its name. */
  private[identity] def firstOf(ranked: Seq[Scored], rules: Seq[Rule]): Option[(Accepted, String)] =
    rules.iterator.map(rule => rule.accepts(ranked).map(_ -> rule.name)).collectFirst { case Right(taken) => taken }

  /** Why each rule a node may be taken by alone refused it: `(rule, the first condition that stopped it)`. */
  def refusals(ranked: Seq[Scored]): Seq[DecisionTrace.Refusal] =
    if (billsSeveralWorks(ranked)) Seq(DecisionTrace.Refusal("alone", "bills several works"))
    else aloneRules.flatMap(rule => rule.accepts(ranked).left.toOption.map(refused =>
      DecisionTrace.Refusal(rule.name, refused.why, refused.film, refused.detail)))

  private[identity] val aloneRules = Seq(Rule("sole-work", soleWorkWhy), Rule("favoured-calibrated", favouredCalibratedWhy), Rule("exact-top-hit", topHitWhy),
    Rule("segment-top-hit", segmentTopHitWhy), Rule("sole-result", soleResultWhy), Rule("imdb-suggested", imdbSuggestedWhy),
    Rule("directors-work", directorsWorkWhy), Rule("directors-title", directorsTitleWhy), Rule("dated-title", datedTitleWhy),
    Rule("house-production", houseProductionWhy), Rule("stage-production", stageProductionWhy), Rule("season-record", seasonRecordWhy))

  private[identity] val pooledRules = Seq(Rule("unrivalled-calibrated", unrivalledCalibratedWhy), Rule("exact-top-hit", topHitWhy),
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
  private[identity] def outweighedBySuggestion(ranked: Seq[Scored], taken: Scored): Boolean =
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
  private[identity] def slashesTwoWorks(listing: IdentityMeasures.Listing, ranked: Seq[Scored]): Boolean = {
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
  private[identity] val SlashBreak = """\s+/\s+""".r

  /** The one film a marathon billing one title names ([[MultiFilmBill.billedOne]]): the one eligible candidate titled
   *  exactly so, when no eligible candidate's title extends it — PL "Inna Mamusia - maraton" is Inna mamusia (2026);
   *  "Władca Pierścieni - maraton" bills Jackson's films, which extend Bakshi's "Władca Pierścieni". */
  private[identity] def billedAlone(ranked: Seq[Scored]): Option[Int] =
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
  private[identity] def markerOf(ranked: Seq[Scored]): Option[String] =
    ranked.headOption.flatMap(_.listing.billMarker)

  /** Is `taken` no film of a programme the listing bills by a word — the word its own title's, if the listing has one? */
  private[identity] def takesNoBill(ranked: Seq[Scored])(taken: (Accepted, String)): Boolean =
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
  private[identity] def takenBy(ranked: Seq[Scored], keep: ((Accepted, String)) => Boolean = _ => true)
                     (rules: => Option[(Accepted, String)]): Option[(Accepted, String)] =
    if (billsSeveralWorks(ranked)) None
    else seasonProduction(ranked).map(_.map(_ -> "season-production")).getOrElse(rules).filter(keep).filter(_._1._1.untaken.isEmpty)
      .filter(clearOfItsRunnerUp(ranked))
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }.filter(takesNoBill(ranked))

  /** EXPERIMENT `experiment/margin-signal` (not landed — docs/identity-experiments.md): margin as a REFUSAL. A take
   *  whose best other eligible candidate stands within [[Acceptance.MarginRefusalRatio]] times its probability is
   *  refused — unless ([[Acceptance.MarginExemptWhenFavoured]]) the listing's own facts favour it over that rival
   *  ([[EvidenceWeights.favours]]) — for the rules [[Acceptance.MarginRefusedRules]] names (every rule when empty). */
  private[identity] def clearOfItsRunnerUp(ranked: Seq[Scored])(taken: (Accepted, String)): Boolean = {
    val ((best, _), rule) = taken
    !(Acceptance.MarginRefusedRules.isEmpty || Acceptance.MarginRefusedRules(rule)) ||
      eligibleOf(ranked).filterNot(_ eq best).maxByOption(_.probability).forall(rival =>
        best.probability >= Acceptance.MarginRefusalRatio * rival.probability ||
          (Acceptance.MarginExemptWhenFavoured && weights.favours(best, rival)))
  }

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
  /** EXPERIMENT `experiment/margin-signal`: [[Acceptance.clearOfItsRunnerUp]]'s ratio, exemption and rules. */
  val MarginRefusalRatio = 1.5
  val MarginExemptWhenFavoured = true
  val MarginRefusedRules: Set[String] = Set.empty
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
