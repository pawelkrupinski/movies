package services.identity

import services.identity.IdentityMeasures.ListingFilm
import services.identity.Scored.Accepted
import services.resolution.YearWindow

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it
 *  TAKES, and at what confidence: [[alone]] for a node on its own evidence, [[pooled]] for a
 *  cluster's pooled evidence. Each is an ordered list of RULES, every one a function of the ranked
 *  candidates alone; the first to accept decides, and the listing's season production, when a
 *  record names it, pre-empts them all ([[seasonProduction]]). A new rule is a new method here and
 *  one entry in a list. */
private[identity] final class Acceptance(calibration: IdentityCalibration) {

  val weights = new EvidenceWeights(calibration)
  import weights.{against, factsAnswered, favours, facts, fitsBetter, own, priorsLent, speaksAgainst}

  private def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filterNot(scored => scored.denied || scored.suggestedOnly)

  /** A rule, by the name an explanation gives it. */
  private final case class Rule(name: String, accepts: Seq[Scored] => Verdict)

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
  private def cut(probability: Double) = f"${probability * 100}%.1f%% < ${calibration.ratingCut * 100}%.1f%%"
  /** `scored` at what its evidence CLASS measured (`IdentityCalibration.classProbability`), lending and never withdrawing. */
  private def classAccepted(scored: Scored, measures: Map[String, IdentityMeasures.Measure]): Verdict =
    calibration.classProbability(ListingFilm, measures).toRight(Refused("no evidence class measured for it", Some(scored.candidate.tmdbId)))
      .flatMap { classProbability =>
        val lent = math.max(scored.probability, classProbability)
        Either.cond(calibration.showsRatings(lent), scored -> lent, Refused("below the rating cut", Some(scored.candidate.tmdbId), cut(lent)))
      }

  /** The first of `rules` to accept, with its name. */
  private def firstOf(ranked: Seq[Scored], rules: Seq[Rule]): Option[(Accepted, String)] =
    rules.iterator.map(rule => rule.accepts(ranked).map(_ -> rule.name)).collectFirst { case Right(taken) => taken }

  /** Why each rule a node may be taken by alone refused it: `(rule, the first condition that stopped it)`. */
  def refusals(ranked: Seq[Scored]): Seq[DecisionTrace.Refusal] =
    if (billsBothItsWorks(ranked)) Seq(DecisionTrace.Refusal("alone", "bills two works"))
    else aloneRules.flatMap(rule => rule.accepts(ranked).left.toOption.map(refused =>
      DecisionTrace.Refusal(rule.name, refused.why, refused.film, refused.detail)))

  private val aloneRules = Seq(Rule("sole-work", soleWorkWhy), Rule("favoured-calibrated", favouredCalibratedWhy), Rule("exact-top-hit", topHitWhy),
    Rule("segment-top-hit", segmentTopHitWhy), Rule("sole-result", soleResultWhy), Rule("imdb-suggested", imdbSuggestedWhy),
    Rule("directors-work", directorsWorkWhy), Rule("directors-title", directorsTitleWhy), Rule("dated-title", datedTitleWhy),
    Rule("house-production", houseProductionWhy), Rule("stage-production", stageProductionWhy), Rule("season-record", seasonRecordWhy))

  /** What [[alone]] takes, with the rule that took it. A family's scope asks it once per node ([[FamilyScope.takenAlone]]). */
  def aloneNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    if (billsBothItsWorks(ranked)) None
    else seasonProduction(ranked).map(_.map(_ -> "season-production")).getOrElse(firstOf(ranked, aloneRules))
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }

  /** A DOUBLE BILL whose title names two eligible films, its facts ruling out neither — or two whole works
   *  ([[IdentityMeasures.billsTwoWholeWorks]]), whatever is held of them: it is neither film.
   *  UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw} ×133 credits
   *  both films' directors, each piece found its own film first, and the two scored 91–93% — which one it
   *  took was popularity's coin. A bill whose facts rule one out is neither film all the same (user rule: a
   *  double programme matches neither); a film whose own whole title it is ("Romeo + Juliet") is no bill. */
  def billsBothItsWorks(ranked: Seq[Scored]): Boolean = ranked.headOption.exists { any =>
    val eligible = eligibleOf(ranked)
    val works    = eligible.filter(c => c.titleNamesIt && !contradicted(c) && !c.category("director").contains("different"))
      .map(c => IdentityMeasures.yearlessTokens(c.candidate.film.title)).distinct
    // the billed second work is a whole film title, not a talk ("+ prelekcja", "+ spotkanie z reżyserem …")
    val billed   = IdentityMeasures.billedSecondWork(any.listing)
    IdentityMeasures.billsTwoWorks(any.listing) && !eligible.exists(_.category("title").contains("exact")) && (
      // two words at least: one is many films' title ("+ SPOTKANIE" is a talk, though TMDB holds three "Spotkanie"s)
      works.sizeIs >= 2 && billed.exists(work => work.sizeIs >= 2 && works.contains(work)) ||
      // or two whole works, one of which no database holds (user rule: a double programme is neither film)
      IdentityMeasures.billsTwoWholeWorks(any.listing))
  }

  /** A node accepts a film ON ITS OWN only when its own facts favour it over the runner-up: a
   *  bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not
   *  decided alone — it follows the film its title's credited siblings chose (the cluster's), or
   *  the pooled vote — unless it is the listing's exact top hit, which is measured as a class. */
  def alone(ranked: Seq[Scored]): Option[Accepted] = aloneNamed(ranked).map(_._1)

  /** The rule [[alone]] took `film` by, for the decision's explanation — every own match names the
   *  rule that took it, however far under the calibrated cut its probability stands. */
  def acceptedBy(ranked: Seq[Scored], film: Int): Option[String] = Acceptance.ruleTaking(aloneNamed(ranked), film)

  /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
   *  best eligible candidate the calibration accepts that no namesake out-fits ([[unrivalledCalibratedWhy]]).
   *  Each the edition of it the listing names, if any ([[editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] = pooledNamed(ranked).map(_._1)

  /** [[pooled]], with the rule that accepted — for the decision's trace. */
  def pooledNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    if (billsBothItsWorks(ranked)) None
    else seasonProduction(ranked).map(_.map(_ -> "season-production"))
      .getOrElse(firstOf(ranked, Seq(Rule("unrivalled-calibrated", unrivalledCalibratedWhy), Rule("exact-top-hit", topHitWhy), Rule("imdb-suggested", imdbSuggestedWhy))))
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }

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
   *  class, or the ranking priors lending ([[EvidenceWeights.priorsLent]]). */
  def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double): String =
    if (confidence <= scored.probability) ""
    else if (topHit(ranked).exists(_._1.candidate.tmdbId == scored.candidate.tmdbId)) " as its exact top hit"
    else " with the ranking priors lending, never withdrawing"

  // ── the rules ──────────────────────────────────────────────────────────────────────────

  /** The listing's EXACT TOP HIT, accepted on what the calibration measured for its evidence as a
   *  CLASS (`IdentityCalibration.classProbability`): the one film its whole title names exactly
   *  that its own title search returned first, in TMDB's order ([[IdentityMeasures.exactTopHits]]),
   *  when the listing's own evidence rules it out on nothing, published nothing against it, and
   *  gives no rival a better fit ([[EvidenceWeights.fitsBetter]]). A bare title credits it with the
   *  naive-Bayes sum of missing facts, a low popularity and its same-titled rivals, which undersells
   *  what the class measured on the labels; here the search standing LENDS confidence and never
   *  withdraws it. */
  def topHit(ranked: Seq[Scored]): Option[Accepted] = topHitWhy(ranked).toOption
  private def topHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any  <- ranked.headOption.toRight(Refused("no candidate"))
      id   <- IdentityMeasures.exactTopHits(any.listing, ranked.map(scored => (scored.candidate.tmdbId, scored.candidate.film, scored.rank))) match {
                case Seq(only) => Right(only)
                case Seq()     => Left(Refused("no film its whole title names is its search's first hit"))
                case ids       => Left(Refused("two exact top hits", None, ids.mkString("; ")))
              }
      best <- eligible.find(_.candidate.tmdbId == id).toRight(Refused("its exact top hit is denied", Some(id),
                ranked.find(_.candidate.tmdbId == id).flatMap(_.denial).getOrElse("")))
      _    <- needOf(!speaksAgainst(best), "a published fact weighs against it", best, against(best).mkString(" "))
      _    <- noneOf(eligible.find(rival => (rival ne best) && fitsBetter(rival, best)), "a rival fits its facts better", best)
      taken <- classAccepted(best, best.measures)
    } yield taken
  }

  /** A programme listing's EXACT TOP HIT once its banner is off: the one candidate whose title is a whole
   *  delimited piece of the listing's (`segment`) and which the listing's own search returned FIRST, when
   *  the title names no other candidate at all (the rest is a banner, not a second film), it bills no two
   *  works, and nothing it publishes speaks against it or fits a rival better. The piece IS a bare title,
   *  so it is credited with what the exact-top-hit class measured for bare titles. PL's "DZIEŃ KINA POLSKIEGO:
   *  Przepraszam, czy tu biją" and ~30 programme listings like it sat at 28.9% with no rule to take them. */
  def segmentTopHit(ranked: Seq[Scored]): Option[Accepted] = segmentTopHitWhy(ranked).toOption
  private def segmentTopHitWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any    <- ranked.headOption.toRight(Refused("no candidate"))
      // a one-word piece names no film here, rival or taken: "Inna Mamusia - maraton" is no "Maraton"
      scored <- one(ranked.filter(scored => scored.titleNamesIt && (scored.category("title").exists(IdentityMeasures.Rivalling) ||
                  IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film))), "the title names no film", "the title names two films")
      _      <- needOf(!scored.denied, "the film it names is denied", scored, scored.denial.getOrElse(""))
      _      <- needOf(scored.rank.contains(1), "not its search's first hit", scored, scored.rank.fold("not ranked")(rank => s"ranked $rank"))
      _      <- needOf(scored.category("title").contains("segment"), "not named by a piece of the title", scored,
                  s"title=${scored.category("title").getOrElse("absent")}")
      _      <- need(!IdentityMeasures.billsTwoWorks(any.listing), "a double bill")
      _      <- needOf(IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film), "its piece cannot stand for the whole title", scored)
      // a year the title states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      _      <- needOf(scored.number("titleYear.delta").forall(delta => math.abs(delta) <= YearWindow.PublishedAdjacency), "the title dates another year",
                  scored, scored.number("titleYear.delta").fold("")(delta => f"titleYear.delta=$delta%.0f"))
      // Read as the bare title its piece is: the banner beside it is no evidence against the film.
      bare    = scored.copy(measures = scored.measures + ("title" -> IdentityMeasures.Category("exact")))
      _      <- needOf(!speaksAgainst(bare), "a published fact weighs against it", scored, against(bare).mkString(" "))
      _      <- noneOf(eligible.find(rival => (rival ne scored) && fitsBetter(rival, bare)), "a rival fits its facts better", scored)
      taken  <- classAccepted(scored, bare.measures)
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
  def imdbSuggested(ranked: Seq[Scored]): Option[Accepted] = imdbSuggestedWhy(ranked).toOption
  private def imdbSuggestedWhy(ranked: Seq[Scored]): Verdict = ranked.headOption.toRight(Refused("no candidate")).flatMap { any =>
    val eligible  = ranked.filterNot(_.denied)
    val suggested = eligible.filter(scored => scored.imdb.isDefined || scored.imdbTitled.nonEmpty)
    def sameDirector(scored: Scored) = scored.category("director").contains("same_person")
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
        IdentityMeasures.takesImdbTitle(scored.listing, scored.candidate.film)
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
    def otherInstalment(scored: Scored) = scored.category("numeral").exists(IdentityMeasures.OtherInstalment)
    for {
      _      <- need(suggested.nonEmpty, "IMDb suggests none of its candidates")
      _      <- need(!IdentityMeasures.billsTwoWorks(any.listing), "a double bill")
      scored <- one(suggested.filter(rung), "no IMDb suggestion is taken by its year, its director or as the only one", "two IMDb suggestions qualify")
      _      <- noneOf(eligible.find(searchNamesAnother(scored, _)), "TMDB ranks another film the title names above it", scored)
      _      <- needOf(!otherInstalment(scored), "the title numbers another instalment", scored, s"numeral=${scored.category("numeral").getOrElse("")}")
      titled  = scored.copy(measures = scored.measures ++ Map("title" -> IdentityMeasures.Category("exact"),
                  "search.rank" -> IdentityMeasures.Number(1), "rivals" -> IdentityMeasures.Number(0)))
      _      <- needOf(!speaksAgainst(titled), "a published fact weighs against it", scored, against(titled).mkString(" "))
      _      <- noneOf(eligible.find(rival => (rival ne scored) && fitsBetter(rival, titled)), "a rival fits its facts better", scored)
      taken  <- classAccepted(scored, titled.measures)
    } yield taken
  }

  /** The ONLY film one of the listing's own title searches returned — the old pipeline's `searchUnique`, which
   *  took it unless a fact CONTRADICTED it: "Loving Karma" [78′] is TMDB's 85-minute record, the one "Loving
   *  Karma" finds, though seven minutes weigh against it in the probability. The title must name it — a piece
   *  standing for the whole ([[IdentityMeasures.standsForTheWhole]]: not "Bhutan" of "Bhutan – Trails of
   *  Happiness") — no other candidate the title names may stand beside it, and a double bill is neither film. */
  def soleResult(ranked: Seq[Scored]): Option[Accepted] = soleResultWhy(ranked).toOption
  /** Is every word of the listing's whole title one of the film's title's — two words at least, or the film's title
   *  before its dash or colon — as the old pipeline took a whole title's only search result: "Dzień Dziecka księdza
   *  Kaczkowskiego" is "Dzień Dziecka księdza Jana Kaczkowskiego", "TAFITI" is "Tafiti – Ab durch die Wüste". */
  private def wordsOfIts(scored: Scored): Boolean = {
    val own = IdentityMeasures.yearlessTokens(scored.listing.title)
    own.nonEmpty && (Seq(scored.candidate.film.title) ++ scored.candidate.film.originalTitle).exists { title =>
      own.forall(IdentityMeasures.yearlessTokens(title).contains) &&
        (own.sizeIs >= 2 || IdentityMeasures.yearlessTokens(MainTitleBreak.split(title).head) == own)
    }
  }
  /** Denied only by the probability cut — no learned rule, no pin — while every word of the listing's title is the
   *  film's: the cut is then the title's reading of a reordered banner, not a fact against it. ES Ocine's "Manon
   *  (BALLET LIVE)" ×7 is TMDB's "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE", the one film its search returns, denied
   *  at 2.4% for its original title "Manon" reading as a fragment of that. */
  private def cutOnly(scored: Scored): Boolean =
    !scored.suggestedOnly && scored.deniedByCutOnly && wordsOfIts(scored)
  private val MainTitleBreak = java.util.regex.Pattern.compile("""\s+[-–—]\s+|:\s""")
  private def soleResultWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      any   <- ranked.headOption.toRight(Refused("no candidate"))
      sole  <- one(ranked.filter(scored => scored.soleResult && (eligible.contains(scored) || cutOnly(scored))),
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
      _     <- need(!IdentityMeasures.billsTwoWorks(any.listing), "a double bill")
      _     <- needOf(sole.category("title").exists(IdentityMeasures.Rivalling) || wordsOfIts(sole) ||
                   IdentityMeasures.standsForTheWhole(any.listing, sole.candidate.film),
                 "its piece cannot stand for the whole title", sole)
      // a film the title names by fewer of its words is no rival to one carrying them all ("Manon" beside "BALLET LIVE.
      // MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)")
      _     <- noneOf(eligible.find(other => (other ne sole) && other.titleNamesIt && !(wordsOfIts(sole) && !wordsOfIts(other))),
                 "the title names another film", sole)
      _     <- needOf(!sole.category("numeral").exists(IdentityMeasures.OtherInstalment), "the title numbers another instalment", sole,
                 s"numeral=${sole.category("numeral").getOrElse("")}")
      titled = sole.copy(measures = sole.measures ++ Map("title" -> IdentityMeasures.Category("exact"),
                 "search.rank" -> IdentityMeasures.Number(1), "rivals" -> IdentityMeasures.Number(0)))
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

  /** The film a node's evidence LEANS to though no rule took it: the best eligible candidate on a listing billing one
   *  work, at [[LeanMargin]] times the runner-up's probability — every fact the listing publishes weighed in it, the
   *  priors lending — and no closer record. Not the runner-up rule's facts alone: a bare title's "facts" are the
   *  records' missing credits (US "Lady Frankenstein", 35.8% beside 2025's "Frankenstein" at 4.0%, lost on its record
   *  crediting a director the listing does not name). Not a match: the film a no-match's card may keep the ratings of, when an
   *  earlier answer gave the card that film's ([[IdentityProjectionPlan]]). PL "Tatarak" read 18.4% for Wajda's 2009
   *  film, its 1965 namesake 3.3%; "Lalka" leans to the 2026 film at 68.1%, not the 1968 one the old pipeline rated
   *  it as; "Bolek i Lolek" (28.9% beside "Reksio" at 28.9%) and the 1986 and 2025 "Caravaggio" lean to neither. */
  def leaning(ranked: Seq[Scored]): Option[Scored] =
    Option.when(ranked.headOption.forall(any => !IdentityMeasures.billsTwoWorks(any.listing)) && !billsBothItsWorks(ranked))(eligibleOf(ranked))
      .flatMap(eligible => eligible.headOption.filter(best => eligible.lift(1).forall(runnerUp =>
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
  private def unrivalledCalibratedWhy(ranked: Seq[Scored]): Verdict =
    for {
      taken <- calibratedWhy(ranked)
      best   = taken._1
      _     <- noneOf(closerThan(best, eligibleOf(ranked)), "its title sits inside a closer record", best)
      _     <- noneOf(eligibleOf(ranked).find(rival => (rival ne best) && {
                 val pieces = IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)
                 val alike  = pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.candidate.film)
                 val apart  = IdentityMeasures.namedApart(best.listing, best.candidate.film, rival.candidate.film)
                 (alike && facts(rival) > facts(best)) || (apart && facts(rival) >= facts(best))
               }), "a film the title names alike fits its facts better", best)
    } yield taken

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
      case Seq(one)    => Some(Some(one -> one.probability))
      case first +: second +: _ => Some(Option.when(own(first) > own(second))(first -> first.probability))
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
    titled.headOption.filter(best => !IdentityMeasures.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(favours(best, _)))
  }

  /** A fact the listing publishes agrees: the director it credits, or the year it gives, exactly. */
  private def corroborated(scored: Scored): Boolean =
    IdentityMeasures.sameDirector(scored.measures) || scored.number("year.distance").contains(0.0)

  /** A published year more than one off, or a runtime 30 minutes or more off: the listing's own facts against it. */
  def contradicted(scored: Scored): Boolean =
    scored.number("year.distance").exists(_ > YearWindow.PublishedAdjacency) || scored.number("runtime.delta").exists(_ >= IdentityMeasures.RuntimeContradiction)

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
    } yield work -> work.probability
  }

  /** The one candidate whose WORK the listing bills under another subtitle, or publishes alone, by
   *  the director it credits, that no published year or runtime contradicts
   *  (`IdentityMeasures.sharesWork`, `titleIsWorkOf`, or the original title the venue publishes, `originalTitleIsWorkOf`:
   *  DE "Ein Hund namens Quill", published as "Quill" by Yōichi Sai, 2004, is his "Quill - Ein Freund für´s Leben"; three venues' "Leonas" is Cotelo's "Leonas, el
   *  instinto más salvaje", fifth in TMDB's search for the word): Multikino's "Cirque du Soleil:
   *  Kurios - Gabinet osobliwości" by Michel Laprise is his "…: KURIOS - Cabinet des curiosités".
   *  The pipeline took such hits by their director on 3,247 listings with no wrong match; two
   *  candidates fitting alike (a director's sequels of one work) are no answer. A one-word work
   *  also needs the published year, a word being many films' title. */
  def directorsWork(ranked: Seq[Scored]): Option[Accepted] = directorsWorkWhy(ranked).toOption
  /** How far a billed work's running time may stand from the venue's: a broadcast's interval and introduction. */
  private val BilledRuntime = 15
  private def directorsWorkWhy(ranked: Seq[Scored]): Verdict = {
    def bareWork(scored: Scored) =
      (IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film) orElse IdentityMeasures.originalTitleIsWorkOf(scored.listing, scored.candidate.film))
        .exists(words => words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))
    // Both titles bill the work under a banner ("Royal Shakespeare Company: Macbeth", "RSC Live: Macbeth") and the
    // running times agree: a staging the credited director made, not one of the word's many films — when it is the
    // only record billing that work, whatever its running time: two seasons of one staging (Oliver Mears's Tosca,
    // 2025/26 and 2026/27, the later with no credits or running time yet) are the season's to tell apart, not this rule's
    // — counting every record billing it that credits no OTHER director: a season's record often credits nobody yet.
    def bills(scored: Scored) = IdentityMeasures.billings(scored.listing, scored.candidate.film).nonEmpty
    lazy val billedAlike = eligibleOf(ranked).count(scored => bills(scored) && !scored.category("director").contains("different"))
    def billedWork(scored: Scored) = IdentityMeasures.sameDirector(scored.measures) && bills(scored) && billedAlike == 1 &&
      scored.number("runtime.delta").exists(_ <= BilledRuntime)
    val found = eligibleOf(ranked).filter(scored => IdentityMeasures.sameDirector(scored.measures) &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored) || billedWork(scored)) && !contradicted(scored))
    one(withoutHollow(found), "no film of its credited director shares its work", "two films of its director share its work", credited(ranked)).map(f => f -> f.probability)
  }

  /** The one eligible candidate the listing's title names EXACTLY that credits the director the
   *  listing credits, when no published year or runtime contradicts it: a title and its director
   *  name one film together, however the database's search ranks it or however few venues list it
   *  (US "Man of Iron" {Andrzej Wajda}, seventh in TMDB's search behind "Iron Man"). Two such
   *  records are no answer. */
  def directorsTitle(ranked: Seq[Scored]): Option[Accepted] = directorsTitleWhy(ranked).toOption
  private def directorsTitleWhy(ranked: Seq[Scored]): Verdict =
    one(withoutHollow(eligibleOf(ranked).filter(candidate => candidate.category("title").contains("exact") &&
      IdentityMeasures.sameDirector(candidate.measures) && !contradicted(candidate))),
      "no film of its exact title by its credited director", "two films of its title by its director", credited(ranked)).map(f => f -> f.probability)

  /** The one eligible candidate the listing's title names EXACTLY from the year its title dates it
   *  (a year off at most, as a release and a premiere differ), when nothing it publishes contradicts
   *  it — a credited director included: US "Troll (1986)" is the 1986 film, though TMDB ranks "Troll 2"
   *  and a 2022 "Troll" above it ([[namedButForItsYear]]: decorated, as "… Encore (2027)", too). Two
   *  such records are no answer. */
  private def datedTitleWhy(ranked: Seq[Scored]): Verdict =
    one(eligibleOf(ranked).filter(candidate => namedButForItsYear(candidate) &&
      candidate.number("titleYear.delta").exists(delta => math.abs(delta) <= YearWindow.PublishedAdjacency) && !contradicted(candidate) &&
      !candidate.category("director").contains("different")),
      "no film of its title from the year its title dates", "two films of its title from that year",
      if (ranked.exists(_.number("titleYear.delta").isDefined)) "" else "the title dates no year").map(f => f -> f.probability)

  /** Is the listing's title, years aside, the candidate's own title or original title — or that
   *  title decorated along one edge ("La Fanciulla del West Encore" of the Met's "… 2026/27: La
   *  Fanciulla del West")? Asked only beside a title year that agrees ([[datedTitleWhy]]). */
  private def namedButForItsYear(candidate: Scored): Boolean = {
    val titles = (Seq(candidate.candidate.film.title) ++ candidate.candidate.film.originalTitle).map(IdentityMeasures.yearlessTokens)
    // One word is many films' title — but not as a whole piece beside the year the listing dates it by, which the
    // rule requires: "Akademia Kina Polskiego: Drogówka (2012)" is Smarzowski's 2013 film.
    // A one-word title must be a whole delimited PIECE of the listing's ("…: Drogówka (2012)"), not its last word
    // ("Fanciulla Encore (2027)" is no "Encore").
    lazy val pieces = IdentityMeasures.titleShapes(candidate.listing).map(IdentityMeasures.yearlessTokens).toSet
    (Seq(candidate.listing.title) ++ candidate.listing.rawTitle).map(IdentityMeasures.yearlessTokens).exists(own => own.nonEmpty &&
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
  def houseProduction(ranked: Seq[Scored]): Option[Accepted] = houseProductionWhy(ranked).toOption
  private def houseProductionWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      // nor one from another year than the title dates the production: US "MetOpera: Carmen (2009)" ×513 is Eyre's
      // 2009 staging, not the Met's 2024 record its house bills
      house <- { val billed = eligible.filter(candidate => candidate.houseProduction && !contradicted(candidate))
                 one(billed.filter(_.number("titleYear.delta").forall(delta => math.abs(delta) <= YearWindow.PublishedAdjacency)),
                   if (billed.isEmpty) "no record bills its work under its house" else "its house's record is from another year than its title dates",
                   "two records bill its work under its house", billed.map(Acceptance.named).mkString("; ")) }
      _     <- noneOf(eligible.find(other => (other ne house) &&
                 (IdentityMeasures.key(other.candidate.film.title) == IdentityMeasures.key(house.candidate.film.title) ||
                   (other.titleNamesIt && own(other) >= own(house)))), "another record carries its title or fits as well", house)
    } yield house -> house.probability
  }

  /** A stage broadcast billed with no season but with its year: the one eligible SEASON record naming the same stage
   *  work ([[StageWorks]], in any language), whose banner meets the listing's ([[IdentityMeasures.bannersMeetOf]]),
   *  from the very year the venue publishes, that no running time contradicts. DE "Royal Ballet & Opera im Kino: Manon"
   *  [2026] {Kenneth MacMillan} ×59 is "Royal Ballet & Opera 2026/27: Manon" (October 2026), not the Met's 2026/27
   *  Manon (April 2027), which "MET Opera Live im Kino: Manon" [2027] ×4 is. */
  def stageProduction(ranked: Seq[Scored]): Option[Accepted] = stageProductionWhy(ranked).toOption
  private def stageProductionWhy(ranked: Seq[Scored]): Verdict =
    for {
      any    <- ranked.headOption.toRight(Refused("no candidate"))
      _      <- need(any.listing.seasonYear.isEmpty, "the title names its season")
      year   <- any.listing.year.toRight(Refused("the listing publishes no year"))
      works   = IdentityMeasures.stageWorks(any.listing)
      _      <- need(works.nonEmpty, "the title names no stage work")
      record <- one(eligibleOf(ranked).filter(scored => IdentityMeasures.filmSeason(scored.candidate.film).isDefined &&
                  IdentityMeasures.stageWorks(scored.candidate.film).exists(works) && IdentityMeasures.bannersMeetOf(any.listing, scored.candidate.film) &&
                  scored.candidate.film.year.contains(year) && !contradicted(scored)),
                  "no season record of its work from its year", "two season records of its work from its year", s"works ${works.toSeq.sorted.mkString(",")}, $year")
    } yield record -> record.probability

  /** The one SEASON record whose title is the listing's own once its season is taken out ("&" and "and" alike), when
   *  the listing names no season: UK "Royal Ballet and Opera: Romeo and Juliet" {Kenneth MacMillan} ×127, screening
   *  May–June 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet" — the cut alone denied it, the season marker
   *  reading as a title the listing does not carry. Two seasons of the title ("…: Tosca", 2025/26 and 2026/27) are the
   *  season's to tell apart, not this rule's; a fact contradicting it, or another director, refuses it. */
  def seasonRecord(ranked: Seq[Scored]): Option[Accepted] = seasonRecordWhy(ranked).toOption
  private def seasonRecordWhy(ranked: Seq[Scored]): Verdict = {
    def plain(title: String) = IdentityMeasures.key(title.replace("&", " and "))
    for {
      any    <- ranked.headOption.toRight(Refused("no candidate"))
      _      <- need(any.listing.seasonYear.isEmpty, "the title names its season")
      own     = (Seq(any.listing.title) ++ any.listing.rawTitle).map(plain).toSet
      record <- one(ranked.filter(scored => (eligibleOf(ranked).contains(scored) || scored.deniedByCutOnly) &&
                  IdentityMeasures.filmSeason(scored.candidate.film).isDefined &&
                  scored.candidate.film.titles.exists(t => own(plain(IdentityMeasures.withoutSeasons(t))))),
                  "no season record carries its title", "two seasons' records carry its title")
      _      <- needOf(!contradicted(record), "a published fact contradicts it", record, contradiction(record))
      _      <- needOf(!record.category("director").exists(Set("different", "different_script")), "it credits another director", record)
    } yield record -> record.probability
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
  val LeanMargin = 2.0
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
