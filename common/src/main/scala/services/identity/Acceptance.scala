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
  import weights.{factsAnswered, favours, facts, fitsBetter, own, priorsLent, speaksAgainst}

  private def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filterNot(_.denied)

  /** A rule, by the name an explanation gives it. */
  private final case class Rule(name: String, accepts: Seq[Scored] => Option[Accepted])

  /** The first of `rules` to accept, with its name. */
  private def firstOf(ranked: Seq[Scored], rules: Seq[Rule]): Option[(Accepted, String)] =
    rules.iterator.flatMap(rule => rule.accepts(ranked).map(_ -> rule.name)).nextOption()

  private val aloneRules = Seq(Rule("sole-work", soleWork), Rule("favoured-calibrated", favouredCalibrated), Rule("exact-top-hit", topHit),
    Rule("segment-top-hit", segmentTopHit),
    Rule("directors-work", directorsWork), Rule("directors-title", directorsTitle), Rule("dated-title", datedTitle),
    Rule("house-production", houseProduction))

  /** What [[alone]] takes, with the rule that took it. */
  private def aloneNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    seasonProduction(ranked).map(_.map(_ -> "season-production")).getOrElse(firstOf(ranked, aloneRules))
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }

  /** A node accepts a film ON ITS OWN only when its own facts favour it over the runner-up: a
   *  bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not
   *  decided alone — it follows the film its title's credited siblings chose (the cluster's), or
   *  the pooled vote — unless it is the listing's exact top hit, which is measured as a class. */
  def alone(ranked: Seq[Scored]): Option[Accepted] = aloneNamed(ranked).map(_._1)

  /** The rule [[alone]] took `film` by, for the decision's explanation — every own match names the
   *  rule that took it, however far under the calibrated cut its probability stands. */
  def acceptedBy(ranked: Seq[Scored], film: Int): Option[String] =
    aloneNamed(ranked).collect { case ((scored, _), rule) if scored.candidate.tmdbId == film => rule }

  /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
   *  best eligible candidate the calibration accepts that no namesake out-fits ([[unrivalledCalibrated]]).
   *  Each the edition of it the listing names, if any ([[editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] =
    seasonProduction(ranked).getOrElse(firstOf(ranked, Seq(Rule("unrivalled-calibrated", unrivalledCalibrated), Rule("exact-top-hit", topHit))).map(_._1))
      .map(editionNamed(ranked))

  /** The probability that `film` is the listing's film — the decision's confidence, on the scale
   *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
   *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
   *  lent when the listing's facts accepted it ([[calibrated]]). */
  def confidenceOf(ranked: Seq[Scored], film: Int): Double =
    topHit(ranked).filter(_._1.candidate.tmdbId == film).map(_._2)
      .orElse(calibrated(ranked).filter(_._1.candidate.tmdbId == film).map(_._2))
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
  def topHit(ranked: Seq[Scored]): Option[Accepted] = ranked.headOption.flatMap { any =>
    val eligible = eligibleOf(ranked)
    IdentityMeasures.exactTopHits(any.listing, ranked.map(scored => (scored.candidate.tmdbId, scored.candidate.film, scored.rank))) match {
      case Seq(id) =>
        eligible.find(_.candidate.tmdbId == id)
          .filter(best => !speaksAgainst(best) && eligible.forall(rival => (rival eq best) || !fitsBetter(rival, best)))
          .flatMap(best => calibration.classProbability(ListingFilm, best.measures).map(classProbability => best -> math.max(best.probability, classProbability)))
          .filter(accepted => calibration.showsRatings(accepted._2))
      case _ => None
    }
  }

  /** A programme listing's EXACT TOP HIT once its banner is off: the one candidate whose title is a whole
   *  delimited piece of the listing's (`segment`) and which the listing's own search returned FIRST, when
   *  the title names no other candidate at all (the rest is a banner, not a second film), it bills no two
   *  works, and nothing it publishes speaks against it or fits a rival better. The piece IS a bare title,
   *  so it is credited with what the exact-top-hit class measured for bare titles. PL's "DZIEŃ KINA POLSKIEGO:
   *  Przepraszam, czy tu biją" and ~30 programme listings like it sat at 28.9% with no rule to take them. */
  def segmentTopHit(ranked: Seq[Scored]): Option[Accepted] = ranked.headOption.flatMap { any =>
    val eligible = eligibleOf(ranked)
    val named    = ranked.filter(_.titleNamesIt)
    named match {
      case Seq(scored) if !scored.denied && scored.rank.contains(1) && scored.category("title").contains("segment") &&
          !IdentityMeasures.billsTwoWorks(any.listing) && IdentityMeasures.standsForTheWhole(any.listing, scored.candidate.film) &&
          // a year the title states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
          scored.number("titleYear.delta").forall(delta => math.abs(delta) <= YearWindow.PublishedAdjacency) =>
        // Read as the bare title its piece is: the banner beside it is no evidence against the film.
        val bare = scored.copy(measures = scored.measures + ("title" -> IdentityMeasures.Category("exact")))
        Option.when(!speaksAgainst(bare) && eligible.forall(rival => (rival eq scored) || !fitsBetter(rival, bare)))(bare)
          .flatMap(bare => calibration.classProbability(ListingFilm, bare.measures))
          .map(classProbability => scored -> math.max(scored.probability, classProbability))
          .filter(accepted => calibration.showsRatings(accepted._2))
      case _ => None
    }
  }

  /** The best eligible candidate, when its probability — the priors lending, never withdrawing
   *  ([[EvidenceWeights.priorsLent]]) — clears the calibration's cut. */
  def calibrated(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    eligible.headOption.map(best => best -> priorsLent(best, eligible)).filter(accepted => calibration.showsRatings(accepted._2))
  }

  /** [[calibrated]], when the listing's own facts also favour it over the runner-up. */
  def favouredCalibrated(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    calibrated(ranked).filter { case (best, _) =>
      eligible.lift(1).forall(favours(best, _)) && !outnamed(best, eligible)
    }
  }

  /** Is there an eligible record the listing's title sits inside ([[titledCloser]]) whose answered
   *  facts `best`'s do not beat? Then `best`, which the title only overlaps, is not the listing's
   *  film on that evidence — on its own or pooled. */
  private def outnamed(best: Scored, eligible: Seq[Scored]): Boolean =
    eligible.exists(closer => (closer ne best) && titledCloser(closer, best) && factsAnswered(best, closer) <= factsAnswered(closer, closer))

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

  /** [[calibrated]] — unless the title names another candidate by the very same pieces and the
   *  pooled FACTS ([[EvidenceWeights.facts]]) fit that one better: four "Camino dla opornych" whose
   *  original title "Santiago" names two films, and whose 113 minutes fit the fourth the search
   *  returned, do not take the 93-minute first. A candidate the title names less specifically
   *  ("Mad Max" inside "Mad Max 2: The Road Warrior") or not at all is no such rival, and
   *  namesakes the facts fit alike stay the calibration's to tell apart — its ranking priors and
   *  the family's venue count are measured evidence there (a bare "Resident Evil" at 148 venues).
   *  Two films the title names by disjoint pieces ([[IdentityMeasures.namedApart]]) are not
   *  namesakes: only the facts may pick one of "Lalka (Dolly)"'s two. */
  def unrivalledCalibrated(ranked: Seq[Scored]): Option[Accepted] =
    calibrated(ranked).filter { case (best, _) =>
      !outnamed(best, eligibleOf(ranked)) && eligibleOf(ranked).forall(rival => (rival eq best) || {
        val pieces = IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)
        val alike  = pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.candidate.film)
        val apart  = IdentityMeasures.namedApart(best.listing, best.candidate.film, rival.candidate.film)
        !(alike && facts(rival) > facts(best)) && !(apart && facts(rival) >= facts(best))
      })
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
      case Seq(one)    => Some(Some(one -> one.probability))
      case first +: second +: _ => Some(Option.when(own(first) > own(second))(first -> first.probability))
    }

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
  def soleWork(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    def isWork(scored: Scored) = IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def sharesItsWork(scored: Scored) = IdentityMeasures.billsWorkOf(scored.listing, scored.candidate.film).exists(_ >= 2)
    def billsWork(scored: Scored) = scored.category("title").contains("fragment") && sharesItsWork(scored)
    eligible.filter(scored => scored.rank.contains(1) && (isWork(scored) || billsWork(scored)) && !contradicted(scored)) match {
      case Seq(one) if !eligible.exists(other => (other ne one) && (other.titleNamesIt || (!isWork(one) && sharesItsWork(other)))) =>
        Some(one -> one.probability)
      case _ => None
    }
  }

  /** The one candidate whose WORK the listing bills under another subtitle, or publishes alone, by
   *  the director it credits, that no published year or runtime contradicts
   *  (`IdentityMeasures.sharesWork`, `titleIsWorkOf`; three venues' "Leonas" is Cotelo's "Leonas, el
   *  instinto más salvaje", fifth in TMDB's search for the word): Multikino's "Cirque du Soleil:
   *  Kurios - Gabinet osobliwości" by Michel Laprise is his "…: KURIOS - Cabinet des curiosités".
   *  The pipeline took such hits by their director on 3,247 listings with no wrong match; two
   *  candidates fitting alike (a director's sequels of one work) are no answer. A one-word work
   *  also needs the published year, a word being many films' title. */
  def directorsWork(ranked: Seq[Scored]): Option[Accepted] = {
    def bareWork(scored: Scored) = IdentityMeasures.titleIsWorkOf(scored.listing, scored.candidate.film).exists(words =>
      words >= 2 || scored.number("year.distance").exists(_ <= YearWindow.PublishedAdjacency))
    eligibleOf(ranked).filter(scored => IdentityMeasures.sameDirector(scored.measures) &&
      (IdentityMeasures.sharesWork(scored.listing, scored.candidate.film) || bareWork(scored)) && !contradicted(scored)) match {
      case Seq(one) => Some(one -> one.probability)
      case _        => None
    }
  }

  /** The one eligible candidate the listing's title names EXACTLY that credits the director the
   *  listing credits, when no published year or runtime contradicts it: a title and its director
   *  name one film together, however the database's search ranks it or however few venues list it
   *  (US "Man of Iron" {Andrzej Wajda}, seventh in TMDB's search behind "Iron Man"). Two such
   *  records are no answer. */
  def directorsTitle(ranked: Seq[Scored]): Option[Accepted] =
    eligibleOf(ranked).filter(candidate => candidate.category("title").contains("exact") &&
      IdentityMeasures.sameDirector(candidate.measures) && !contradicted(candidate)) match {
      case Seq(one) => Some(one -> one.probability)
      case _        => None
    }

  /** The one eligible candidate the listing's title names EXACTLY from the year its title dates it
   *  (a year off at most, as a release and a premiere differ), when nothing it publishes contradicts
   *  it — a credited director included: US "Troll (1986)" is the 1986 film, though TMDB ranks "Troll 2"
   *  and a 2022 "Troll" above it ([[namedButForItsYear]]: decorated, as "… Encore (2027)", too). Two
   *  such records are no answer. */
  def datedTitle(ranked: Seq[Scored]): Option[Accepted] =
    eligibleOf(ranked).filter(candidate => namedButForItsYear(candidate) &&
      candidate.number("titleYear.delta").exists(delta => math.abs(delta) <= YearWindow.PublishedAdjacency) && !contradicted(candidate) &&
      !candidate.category("director").contains("different")) match {
      case Seq(one) => Some(one -> one.probability)
      case _        => None
    }

  /** Is the listing's title, years aside, the candidate's own title or original title — or that
   *  title of two words or more decorated along one edge ("La Fanciulla del West Encore" of the
   *  Met's "… 2026/27: La Fanciulla del West")? */
  private def namedButForItsYear(candidate: Scored): Boolean = {
    val titles = (Seq(candidate.candidate.film.title) ++ candidate.candidate.film.originalTitle).map(IdentityMeasures.yearlessTokens)
    (Seq(candidate.listing.title) ++ candidate.listing.rawTitle).map(IdentityMeasures.yearlessTokens).exists(own => own.nonEmpty &&
      titles.exists(title => title == own || (title.sizeIs >= 2 && services.movies.TitleContainment.isTokenRun(title, own))))
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
  def houseProduction(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    eligible.filter(candidate => candidate.houseProduction && !contradicted(candidate)) match {
      case Seq(one) if !eligible.exists(other => (other ne one) &&
          (IdentityMeasures.key(other.candidate.film.title) == IdentityMeasures.key(one.candidate.film.title) ||
            (other.titleNamesIt && own(other) >= own(one)))) =>
        Some(one -> one.probability)
      case _ => None
    }
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
