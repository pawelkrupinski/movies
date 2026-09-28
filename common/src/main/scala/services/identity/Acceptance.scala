package services.identity

import services.identity.IdentityMeasures.ListingFilm
import services.identity.Scored.Accepted

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it
 *  TAKES, and at what confidence: [[alone]] for a node on its own evidence, [[pooled]] for a
 *  cluster's pooled evidence. Each is an ordered list of RULES, every one a function of the ranked
 *  candidates alone; the first to accept decides, and the listing's season production, when a
 *  record names it, pre-empts them all ([[seasonProduction]]). A new rule is a new method here and
 *  one entry in a list. */
private[identity] final class Acceptance(calibration: IdentityCalibration) {

  val weights = new EvidenceWeights(calibration)
  import weights.{favours, facts, fitsBetter, own, priorsLent, speaksAgainst}

  private def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filterNot(_.denied)

  private def firstOf(ranked: Seq[Scored], rules: Seq[Seq[Scored] => Option[Accepted]]): Option[Accepted] =
    rules.iterator.flatMap(_(ranked)).nextOption()

  /** A node accepts a film ON ITS OWN only when its own facts favour it over the runner-up: a
   *  bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not
   *  decided alone — it follows the film its title's credited siblings chose (the cluster's), or
   *  the pooled vote — unless it is the listing's exact top hit, which is measured as a class. */
  def alone(ranked: Seq[Scored]): Option[Accepted] =
    seasonProduction(ranked).getOrElse(firstOf(ranked, Seq(soleWork, favouredCalibrated, topHit, directorsWork, houseProduction)))
      .map(editionNamed(ranked))

  /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
   *  best eligible candidate the calibration accepts that no namesake out-fits ([[unrivalledCalibrated]]).
   *  Each the edition of it the listing names, if any ([[editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] =
    seasonProduction(ranked).getOrElse(firstOf(ranked, Seq(unrivalledCalibrated, topHit)))
      .map(editionNamed(ranked))

  /** The probability that `film` is the listing's film — the decision's confidence, on the scale
   *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
   *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
   *  lent when the listing's facts accepted it ([[calibrated]]). */
  def confidenceOf(ranked: Seq[Scored], film: Int): Double =
    topHit(ranked).filter(_._1.c.tmdbId == film).map(_._2)
      .orElse(calibrated(ranked).filter(_._1.c.tmdbId == film).map(_._2))
      .orElse(pooled(ranked).filter(_._1.c.tmdbId == film).map(_._2))
      .getOrElse(eligibleOf(ranked).find(_.c.tmdbId == film).fold(0.0)(_.p))

  /** Why an own match's confidence stands above its calibrated probability: its exact top hit's
   *  class, or the ranking priors lending ([[EvidenceWeights.priorsLent]]). */
  def liftedBy(ranked: Seq[Scored], s: Scored, confidence: Double): String =
    if (confidence <= s.p) ""
    else if (topHit(ranked).exists(_._1.c.tmdbId == s.c.tmdbId)) " as its exact top hit"
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
    IdentityMeasures.exactTopHits(any.listing, ranked.map(s => (s.c.tmdbId, s.c.film, s.rank))) match {
      case Seq(id) =>
        eligible.find(_.c.tmdbId == id)
          .filter(best => !speaksAgainst(best) && eligible.forall(r => (r eq best) || !fitsBetter(r, best)))
          .flatMap(best => calibration.classProbability(ListingFilm, best.measures).map(cp => best -> math.max(best.p, cp)))
          .filter(x => calibration.showsRatings(x._2))
      case _ => None
    }
  }

  /** The best eligible candidate, when its probability — the priors lending, never withdrawing
   *  ([[EvidenceWeights.priorsLent]]) — clears the calibration's cut. */
  def calibrated(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    eligible.headOption.map(b => b -> priorsLent(b, eligible)).filter(x => calibration.showsRatings(x._2))
  }

  /** [[calibrated]], when the listing's own facts also favour it over the runner-up. */
  def favouredCalibrated(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    calibrated(ranked).filter { case (best, _) => eligible.lift(1).forall(favours(best, _)) }
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
      eligibleOf(ranked).forall(r => (r eq best) || {
        val pieces = IdentityMeasures.namingPieces(r.listing, r.c.film)
        val alike  = pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.c.film)
        val apart  = IdentityMeasures.namedApart(best.listing, best.c.film, r.c.film)
        !(alike && facts(r) > facts(best)) && !(apart && facts(r) >= facts(best))
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
    ranked.filter(s => !s.denied && s.seasonProduction).sortBy(s => (-own(s), s.c.tmdbId)) match {
      case Seq()       => None
      case Seq(one)    => Some(Some(one -> one.p))
      case a +: b +: _ => Some(Option.when(own(a) > own(b))(a -> a.p))
    }

  /** A published year more than one off, or a runtime 30 minutes or more off: the listing's own facts against it. */
  def contradicted(s: Scored): Boolean =
    s.number("year.distance").exists(_ > 1) || s.number("runtime.delta").exists(_ >= 30)

  /** The film whose WORK the listing's whole title is, when TMDB ranks it first, it is the only such
   *  candidate, and no other candidate is one the title names — the rest only a director's
   *  filmography reached. US venues' "BTS WORLD TOUR 'ARIRANG' IN BUENOS AIRES" (×1,915 with São
   *  Paulo) is "…: Live Viewing", not the 2022 Seoul concert film its director also made; neither
   *  pipeline matched them. A work of one word is too many films' title ("It" of "It: Chapter Two"). */
  def soleWork(ranked: Seq[Scored]): Option[Accepted] = {
    val eligible = eligibleOf(ranked)
    eligible.filter(s => s.rank.contains(1) && IdentityMeasures.titleIsWorkOf(s.listing, s.c.film).exists(_ >= 2) && !contradicted(s)) match {
      case Seq(one) if !eligible.exists(o => (o ne one) && o.titleNamesIt) => Some(one -> one.p)
      case _                                                                 => None
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
    def bareWork(s: Scored) = IdentityMeasures.titleIsWorkOf(s.listing, s.c.film).exists(words =>
      words >= 2 || s.number("year.distance").exists(_ <= 1))
    eligibleOf(ranked).filter(s => IdentityMeasures.sameDirector(s.measures) &&
      (IdentityMeasures.sharesWork(s.listing, s.c.film) || bareWork(s)) && !contradicted(s)) match {
      case Seq(one) => Some(one -> one.p)
      case _        => None
    }
  }

  /** The one eligible record billing the listing's work under the listing's OWN house
   *  ([[Scored.houseProduction]]) that no published year or runtime contradicts: Flicks' "NT Live:
   *  The Misanthrope" is the National Theatre's 2026 broadcast, "National Theatre Live: The
   *  Misanthrope", though TMDB's search ranks three bare records of Molière's play above it — a
   *  house's banner names its production as a season names one ([[seasonProduction]]). Two such
   *  records (a broadcast and its encore) are no answer. */
  def houseProduction(ranked: Seq[Scored]): Option[Accepted] =
    eligibleOf(ranked).filter(candidate => candidate.houseProduction && !contradicted(candidate)) match {
      case Seq(one) => Some(one -> one.p)
      case _        => None
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
    else ranked.filter(e => (e ne work) && !e.deniedByPin && Acceptance.namedAs(e.category("title")) > named && (
      if (named == 0) IdentityMeasures.editionOf(e.c.film, work.c.film)
      else IdentityMeasures.editionOf(e.c.film, work.c.film.copy(alternativeTitles = Nil)) && !e.category("director").contains("different"))) match {
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
