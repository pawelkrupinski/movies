package services.identity
package adapters

import services.identity.staging.{StagedFact, StagedNode}

/**
 * What the staging screen lists of what a take was decided on (docs/design/identity-questions.md, "The staging
 * screen"): a listing's facts as the identity model read them, a TMDB film's as its record states them, and a decision's
 * trace of one listing — its measures against the film, its score and the rivals and rules it weighed. Each a list of
 * named values, the screen showing them as given; a value the model holds none of is left out.
 *
 * Casts are held on both sides only as hashed name pairs (`ListedCast`, `FilmCast`): their sizes are shown, and how
 * many names they share is the node's `cast` measure.
 */
object StagedEvidence {
  def listing(l: Listing): Seq[StagedFact] = facts(
    "title"            -> Some(l.title),
    "clean title"      -> Some(l.cleanTitle),
    "search title"     -> l.searchTitle,
    "proper title"     -> l.properTitle,
    "original title"   -> l.originalTitle,
    "year"             -> l.year.map(_.toString),
    "year at most"     -> l.yearUpperBound.map(_.toString),
    "performance year" -> l.performanceYear.map(_.toString),
    "runtime"          -> l.runtime.map(m => s"$m min"),
    "directors"        -> list(l.directors),
    "countries"        -> list(l.countries),
    "certificate"      -> l.certificate,
    "cast listed"      -> Option.when(l.listedCast.nonEmpty)(s"${l.listedCast.size} name pairs"),
    "catalogue ids"    -> list(l.catalogueIds.map(c => s"${c.source}:${c.id}")),
    "page facts"       -> Option.when(l.pageFacts)("read from its page"),
    "page"             -> l.page,
    "poster"           -> l.poster)

  def film(id: Int, f: IdentityMeasures.Film): Seq[StagedFact] = facts(
    "tmdb"               -> Some(id.toString),
    "imdb"               -> Option.when(f.imdbNumber > 0)(f"tt${f.imdbNumber}%07d"),
    "title"              -> Some(f.title),
    "original title"     -> f.originalTitle,
    "alternative titles" -> list(f.alternativeTitles),
    "original language"  -> f.originalLanguage,
    "year"               -> f.year.map(_.toString),
    "released"           -> f.released.map(_.toString),
    "runtime"            -> f.runtime.map(m => s"$m min"),
    "other runtimes"     -> list(f.alternativeRuntimes.map(m => s"$m min")),
    "directors"          -> f.directors.map(ds => if (ds.isEmpty) "none credited" else ds.mkString(", ")),
    "countries"          -> f.countries.flatMap(list),
    "release countries"  -> f.releaseCountries.filter(_.nonEmpty).map(_.grouped(2).mkString(" ")),
    "certificates"       -> f.certificates,
    "genres"             -> list(f.genreIds.toSeq.sorted.map(_.toString)),
    "cast billed"        -> f.cast.map(c => s"${c.pairs.size} names"),
    "popularity"         -> f.popularity.map(p => f"$p%.1f"))

  /** A decision's trace of `key`, its films titled by `titleOf`: the rule that decided it (`decidedBy`) and the stage
   *  (`stage`) when known, then the node's own. */
  def node(key: services.movies.ListingKey, n: DecisionTrace.Node, titleOf: Int => Option[String],
           decidedBy: Option[String] = None, stage: Option[String] = None): StagedNode = {
    def film(id: Int) = s"tmdb $id${titleOf(id).fold("")(" " + _)}"
    StagedNode(key.venue, key.rawTitle, facts(
      "decided by" -> decidedBy,
      "stage"      -> stage,
      "candidate"  -> n.candidate.map(film),
      "score"      -> n.score.map(ResolverDecision.percent),
      "runner-up"  -> n.runnerUp.map(r => s"${film(r.film)} at ${ResolverDecision.percent(r.score)}"),
      "accepted"   -> n.accepted,
      "joins"      -> list(n.joins),
      "kept apart" -> list(n.apart),
      "blocker"    -> n.blocker,
      "searched"   -> list(n.searched),
      "candidates" -> list(n.candidates)) ++
      n.refusals.map(r => StagedFact("refused", s"${r.rule}: ${r.why}${r.film.fold("")(f => s" (${film(f)})")}${if (r.detail.isEmpty) "" else " — " + r.detail}")) ++
      n.measures.toSeq.sortBy(_._1).map { case (name, m) => StagedFact(s"measure $name", measure(m)) })
  }

  private def measure(m: IdentityMeasures.Measure): String = m match {
    case IdentityMeasures.Category(value) => value
    case IdentityMeasures.Number(value)   => if (value == math.rint(value)) value.toLong.toString else f"$value%.3f"
    case IdentityMeasures.Missing(side)   => s"missing on the $side"
  }

  private def list(values: Seq[String]): Option[String] = Option.when(values.nonEmpty)(values.mkString(", "))
  private def facts(named: (String, Option[String])*): Seq[StagedFact] =
    named.collect { case (name, Some(value)) if value.nonEmpty => StagedFact(name, value) }
}
