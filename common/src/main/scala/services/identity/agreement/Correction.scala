package services.identity.agreement

import services.identity.{FactRelations, IdentityMeasures}

/**
 * What the agreement stage makes of the evidence against a film the MODEL took ([[AgreementStage]]'s corrections): each
 * independent kind of evidence that names another film than the model's, or contradicts it —
 *
 *  - [[Filmweb]]: the venues' own Filmweb programmes list another film under the listing's title on its days
 *    ([[VenueListings]]), one whose year or director the model's record contradicts ([[contradicts]]);
 *  - [[Poster]]: a venue poster matches another candidate and not the model's film ([[services.identity.PosterEvidence.veto]]);
 *  - [[Families]]: more of the other film database families take another film than take the model's.
 *
 * The take is WITHDRAWN (the cluster left without a film) when Filmweb's programme contradicts it, or when the posters
 * and the families name the same other film and the two films share no director — a TMDB duplicate or a re-release's
 * record shares its film's director (UK "Queen Rock Montreal", 2024 and 2007, both Saul Swimmer's). It is SWITCHED to
 * the other film only where two kinds of evidence or more name that same TMDB film, the two sharing no director.
 * Measured over the five recorded corpora (2026-10-06, `model-take-audit.tsv`): the poster veto alone contradicted 10
 * takes, 4 of them wrong; Filmweb's programmes 2 prod clusters, both wrong; every take the rules below correct was wrong.
 * Wrong beats missing: a withdrawal costs a card its film, a wrong take
 * shows the wrong one.
 */
object Correction {
  val Filmweb  = "filmweb"
  val Poster   = "poster"
  val Families = "families"

  /** One kind of evidence against the model's take: the TMDB film it names instead (none: a film TMDB holds no record
   *  of, or one it only contradicts), and what it says, for the explanation. */
  final case class Against(evidence: String, film: Option[Int], says: String)

  /** The correction: the film switched to (`None`: withdrawn), and the line that explains it. */
  final case class Outcome(film: Option[Int], line: String)

  /** The take corrected, if the evidence says so. `sharesDirector(film)`: do `film` and the model's share a director —
   *  `None` where either record credits none, which never lets a correction stand on the guard. */
  def decide(model: String, against: Seq[Against], sharesDirector: Int => Option[Boolean]): Option[Outcome] = {
    def says = against.map(a => s"${a.evidence}: ${a.says}").mkString("; ")
    val apart = (film: Int) => sharesDirector(film).contains(false)
    // a film two kinds of evidence or more name, sharing no director with the model's: the take switches to it
    val named = against.flatMap(a => a.film.map(_ -> a.evidence)).distinct.groupMap(_._1)(_._2).filter { case (film, kinds) => kinds.size >= 2 && apart(film) }
    named.toSeq match {
      case Seq((film, kinds)) => Some(Outcome(Some(film), s"corrected from $model by ${kinds.sorted.mkString(" and ")} — $says"))
      case _ =>
        val filmweb = against.exists(_.evidence == Filmweb)
        val postersAndFamilies = against.filter(_.evidence == Poster).flatMap(_.film).exists(film =>
          against.exists(a => a.evidence == Families && a.film.contains(film)) && apart(film))
        Option.when(filmweb || postersAndFamilies)(Outcome(None, s"withdrawn $model — $says"))
    }
  }

  /** Does `record` (another database's film) contradict `model` (the model's TMDB film) by its facts — years more than
   *  one apart, or Latin-script directors who are other people, sharing no name's stem? A missing fact contradicts
   *  nothing: a title alone never does. */
  def contradicts(record: IdentityMeasures.Film, model: IdentityMeasures.Film): Boolean =
    FactRelations.yearsApart(record.year, model.year) ||
      FactRelations.otherPeople(record.directors.getOrElse(Nil), model.directors.getOrElse(Nil), acrossScripts = true)

  /** Do two films' credited directors share one — the same person, or a shared name's stem ("Simona Risi", "Simona
   *  Lina Risi")? `None` where either credits none. */
  def shareDirector(a: Seq[String], b: Seq[String]): Option[Boolean] =
    Option.when(a.nonEmpty && b.nonEmpty)(!FactRelations.otherPeople(a, b, acrossScripts = true))

}
