package services.review

import services.identity.IdentityMeasures

/** One thing some venues state about a listing that the film contradicts — every venue stating it, once. */
final case class Disagreement(venues: Seq[String], verb: Disagreement.Verb, claim: String) {
  def render: String = (venues match {
    case Seq(venue) => s"$venue ${verb.one}"
    case many       => s"${many.size} cinemas ${verb.many}"
  }) + " " + claim
}

object Disagreement {
  final case class Verb(one: String, many: String)
  val States  = Verb("states", "state")
  val Credits = Verb("credits", "credit")
}

/**
 * Whether the film an answer chose contradicts what a venue itself stated about the listing: a year
 * more than one off (a festival print is often dated a year either side of its release), or credited
 * directors none of whom direct the film. Only the VENUE's own facts are weighed — a catalogue's
 * claim is the kind of thing a review overrules. A director is no contradiction where the film credits
 * a company ([[tools.OrganisationName]]), or where the listing bills a stage relay: its venue credits
 * the stage director, its film record the screen director or the house.
 */
object FactCheck {
  def disagreements(members: Seq[ReviewMember], film: FilmFacts): Seq[Disagreement] = {
    val directorsWeighed = film.directors.nonEmpty && !film.directors.forall(tools.OrganisationName(_))
    val found = members.flatMap { m =>
      val year = for { stated <- m.year; actual <- film.year if math.abs(stated - actual) > 1 }
        yield (Disagreement.States, s"${m.rawTitle} is from $stated; ${film.describe} is from $actual") -> m.venue
      val directors = Option.when(directorsWeighed && m.directors.nonEmpty &&
          !IdentityMeasures.billsStageWork(Seq(m.rawTitle), m.rawTitle) &&
          !m.directors.exists(stated => film.directors.exists(services.movies.SamePerson(stated, _))))(
        (Disagreement.Credits, s"${m.directors.mkString(", ")}; ${film.describe} is directed by ${film.directors.mkString(", ")}") -> m.venue)
      year.toSeq ++ directors
    }
    found.map(_._1).distinct.map { case key @ (verb, claim) =>
      Disagreement(found.collect { case (`key`, venue) => venue }.distinct, verb, claim)
    }
  }

  /** Each disagreement as one line — what an answer stores and the labels export lists. */
  def warnings(members: Seq[ReviewMember], film: FilmFacts): Seq[String] = disagreements(members, film).map(_.render)
}
