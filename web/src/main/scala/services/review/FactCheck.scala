package services.review

/**
 * Whether the film an answer chose contradicts what a venue itself stated about the listing: a year
 * more than one off (a festival print is often dated a year either side of its release), or credited
 * directors none of whom direct the film. Only the VENUE's own facts are weighed — a catalogue's
 * claim is the kind of thing a review overrules.
 */
object FactCheck {
  def warnings(members: Seq[ReviewMember], film: FilmFacts): Seq[String] =
    members.flatMap { m =>
      val year = for { stated <- m.year; actual <- film.year if math.abs(stated - actual) > 1 }
        yield s"${m.venue} states ${m.rawTitle} is from $stated; ${film.describe} is from $actual"
      val directors = Option.when(m.directors.nonEmpty && film.directors.nonEmpty &&
          !m.directors.exists(stated => film.directors.exists(services.movies.SamePerson(stated, _))))(
        s"${m.venue} credits ${m.directors.mkString(", ")}; ${film.describe} is directed by ${film.directors.mkString(", ")}")
      year.toSeq ++ directors
    }.distinct
}
