package services.review

/**
 * Which refs name one film across databases — from links the corpus already holds, never a live lookup.
 * Two refs are PROVABLY different films only when one database names them by different ids: directly
 * (`tmdb:1` vs `tmdb:2`), or through a link (`filmweb:x` is linked to `tmdb:1`, and the other ref is
 * `tmdb:2`). A pair nothing links is not known to differ — the same film often goes by a TMDB id on one
 * answer and a Filmweb or RT one on the label it answers.
 */
trait FilmIdentity {
  /** Every ref known to name the same film as `ref`, `ref` included. */
  def sameAs(ref: FilmRef): Set[FilmRef]

  def provablyDifferent(a: FilmRef, b: FilmRef): Boolean = {
    val (as, bs) = (sameAs(a), sameAs(b))
    as.intersect(bs).isEmpty && as.exists(x => bs.exists(y => x.source == y.source && x.id != y.id))
  }
}

object FilmIdentity {
  /** Nothing linked: only one database's own ids tell films apart. */
  val Unlinked: FilmIdentity = ref => Set(ref)

  /** What the corpus links between the refs `answers` name, each answer read in its own country's corpus. */
  def linking(answers: Seq[ReviewAnswer], sources: Map[models.Country, ReviewSource]): FilmIdentity =
    of(answers.groupBy(_.country).toSeq.flatMap { case (code, inCountry) =>
      models.Country.byCode(code).flatMap(sources.get).toSeq
        .flatMap(_.filmLinks(inCountry.flatMap(a => a.ref.toSeq ++ a.shown.map(_.ref)).distinct))
    })

  /** Each set one film's refs. */
  def of(films: Seq[Set[FilmRef]]): FilmIdentity = {
    val byRef = films.flatMap(film => film.map(_ -> film)).groupMapReduce(_._1)(_._2)(_ ++ _)
    ref => byRef.getOrElse(ref, Set.empty) + ref
  }
}
