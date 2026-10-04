package services.identity

/**
 * Candidate ids of films no TMDB record holds, found in a FALLBACK SOURCE — a film database the resolver turns to only
 * once TMDB's candidates gave it no film (an [[CandidateQuery.ImdbTitled]] answer carries them). The resolver keys every candidate by an Int, a
 * TMDB id; a fallback film takes a NEGATIVE one, its source in the high bits and its own number in the low, so the two
 * never meet and a decision's TMDB `film` is never one ([[ResolverDecision.fallback]] carries it instead).
 */
object FallbackIds {

  /** A fallback source, by the code its ids carry and the name a decision stores. */
  enum Source(val code: Int, val label: String) {
    /** IMDb: a title's number (tt0064570 → 64570). */
    case Imdb extends Source(1, "imdb")
  }

  /** The bits a source's own number may take: IMDb's numbers run to tens of millions. */
  private val NumberBits = 27
  private val NumberMask = (1 << NumberBits) - 1

  /** The candidate id of `source`'s film `number`; `None` for a number the id space cannot hold. */
  def of(source: Source, number: Int): Option[Int] =
    Option.when(number > 0 && number <= NumberMask)(-((source.code << NumberBits) | number))

  /** Is `id` a fallback source's film, not a TMDB one? */
  def isFallback(id: Int): Boolean = id < 0

  /** The source and number a fallback id names. */
  def unapply(id: Int): Option[(Source, Int)] =
    Option.when(isFallback(id)) { val bits = -id; (bits >>> NumberBits, bits & NumberMask) }
      .flatMap { case (code, number) => Source.values.find(_.code == code).map(_ -> number) }

  /** The IMDb id an IMDb fallback id names ("tt0064570"). */
  def imdbId(id: Int): Option[String] = unapply(id).collect { case (Source.Imdb, number) => f"tt$number%07d" }

  /** The fallback id of an IMDb id; `None` for anything else. */
  def ofImdbId(imdbId: String): Option[Int] =
    Option.when(imdbId.startsWith("tt"))(imdbId.drop(2)).flatMap(_.toIntOption).flatMap(of(Source.Imdb, _))
}
