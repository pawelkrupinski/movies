package services.movies

/** A person's name as an order-insensitive whole-name key: accents folded, lowercased, its
 *  words sorted, so "Makoto Shinkai" = "Shinkai Makoto". One definition for every place that
 *  asks whether two credits name the same person by name alone. */
object PersonKey {
  def of(name: String): String = TitleContainment.tokens(name).sorted.mkString(" ")
}

/**
 * Whether two cast lists describe the same film — as MATCHING evidence.
 *
 * Neither length nor order is evidence. A venue prints its top three billed and TMDB keeps
 * its top five, so the shorter list sitting inside the longer one is FULL agreement; and
 * billing order moves between two pages of one venue (Kinoteka's two "Rozważna i
 * romantyczna" pages bill Fiona Shaw and Daisy Edgar-Jones in opposite orders). So the
 * overlap is the shared names over the SMALLER list's size.
 *
 * With a floor on that size: one shared name is what two films by one director share all
 * the time, so a one- or two-name list can never reach full agreement on its own.
 */
object CastAgreement {

  /** The smallest list size an overlap is measured against. A venue's typical top-billed
   *  three: at three shared names a subset reads as full agreement, one shared name as a
   *  third of it. */
  val ComparableFloor = 3

  /** The overlap at and above which two casts agree. Two names of a three-name list do;
   *  one name alone ([[ComparableFloor]] = 3) never does. */
  val AgreementThreshold = 0.5

  private def people(cast: Iterable[String]): Set[String] =
    cast.iterator.map(PersonKey.of).filter(_.nonEmpty).toSet

  /** Shared names over the smaller list's size (floored at [[ComparableFloor]]), or None
   *  when either side names nobody — no cast is no evidence, not disagreement. */
  def overlap(a: Iterable[String], b: Iterable[String]): Option[Double] = {
    val (pa, pb) = (people(a), people(b))
    Option.when(pa.nonEmpty && pb.nonEmpty)(
      (pa intersect pb).size.toDouble / math.max(math.min(pa.size, pb.size), ComparableFloor))
  }

  /** Do the two casts agree — enough of the smaller one named by the other? */
  def agrees(a: Iterable[String], b: Iterable[String]): Boolean =
    overlap(a, b).exists(_ >= AgreementThreshold)
}
