package models

/** A point on the globe — a US venue's harvested `lat`/`lon`, or the centroid a
 *  metro's venues average out to. */
private[models] final case class GeoPoint(lat: Double, lon: Double)

/** Places a point on the compass around a centre: inside a radius it is
 *  Central, outside it takes the compass point of its bearing.
 *
 *  Split out from [[UsMetroSubAreas]] because it is pure geometry with no idea
 *  what a cinema or a metro is — and because the boundary cases (a point ON the
 *  centre, a bearing exactly on a diagonal) are worth testing directly rather
 *  than through 133 Los Angeles venues. */
private[models] object CompassPlacement {
  /** Earth's mean radius, the constant `data/us/scripts/cluster_metros.py`
   *  clusters the metros themselves with. */
  private val EarthRadiusKm = 6371.0

  /** The arithmetic centre of a set of points. Fine at metro scale — the widest
   *  of them spans ~150 km, where the error against a proper spherical centroid
   *  is metres — and it is what `generate_roster.py` already centres a state's
   *  map on. */
  def centroid(points: Seq[GeoPoint]): GeoPoint =
    GeoPoint(points.map(_.lat).sum / points.size, points.map(_.lon).sum / points.size)

  /** Great-circle distance in km. */
  def distanceKm(from: GeoPoint, to: GeoPoint): Double = {
    val lat1 = math.toRadians(from.lat)
    val lat2 = math.toRadians(to.lat)
    val dLat = math.toRadians(to.lat - from.lat)
    val dLon = math.toRadians(to.lon - from.lon)
    val h = math.pow(math.sin(dLat / 2), 2) +
      math.cos(lat1) * math.cos(lat2) * math.pow(math.sin(dLon / 2), 2)
    2 * EarthRadiusKm * math.asin(math.min(1.0, math.sqrt(h)))
  }

  /** Initial great-circle bearing from `from` to `to`, in degrees clockwise from
   *  north — 0 due north, 90 due east. A point on the centre reads as 0. */
  def bearingDegrees(from: GeoPoint, to: GeoPoint): Double = {
    val lat1 = math.toRadians(from.lat)
    val lat2 = math.toRadians(to.lat)
    val dLon = math.toRadians(to.lon - from.lon)
    val y = math.sin(dLon) * math.cos(lat2)
    val x = math.cos(lat1) * math.sin(lat2) - math.sin(lat1) * math.cos(lat2) * math.cos(dLon)
    (math.toDegrees(math.atan2(y, x)) + 360) % 360
  }

  /** Which of `CinemaArea.compass` a point belongs to, relative to `centre`.
   *  The quadrants split at the diagonals and their bounds are half-open, so
   *  every bearing lands in exactly one of them. */
  def areaOf(centre: GeoPoint, point: GeoPoint, centralRadiusKm: Double): CinemaArea =
    if (distanceKm(centre, point) <= centralRadiusKm) CinemaArea.Central
    else bearingDegrees(centre, point) match {
      case b if b < 45  => CinemaArea.North
      case b if b < 135 => CinemaArea.East
      case b if b < 225 => CinemaArea.South
      case b if b < 315 => CinemaArea.West
      case _            => CinemaArea.North
    }
}

/** The compass sub-areas of a US metro too big to browse as one list — the
 *  second level under a state's metro grouping (`UsRoster.metroAreas`), and the
 *  same five areas London is split into.
 *
 *  London's split is a HAND-WRITTEN map of 133 named venues onto
 *  `CinemaArea.compass`. That does not scale here: the metros this catches hold
 *  470 venues between them and the roster is regenerated from `venues.json`
 *  whenever it is re-harvested, so the placement has to be DERIVED. Every US
 *  venue carries coordinates (`UsRoster.locationByCinema`), which is enough:
 *  average the metro's venues into a centroid, call everything within
 *  [[CentralRadiusKm]] of it Central, and give everything else the compass point
 *  of its bearing from that centroid.
 *
 *  The areas are `CinemaArea`'s compass SINGLETONS, so labels and slugs are
 *  byte-identical to London's — clients persist an area slug as a group key, and
 *  a second spelling of "north" would read as a different group. */
object UsMetroSubAreas {

  /** At or past this many venues a metro is sub-divided by compass; below it the
   *  metro stays one list.
   *
   *  Five metros clear it — Los Angeles (133), New York (102), San Francisco
   *  (79), Chicago and Dallas Fort Worth (78 each) — and the next two down,
   *  Seattle (70) and Boston (62), do not.
   *
   *  75 rather than a round 80 because San Francisco is 79: the three metros
   *  this feature was asked for are LA, SF and New York, and 80 would have
   *  excluded one of them by a single venue. Nothing distinguishes Chicago and
   *  Dallas Fort Worth at 78 from San Francisco at 79, so the threshold takes
   *  all five rather than drawing a line through the middle of one scale.
   *
   *  One number, one edit: raise it to sub-divide fewer metros, lower it to
   *  catch Seattle and Boston too. `UsMetroSubAreasSpec` pins the set it
   *  currently selects. */
  val MinCinemasForCompassSplit: Int = 75

  /** How far from a metro's centroid still counts as Central.
   *
   *  Tuned against the OUTCOME across all five metros, not in the abstract. At
   *  10 km Central is a token 3–8 venues in the polycentric metros; at 15 km it
   *  swallows New York (56 of 102) and half of Los Angeles. 12 km is the widest
   *  radius where no Central dominates its metro (the largest, LA and NY, take
   *  33 of 133 and 33 of 102) and none collapses to a token few (the smallest,
   *  the Bay Area and the Dallas metroplex, keep 6).
   *
   *  At 12 km Central is a core a resident recognises where the metro HAS one:
   *  Los Angeles' is Hollywood / Downtown / Beverly Hills / Culver City, New
   *  York's is Manhattan above midtown plus western Queens and north Brooklyn.
   *  Where the metro is polycentric it honestly reports that instead — the Bay
   *  Area's centroid falls in the bay itself, so its Central is the six
   *  east-shore venues nearest the middle and the recognisable places land on
   *  the compass (San Francisco west, the peninsula and San Jose south, the East
   *  Bay north). Dallas Fort Worth is the same shape: Dallas east, Fort Worth
   *  west, the mid-cities between them Central. */
  val CentralRadiusKm: Double = 12.0

  /** This metro's venues split by compass, or `Nil` when it is under
   *  [[MinCinemasForCompassSplit]] — or is not a US metro at all. Keyed by the
   *  state's `City.slug` and the metro's `CinemaArea.slug`, the two path
   *  segments a `/{state}/{metro}/` page already has.
   *
   *  A pure function of the roster: same roster, byte-identical answer. */
  def forMetro(stateSlug: String, metroSlug: String): Seq[CinemaAreaGroup] =
    byMetro.getOrElse((stateSlug, metroSlug), Nil)

  /** Computed once for every metro past the threshold — five of the 470, so the
   *  whole table costs one pass over ~470 venues at class-load. */
  private val byMetro: Map[(String, String), Seq[CinemaAreaGroup]] =
    UsRoster.regions.flatMap { region =>
      region.areas
        .filter(_.cinemas.sizeIs >= MinCinemasForCompassSplit)
        .map(metro => (region.slug, metro.area.slug) -> compassAreas(metro.cinemas))
    }.toMap

  /** One group per compass area, in `CinemaArea.compass`'s display order, each
   *  keeping the metro's own cinema order. Empty groups are dropped, so the five
   *  areas are a partition of `cinemas` and never a row of chrome over nothing.
   *
   *  Returns `Nil` if any venue is missing coordinates: half a metro is worse
   *  than no split, and the whole point is that the groups partition it. Nothing
   *  in the generated roster can hit this — `generate_roster.py` drops a venue
   *  whose `lat`/`lon` it cannot parse — but the map lookup has to answer
   *  something, and "don't split" is the answer that keeps the invariant. */
  private def compassAreas(cinemas: Seq[Cinema]): Seq[CinemaAreaGroup] = {
    val located = cinemas.flatMap(c => UsRoster.locationByCinema.get(c).map(c -> _))
    if (located.sizeIs < cinemas.size) Nil
    else {
      val centre = CompassPlacement.centroid(located.map(_._2))
      val areaOf = located.map { case (c, at) =>
        c -> CompassPlacement.areaOf(centre, at, CentralRadiusKm)
      }.toMap
      CinemaArea.compass
        .map(area => CinemaAreaGroup(area, cinemas.filter(areaOf.get(_).contains(area))))
        .filter(_.cinemas.nonEmpty)
    }
  }
}
