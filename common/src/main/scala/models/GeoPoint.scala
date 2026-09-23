package models

/** A point on the globe, in decimal degrees. */
final case class GeoPoint(lat: Double, lon: Double) {

  /** Great-circle (haversine) distance to `that`, in km. */
  def kmTo(that: GeoPoint): Double = {
    val (p1, p2) = (math.toRadians(lat), math.toRadians(that.lat))
    val dPhi     = math.toRadians(that.lat - lat)
    val dLambda  = math.toRadians(that.lon - lon)
    val h = math.pow(math.sin(dPhi / 2), 2) +
            math.cos(p1) * math.cos(p2) * math.pow(math.sin(dLambda / 2), 2)
    2 * GeoPoint.EarthRadiusKm * math.asin(math.sqrt(h))
  }
}

object GeoPoint {
  private val EarthRadiusKm = 6371.0
}
