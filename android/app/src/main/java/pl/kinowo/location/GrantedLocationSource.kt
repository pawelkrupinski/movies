package pl.kinowo.location

/**
 * A coarse `(lat, lon)` fix, but only when location permission is ALREADY
 * granted — never prompts. Backs the "you're nearer another city" check, which
 * must stay silent (no system dialog) when location was never granted.
 * [LocationCityResolver] is the real one; a ViewModel test substitutes a fixed fix.
 */
fun interface GrantedLocationSource {
    suspend fun resolveIfGranted(): Pair<Double, Double>?
}
