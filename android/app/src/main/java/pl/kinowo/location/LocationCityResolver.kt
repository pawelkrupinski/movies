package pl.kinowo.location

import android.annotation.SuppressLint
import android.content.Context
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.Priority
import kotlinx.coroutines.suspendCancellableCoroutine
import pl.kinowo.model.Cities
import pl.kinowo.model.City
import kotlin.coroutines.resume

/**
 * One-shot location → city resolver for the first-launch gate. Asks Fused
 * Location for a coarse fix (last known, falling back to a fresh current fix),
 * then maps it to the nearest supported [City] within 100 km via
 * [Cities.nearestWithin100km]. Returns null on no permission, no fix, or when
 * the user is out of range of every city — the caller then offers an explicit
 * pick. The permission check itself lives at the gate, so this is only called
 * once `ACCESS_COARSE_LOCATION` is granted.
 */
class LocationCityResolver(private val context: Context) {

    @SuppressLint("MissingPermission") // the gate requests ACCESS_COARSE_LOCATION before calling
    suspend fun resolveNearestCity(): City? {
        val client = LocationServices.getFusedLocationProviderClient(context)
        val fix = lastLocation(client) ?: currentLocation(client) ?: return null
        return Cities.nearestWithin100km(fix.first, fix.second)
    }

    @SuppressLint("MissingPermission")
    private suspend fun lastLocation(
        client: com.google.android.gms.location.FusedLocationProviderClient,
    ): Pair<Double, Double>? = suspendCancellableCoroutine { cont ->
        client.lastLocation
            .addOnSuccessListener { loc -> cont.resume(loc?.let { it.latitude to it.longitude }) }
            .addOnFailureListener { cont.resume(null) }
    }

    @SuppressLint("MissingPermission")
    private suspend fun currentLocation(
        client: com.google.android.gms.location.FusedLocationProviderClient,
    ): Pair<Double, Double>? = suspendCancellableCoroutine { cont ->
        client.getCurrentLocation(Priority.PRIORITY_BALANCED_POWER_ACCURACY, null)
            .addOnSuccessListener { loc -> cont.resume(loc?.let { it.latitude to it.longitude }) }
            .addOnFailureListener { cont.resume(null) }
    }
}
