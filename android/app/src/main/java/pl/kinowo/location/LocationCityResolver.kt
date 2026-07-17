package pl.kinowo.location

import android.Manifest
import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.PackageManager
import androidx.core.content.ContextCompat
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
    suspend fun resolveNearestCity(countryCode: String): City? {
        val fix = locationFix() ?: return null
        return Cities.nearestWithin100km(fix.first, fix.second, countryCode)
    }

    /**
     * A coarse `(lat, lon)` fix, but only when `ACCESS_COARSE_LOCATION` is
     * *already* granted — never triggers a permission request. Used by the
     * "you're nearer another city" prompt, which must stay silent (no system
     * dialog) when location was never granted. Returns null on no permission,
     * no fix, or any failure.
     */
    suspend fun resolveIfGranted(): Pair<Double, Double>? {
        val granted = ContextCompat.checkSelfPermission(
            context, Manifest.permission.ACCESS_COARSE_LOCATION,
        ) == PackageManager.PERMISSION_GRANTED
        if (!granted) return null
        return locationFix()
    }

    @SuppressLint("MissingPermission")
    private suspend fun locationFix(): Pair<Double, Double>? {
        val client = LocationServices.getFusedLocationProviderClient(context)
        return lastLocation(client) ?: currentLocation(client)
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
