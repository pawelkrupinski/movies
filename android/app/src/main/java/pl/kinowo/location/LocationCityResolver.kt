package pl.kinowo.location

import android.Manifest
import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.PackageManager
import android.os.SystemClock
import androidx.core.content.ContextCompat
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.Priority
import com.google.android.gms.tasks.CancellationToken
import com.google.android.gms.tasks.CancellationTokenSource
import com.google.android.gms.tasks.Task
import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.coroutines.withTimeoutOrNull
import pl.kinowo.model.City
import pl.kinowo.model.nearestWithin100km
import java.util.concurrent.Executor
import kotlin.coroutines.resume

/**
 * One-shot location → city resolver for the first-launch gate. Asks Fused
 * Location for a coarse fix (last known if under 15 min old, else a fresh one
 * bounded to 8 s, else the old one),
 * then maps it to the nearest supported [City] within 100 km via
 * [Cities.nearestWithin100km]. Returns null on no permission, no fix, or when
 * the user is out of range of every city — the caller then offers an explicit
 * pick. The permission check itself lives at the gate, so this is only called
 * once `ACCESS_COARSE_LOCATION` is granted.
 */
class LocationCityResolver(private val context: Context) : GrantedLocationSource {

    @SuppressLint("MissingPermission") // the gate requests ACCESS_COARSE_LOCATION before calling
    suspend fun resolveNearestCity(countryCode: String, cities: List<City>): City? {
        val fix = locationFix() ?: return null
        return cities.nearestWithin100km(fix.first, fix.second, countryCode)
    }

    /**
     * Like [resolveNearestCity], but unscoped — matches the fix against every
     * country's cities rather than one. Backs the manual picker's "use my
     * location" button, which should find the right city even when the
     * country tab currently open isn't the one the device is actually in.
     */
    @SuppressLint("MissingPermission") // the gate requests ACCESS_COARSE_LOCATION before calling
    suspend fun resolveNearestCityAnyCountry(cities: List<City>): City? {
        val fix = locationFix() ?: return null
        return cities.nearestWithin100km(fix.first, fix.second)
    }

    /** Checks `ACCESS_COARSE_LOCATION` without requesting it. Returns null on
     *  no permission, no fix, or any failure. */
    override suspend fun resolveIfGranted(): Pair<Double, Double>? {
        val granted = ContextCompat.checkSelfPermission(
            context, Manifest.permission.ACCESS_COARSE_LOCATION,
        ) == PackageManager.PERMISSION_GRANTED
        if (!granted) return null
        return locationFix()
    }

    @SuppressLint("MissingPermission")
    private suspend fun locationFix(): Pair<Double, Double>? {
        val client = LocationServices.getFusedLocationProviderClient(context)
        val fix = preferRecentHeldFix(
            held = awaitTaskOrNull { client.lastLocation },
            ageMs = { (SystemClock.elapsedRealtimeNanos() - it.elapsedRealtimeNanos) / 1_000_000 },
        ) {
            withTimeoutOrNull(FRESH_FIX_TIMEOUT_MS) {
                awaitTaskOrNull { token -> client.getCurrentLocation(Priority.PRIORITY_BALANCED_POWER_ACCURACY, token) }
            }
        }
        return fix?.let { it.latitude to it.longitude }
    }

    private companion object {
        /** How long to wait on a FRESH fix — iOS's `fixTimeout`. Past it the
         *  request is cancelled (radio off) and the caller gets no fix. */
        const val FRESH_FIX_TIMEOUT_MS = 8_000L
    }
}

/** A held fix this recent is as good as a fresh one for picking a CITY —
 *  nobody crosses a 100 km radius in a quarter of an hour. iOS's `maxCachedFixAge`. */
internal const val MAX_HELD_FIX_AGE_MS = 15 * 60 * 1000L

/**
 * The fix to use: the one the system already [held] when it is recent enough,
 * else a [fresh] one — and when no fresh one comes, the old held fix anyway,
 * which names the right city far more often than not. Mirrors iOS
 * `LocationCityResolver.requestFix` / `deliverNoFix`, so the same phone in the
 * same place gets the same answer on both platforms.
 */
internal suspend fun <L : Any> preferRecentHeldFix(held: L?, ageMs: (L) -> Long, fresh: suspend () -> L?): L? =
    if (held != null && ageMs(held) <= MAX_HELD_FIX_AGE_MS) held else fresh() ?: held

/**
 * The result of the Play Services task [start] builds, or null when it fails,
 * is cancelled, or answers null. Cancelling the caller cancels the task through
 * the token [start] receives — so a cleared ViewModel or a timeout stops a
 * location request rather than leaving the radio on for an answer nobody reads.
 * A task cancelled from outside answers null rather than suspending forever.
 * Listeners run on the completing thread: nothing here needs the main thread.
 */
internal suspend fun <T : Any> awaitTaskOrNull(start: (CancellationToken) -> Task<T?>): T? =
    suspendCancellableCoroutine { cont ->
        val cancellation = CancellationTokenSource()
        cont.invokeOnCancellation { cancellation.cancel() }
        val direct = Executor(Runnable::run)
        start(cancellation.token)
            .addOnSuccessListener(direct) { cont.resume(it) }
            .addOnFailureListener(direct) { cont.resume(null) }
            .addOnCanceledListener(direct) { cont.resume(null) }
    }
