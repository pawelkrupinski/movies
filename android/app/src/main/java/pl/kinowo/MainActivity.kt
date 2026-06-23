package pl.kinowo

import android.content.Intent
import android.graphics.Color
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.SystemBarStyle
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.viewModels
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.Surface
import androidx.compose.ui.Modifier
import okhttp3.OkHttpClient
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.HttpUserStateClient
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoApp
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.dev.ShowtimeTuningScreen
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.KinowoTheme
import java.util.concurrent.TimeUnit

/**
 * Single-activity entry point. Manual dependency wiring (this app's
 * composition root — no DI framework needed at this size): one shared
 * OkHttp client (with a disk-backed cookie jar so the signed-in session
 * survives restarts), the [KinowoApi], a file cache, the repository,
 * DataStore prefs, and the auth + state-sync collaborators — all handed to
 * the [KinowoViewModel].
 */
class MainActivity : ComponentActivity() {

    private val viewModel: KinowoViewModel by viewModels {
        // One client shared by every caller so the auth session cookie set at
        // /auth/exchange is carried on /api/me, /api/me/state, etc.
        val cookieJar = PersistentCookieJar(applicationContext)
        val httpClient = OkHttpClient.Builder()
            .cookieJar(cookieJar)
            .connectTimeout(15, TimeUnit.SECONDS)
            .readTimeout(20, TimeUnit.SECONDS)
            .build()
        val api = KinowoApi(client = httpClient)
        val repository = RepertoireRepository(api, JsonListCache(cacheDir, "repertoire", Film.serializer()))
        val detailsRepository = DetailsRepository(api, JsonListCache(cacheDir, "details", FilmDetails.serializer()))
        val prefs = UserPreferences(applicationContext)
        val authRepository = AuthRepository(httpClient, cookieJar)
        val userStateClient = HttpUserStateClient(client = httpClient)
        KinowoViewModel.Factory(repository, detailsRepository, prefs, authRepository, userStateClient)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        // The app is dark-only (see KinowoTheme). Force the dark system-bar
        // style so the status/nav-bar icons stay light regardless of the
        // device's night mode — the default `auto` style derives icon colour
        // from the system setting, which on a light-mode device yields dark
        // icons that vanish against our near-black background (the status bar
        // then reads as an empty gap at the top).
        enableEdgeToEdge(
            statusBarStyle = SystemBarStyle.dark(Color.TRANSPARENT),
            navigationBarStyle = SystemBarStyle.dark(Color.TRANSPARENT),
        )
        super.onCreate(savedInstanceState)
        handleAuthDeepLink(intent)
        handleNavDeepLink(intent)
        // Non-prod tweak screen, gated behind a launch extra so it never shows in
        // a normal run, AND behind BuildConfig.ENABLE_TUNING so it's compiled out
        // of the public `release` build (it's on for `debug` + `tuneRelease`).
        // The "Kinowo Tune" launcher icon (TuningLauncherActivity, src/tuning)
        // passes the extra; or via adb:
        //   adb shell am start -n pl.kinowo/.MainActivity --ez kinowo_tuning true
        val tuning = BuildConfig.ENABLE_TUNING && intent.getBooleanExtra("kinowo_tuning", false)
        setContent {
            KinowoTheme {
                Surface(modifier = Modifier.fillMaxSize(), color = Background) {
                    if (tuning) ShowtimeTuningScreen() else KinowoApp(viewModel)
                }
            }
        }
    }

    // The OAuth callback bounces back as `kinowo://auth-done?code=…`. With
    // `singleTask` launch mode a redirect into the already-running app lands
    // here; a cold start (app was killed) lands in onCreate's intent instead,
    // so both paths funnel through this.
    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        handleAuthDeepLink(intent)
        handleNavDeepLink(intent)
    }

    private fun handleAuthDeepLink(intent: Intent?) {
        val data = intent?.data ?: return
        if (data.scheme == "kinowo" && data.host == "auth-done") {
            data.getQueryParameter("code")?.let { viewModel.handleAuthRedirect(it) }
        }
    }

    // A kinowo.fly.dev App Link or kinowo://<city>/… link. The ViewModel parses
    // it (rejecting the auth-done callback handled above), switches the city, and
    // applies the filters + film. Harmless on a plain MAIN/LAUNCHER start: a null
    // or unrecognised data URI is a no-op.
    private fun handleNavDeepLink(intent: Intent?) {
        intent?.data?.let { viewModel.handleDeepLink(it.toString()) }
    }
}
