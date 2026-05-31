package pl.kinowo

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.viewModels
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.Surface
import androidx.compose.ui.Modifier
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.ui.KinowoApp
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.Background
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Single-activity entry point. Manual dependency wiring (this app's
 * composition root — no DI framework needed at this size): one OkHttp-backed
 * [KinowoApi], a file cache, the repository, and DataStore prefs, all handed
 * to the [KinowoViewModel].
 */
class MainActivity : ComponentActivity() {

    private val viewModel: KinowoViewModel by viewModels {
        val api = KinowoApi()
        val repo = RepertoireRepository(api, JsonListCache(cacheDir, "repertoire", Film.serializer()))
        val detailsRepo = DetailsRepository(api, JsonListCache(cacheDir, "details", FilmDetails.serializer()))
        val prefs = UserPreferences(applicationContext)
        KinowoViewModel.Factory(repo, detailsRepo, prefs)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        enableEdgeToEdge()
        super.onCreate(savedInstanceState)
        setContent {
            KinowoTheme {
                Surface(modifier = Modifier.fillMaxSize(), color = Background) {
                    KinowoApp(viewModel)
                }
            }
        }
    }
}
