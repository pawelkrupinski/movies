package pl.kinowo

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.activity.viewModels
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.Surface
import androidx.compose.ui.Modifier
import pl.kinowo.data.RepertoireCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
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
        val cache = RepertoireCache(cacheDir)
        val repo = RepertoireRepository(api, cache)
        val prefs = UserPreferences(applicationContext)
        KinowoViewModel.Factory(repo, prefs)
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
