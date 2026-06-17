package pl.kinowo.ui

import android.Manifest
import android.content.pm.PackageManager
import android.net.Uri
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.platform.LocalContext
import androidx.core.content.ContextCompat
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.compose.LifecycleEventEffect
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import kotlinx.coroutines.launch
import pl.kinowo.location.LocationCityResolver
import pl.kinowo.model.City
import pl.kinowo.ui.city.CityChoiceScreen
import pl.kinowo.ui.city.CityConfirmScreen
import pl.kinowo.ui.common.LocalCitySlug
import pl.kinowo.ui.detail.DetailScreen
import pl.kinowo.ui.list.ListScreen

/** Nav graph: the repertoire list and a per-film detail screen. The detail
 *  screen reads its `Film` straight from the in-memory payload by title, so
 *  there's no per-screen network fetch.
 *
 *  A city gate sits in front of the whole graph: until a city is chosen the
 *  repertoire fetch is suppressed (see [KinowoViewModel.start]) and we resolve
 *  one — by location if granted + in range, else by explicit pick. */
@Composable
fun KinowoApp(viewModel: KinowoViewModel) {
    LaunchedEffect(Unit) { viewModel.start() }
    LifecycleEventEffect(Lifecycle.Event.ON_RESUME) { viewModel.onResume() }

    // A non-delegated `val` (not `by`) so the null check smart-casts it to a
    // non-null slug inside the else branch, which scopes every film share link.
    val city = viewModel.selectedCity.collectAsState().value
    if (city == null) {
        CityGate(viewModel)
    } else {
        CompositionLocalProvider(LocalCitySlug provides city) {
            Repertoire(viewModel)
            NearerCityPrompt(viewModel)
        }
    }
}

/**
 * "You're nearer another city — switch?" prompt. Once a city is chosen, checks
 * (on entry and on every resume) whether a granted-only location fix lands
 * nearer a different supported city; if so, [KinowoViewModel.checkCitySwitch]
 * surfaces a suggestion we render as an [AlertDialog]. The check never requests
 * location permission and fires at most once per `chosen→nearest` pair.
 */
@Composable
private fun NearerCityPrompt(viewModel: KinowoViewModel) {
    val context = LocalContext.current
    LaunchedEffect(Unit) { viewModel.checkCitySwitch(context) }
    LifecycleEventEffect(Lifecycle.Event.ON_RESUME) { viewModel.checkCitySwitch(context) }

    val suggestion = viewModel.citySwitchSuggestion ?: return
    val target = suggestion.target
    AlertDialog(
        onDismissRequest = { viewModel.dismissCitySwitch() },
        title = { Text("Jesteś bliżej miasta ${target.name}") },
        text = { Text("Przełączyć repertuar na ${target.name}?") },
        confirmButton = {
            TextButton(onClick = { viewModel.setCity(target.slug) }) { Text("Przełącz") }
        },
        dismissButton = {
            TextButton(onClick = { viewModel.dismissCitySwitch() }) { Text("Nie teraz") }
        },
    )
}

/**
 * First-launch city resolution. Requests coarse location; on grant + a fix
 * within 100 km of a supported city, ASKS the user to confirm that city
 * ([CityConfirmScreen]) rather than adopting it silently. On denial, no fix,
 * out-of-range, or "choose other", falls back to [CityChoiceScreen].
 */
@Composable
private fun CityGate(viewModel: KinowoViewModel) {
    val context = LocalContext.current
    val scope = rememberCoroutineScope()
    // Show the chooser once the location attempt is done without a hit — until
    // then we keep it hidden so the chooser doesn't flash before the fix lands.
    var showChooser by remember { mutableStateOf(false) }
    // The detected city awaiting the user's confirmation (null until a fix
    // lands on a supported city).
    var detected by remember { mutableStateOf<City?>(null) }
    // The detected nearest city, retained even after "choose other" clears
    // `detected`, so a deliberate pick of a different city can pre-suppress the
    // "you're nearer …" prompt that would otherwise fire on the next screen.
    var nearest by remember { mutableStateOf<City?>(null) }

    val resolver = remember { LocationCityResolver(context) }
    fun resolveFromLocation() = scope.launch {
        val city = resolver.resolveNearestCity()
        if (city != null) { detected = city; nearest = city } else showChooser = true
    }

    val permissionLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission(),
    ) { granted -> if (granted) resolveFromLocation() else showChooser = true }

    LaunchedEffect(Unit) {
        val alreadyGranted = ContextCompat.checkSelfPermission(
            context, Manifest.permission.ACCESS_COARSE_LOCATION,
        ) == PackageManager.PERMISSION_GRANTED
        if (alreadyGranted) resolveFromLocation() else permissionLauncher.launch(Manifest.permission.ACCESS_COARSE_LOCATION)
    }

    val city = detected
    when {
        showChooser     -> CityChoiceScreen(onPick = { viewModel.chooseCityAtGate(it.slug, nearest?.slug) })
        city != null    -> CityConfirmScreen(
            city = city,
            onConfirm = { viewModel.setCity(city.slug) },
            onChooseOther = { detected = null; showChooser = true },
        )
        // else: still resolving — keep the screen blank (no flash) until the
        // fix lands or the chooser/confirm takes over.
    }
}

@Composable
private fun Repertoire(viewModel: KinowoViewModel) {
    val nav = rememberNavController()
    NavHost(navController = nav, startDestination = "list") {
        composable("list") {
            ListScreen(viewModel = viewModel, onOpenFilm = { title -> nav.navigate("detail/${Uri.encode(title)}") })
        }
        composable(
            route = "detail/{title}",
            arguments = listOf(navArgument("title") { type = NavType.StringType }),
        ) { entry ->
            val title = entry.arguments?.getString("title").orEmpty()
            // Observe both maps so the screen fills in synopsis/trailers when
            // the parallel /api/details fetch lands after navigation.
            val films by viewModel.films.collectAsState()
            val details by viewModel.details.collectAsState()
            DetailScreen(
                film = films.firstOrNull { it.title == title },
                details = details[title],
                onBack = { nav.popBackStack() },
            )
        }
    }
}
