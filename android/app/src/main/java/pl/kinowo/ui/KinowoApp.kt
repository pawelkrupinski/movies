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
import androidx.compose.ui.res.stringResource
import androidx.core.content.ContextCompat
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.compose.LifecycleEventEffect
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import kotlinx.coroutines.launch
import pl.kinowo.R
import pl.kinowo.location.LocationCityResolver
import pl.kinowo.model.Catalog
import pl.kinowo.model.City
import pl.kinowo.ui.city.CityChoiceScreen
import pl.kinowo.ui.city.CityConfirmScreen
import pl.kinowo.ui.city.CityGateStart
import pl.kinowo.ui.common.LocalCitySlug
import pl.kinowo.ui.common.LocalShareOrigin
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
        CompositionLocalProvider(
            LocalCitySlug provides city,
            // Share links are country-scoped: a Barcelona film lives on
            // showtimes.cc/es, not on the Polish host.
            LocalShareOrigin provides viewModel.shareOrigin(),
        ) {
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
        title = { Text(stringResource(R.string.nearer_city_title, target.name)) },
        text = { Text(stringResource(R.string.switch_repertoire_question, target.name)) },
        confirmButton = {
            TextButton(onClick = { viewModel.setCity(target.slug) }) { Text(stringResource(R.string.switch_action)) }
        },
        dismissButton = {
            TextButton(onClick = { viewModel.dismissCitySwitch() }) { Text(stringResource(R.string.not_now)) }
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
    val resolver = remember { LocationCityResolver(context) }
    CityGate(
        // `gateStart`, not `selectedCountryCode`: the gate must be able to tell
        // "still reading the stored choices" from "nothing stored", and only the
        // former is a reason to wait. See its doc on the ViewModel.
        start = viewModel.gateStart.collectAsState().value,
        catalog = viewModel.countryCatalog.collectAsState().value,
        onPick = { city, nearest -> viewModel.chooseCityAtGate(city.slug, nearest?.slug) },
        onConfirm = { viewModel.setCity(it.slug) },
        onCountry = { viewModel.setCountry(it) },
        resolveNearest = resolver::resolveNearestCity,
    )
}

/**
 * The gate's own state machine, with the ViewModel and CoreLocation lifted out
 * so a test can drive it: [start] says which country to scope to and whether a
 * located city may be offered at all (null while the stored choices are still
 * being read), and [resolveNearest] maps a country + the catalog's cities to
 * the nearest one within 100 km, or null.
 */
@Composable
internal fun CityGate(
    start: CityGateStart?,
    catalog: Catalog,
    onPick: (City, City?) -> Unit,
    onConfirm: (City) -> Unit,
    onCountry: (String) -> Unit,
    resolveNearest: suspend (String, List<City>) -> City?,
) {
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

    fun resolveIn(country: String) = scope.launch {
        val city = resolveNearest(country, catalog.cities)
        if (city != null) { detected = city; nearest = city } else showChooser = true
    }

    val permissionLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission(),
    ) { granted ->
        val country = start?.countryCode
        if (granted && country != null) resolveIn(country) else showChooser = true
    }

    // Keyed on the start rather than fired once: it arrives asynchronously and
    // changes when the user switches countries, and each value gets its own
    // resolution scoped to it. Waiting out the null is what keeps a freshly
    // chosen Germany from being searched as Poland; clearing the previous
    // answers is what stops the outgoing country's city from being offered
    // while the new one resolves.
    LaunchedEffect(start) {
        val opening = start ?: return@LaunchedEffect
        detected = null
        nearest = null
        showChooser = false
        // The user reached this gate by naming a country, so the answer they are
        // owed is that country's cities — not a location fix, and not the
        // permission dialog that taking one would raise.
        if (!opening.locate) {
            showChooser = true
            return@LaunchedEffect
        }
        val alreadyGranted = ContextCompat.checkSelfPermission(
            context, Manifest.permission.ACCESS_COARSE_LOCATION,
        ) == PackageManager.PERMISSION_GRANTED
        if (alreadyGranted) resolveIn(opening.countryCode)
        else permissionLauncher.launch(Manifest.permission.ACCESS_COARSE_LOCATION)
    }

    val city = detected
    when {
        showChooser     -> CityChoiceScreen(
            catalog = catalog,
            onPick = { onPick(it, nearest) },
            selectedCountryCode = start?.countryCode,
            onCountry = onCountry,
        )
        city != null    -> CityConfirmScreen(
            city = city,
            onConfirm = { onConfirm(city) },
            onChooseOther = { detected = null; showChooser = true },
        )
        // else: still resolving (or still waiting for the stored country) — keep
        // the screen blank (no flash) until the fix lands or the chooser/confirm
        // takes over.
    }
}

// `internal` (not `private`) so the on-device deep-link test can mount the real
// nav graph directly, without going through the CityGate's location flow.
@Composable
internal fun Repertoire(viewModel: KinowoViewModel) {
    val nav = rememberNavController()
    // A deep link asked to open a specific film: navigate once the title has
    // been confirmed present in the loaded repertoire (set by the ViewModel).
    val pendingFilm = viewModel.pendingFilmNav
    LaunchedEffect(pendingFilm) {
        val title = pendingFilm ?: return@LaunchedEffect
        nav.navigate("detail/${Uri.encode(title)}")
        viewModel.clearPendingFilmNav()
    }
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
