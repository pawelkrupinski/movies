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
import pl.kinowo.ui.city.CityChoiceScreen
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
fun KinowoApp(vm: KinowoViewModel) {
    LaunchedEffect(Unit) { vm.start() }
    LifecycleEventEffect(Lifecycle.Event.ON_RESUME) { vm.onResume() }

    val city by vm.selectedCity.collectAsState()
    if (city == null) {
        CityGate(vm)
    } else {
        Repertoire(vm)
        NearerCityPrompt(vm)
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
private fun NearerCityPrompt(vm: KinowoViewModel) {
    val context = LocalContext.current
    LaunchedEffect(Unit) { vm.checkCitySwitch(context) }
    LifecycleEventEffect(Lifecycle.Event.ON_RESUME) { vm.checkCitySwitch(context) }

    val suggestion = vm.citySwitchSuggestion ?: return
    val target = suggestion.target
    AlertDialog(
        onDismissRequest = { vm.dismissCitySwitch() },
        title = { Text("Jesteś bliżej miasta ${target.name}") },
        text = { Text("Przełączyć repertuar na ${target.name}?") },
        confirmButton = {
            TextButton(onClick = { vm.setCity(target.slug) }) { Text("Przełącz") }
        },
        dismissButton = {
            TextButton(onClick = { vm.dismissCitySwitch() }) { Text("Nie teraz") }
        },
    )
}

/**
 * First-launch city resolution. Requests coarse location; on grant + a fix
 * within 100 km of a supported city, sets that city. On denial, no fix, or
 * out-of-range, falls back to [CityChoiceScreen] for an explicit pick.
 */
@Composable
private fun CityGate(vm: KinowoViewModel) {
    val context = LocalContext.current
    val scope = rememberCoroutineScope()
    // Show the chooser once the location attempt is done without a hit — until
    // then we keep it hidden so the chooser doesn't flash before the fix lands.
    var showChooser by remember { mutableStateOf(false) }

    val resolver = remember { LocationCityResolver(context) }
    fun resolveFromLocation() = scope.launch {
        val city = resolver.resolveNearestCity()
        if (city != null) vm.setCity(city.slug) else showChooser = true
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

    if (showChooser) {
        CityChoiceScreen(onPick = { vm.setCity(it.slug) })
    }
}

@Composable
private fun Repertoire(vm: KinowoViewModel) {
    val nav = rememberNavController()
    NavHost(navController = nav, startDestination = "list") {
        composable("list") {
            ListScreen(vm = vm, onOpenFilm = { title -> nav.navigate("detail/${Uri.encode(title)}") })
        }
        composable(
            route = "detail/{title}",
            arguments = listOf(navArgument("title") { type = NavType.StringType }),
        ) { entry ->
            val title = entry.arguments?.getString("title").orEmpty()
            // Observe both maps so the screen fills in synopsis/trailers when
            // the parallel /api/details fetch lands after navigation.
            val films by vm.films.collectAsState()
            val details by vm.details.collectAsState()
            DetailScreen(
                film = films.firstOrNull { it.title == title },
                details = details[title],
                onBack = { nav.popBackStack() },
            )
        }
    }
}
