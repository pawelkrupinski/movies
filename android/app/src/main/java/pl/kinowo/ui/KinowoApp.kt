package pl.kinowo.ui

import android.net.Uri
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.lifecycle.Lifecycle
import androidx.lifecycle.compose.LifecycleEventEffect
import androidx.navigation.NavType
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import androidx.navigation.navArgument
import pl.kinowo.ui.detail.DetailScreen
import pl.kinowo.ui.list.ListScreen

/** Nav graph: the repertoire list and a per-film detail screen. The detail
 *  screen reads its `Film` straight from the in-memory payload by title, so
 *  there's no per-screen network fetch. */
@Composable
fun KinowoApp(vm: KinowoViewModel) {
    val nav = rememberNavController()

    LaunchedEffect(Unit) { vm.start() }
    LifecycleEventEffect(Lifecycle.Event.ON_RESUME) { vm.onResume() }

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
