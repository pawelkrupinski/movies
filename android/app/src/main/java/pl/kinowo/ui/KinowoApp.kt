package pl.kinowo.ui

import android.net.Uri
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
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
            DetailScreen(film = vm.filmByTitle(title), onBack = { nav.popBackStack() })
        }
    }
}
