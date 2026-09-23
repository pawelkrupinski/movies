package pl.kinowo

import android.app.Application
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.Country
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.RepertoireApi
import java.time.LocalDate
import java.time.ZoneId

/** The real MainActivity over an offline graph: every city lists [TARGET]. */
class OfflineKinowoApplication : Application(), KinowoGraphProvider {
    override fun viewModelFactory(country: Country): ViewModelProvider.Factory =
        object : ViewModelProvider.Factory {
            @Suppress("UNCHECKED_CAST")
            override fun <T : ViewModel> create(modelClass: Class<T>): T {
                val api = object : RepertoireApi {
                    override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?) =
                        KinowoApi.Fetched(listOf(film(TARGET), film("Other")), null, false)
                }
                val repository = RepertoireRepository(api, JsonListCache(cacheDir, "rep_recreate", Film.serializer()))
                return testKinowoViewModel(this@OfflineKinowoApplication, repository) as T
            }
        }

    private fun film(title: String): Film {
        // Tomorrow, so past-showing pruning never drops it whatever the zone.
        val tomorrow = LocalDate.now(ZoneId.of("Europe/Warsaw")).plusDays(1).toString()
        return Film(
            title = title,
            showings = listOf(DayShowings(tomorrow, "label", listOf(CinemaShowings("Kino", showtimes = listOf(Showtime("20:00")))))),
        )
    }

    companion object {
        const val TARGET = "Target"
    }
}
