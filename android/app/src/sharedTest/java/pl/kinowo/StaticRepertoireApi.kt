package pl.kinowo

import pl.kinowo.model.Film
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.RepertoireApi
import java.util.concurrent.atomic.AtomicInteger

/**
 * A [RepertoireApi] that answers every fetch with a fixed listing: [byCity]
 * for a city it names (an unknown city is an empty listing), else [everyCity].
 * [fetchCount] counts the calls, for tests that pin how often a fetch happens.
 */
class StaticRepertoireApi private constructor(
    private val byCity: Map<String, List<Film>>,
    private val everyCity: List<Film>,
) : RepertoireApi {
    constructor(films: List<Film> = emptyList()) : this(emptyMap(), films)
    constructor(byCity: Map<String, List<Film>>) : this(byCity, emptyList())

    private val fetches = AtomicInteger()
    val fetchCount: Int get() = fetches.get()

    override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?): KinowoApi.Fetched<Film> {
        fetches.incrementAndGet()
        return KinowoApi.Fetched(byCity[citySlug] ?: everyCity, null, false)
    }
}
