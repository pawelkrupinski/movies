package pl.kinowo.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

/**
 * The live country + city catalog the app runs off — fetched from `/api/catalog`,
 * seeded from the bundled snapshot, and queried by the pickers / nearest-city
 * gate (via the `List<City>` / `List<Country>` extensions). Pure data + parsing;
 * the I/O lives in `CatalogRepository`.
 */
data class Catalog(
    val countries: List<Country>,
    val cities: List<City>,
    /** The server's answer for a bare slug several countries answer to. */
    val bareSlugOwners: BareSlugOwners = BareSlugOwners(),
) {
    /** The version-filter pair of [countryCode] — what a deep link's `?lang=` is
     *  validated against, the pair being the linked city's country's own. */
    fun versionTokensOfCountry(countryCode: String): VersionTokens =
        countries.selected(countryCode).versionTokens

    companion object {
        /** Compile-time fallback — used only if the bundled seed ever fails to
         *  decode (it always ships and is guard-tested), so effectively never. */
        val fallback = Catalog(Country.all, Cities.all)

        private val json = Json { ignoreUnknownKeys = true }

        /** Parse a `/api/catalog` 200 body (`{countries,cities}`), or null on error. */
        fun parseBody(body: String): Catalog? = runCatching {
            json.decodeFromString<CatalogBody>(body).toCatalog()
        }.getOrNull()

        /** Parse the bundled seed (`{etag,catalog}`) → (etag, catalog), or null. */
        fun parseSeed(seed: String): Pair<String, Catalog>? = runCatching {
            val e = json.decodeFromString<CatalogEnvelope>(seed)
            e.etag to e.catalog.toCatalog()
        }.getOrNull()
    }
}

/** The `/api/catalog` response body (and the `catalog` field of the seed);
 *  `bareSlugOwners` is absent from an older server's. */
@Serializable
data class CatalogBody(
    val countries: List<CountryDto>,
    val cities: List<City>,
    val bareSlugOwners: BareSlugOwners? = null,
) {
    fun toCatalog() = Catalog(countries.map { it.toCountry() }, cities, bareSlugOwners ?: BareSlugOwners())
}

/**
 * The SERVER's answer for a bare slug — one whose link names no country — that
 * several countries answer to: slug → country code. [apex] is the brand front
 * door (`showtimes.cc/{slug}/`, `Country.frontDoorTarget`), [scheme] the
 * `kinowo://{slug}` custom scheme (`Country.customSchemeTarget`). Read from the
 * catalog, so the app and the web send `showtimes.cc/boston/` to the same Boston
 * instead of each picking its own.
 */
@Serializable
data class BareSlugOwners(
    val apex: Map<String, String> = emptyMap(),
    val scheme: Map<String, String> = emptyMap(),
)

/** The bundled seed shape: `{"etag":<server ETag>,"catalog":<body>}`. */
@Serializable
data class CatalogEnvelope(val etag: String, val catalog: CatalogBody)
