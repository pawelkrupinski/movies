package pl.kinowo.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

/**
 * The live country + city catalog the app runs off — fetched from `/api/catalog`,
 * seeded from the bundled snapshot, and queried by the pickers / nearest-city
 * gate (via the `List<City>` / `List<Country>` extensions). Pure data + parsing;
 * the I/O lives in `CatalogRepository`.
 */
data class Catalog(val countries: List<Country>, val cities: List<City>) {
    companion object {
        /** Compile-time fallback — used only if the bundled seed ever fails to
         *  decode (it always ships and is guard-tested), so effectively never. */
        val fallback = Catalog(Country.all, Cities.all)

        private val json = Json { ignoreUnknownKeys = true }

        /** Parse a `/api/catalog` 200 body (`{countries,cities}`), or null on error. */
        fun parseBody(body: String): Catalog? = runCatching {
            val b = json.decodeFromString<CatalogBody>(body)
            Catalog(b.countries.map { it.toCountry() }, b.cities)
        }.getOrNull()

        /** Parse the bundled seed (`{etag,catalog}`) → (etag, catalog), or null. */
        fun parseSeed(seed: String): Pair<String, Catalog>? = runCatching {
            val e = json.decodeFromString<CatalogEnvelope>(seed)
            e.etag to Catalog(e.catalog.countries.map { it.toCountry() }, e.catalog.cities)
        }.getOrNull()
    }
}

/** The `/api/catalog` response body (and the `catalog` field of the seed). */
@Serializable
data class CatalogBody(val countries: List<CountryDto>, val cities: List<City>)

/** The bundled seed shape: `{"etag":<server ETag>,"catalog":<body>}`. */
@Serializable
data class CatalogEnvelope(val etag: String, val catalog: CatalogBody)
