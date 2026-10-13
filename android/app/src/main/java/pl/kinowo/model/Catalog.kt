package pl.kinowo.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonTransformingSerializer

/**
 * The live country + city catalog the app runs off — fetched from `/api/catalog`,
 * seeded from the bundled snapshot, and queried by the pickers / nearest-city
 * gate (via the `List<City>` / `List<Country>` extensions). Pure data + parsing;
 * the I/O lives in `CatalogRepository`.
 */
data class Catalog(
    val countries: List<Country>,
    val cities: List<City>,
    /** The server's old-owner table: which country a link naming none means. */
    val bareSlugOwners: Map<String, String> = emptyMap(),
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
    val cities: List<@Serializable(with = CatalogCitySerializer::class) City>,
    val bareSlugOwners: Map<String, String>? = null,
) {
    fun toCatalog() = Catalog(countries.map { it.toCountry() }, cities, bareSlugOwners.orEmpty())
}

/**
 * A catalog city keyed by its PAGE slug: the wire's `pageSlug` — the city's own
 * slug on its own country, which every URL, saved selection and (country, slug)
 * lookup in this build uses — becomes [City.slug]. The catalog's `slug` is
 * unique across countries for installed builds from before slugs were per
 * country (`york-north-yorkshire` for the UK's York, Pennsylvania's being `york`).
 *
 * TEMPORARY: the fallback to `slug` (no `pageSlug`) is for a server older than
 * the field; drop this with the server's `models.LegacySlugOwners` once the
 * 2.0.11 builds are gone and `slug` is the page slug again.
 */
object CatalogCitySerializer : JsonTransformingSerializer<City>(City.serializer()) {
    override fun transformDeserialize(element: JsonElement): JsonElement {
        val city = element as? JsonObject ?: return element
        val pageSlug = city["pageSlug"] ?: return element
        return JsonObject(city - "pageSlug" + ("slug" to pageSlug))
    }
}

/** The bundled seed shape: `{"etag":<server ETag>,"catalog":<body>}`. */
@Serializable
data class CatalogEnvelope(val etag: String, val catalog: CatalogBody)
