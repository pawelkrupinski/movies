package pl.kinowo.contracts

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import java.io.File

/** The repo's checked-in retry/error classification table
 *  (`test/resources/retry-classification.json`), which the Scala, Swift and
 *  Kotlin specs each hold their production classifiers to, row by row — so a
 *  verdict changed on one platform and not the others fails a test instead of
 *  shipping (the 403 that dropped language picks on web, iOS and Android
 *  alike, 78a6c72a2). */
@Serializable
data class RetryClassificationTable(val sources: Map<String, Source>, val rows: List<Row>) {

    @Serializable
    data class Source(val consumers: List<String>)

    @Serializable
    data class Row(val source: String, val error: String, val verdict: String) {
        val isPermanent: Boolean get() = verdict == "permanent"

        /** The status an `http:NNN` error names. */
        val status: Int? get() = error.split(':').takeIf { it.size == 2 }?.get(1)?.toIntOrNull()

        override fun toString(): String = "$source/$error → $verdict"
    }

    fun rowsFor(source: String): List<Row> {
        require(source in sources) { "no source '$source' in the table" }
        return rows.filter { it.source == source }
    }

    /** Every source this platform's code enforces. */
    fun sourcesConsumedBy(platform: String): Set<String> =
        sources.filterValues { platform in it.consumers }.keys

    companion object {
        private val json = Json { ignoreUnknownKeys = true }

        /** Gradle runs unit tests from the module dir (`android/app`); walk up to the repo root. */
        fun load(): RetryClassificationTable {
            val start = File(System.getProperty("user.dir") ?: ".").absoluteFile
            val file = generateSequence(start) { it.parentFile }
                .map { it.resolve("test/resources/retry-classification.json") }
                .firstOrNull { it.isFile }
                ?: error("test/resources/retry-classification.json not found above $start")
            return json.decodeFromString(serializer(), file.readText())
        }
    }
}
