package pl.kinowo.ui.dev

import kotlin.math.roundToInt

/**
 * Read-only display-geometry readout for the dev tuning screen. It leads with
 * the *available* logical size that actually drives layout (dp) — more useful
 * than the raw panel resolution for dialling in the two-per-row showtime fit —
 * then the display density and the physical pixel resolution it works out to.
 *
 * Pure → JVM-unit-tested without an emulator; the Compose screen feeds it
 * `LocalConfiguration` dp + `LocalDensity` density.
 */
object DisplayInfo {
    /** e.g. `viewport 411×891 dp · 2.625× → 1079×2339 px` */
    fun tuningReadout(widthDp: Int, heightDp: Int, density: Float): String {
        val px = (widthDp * density).roundToInt()
        val py = (heightDp * density).roundToInt()
        return "viewport ${widthDp}×${heightDp} dp · ${densityText(density)}× → ${px}×${py} px"
    }

    /** `3.0` → `3`, `2.625` → `2.625` — drop a trailing `.0`, keep real fractions. */
    private fun densityText(density: Float): String =
        if (density == density.toInt().toFloat()) density.toInt().toString() else density.toString()
}
