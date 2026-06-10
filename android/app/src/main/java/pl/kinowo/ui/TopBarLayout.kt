package pl.kinowo.ui

/**
 * Decides, from the viewport width, where the search field lives.
 *
 * On wide screens — tablets and landscape phones — there is room to sit
 * search on the top bar, after the "Wszystkie" date pill and before the
 * Filtry button. On narrow screens (portrait phones) there isn't, so it
 * stays as the floating pill at the bottom of the grid.
 *
 * The threshold is shared verbatim with the iOS app's `TopBarLayout`.
 */
object TopBarLayout {
    /** Width (dp) at or above which search moves inline. 600 cleanly
     *  separates portrait phones (≤ ~440dp) from everything wider —
     *  landscape phones and tablets in either orientation. */
    const val WideThresholdDp = 600

    fun searchInline(widthDp: Int): Boolean = widthDp >= WideThresholdDp

    /**
     * Decides whether a date pill stretches to share the row width equally
     * (via `Modifier.weight`) or keeps its intrinsic content width.
     *
     * On wide screens (landscape phones, tablets) there is ample room, so all
     * four pills — including "Wszystkie" — get equal weight and read as one
     * evenly-spaced segmented control.
     *
     * On narrow screens (portrait phones) the three short dated pills (Dziś /
     * Jutro / 7 dni) get weight and "Wszystkie" keeps its intrinsic width. The
     * dated pills therefore absorb ALL the width left between the 🎬 mark and the
     * Filtry button — the pill row fills the navbar with no trailing slack —
     * while "Wszystkie" stays exactly as wide as its long label needs. Their
     * compact-screen padding (see DatePill) is tuned so the filled share still
     * clears the labels at 14sp on a 360dp phone (Galaxy S24): they fill WITHOUT
     * clipping. Guarded by DayPillFitTest.
     */
    fun datePillFillsRow(isAnytime: Boolean, wide: Boolean): Boolean = wide || !isAnytime
}
