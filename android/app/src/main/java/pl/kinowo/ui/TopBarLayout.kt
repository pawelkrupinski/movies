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
     * On narrow screens (portrait phones) NO pill stretches: each takes its
     * intrinsic content width so the label can never be squeezed below the text
     * it has to show. Forcing the three dated pills to share the leftover width
     * via `weight` starved them below "Jutro" / "7 dni" at 14sp on a 360dp phone
     * (Galaxy S24) — the weighted share landed under the text's intrinsic width
     * and the labels clipped. Intrinsic-width pills (packed left, with the Filtry
     * button pushed flush-right by a spacer) fit the row with room to spare. See
     * DayPillFitTest.
     */
    fun datePillFillsRow(wide: Boolean): Boolean = wide
}
