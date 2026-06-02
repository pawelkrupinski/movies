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
}
