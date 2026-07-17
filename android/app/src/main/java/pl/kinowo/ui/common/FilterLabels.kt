package pl.kinowo.ui.common

import androidx.compose.runtime.Composable
import androidx.compose.ui.res.stringResource
import pl.kinowo.R
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.SortOption

/**
 * Per-locale display labels for the filter/sort domain enums. Kept in the UI
 * layer (not on the enums themselves) so `pl.kinowo.filter` stays free of
 * Android string resources — the label is resolved from the active locale at
 * the composable call site.
 *
 * A [DateFilter.Specific] shows its raw ISO date; the fixed [DateFilter.Kind]
 * options resolve to the translated pill labels ("Dziś"/"Today", …).
 */
@Composable
fun DateFilter.labelText(): String = when (this) {
    is DateFilter.Specific -> date
    DateFilter.Kind.TODAY -> stringResource(R.string.date_today)
    DateFilter.Kind.TOMORROW -> stringResource(R.string.date_tomorrow)
    DateFilter.Kind.WEEK -> stringResource(R.string.date_week)
    DateFilter.Kind.ANYTIME -> stringResource(R.string.all)
}

@Composable
fun SortOption.labelText(): String = when (this) {
    SortOption.EARLIEST -> stringResource(R.string.sort_earliest)
    SortOption.RATING -> stringResource(R.string.sort_rating)
}
