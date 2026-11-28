package pl.kinowo.ui.list

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material3.Checkbox
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.material3.TriStateCheckbox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.pluralStringResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.state.ToggleableState
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.filter.CinemaCheck
import pl.kinowo.filter.CinemaFilterSection
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.FilterFacets
import pl.kinowo.model.CinemaArea
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.TextSecondary

/**
 * Kina — the single cinema filter, over the `disabledCinemas` exclusion set.
 *
 * A **flat** city renders one checkbox per cinema under a "Wszystkie kina"
 * master; a **split** city (e.g. London) renders the same universe grouped into
 * collapsible areas, each with its own tri-state checkbox. Both shapes and all
 * the toggle arithmetic come from [CinemaFilterSection] — this composable only
 * draws it. Mirrors the web's Filtry cinema panel and iOS FiltersSheet's "Kina".
 *
 * Cinema universe: the city catalog when it has loaded (it includes venues with
 * no showings today, so the list doesn't flicker as the day changes), otherwise
 * the cinemas actually present in [films]. Exercised by
 * FiltersSheetCinemaSectionTest.
 */
@Composable
internal fun CinemasSection(viewModel: KinowoViewModel, films: List<Film>) {
    val catalog by viewModel.catalog.collectAsState()
    val disabled by viewModel.disabledCinemas.collectAsState()
    val fromFilms = remember(films) { FilterFacets.allCinemas(films) }
    val effective = remember(catalog, fromFilms) {
        if (catalog.cinemas.isNotEmpty()) catalog else CinemaCatalog(fromFilms, emptyList())
    }
    if (effective.cinemas.isEmpty()) return

    val section = CinemaFilterSection(effective, disabled)
    val offCount = section.cityCinemas.size - section.enabledCount

    CollapsibleSection(
        stringResource(R.string.filter_cinemas),
        if (offCount > 0) pluralStringResource(R.plurals.cinemas_off_count, offCount, offCount) else null,
    ) {
        // A plain Column (not a capped, inner-scrolling LazyColumn) so expanding
        // shows EVERY cinema — the outer sheet scrolls.
        Column(Modifier.fillMaxWidth()) {
            ToggleRow(stringResource(R.string.areabar_all_cinemas), section.allCheck == CinemaCheck.ON) { on ->
                viewModel.setDisabledCinemas(section.settingAll(on))
            }
            if (section.isSplit) {
                effective.areas.forEach { area ->
                    CinemaAreaGroup(
                        area = area,
                        catalog = effective,
                        section = section,
                        onSetArea = { on -> viewModel.setDisabledCinemas(section.settingArea(area, on)) },
                        onSetCinema = { cinema, on ->
                            viewModel.setDisabledCinemas(section.settingCinema(cinema, on))
                        },
                    )
                }
            } else {
                effective.seasonOrdered(section.cityCinemas).forEach { cinema ->
                    CinemaRow(
                        cinema = cinema,
                        catalog = effective,
                        checked = section.checkOfCinema(cinema) == CinemaCheck.ON,
                    ) { on -> viewModel.setDisabledCinemas(section.settingCinema(cinema, on)) }
                }
            }
        }
    }
}

/**
 * One area group inside [CinemasSection]: a checkbox covering the whole area
 * (ticked / cleared / indeterminate) beside a tappable name that folds out the
 * area's own cinemas. Only used by split cities.
 */
@Composable
private fun CinemaAreaGroup(
    area: CinemaArea,
    catalog: CinemaCatalog,
    section: CinemaFilterSection,
    onSetArea: (Boolean) -> Unit,
    onSetCinema: (String, Boolean) -> Unit,
) {
    var open by rememberSaveable(area.slug) { mutableStateOf(false) }
    val check = section.checkOfArea(area)
    Column(Modifier.fillMaxWidth()) {
        AreaHeader(area, check, open, onToggleOpen = { open = !open }, onSetArea)
        if (open) {
            Column(Modifier.fillMaxWidth().padding(start = 24.dp)) {
                catalog.seasonOrdered(area.cinemas).forEach { cinema ->
                    CinemaRow(
                        cinema = cinema,
                        catalog = catalog,
                        checked = section.checkOfCinema(cinema) == CinemaCheck.ON,
                    ) { on -> onSetCinema(cinema, on) }
                }
            }
        }
    }
}

@Composable
private fun AreaHeader(
    area: CinemaArea,
    check: CinemaCheck,
    open: Boolean,
    onToggleOpen: () -> Unit,
    onSetArea: (Boolean) -> Unit,
) {
    Row(
        Modifier.fillMaxWidth().padding(vertical = 4.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        TriStateCheckbox(
            state = when (check) {
                CinemaCheck.ON -> ToggleableState.On
                CinemaCheck.OFF -> ToggleableState.Off
                CinemaCheck.MIXED -> ToggleableState.Indeterminate
            },
            onClick = { onSetArea(check != CinemaCheck.ON) },
        )
        Row(
            Modifier.weight(1f).clickable(onClick = onToggleOpen),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(area.name, modifier = Modifier.weight(1f))
            Text(
                "${area.cinemas.size}",
                color = TextSecondary,
                fontSize = 12.sp,
                modifier = Modifier.padding(end = 8.dp),
            )
            Icon(
                if (open) painterResource(R.drawable.ic_expand_less) else painterResource(R.drawable.ic_expand_more),
                contentDescription = null,
            )
        }
    }
}

/**
 * One cinema's checkbox row, labelled with its pill name. An out-of-season venue
 * (open-air, drive-in) carries a muted second line, "back in season · May–Sept",
 * in the UI language. Exercised by CinemaRowTest.
 */
@Composable
internal fun CinemaRow(cinema: String, catalog: CinemaCatalog, checked: Boolean, onChange: (Boolean) -> Unit) {
    val language = LocalConfiguration.current.locales[0].language
    val season = catalog.seasonOf(cinema)?.takeIf { it.offSeason }
    Row(
        Modifier.fillMaxWidth().clickable { onChange(!checked) }.padding(vertical = 1.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Checkbox(checked = checked, onCheckedChange = onChange)
        Column {
            Text(CinemaSection.pillName(cinema), fontSize = 14.sp)
            if (season != null) {
                Text(season.backInSeasonLine(language), fontSize = 12.sp, color = TextSecondary)
            }
        }
    }
}
