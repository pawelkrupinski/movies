package pl.kinowo.ui.city

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.ui.res.painterResource
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.Catalog
import pl.kinowo.model.City
import pl.kinowo.model.CityPickerRow
import pl.kinowo.model.CitySearchLevel
import pl.kinowo.model.CitySearchRow
import pl.kinowo.model.Country
import pl.kinowo.model.matching
import pl.kinowo.model.matchingInSubregion
import pl.kinowo.model.regionsIn
import pl.kinowo.model.rowKey
import pl.kinowo.model.searchLevelsFor
import pl.kinowo.model.searchRows
import pl.kinowo.model.secondLevelRows
import pl.kinowo.model.topLevelRows
import pl.kinowo.ui.CountryPicker
import pl.kinowo.ui.common.SystemBarsColumn
import pl.kinowo.ui.theme.TextSecondary

/** Height floor for the city-gate controls — a comfortable native touch target,
 *  well above Material's compact 40dp default so the buttons read as primary
 *  actions rather than small links. */
private val ControlMinHeight = 56.dp

/** The localized noun for one [CitySearchLevel] — the only place an abstract
 *  level identifier meets an `R.string`, so [searchLevelsFor] itself stays a
 *  plain JVM function with no Android/Compose dependency. */
private val CitySearchLevel.labelRes: Int
    get() = when (this) {
        CitySearchLevel.UkRegion -> R.string.search_level_uk_region
        CitySearchLevel.UkSubregion -> R.string.search_level_uk_subregion
        CitySearchLevel.UsRegion -> R.string.search_level_us_region
        CitySearchLevel.DeRegion -> R.string.search_level_de_region
    }

/** What the picker is browsing: the search text, and the region and subregion drilled into. */
private class PickerState {
    var query by mutableStateOf("")
    /** The region being browsed, on a country whose cities are grouped. Null is
     *  the first step (pick a state); non-null the second (pick a city in it). */
    var region by mutableStateOf<String?>(null)
    /** The subregion being browsed, within [region] — the third step, reached
     *  only where [region] itself splits into sub-groups holding more than one
     *  city (a UK county: Glamorgan, West Midlands…). Null everywhere else. */
    var subregion by mutableStateOf<String?>(null)

    fun reset() {
        query = ""
        region = null
        subregion = null
    }

    fun open(region: String?, subregion: String? = null) {
        this.region = region
        this.subregion = subregion
        query = ""
    }
}

/**
 * Fallback city picker shown when the location gate can't place the user
 * (permission denied, no fix, or out of range of every supported city). A
 * search box narrows the 41-city list by diacritic-insensitive substring match
 * ("lodz" finds "Łódź"); the matches scroll in a [LazyColumn], one tap target
 * per city.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CityChoiceScreen(
    catalog: Catalog,
    onPick: (City) -> Unit,
    selectedCountryCode: String? = null,
    onCountry: (String) -> Unit = {},
    onLocateMe: () -> Unit = {},
    locating: Boolean = false,
    locateFailed: Boolean = false,
) {
    val country = Country.normalizeCode(selectedCountryCode) ?: Country.default.code
    val picker = remember { PickerState() }
    // Switching country changes what every step means, so neither a half-typed
    // query nor a state from the country just left may survive it.
    LaunchedEffect(country) { picker.reset() }

    // A country that groups its cities (US states, Bundesländer, UK nations) is picked in
    // two steps; in Poland and Spain the regions are empty and it is the single flat list.
    val regions = catalog.cities.regionsIn(country)
    val pickingRegion = regions.isNotEmpty() && picker.region == null

    // The inset padding keeps the "Country" label from sitting under the clock and the inline
    // locate button under the battery icon on tall-status-bar phones (Pixel 9 Pro XL).
    SystemBarsColumn {
        CountryPicker(
            countries = catalog.countries,
            selectedCode = selectedCountryCode,
            onSelect = onCountry,
            onLocateMe = onLocateMe,
            locating = locating,
            modifier = Modifier.padding(top = 24.dp),
        )
        if (locateFailed) NoNearbyNote()
        PickerHeading(picker, pickingRegion)
        BackControl(picker)
        PickerSearchField(picker, country)
        PickerStep(catalog, country, regions.isNotEmpty(), pickingRegion, picker, onPick)
    }
}

/** The step the picker is on. A query flattens the WHOLE country instead of narrowing just
 *  the level currently drilled into — see `searchRows`. Only meaningful for a grouped
 *  country: Poland and Spain have no regions to flatten, so their own per-level search
 *  already reaches every city. */
@Composable
private fun PickerStep(
    catalog: Catalog,
    country: String,
    grouped: Boolean,
    pickingRegion: Boolean,
    picker: PickerState,
    onPick: (City) -> Unit,
) {
    when {
        grouped && picker.query.isNotBlank() -> SearchStep(catalog, country, picker, onPick)
        pickingRegion -> RegionStep(catalog, country, picker, onPick)
        picker.subregion == null -> CityStep(catalog, country, picker, onPick)
        else -> SubregionStep(catalog, country, picker, onPick)
    }
}

@Composable
private fun NoNearbyNote() {
    Text(
        stringResource(R.string.no_city_nearby_locate),
        fontSize = 13.sp,
        color = TextSecondary,
        modifier = Modifier.padding(top = 8.dp),
    )
}

@Composable
private fun PickerHeading(picker: PickerState, pickingRegion: Boolean) {
    Text(
        stringResource(if (pickingRegion) R.string.choose_region_title else R.string.choose_city_title),
        fontSize = 22.sp,
        fontWeight = FontWeight.Bold,
        modifier = Modifier.padding(top = 24.dp),
    )
    Text(
        // Inside a region or subregion, the subtitle names it: it is the
        // only thing on this screen that says which state's (or county's)
        // cities these are.
        picker.subregion ?: picker.region ?: stringResource(
            if (pickingRegion) R.string.choose_region_subtitle else R.string.choose_city_subtitle
        ),
        fontSize = 14.sp,
        color = TextSecondary,
        modifier = Modifier.padding(top = 6.dp, bottom = 16.dp),
    )
}

/** The back control of the second and third step. The subregion's own returns to the
 *  region's list, not all the way out to the top region list (`back_to_regions`, one
 *  step further). */
@Composable
private fun BackControl(picker: PickerState) {
    val subregion = picker.subregion
    val region = picker.region
    if (subregion != null) {
        BackButton(R.string.back) { picker.open(region) }
    } else if (region != null) {
        BackButton(R.string.back_to_regions) { picker.open(null) }
    }
}

@Composable
private fun BackButton(label: Int, onClick: () -> Unit) {
    TextButton(onClick = onClick, modifier = Modifier.fillMaxWidth().padding(bottom = 4.dp)) {
        Icon(painterResource(R.drawable.ic_arrow_back), contentDescription = null)
        Text(stringResource(label), fontSize = 15.sp, modifier = Modifier.padding(start = 8.dp))
    }
}

/** The search box. Its own copy names what a query can ACTUALLY reach from THIS level —
 *  "state" in the US, "nation"/"county" in the UK, never a one-size-fits-all "region" that
 *  would misname either. Mirrors `pickerSearchLevels`/`updatePickerSearchCopy` in
 *  landing.scala.html so the same visitor reads the same wording on web and here. */
@Composable
private fun PickerSearchField(picker: PickerState, country: String) {
    val searchLevels = searchLevelsFor(country, picker.region, picker.subregion)
    val searchHint = when (searchLevels.size) {
        2 -> stringResource(
            R.string.search_levels_2,
            stringResource(searchLevels[0].labelRes),
            stringResource(searchLevels[1].labelRes),
        )
        1 -> stringResource(R.string.search_levels_1, stringResource(searchLevels[0].labelRes))
        else -> stringResource(R.string.search_city_hint)
    }
    OutlinedTextField(
        value = picker.query,
        onValueChange = { picker.query = it },
        singleLine = true,
        placeholder = { Text(searchHint) },
        leadingIcon = { Icon(painterResource(R.drawable.ic_search), contentDescription = null) },
        trailingIcon = {
            if (picker.query.isNotEmpty()) {
                Icon(
                    painterResource(R.drawable.ic_close),
                    contentDescription = stringResource(R.string.clear),
                    modifier = Modifier.clickable { picker.query = "" },
                )
            }
        },
        keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
        modifier = Modifier.fillMaxWidth(),
    )
}

/** One step's rows as tall buttons, or the "nothing matched" line when there are none. */
@Composable
private fun <T> StepRows(
    rows: List<T>,
    query: String,
    noMatchMessage: Int,
    key: (T) -> Any,
    row: @Composable (T) -> Unit,
) {
    if (rows.isEmpty()) {
        NoMatches(query, noMatchMessage)
    } else {
        LazyColumn(Modifier.fillMaxWidth().padding(top = 8.dp)) {
            items(rows, key = key) { row(it) }
        }
    }
}

/** Replaces whichever step was open with ONE flat, alphabetically merged list spanning
 *  every region, subregion and city in the country — the whole point of flattening the
 *  search is that it no longer matters which level the visitor was browsing when they
 *  started typing. */
@Composable
private fun SearchStep(catalog: Catalog, country: String, picker: PickerState, onPick: (City) -> Unit) {
    StepRows(catalog.cities.searchRows(picker.query, country), picker.query, R.string.no_city_matching, { it.rowKey }) { row ->
        when (row) {
            is CitySearchRow.Region -> TallFilledButton(row.label) { picker.open(row.label) }
            is CitySearchRow.Subregion -> TallFilledButton(row.label) { picker.open(row.region, row.label) }
            is CitySearchRow.CityRow -> TallFilledButton(row.city.name) { onPick(row.city) }
        }
    }
}

/** Each region's heading interleaved with any city whose top group collapsed onto it alone
 *  (Berlin, Hamburg; Delaware, Vermont), in the catalog's own order (see `mergedPickerRows`)
 *  — not every heading followed by every direct city, which stranded a heading like "West
 *  Midlands" ahead of its alphabetical neighbours. */
@Composable
private fun RegionStep(catalog: Catalog, country: String, picker: PickerState, onPick: (City) -> Unit) {
    StepRows(catalog.cities.topLevelRows(picker.query, country), picker.query, R.string.no_region_matching, { it.rowKey }) { row ->
        when (row) {
            is CityPickerRow.Heading -> TallFilledButton(row.label) { picker.open(row.label) }
            is CityPickerRow.CityRow -> TallFilledButton(row.city.name) { onPick(row.city) }
        }
    }
}

/** The second step's rows — subregion headings (the UK's counties: Glamorgan, West
 *  Midlands…) interleaved with the region's own directly-listed cities, same merge as the
 *  first step. Falls back to the whole flat list (as plain city rows) for a country with no
 *  grouping, where the region is always null. */
@Composable
private fun CityStep(catalog: Catalog, country: String, picker: PickerState, onPick: (City) -> Unit) {
    val region = picker.region
    val rows = if (region != null) {
        catalog.cities.secondLevelRows(picker.query, country, region)
    } else {
        catalog.cities.matching(picker.query, country).map { CityPickerRow.CityRow(it) }
    }
    StepRows(rows, picker.query, R.string.no_city_matching, { it.rowKey }) { row ->
        when (row) {
            is CityPickerRow.Heading -> TallFilledButton(row.label) { picker.open(region, row.label) }
            is CityPickerRow.CityRow -> TallFilledButton(row.city.name) { onPick(row.city) }
        }
    }
}

@Composable
private fun SubregionStep(catalog: Catalog, country: String, picker: PickerState, onPick: (City) -> Unit) {
    val region = checkNotNull(picker.region) { "subregion is set, so its region must be too" }
    val subregion = checkNotNull(picker.subregion)
    val cities = catalog.cities.matchingInSubregion(picker.query, country, region, subregion)
    StepRows(cities, picker.query, R.string.no_city_matching, { it.slug }) { city ->
        TallFilledButton(city.name) { onPick(city) }
    }
}

/** The "nothing matched <query>" line, shared by both steps of the picker. */
@Composable
private fun NoMatches(query: String, message: Int) {
    Text(
        stringResource(message, query),
        fontSize = 14.sp,
        color = TextSecondary,
        modifier = Modifier.padding(top = 20.dp),
    )
}

/**
 * First-launch confirmation shown when location placed the user near a
 * supported [city]. We confirm rather than silently adopt it, so someone near
 * a border (or who simply wants another city's repertoire) can pick again via
 * [onChooseOther]. [onConfirm] adopts the detected city.
 */
@Composable
fun CityConfirmScreen(city: City, onConfirm: () -> Unit, onChooseOther: () -> Unit) {
    SystemBarsColumn(verticalArrangement = Arrangement.Center) {
        Text(stringResource(R.string.near_city_label), fontSize = 14.sp, color = TextSecondary)
        Text(
            city.name,
            fontSize = 24.sp,
            fontWeight = FontWeight.Bold,
            modifier = Modifier.padding(top = 4.dp, bottom = 20.dp),
        )
        TallFilledButton(stringResource(R.string.show_repertoire, city.name), onClick = onConfirm)
        TallOutlinedButton(stringResource(R.string.choose_other_city), onClick = onChooseOther)
    }
}

/** A full-width, tall (≥[ControlMinHeight]) filled button — the primary city action. */
@Composable
private fun TallFilledButton(text: String, onClick: () -> Unit) {
    Button(
        onClick = onClick,
        modifier = Modifier.fillMaxWidth().padding(vertical = 6.dp).heightIn(min = ControlMinHeight),
    ) { Text(text, fontSize = 17.sp, fontWeight = FontWeight.SemiBold) }
}

/** A full-width, tall (≥[ControlMinHeight]) outlined button — a real secondary
 *  control rather than a thin text link. */
@Composable
private fun TallOutlinedButton(text: String, onClick: () -> Unit) {
    OutlinedButton(
        onClick = onClick,
        modifier = Modifier.fillMaxWidth().padding(vertical = 6.dp).heightIn(min = ControlMinHeight),
    ) { Text(text, fontSize = 17.sp) }
}
