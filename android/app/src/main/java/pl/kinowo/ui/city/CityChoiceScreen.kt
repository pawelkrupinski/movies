package pl.kinowo.ui.city

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.MyLocation
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.Button
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
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
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.Catalog
import pl.kinowo.model.City
import pl.kinowo.model.Country
import pl.kinowo.model.directCitiesIn
import pl.kinowo.model.matching
import pl.kinowo.model.matchingInSubregion
import pl.kinowo.model.regionsIn
import pl.kinowo.model.regionsMatching
import pl.kinowo.model.subregionsMatching
import pl.kinowo.ui.CountryPicker
import pl.kinowo.ui.theme.TextSecondary

/** Height floor for the city-gate controls — a comfortable native touch target,
 *  well above Material's compact 40dp default so the buttons read as primary
 *  actions rather than small links. */
private val ControlMinHeight = 56.dp

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
    var query by remember { mutableStateOf("") }
    // The region being browsed, on a country whose cities are grouped. Null is
    // the first step (pick a state); non-null the second (pick a city in it).
    var region by remember { mutableStateOf<String?>(null) }
    // The subregion being browsed, within [region] — the third step, reached
    // only where [region] itself splits into sub-groups holding more than one
    // city (the UK's West Midlands / Glamorgan / Antrim). Null everywhere else.
    var subregion by remember { mutableStateOf<String?>(null) }
    // Switching country changes what every step means, so neither a half-typed
    // query nor a state from the country just left may survive it.
    LaunchedEffect(country) {
        query = ""
        region = null
        subregion = null
    }

    // A country that groups its cities — the US by state, Germany by Bundesland,
    // the UK by nation — is picked in two steps: 468 US places in one A-to-Z is
    // not a list anybody reads. In Poland and Spain the regions are empty and
    // this collapses to the single flat list it always was.
    val regions = catalog.cities.regionsIn(country)
    val pickingRegion = regions.isNotEmpty() && region == null

    // A Box, not another Column row: the city list below already has no
    // height budget to spare (a LazyColumn with no explicit/weighted height
    // sizes to its full content here, and a UK subregion's three tall rows
    // already reach the fold) — an absolutely-positioned overlay is the only
    // way to offer the button without pushing a lower row off-screen.
    Box(Modifier.fillMaxSize()) {
        Column(
            Modifier.fillMaxSize().padding(horizontal = 24.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            CountryPicker(
                countries = catalog.countries,
                selectedCode = selectedCountryCode,
                onSelect = onCountry,
                modifier = Modifier.padding(top = 24.dp),
            )
            if (locateFailed) {
                Text(
                    stringResource(R.string.no_city_nearby_locate),
                    fontSize = 13.sp,
                    color = TextSecondary,
                    modifier = Modifier.padding(top = 8.dp),
                )
            }
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
                subregion ?: region ?: stringResource(
                    if (pickingRegion) R.string.choose_region_subtitle else R.string.choose_city_subtitle
                ),
                fontSize = 14.sp,
                color = TextSecondary,
                modifier = Modifier.padding(top = 6.dp, bottom = 16.dp),
            )
            if (subregion != null) {
                // Its own back control — returns to the region's own list, not all
                // the way out to the top region list (that's `back_to_regions`,
                // one step further).
                TextButton(
                    onClick = { subregion = null; query = "" },
                    modifier = Modifier.fillMaxWidth().padding(bottom = 4.dp),
                ) {
                    Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = null)
                    Text(stringResource(R.string.back), fontSize = 15.sp, modifier = Modifier.padding(start = 8.dp))
                }
            } else if (region != null) {
                TextButton(
                    onClick = { region = null; query = "" },
                    modifier = Modifier.fillMaxWidth().padding(bottom = 4.dp),
                ) {
                    Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = null)
                    Text(
                        stringResource(R.string.back_to_regions),
                        fontSize = 15.sp,
                        modifier = Modifier.padding(start = 8.dp),
                    )
                }
            }
            OutlinedTextField(
                value = query,
                onValueChange = { query = it },
                singleLine = true,
                placeholder = {
                    Text(stringResource(if (pickingRegion) R.string.search_region_hint else R.string.search_city_hint))
                },
                leadingIcon = { Icon(Icons.Filled.Search, contentDescription = null) },
                trailingIcon = {
                    if (query.isNotEmpty()) {
                        Icon(
                            Icons.Filled.Close,
                            contentDescription = stringResource(R.string.clear),
                            modifier = Modifier.clickable { query = "" },
                        )
                    }
                },
                keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
                modifier = Modifier.fillMaxWidth(),
            )
            if (pickingRegion) {
                val shownRegions = catalog.cities.regionsMatching(query, country)
                // Cities whose TOP group collapsed onto them alone (Berlin, Hamburg
                // — Germany's single-region city-states; Delaware, Vermont — US
                // states too small to split), shown as direct rows right on this
                // step since there is no group left to name.
                val topDirect = catalog.cities.matching(query, country).filter { it.region == null }
                if (shownRegions.isEmpty() && topDirect.isEmpty()) {
                    NoMatches(query, R.string.no_region_matching)
                } else {
                    LazyColumn(Modifier.fillMaxWidth().padding(top = 8.dp)) {
                        items(shownRegions, key = { it }) { name ->
                            TallFilledButton(name) { region = name; query = "" }
                        }
                        items(topDirect, key = { it.slug }) { city ->
                            TallFilledButton(city.name) { onPick(city) }
                        }
                    }
                }
            } else if (subregion == null) {
                val currentRegion = region
                val shownSubregions = if (currentRegion != null) catalog.cities.subregionsMatching(query, country, currentRegion) else emptyList()
                // The second step's direct rows: cities in `region` with no
                // subregion of their own — the whole list for a flat country,
                // where `region` is always null.
                val direct = if (currentRegion != null) {
                    catalog.cities.directCitiesIn(query, country, currentRegion)
                } else {
                    catalog.cities.matching(query, country)
                }
                if (shownSubregions.isEmpty() && direct.isEmpty()) {
                    NoMatches(query, R.string.no_city_matching)
                } else {
                    LazyColumn(Modifier.fillMaxWidth().padding(top = 8.dp)) {
                        items(shownSubregions, key = { it }) { name ->
                            TallFilledButton(name) { subregion = name; query = "" }
                        }
                        items(direct, key = { it.slug }) { city ->
                            TallFilledButton(city.name) { onPick(city) }
                        }
                    }
                }
            } else {
                val currentRegion = region
                val currentSubregion = subregion
                checkNotNull(currentRegion) { "subregion is set, so its region must be too" }
                checkNotNull(currentSubregion)
                val cities = catalog.cities.matchingInSubregion(query, country, currentRegion, currentSubregion)
                if (cities.isEmpty()) {
                    NoMatches(query, R.string.no_city_matching)
                } else {
                    LazyColumn(Modifier.fillMaxWidth().padding(top = 8.dp)) {
                        items(cities, key = { it.slug }) { city ->
                            TallFilledButton(city.name) { onPick(city) }
                        }
                    }
                }
            }
        }
        // Re-runs the same first-launch location check on demand — for a
        // visitor who's already reached this screen (location found nothing
        // the first time, or a deliberate "choose other city") and wants to
        // check what's nearby without typing it. Overlaid rather than laid
        // out inline so it costs the list below it no height.
        IconButton(
            onClick = onLocateMe,
            enabled = !locating,
            modifier = Modifier.align(Alignment.TopEnd).padding(top = 20.dp, end = 12.dp),
        ) {
            if (locating) {
                CircularProgressIndicator(Modifier.size(20.dp), strokeWidth = 2.dp)
            } else {
                Icon(Icons.Filled.MyLocation, contentDescription = stringResource(R.string.locate_me))
            }
        }
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
    Column(
        Modifier.fillMaxSize().padding(horizontal = 24.dp),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally,
    ) {
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
