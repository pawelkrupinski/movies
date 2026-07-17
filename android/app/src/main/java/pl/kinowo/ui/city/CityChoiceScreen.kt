package pl.kinowo.ui.city

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.Button
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
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
import pl.kinowo.model.Cities
import pl.kinowo.model.City
import pl.kinowo.model.Country
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
    onPick: (City) -> Unit,
    selectedCountryCode: String? = null,
    onCountry: (String) -> Unit = {},
) {
    var query by remember { mutableStateOf("") }
    // Scope the list to the selected country so the chooser shows UK regions for
    // a GB user, Polish cities for a PL user.
    val cities = Cities.matching(query, selectedCountryCode ?: Country.default.code)

    Column(
        Modifier.fillMaxSize().padding(horizontal = 24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
    ) {
        CountryPicker(
            selectedCode = selectedCountryCode,
            onSelect = onCountry,
            modifier = Modifier.padding(top = 24.dp),
        )
        Text(
            stringResource(R.string.choose_city_title),
            fontSize = 22.sp,
            fontWeight = FontWeight.Bold,
            modifier = Modifier.padding(top = 24.dp),
        )
        Text(
            stringResource(R.string.choose_city_subtitle),
            fontSize = 14.sp,
            color = TextSecondary,
            modifier = Modifier.padding(top = 6.dp, bottom = 16.dp),
        )
        OutlinedTextField(
            value = query,
            onValueChange = { query = it },
            singleLine = true,
            placeholder = { Text(stringResource(R.string.search_city_hint)) },
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
        if (cities.isEmpty()) {
            Text(
                stringResource(R.string.no_city_matching, query),
                fontSize = 14.sp,
                color = TextSecondary,
                modifier = Modifier.padding(top = 20.dp),
            )
        } else {
            LazyColumn(Modifier.fillMaxWidth().padding(top = 8.dp)) {
                items(cities, key = { it.slug }) { city ->
                    TallFilledButton(city.name) { onPick(city) }
                }
            }
        }
    }
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
