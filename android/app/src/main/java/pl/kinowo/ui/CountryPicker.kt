package pl.kinowo.ui

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.selection.selectableGroup
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.Country
import pl.kinowo.model.isSwitchable
import pl.kinowo.model.withCode
import pl.kinowo.ui.theme.TextSecondary

/**
 * The in-app country switch: one pill per country in [countries] (the live
 * catalog's list). Selecting a country persists it (via [onSelect]); the
 * activity recreates so the app re-points at that country's deployment and
 * forces its UI language. Kept compact so it can sit above the city list on the
 * first-launch gate without disturbing the two-per-row card layout further down.
 *
 * Renders NOTHING when fewer than two countries are deployed ([isSwitchable]) —
 * a one-pill row is a control the user can't act on, and the label above it
 * would announce a choice that doesn't exist. Guarding here rather than at the
 * call site keeps every caller correct; the filters sheet's parallel
 * [pl.kinowo.ui.list] country section applies the same test.
 */
@Composable
fun CountryPicker(
    countries: List<Country>,
    selectedCode: String?,
    onSelect: (String) -> Unit,
    modifier: Modifier = Modifier,
) {
    if (!countries.isSwitchable) return
    val current = countries.withCode(Country.normalizeCode(selectedCode)) ?: Country.default
    Column(modifier.fillMaxWidth()) {
        Text(
            stringResource(R.string.country_label),
            fontSize = 13.sp,
            color = TextSecondary,
            modifier = Modifier.padding(bottom = 6.dp),
        )
        Row(
            Modifier.fillMaxWidth().selectableGroup(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            countries.forEach { country ->
                val selected = country.code == current.code
                if (selected) {
                    Button(onClick = { onSelect(country.code) }) {
                        Text(country.displayName, fontWeight = FontWeight.SemiBold)
                    }
                } else {
                    OutlinedButton(
                        onClick = { onSelect(country.code) },
                        colors = ButtonDefaults.outlinedButtonColors(),
                    ) {
                        Text(country.displayName)
                    }
                }
            }
        }
    }
}
