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
import pl.kinowo.ui.theme.TextSecondary

/**
 * The in-app country switch: one pill per [Country.all]. Selecting a country
 * persists it (via [onSelect]); the activity recreates so the app re-points at
 * that country's deployment and forces its UI language. Kept compact so it can
 * sit above the city list on the first-launch gate without disturbing the
 * two-per-row card layout further down.
 */
@Composable
fun CountryPicker(
    selectedCode: String?,
    onSelect: (String) -> Unit,
    modifier: Modifier = Modifier,
) {
    val current = Country.byCode(selectedCode)
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
            Country.all.forEach { country ->
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
