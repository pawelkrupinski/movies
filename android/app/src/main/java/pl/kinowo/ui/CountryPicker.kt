package pl.kinowo.ui

import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.selection.selectableGroup
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.MyLocation
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R
import pl.kinowo.model.Country
import pl.kinowo.model.selected
import pl.kinowo.ui.theme.TextSecondary

/** The gap between the "Country" label and the pill row below it, and — kept
 *  equal on purpose — the gap between the pill row and the locate-me icon
 *  beside it, so the header reads as one evenly-spaced group. */
private val CountryPickerGap = 6.dp

/**
 * The in-app country switch: one pill per country in [countries] (the live
 * catalog's list). Selecting a country persists it (via [onSelect]); the
 * activity recreates so the app re-points at that country's deployment and
 * forces its UI language. Kept compact so it can sit above the city list on the
 * first-launch gate without disturbing the two-per-row card layout further down.
 *
 * The pill row SCROLLS horizontally and each label stays on ONE line: a plain
 * [Row] squeezes its children once the labels stop fitting ("Polska",
 * "United Kingdom", "Deutschland", "United States" already overflow a phone
 * width), and a squeezed label wraps, which grows the row's height and pushes
 * the city list below the fold. Scrolling keeps the header exactly one pill tall
 * however many countries the catalog carries.
 *
 * [onLocateMe] renders as an icon trailing the pill row — inline rather than
 * overlaid, so it costs no extra height and its gap from the pills can match
 * [CountryPickerGap] exactly, the same as the label's gap from the pills.
 */
@Composable
fun CountryPicker(
    countries: List<Country>,
    selectedCode: String?,
    onSelect: (String) -> Unit,
    modifier: Modifier = Modifier,
    onLocateMe: () -> Unit = {},
    locating: Boolean = false,
) {
    val current = countries.selected(selectedCode)
    Column(modifier.fillMaxWidth()) {
        Text(
            stringResource(R.string.country_label),
            fontSize = 19.5.sp,
            color = TextSecondary,
            modifier = Modifier.padding(bottom = CountryPickerGap),
        )
        Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
            Row(
                Modifier
                    .weight(1f)
                    .horizontalScroll(rememberScrollState())
                    .selectableGroup(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                countries.forEach { country ->
                    val selected = country.code == current.code
                    if (selected) {
                        Button(onClick = { onSelect(country.code) }) {
                            Text(country.displayName, fontWeight = FontWeight.SemiBold, maxLines = 1)
                        }
                    } else {
                        OutlinedButton(
                            onClick = { onSelect(country.code) },
                            colors = ButtonDefaults.outlinedButtonColors(),
                        ) {
                            Text(country.displayName, maxLines = 1)
                        }
                    }
                }
            }
            IconButton(
                onClick = onLocateMe,
                enabled = !locating,
                modifier = Modifier.padding(start = CountryPickerGap),
            ) {
                if (locating) {
                    CircularProgressIndicator(Modifier.size(20.dp), strokeWidth = 2.dp)
                } else {
                    Icon(Icons.Filled.MyLocation, contentDescription = stringResource(R.string.locate_me))
                }
            }
        }
    }
}
