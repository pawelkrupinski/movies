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
import androidx.compose.material3.LocalMinimumInteractiveComponentSize
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.Text
import androidx.compose.runtime.CompositionLocalProvider
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

/** The gap between the header row (the "Country" label + locate-me icon) and
 *  the pill row below it. */
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
 * [onLocateMe] renders as an icon trailing the "Country" label, ABOVE the pill
 * row rather than beside it — its own row costs no extra height on top of the
 * label's, and its gap down to the pills matches [CountryPickerGap] exactly.
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
        Row(
            Modifier.fillMaxWidth().padding(bottom = CountryPickerGap),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                stringResource(R.string.country_label),
                fontSize = 19.5.sp,
                color = TextSecondary,
                modifier = Modifier.weight(1f),
            )
            // The default 48dp touch target is meant for a row with no other
            // height driver — here it would inflate the whole header row well
            // past what the "Country" label needs, pushing the city list (and
            // on a short subregion list, its last row) further down for no
            // visual gain. Sized instead to sit comfortably beside the label.
            CompositionLocalProvider(LocalMinimumInteractiveComponentSize provides 32.dp) {
                IconButton(onClick = onLocateMe, enabled = !locating, modifier = Modifier.size(32.dp)) {
                    if (locating) {
                        CircularProgressIndicator(Modifier.size(18.dp), strokeWidth = 2.dp)
                    } else {
                        Icon(
                            Icons.Filled.MyLocation,
                            contentDescription = stringResource(R.string.locate_me),
                            modifier = Modifier.size(20.dp),
                        )
                    }
                }
            }
        }
        Row(
            Modifier
                .fillMaxWidth()
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
    }
}
