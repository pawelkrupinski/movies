package pl.kinowo.ui.city

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.model.Cities
import pl.kinowo.model.City
import pl.kinowo.ui.theme.TextSecondary

/**
 * Fallback city picker shown when the location gate can't place the user
 * (permission denied, no fix, or out of range of every supported city). Lists
 * every [Cities.all] entry as a tap target — with one city it's a single tap,
 * but it scales to a row per city as the catalogue grows.
 */
@Composable
fun CityChoiceScreen(onPick: (City) -> Unit) {
    Column(
        Modifier.fillMaxSize().padding(horizontal = 24.dp),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally,
    ) {
        Text("Wybierz miasto", fontSize = 22.sp, fontWeight = FontWeight.Bold)
        Text(
            "Repertuar pokazujemy dla wybranego miasta.",
            fontSize = 14.sp,
            color = TextSecondary,
            modifier = Modifier.padding(top = 6.dp, bottom = 20.dp),
        )
        for (city in Cities.all) {
            Button(
                onClick = { onPick(city) },
                modifier = Modifier.fillMaxWidth().padding(vertical = 4.dp),
            ) { Text(city.name) }
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
        Text("Jesteś w pobliżu miasta", fontSize = 14.sp, color = TextSecondary)
        Text(
            city.name,
            fontSize = 24.sp,
            fontWeight = FontWeight.Bold,
            modifier = Modifier.padding(top = 4.dp, bottom = 20.dp),
        )
        Button(
            onClick = onConfirm,
            modifier = Modifier.fillMaxWidth().padding(vertical = 4.dp),
        ) { Text("Pokaż repertuar — ${city.name}") }
        TextButton(
            onClick = onChooseOther,
            modifier = Modifier.fillMaxWidth().padding(vertical = 4.dp),
        ) { Text("Wybierz inne miasto") }
    }
}
