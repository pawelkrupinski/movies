package pl.kinowo.ui.list

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Icon
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.R

// Momentary screen-name pill (Filmy / Kina), flashed centre-screen on arrival.
// The iOS counterpart is `DayLabelOverlay` in ListingOverlays.swift.
@Composable
internal fun TabLabelOverlay(text: String, modifier: Modifier = Modifier) {
    Surface(modifier = modifier, shape = CircleShape, color = Color.Black.copy(alpha = 0.55f)) {
        Text(
            text,
            color = Color.White,
            fontSize = 26.sp,
            fontWeight = FontWeight.SemiBold,
            modifier = Modifier.padding(horizontal = 28.dp, vertical = 12.dp),
        )
    }
}

// Once-a-day onboarding hint: a swipe icon over one line of copy, telling
// first-time users they can swipe to change the selected day.
@Composable
internal fun SwipeHintOverlay(modifier: Modifier = Modifier) {
    Surface(modifier = modifier, shape = RoundedCornerShape(20.dp), color = Color.Black.copy(alpha = 0.6f)) {
        Column(
            Modifier.padding(horizontal = 24.dp, vertical = 16.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
        ) {
            Icon(
                painterResource(R.drawable.ic_swipe),
                contentDescription = null,
                tint = Color.White,
                modifier = Modifier.size(34.dp),
            )
            Text(
                stringResource(R.string.swipe_change_day),
                color = Color.White,
                fontSize = 15.sp,
                fontWeight = FontWeight.Medium,
                modifier = Modifier.padding(top = 8.dp),
            )
        }
    }
}
