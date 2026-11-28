package pl.kinowo.ui.dev

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.model.Film
import pl.kinowo.ui.common.CardSpacingStyle
import pl.kinowo.ui.common.CinemaHeaderStyle
import pl.kinowo.ui.common.FilmDetailStyle
import pl.kinowo.ui.common.RatingBadgeMetrics
import pl.kinowo.ui.common.RatingPillStyle
import pl.kinowo.ui.common.ShowtimeChipStyle

@Composable
internal fun PageControls(
    page: TuningPage,
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
    cinemaHeader: CinemaHeaderStyle,
    onCinemaHeaderChange: (CinemaHeaderStyle) -> Unit,
    filmStyle: FilmDetailStyle,
    onFilmStyleChange: (FilmDetailStyle) -> Unit,
) {
    Column(
        Modifier
            .fillMaxWidth()
            .verticalScroll(rememberScrollState())
            .padding(horizontal = 16.dp, vertical = 12.dp),
        verticalArrangement = Arrangement.spacedBy(14.dp),
    ) {
        ResolutionReadout()
        when (page) {
            TuningPage.Card -> CardControls(style, onStyleChange, ratingStyle, onRatingStyleChange, cardSpacing, onCardSpacingChange)
            TuningPage.Kina -> KinaControls(cinemaHeader, onCinemaHeaderChange)
            TuningPage.Film -> FilmControls(filmStyle, onFilmStyleChange)
        }
    }
}

// ── per-page controls ────────────────────────────────────────────────────────

@Composable
private fun CardControls(
    style: ShowtimeChipStyle,
    onStyleChange: (ShowtimeChipStyle) -> Unit,
    ratingStyle: RatingPillStyle,
    onRatingStyleChange: (RatingPillStyle) -> Unit,
    cardSpacing: CardSpacingStyle,
    onCardSpacingChange: (CardSpacingStyle) -> Unit,
) {
    ChipControls(style, onStyleChange)
    CardSpacingControls(cardSpacing, onCardSpacingChange)
    RatingControls(ratingStyle, onRatingStyleChange)
}

@Composable
private fun ChipControls(style: ShowtimeChipStyle, onStyleChange: (ShowtimeChipStyle) -> Unit) {
    Group("Czas (time)") {
        WeightRow("Grubość", style.timeWeight) { onStyleChange(style.copy(timeWeight = it)) }
        SpSlider("Rozmiar", style.timeFontSize.value, 7f..20f, TuningTags.TimeSizeSlider) { onStyleChange(style.copy(timeFontSize = it.sp)) }
    }
    Group("Format") {
        WeightRow("Grubość", style.formatWeight) { onStyleChange(style.copy(formatWeight = it)) }
        SpSlider("Rozmiar", style.formatFontSize.value, 5f..16f) { onStyleChange(style.copy(formatFontSize = it.sp)) }
    }
    Group("Padding (pigułki)") {
        DpSlider("Poziomy", style.horizontalInset, 0f..14f) { onStyleChange(style.copy(horizontalInset = it)) }
        DpSlider("Pionowy", style.verticalInset, 0f..14f) { onStyleChange(style.copy(verticalInset = it)) }
    }
    Group("Odstępy") {
        DpSlider("Czas ↔ format", style.internalGap, 0f..12f) { onStyleChange(style.copy(internalGap = it)) }
        DpSlider("Między pigułkami", style.interPillGap, 0f..16f) { onStyleChange(style.copy(interPillGap = it)) }
    }
}

@Composable
private fun CardSpacingControls(cardSpacing: CardSpacingStyle, onCardSpacingChange: (CardSpacingStyle) -> Unit) {
    Group("Odstępy karty") {
        DpSlider("Tytuł ↔ meta", cardSpacing.titleToMeta, 0f..24f) { onCardSpacingChange(cardSpacing.copy(titleToMeta = it)) }
        DpSlider("Meta ↔ oceny", cardSpacing.metaToRatings, 0f..24f) { onCardSpacingChange(cardSpacing.copy(metaToRatings = it)) }
        DpSlider("Oceny ↔ seanse", cardSpacing.ratingsToShowings, 0f..24f) { onCardSpacingChange(cardSpacing.copy(ratingsToShowings = it)) }
        DpSlider("Dzień → kino", cardSpacing.dayToCinema, 0f..24f) { onCardSpacingChange(cardSpacing.copy(dayToCinema = it)) }
        DpSlider("Blok seansów", cardSpacing.showingsBlock, 0f..24f) { onCardSpacingChange(cardSpacing.copy(showingsBlock = it)) }
    }
}

@Composable
private fun RatingControls(ratingStyle: RatingPillStyle, onRatingStyleChange: (RatingPillStyle) -> Unit) {
    Group("Ocena: rozmiar bazowy") {
        // Android scales rating pills by device width (unlike iOS's flat
        // sizing), so the slider sets the BASE sp and the readout shows what
        // this device actually renders after RatingBadgeMetrics.scale().
        RatingScaleReadout(ratingStyle.baseFontSize)
        SpSlider("Rozmiar bazowy", ratingStyle.baseFontSize.value, 6f..18f) { onRatingStyleChange(ratingStyle.copy(baseFontSize = it.sp)) }
    }
    Group("Ocena: grubości") {
        WeightRow("Etykieta", ratingStyle.labelWeight) { onRatingStyleChange(ratingStyle.copy(labelWeight = it)) }
        WeightRow("Wartość", ratingStyle.valueWeight) { onRatingStyleChange(ratingStyle.copy(valueWeight = it)) }
        WeightRow("Solid (MC)", ratingStyle.solidWeight) { onRatingStyleChange(ratingStyle.copy(solidWeight = it)) }
    }
    Group("Ocena: padding") {
        DpSlider("Poziomy", ratingStyle.hPad, 0f..12f) { onRatingStyleChange(ratingStyle.copy(hPad = it)) }
        DpSlider("Pionowy", ratingStyle.vPad, 0f..10f) { onRatingStyleChange(ratingStyle.copy(vPad = it)) }
    }
    Group("Ocena: kształt") {
        DpSlider("Zaokrąglenie", ratingStyle.corner, 0f..14f) { onRatingStyleChange(ratingStyle.copy(corner = it)) }
        DpSlider("Między pigułkami", ratingStyle.interPillGap, 0f..16f) { onRatingStyleChange(ratingStyle.copy(interPillGap = it)) }
    }
}

@Composable
private fun KinaControls(cinemaHeader: CinemaHeaderStyle, onChange: (CinemaHeaderStyle) -> Unit) {
    Group("Nagłówek kina") {
        WeightRow("Grubość", cinemaHeader.fontWeight) { onChange(cinemaHeader.copy(fontWeight = it)) }
        SpSlider("Rozmiar", cinemaHeader.fontSize.value, 10f..24f, TuningTags.CinemaHeaderFontSlider) { onChange(cinemaHeader.copy(fontSize = it.sp)) }
    }
    Group("Odstępy") {
        DpSlider("Nagłówek górny", cinemaHeader.headerTopGap, 0f..24f) { onChange(cinemaHeader.copy(headerTopGap = it)) }
        DpSlider("Nagłówek → siatka", cinemaHeader.headerToGrid, 0f..16f) { onChange(cinemaHeader.copy(headerToGrid = it)) }
        DpSlider("Między rzędami", cinemaHeader.sectionSpacing, 0f..32f) { onChange(cinemaHeader.copy(sectionSpacing = it)) }
    }
}

@Composable
private fun FilmControls(filmStyle: FilmDetailStyle, onChange: (FilmDetailStyle) -> Unit) {
    Group("Układ") {
        DpSlider("Sekcje", filmStyle.outerSpacing, 0f..32f) { onChange(filmStyle.copy(outerSpacing = it)) }
        DpSlider("Nagłówek (kolumna)", filmStyle.headerSpacing, 0f..20f) { onChange(filmStyle.copy(headerSpacing = it)) }
    }
    Group("Tytuł") {
        WeightRow("Grubość", filmStyle.titleWeight) { onChange(filmStyle.copy(titleWeight = it)) }
        SpSlider("Rozmiar", filmStyle.titleFontSize.value, 14f..34f, TuningTags.DetailTitleFontSlider) { onChange(filmStyle.copy(titleFontSize = it.sp)) }
        SpSlider("Tytuł oryg.", filmStyle.originalTitleFontSize.value, 10f..22f) { onChange(filmStyle.copy(originalTitleFontSize = it.sp)) }
    }
    Group("Bloki meta") {
        SpSlider("Etykieta", filmStyle.sectionLabelFontSize.value, 8f..16f) { onChange(filmStyle.copy(sectionLabelFontSize = it.sp)) }
        SpSlider("Wartość", filmStyle.sectionBodyFontSize.value, 10f..20f) { onChange(filmStyle.copy(sectionBodyFontSize = it.sp)) }
    }
    Group("Seanse") {
        SpSlider("Nagłówek", filmStyle.showingsHeaderFontSize.value, 12f..26f) { onChange(filmStyle.copy(showingsHeaderFontSize = it.sp)) }
    }
}
