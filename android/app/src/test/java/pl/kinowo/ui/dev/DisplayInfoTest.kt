package pl.kinowo.ui.dev

import org.junit.Assert.assertEquals
import org.junit.Test

class DisplayInfoTest {
    @Test
    fun multipliesDpByDensity() {
        assertEquals(
            "viewport 411×891 dp · 2.625× → 1079×2339 px",
            DisplayInfo.tuningReadout(411, 891, 2.625f),
        )
    }

    @Test
    fun integerDensityDropsDecimal() {
        assertEquals(
            "viewport 360×800 dp · 3× → 1080×2400 px",
            DisplayInfo.tuningReadout(360, 800, 3f),
        )
    }
}
