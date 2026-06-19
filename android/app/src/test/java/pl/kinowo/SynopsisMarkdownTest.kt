package pl.kinowo

import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test
import pl.kinowo.ui.common.SynopsisMarkdown

class SynopsisMarkdownTest {

    @Test
    fun preservesNewlinesAndStripsMarkers() {
        val a = SynopsisMarkdown.annotated("a **b** *c*\n\nd")
        assertEquals("a b c\n\nd", a.text)
    }

    @Test
    fun boldAndItalicSpansCoverTheRightWords() {
        val a = SynopsisMarkdown.annotated("zwykły **pogrubiony** i *kursywa*")
        assertEquals("zwykły pogrubiony i kursywa", a.text)
        val bold = a.spanStyles.firstOrNull { it.item.fontWeight == FontWeight.Bold }
        val italic = a.spanStyles.firstOrNull { it.item.fontStyle == FontStyle.Italic }
        assertNotNull("expected a bold span from **…**", bold)
        assertNotNull("expected an italic span from *…*", italic)
        assertEquals("pogrubiony", a.text.substring(bold!!.start, bold.end))
        assertEquals("kursywa", a.text.substring(italic!!.start, italic.end))
    }

    @Test
    fun plainTextHasNoSpans() {
        val a = SynopsisMarkdown.annotated("bez znaczników")
        assertEquals("bez znaczników", a.text)
        assertEquals(0, a.spanStyles.size)
    }
}
