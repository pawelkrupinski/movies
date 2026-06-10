package pl.kinowo.ui.detail

import android.annotation.SuppressLint
import android.graphics.Outline
import android.view.View
import android.view.ViewOutlineProvider
import android.webkit.WebChromeClient
import android.webkit.WebView
import android.widget.FrameLayout
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.OpenInNew
import androidx.compose.material.icons.filled.Share
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.viewinterop.AndroidView
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.ui.common.FullScreenPoster
import pl.kinowo.ui.common.LocalFilmDetailStyle
import pl.kinowo.ui.common.MetaPills
import pl.kinowo.ui.common.PosterImage
import pl.kinowo.ui.common.RatingBadges
import pl.kinowo.ui.common.Showings
import pl.kinowo.ui.common.openUrl
import pl.kinowo.ui.common.shareFilm
import pl.kinowo.ui.theme.Brand
import pl.kinowo.ui.theme.CardElevated
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.TextSecondary

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class, ExperimentalFoundationApi::class)
@Composable
fun DetailScreen(film: Film?, details: FilmDetails?, onBack: () -> Unit) {
    val style = LocalFilmDetailStyle.current
    // Whether the poster is currently shown full-screen (tap / long-press on it).
    var showFullPoster by remember { mutableStateOf(false) }
    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text(film?.title ?: "", maxLines = 1, fontSize = 18.sp) },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Wstecz")
                    }
                },
                actions = {
                    // Shares the canonical `/film?title=…` link — same URL a
                    // user would copy from the website's address bar.
                    if (film != null) {
                        val context = LocalContext.current
                        IconButton(onClick = { shareFilm(context, film.title) }) {
                            Icon(Icons.Filled.Share, contentDescription = "Udostępnij")
                        }
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(),
            )
        },
    ) { padding ->
        if (film == null) {
            Box(Modifier.fillMaxSize().padding(padding), contentAlignment = Alignment.Center) {
                Text("Nie znaleziono filmu.", color = TextSecondary)
            }
            return@Scaffold
        }
        Column(
            Modifier.fillMaxSize().padding(padding).verticalScroll(rememberScrollState()).padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(style.outerSpacing),
        ) {
            // Header: poster + title/ratings.
            Row(horizontalArrangement = Arrangement.spacedBy(12.dp)) {
                val hasPoster = film.posterChain.isNotEmpty()
                PosterImage(
                    chain = film.posterChain,
                    contentDescription = film.title,
                    modifier = Modifier
                        .width(150.dp)
                        .aspectRatio(2f / 3f)
                        .clip(RoundedCornerShape(10.dp))
                        .testTag(DetailPosterTag)
                        // Tap or long-press opens the full-screen viewer.
                        .combinedClickable(
                            enabled = hasPoster,
                            onClick = { showFullPoster = true },
                            onLongClick = { showFullPoster = true },
                        ),
                )
                Column(Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(style.headerSpacing)) {
                    Text(film.title, fontSize = style.titleFontSize, fontWeight = style.titleWeight, color = Color.White)
                    // Original (production-language) title, when the backend
                    // reports one distinct from the Polish cinema title.
                    details?.originalTitle?.takeIf { it.isNotBlank() }?.let { original ->
                        Text(
                            original,
                            fontSize = style.originalTitleFontSize,
                            fontStyle = FontStyle.Italic,
                            color = Color(0xFFAAAAAA),
                        )
                    }
                    // Runtime / year / genre pills — the `/film` title block
                    // shows every genre (no cap, unlike the listing card).
                    MetaPills(
                        runtimeMinutes = film.runtimeMinutes,
                        releaseYear = film.releaseYear,
                        genres = film.genres,
                    )
                    RatingBadges(film.ratings)
                }
            }

            // Cinema links.
            if (film.cinemaLinks.isNotEmpty()) {
                FlowRow(horizontalArrangement = Arrangement.spacedBy(8.dp), verticalArrangement = Arrangement.spacedBy(8.dp)) {
                    val context = LocalContext.current
                    for (link in film.cinemaLinks) {
                        Row(
                            Modifier
                                .clip(RoundedCornerShape(8.dp))
                                .background(CardElevated)
                                .clickable { openUrl(context, link.url) }
                                .padding(horizontal = 10.dp, vertical = 6.dp),
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(4.dp),
                        ) {
                            Text(link.cinema, color = CinemaBlue, fontSize = 13.sp, fontWeight = FontWeight.Medium)
                            Icon(Icons.Filled.OpenInNew, contentDescription = null, tint = CinemaBlue, modifier = Modifier.size(13.dp))
                        }
                    }
                }
            }

            // Meta blocks. Synopsis comes from the parallel /api/details fetch;
            // director/cast are already on the listing Film.
            MetaBlock("Opis", details?.synopsis)
            MetaBlock("Reżyseria", film.directors.joinToString(", ").ifEmpty { null })
            MetaBlock("Obsada", film.cast.joinToString(", ").ifEmpty { null })
            MetaBlock("Kraj(e) produkcji", film.countries.joinToString(", ").ifEmpty { null })

            // Trailers (from /api/details).
            val trailers = details?.trailerURLs.orEmpty()
            if (trailers.isNotEmpty()) {
                TrailerSection(trailers)
            }

            // Seanse (full).
            if (film.showings.isNotEmpty()) {
                Column(verticalArrangement = Arrangement.spacedBy(6.dp)) {
                    Text("Seanse", fontSize = style.showingsHeaderFontSize, fontWeight = FontWeight.SemiBold, color = Color.White)
                    Showings(film = film, showCinemaHeaders = true, maxChips = null)
                }
            }
        }

        if (showFullPoster && film.posterChain.isNotEmpty()) {
            FullScreenPoster(
                chain = film.posterChain,
                contentDescription = film.title,
                onDismiss = { showFullPoster = false },
            )
        }
    }
}

/** testTag the UI/unit tests use to find the detail-header poster. */
const val DetailPosterTag = "detailPoster"

@Composable
private fun MetaBlock(label: String, value: String?) {
    if (value.isNullOrBlank()) return
    val style = LocalFilmDetailStyle.current
    Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
        Text(label.uppercase(), color = TextSecondary, fontSize = style.sectionLabelFontSize, fontWeight = FontWeight.SemiBold)
        Text(value, color = Color.White, fontSize = style.sectionBodyFontSize, lineHeight = 20.sp)
    }
}

@Composable
private fun TrailerSection(trailers: List<String>) {
    // Null until a pill is tapped: the trailer must NOT autoplay when the film
    // screen opens — it starts only on an explicit tap. (Tapping is also the user
    // gesture that makes the embed's autoplay reliable.)
    var selected by remember(trailers) { mutableStateOf<Int?>(null) }
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text("Zwiastuny", color = TextSecondary, fontSize = 11.sp, fontWeight = FontWeight.SemiBold)
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            trailers.forEachIndexed { i, _ ->
                val active = i == selected
                Text(
                    "Zwiastun ${i + 1}",
                    color = if (active) Color.Black else CinemaBlue,
                    fontSize = 13.sp,
                    fontWeight = FontWeight.Medium,
                    modifier = Modifier
                        .clip(RoundedCornerShape(8.dp))
                        .background(if (active) Brand else CardElevated)
                        .clickable { selected = i }
                        .padding(horizontal = 12.dp, vertical = 6.dp),
                )
            }
        }
        selected?.let { TrailerPlayer(trailers[it]) }
    }
}

@SuppressLint("SetJavaScriptEnabled")
@Composable
private fun TrailerPlayer(embedUrl: String) {
    val html = remember(embedUrl) { embedHtml(embedUrl) }
    AndroidView(
        // No `Modifier.clip` here — it forces the view into an offscreen layer the
        // video can't composite into. Corners are rounded on the wrapper below.
        modifier = Modifier.fillMaxWidth().aspectRatio(16f / 9f),
        factory = { ctx ->
            val web = WebView(ctx).apply {
                settings.javaScriptEnabled = true
                settings.mediaPlaybackRequiresUserGesture = false
                settings.domStorageEnabled = true
                // An HTML5 iframe player (YouTube/Vimeo) needs a chrome client to
                // actually play video in a WebView (and to hand off fullscreen) —
                // without it the embed renders but tapping play does nothing.
                webChromeClient = WebChromeClient()
                // YouTube renders inline video into a SurfaceView that punches a
                // transparent hole through the window compositor. An OPAQUE WebView
                // background (e.g. BLACK) paints over that hole, so the trailer
                // plays audio but shows black. Keep it transparent and on a
                // hardware layer so the video surface composites through.
                setBackgroundColor(android.graphics.Color.TRANSPARENT)
                setLayerType(View.LAYER_TYPE_HARDWARE, null)
            }
            // Wrap the WebView in a plain ViewGroup: returning the SurfaceView-hosting
            // WebView directly as the AndroidView root lets Compose impose an
            // offscreen layer that blacks the video; the FrameLayout insulates it.
            // Round the corners here, on the wrapper, not on the video surface.
            FrameLayout(ctx).apply {
                val radiusPx = 8f * ctx.resources.displayMetrics.density
                clipToOutline = true
                outlineProvider = object : ViewOutlineProvider() {
                    override fun getOutline(view: View, outline: Outline) =
                        outline.setRoundRect(0, 0, view.width, view.height, radiusPx)
                }
                addView(
                    web,
                    FrameLayout.LayoutParams(
                        FrameLayout.LayoutParams.MATCH_PARENT,
                        FrameLayout.LayoutParams.MATCH_PARENT,
                    ),
                )
            }
        },
        // `update` re-fires on every recomposition; only (re)load when the embed
        // actually changes, otherwise an in-progress trailer is reloaded from
        // scratch each time the flow-driven detail screen recomposes.
        update = { frame ->
            val web = (frame as FrameLayout).getChildAt(0) as WebView
            if (web.tag != html) {
                web.tag = html
                web.loadDataWithBaseURL(TrailerEmbedBaseUrl, html, "text/html", "utf-8", null)
            }
        },
    )
}

/**
 * The base origin the trailer iframe is loaded under. It MUST be a genuine
 * third-party https origin — our own site, the same one the web template embeds
 * trailers from — not `youtube.com` and not a synthetic/opaque origin.
 *
 * History: loading the embed as a top-level `WebView.loadUrl` navigation
 * (`youtube.com/embed/ID`) renders the page but YouTube refuses to play — embed
 * URLs are built for iframes, and a top-level load has no embedding referer.
 * Wrapping it in an iframe under `baseURL = "https://www.youtube.com"` also
 * failed: a fake same-origin youtube.com parent breaks the player's parent
 * handshake, so it renders but the play button is dead. Using our real site
 * origin makes Android present exactly the referer YouTube already allows
 * playback for on the web — this matches the proven iOS fix
 * (TrailerEmbedHTML + WKWebView under the same `kinowo.fly.dev` base).
 */
internal const val TrailerEmbedBaseUrl = "https://kinowo.fly.dev"

/**
 * Minimal full-bleed iframe page for a provider embed URL
 * ([services.movies.TrailerEmbed.embedUrlFor] shape from the server —
 * `youtube.com/embed/ID`, `player.vimeo.com/video/ID`), with inline autoplay
 * params appended so the trailer starts on open.
 */
internal fun embedHtml(embedUrl: String): String {
    val src = trailerEmbedUrl(embedUrl).replace("&", "&amp;").replace("\"", "&quot;")
    return """
        <!DOCTYPE html><html><head>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <style>*{margin:0;padding:0;overflow:hidden}body{background:#000}
        iframe{width:100%;height:100%;position:absolute;top:0;left:0;border:0}</style>
        </head><body>
        <iframe src="$src" allow="autoplay; encrypted-media; picture-in-picture" allowfullscreen></iframe>
        </body></html>
    """.trimIndent()
}

/** A provider embed URL with inline autoplay params appended (idempotent). */
internal fun trailerEmbedUrl(embedUrl: String): String =
    if (embedUrl.contains("autoplay=")) embedUrl
    else embedUrl + (if (embedUrl.contains("?")) "&" else "?") + "autoplay=1&playsinline=1"
