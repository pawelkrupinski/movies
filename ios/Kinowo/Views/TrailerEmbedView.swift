import SwiftUI

#if canImport(WebKit)
import WebKit

struct TrailerEmbedView: UIViewRepresentable {
    let url: URL

    final class Coordinator { var loadedURL: URL? }
    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        let view = WKWebView(frame: .zero, configuration: config)
        view.backgroundColor = .black
        view.isOpaque = false
        view.scrollView.isScrollEnabled = false
        return view
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        let embedURL = TrailerEmbedHTML.withAutoplay(url)
        guard context.coordinator.loadedURL != embedURL else { return }
        context.coordinator.loadedURL = embedURL
        webView.loadHTMLString(
            TrailerEmbedHTML.embedPage(videoURL: embedURL),
            baseURL: kinowoBaseURL
        )
    }
}
#else
struct TrailerEmbedView: View {
    let url: URL
    var body: some View { Link("filmdetail.open_trailer", destination: url) }
}
#endif
