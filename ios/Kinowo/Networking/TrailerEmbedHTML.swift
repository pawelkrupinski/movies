import Foundation

enum TrailerEmbedHTML {

    static func withAutoplay(_ url: URL) -> URL {
        var components = URLComponents(url: url, resolvingAgainstBaseURL: false)
        var items = components?.queryItems ?? []
        guard !items.contains(where: { $0.name == "autoplay" }) else {
            return url
        }
        items.append(URLQueryItem(name: "autoplay", value: "1"))
        items.append(URLQueryItem(name: "playsinline", value: "1"))
        components?.queryItems = items
        return components?.url ?? url
    }

    static func embedPage(videoURL: URL) -> String {
        let source = videoURL.absoluteString
            .replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "\"", with: "&quot;")
        return """
        <!DOCTYPE html>\
        <html><head>\
        <meta name="viewport" content="width=device-width,initial-scale=1">\
        <style>*{margin:0;padding:0;overflow:hidden}body{background:#000}\
        iframe{width:100%;height:100%;position:absolute;top:0;left:0;border:0}</style>\
        </head><body>\
        <iframe src="\(source)" \
        allow="autoplay; encrypted-media; picture-in-picture" \
        allowfullscreen></iframe>\
        </body></html>
        """
    }
}
