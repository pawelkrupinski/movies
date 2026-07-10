import Foundation

struct UserProfile: Codable {
    let displayName: String?
    let email: String?
    let avatarUrl: URL?
    let provider: String
}

/// The base URL every API + auth request is built on — the SELECTED country's
/// deployment (Poland's prod URL by default). Computed, so a country switch
/// (persisted via `CountrySelection.select`) re-points new requests without any
/// call site changing. Stores capture this at init and re-point on switch via
/// `use(country:)`.
var kinowoBaseURL: URL { CountrySelection.current().baseURL }
