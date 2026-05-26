import Foundation

struct UserProfile: Codable {
    let displayName: String?
    let email: String?
    let avatarUrl: URL?
    let provider: String
}

let kinowoBaseURL = URL(string: "https://kinowo.fly.dev")!
