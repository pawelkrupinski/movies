import Foundation

/// Formats a `DayShowings.date` (`YYYY-MM-DD`) into the long weekday + day +
/// month label the web renders for the same date (`DateFormatter.format` in
/// `web/src/main/scala/controllers/DateFormatter.scala`), e.g.
/// "Czwartek 4 czerwca" / "Thursday 4 June" — in the CALLER'S locale, never
/// baked server-side.
///
/// `DayShowings.label` used to be shown directly, but it's rendered ONCE by
/// the server in the deployment's fixed default language and never changes
/// when the visitor picks a different in-app language (`LanguageSelection`,
/// independent of the deployment/country) — the weekday/month text stayed
/// stuck in the old language after a switch while every other string on
/// screen followed the pick. Deriving it here, from the raw `date`, in
/// whatever `Locale` the caller passes (typically `@Environment(\.locale)`,
/// which `KinowoApp` keeps in step with the pick) fixes that the same way
/// the web's client-side `formatDateLabel` (`shared.js`) does.
enum DateLabel {
    private static let polishDays = [
        "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela",
    ]
    private static let polishMonths = [
        "", "stycznia", "lutego", "marca", "kwietnia", "maja", "czerwca",
        "lipca", "sierpnia", "września", "października", "listopada", "grudnia",
    ]

    private static let isoCalendar: Calendar = {
        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = TimeZone(identifier: "UTC")!
        return calendar
    }()

    /// `isoDate` is `YYYY-MM-DD`. Returns `isoDate` itself if it doesn't parse
    /// (defensive only — every `date` this app decodes comes from the API's
    /// own ISO-formatted field).
    static func format(isoDate: String, locale: Locale) -> String {
        let parts = isoDate.split(separator: "-").compactMap { Int($0) }
        guard parts.count == 3,
              let date = isoCalendar.date(from: DateComponents(year: parts[0], month: parts[1], day: parts[2]))
        else { return isoDate }

        let (year, month, day) = (parts[0], parts[1], parts[2])
        let currentYear = isoCalendar.component(.year, from: Date())
        let yearSuffix  = year == currentYear ? "" : " \(year)"

        // `locale.language.languageCode` isn't available on Linux's
        // swift-corelibs-foundation — KinowoCore's `swift test` runs there in
        // CI's mobile-local-server job (Apple-only Foundation APIs compile
        // fine on local macOS `swift test`/`xcodebuild` but fail there; see
        // the Linux-Foundation-gap memory). Every `Locale` this app ever
        // builds is a bare 2-letter code (`LanguageSelection.supported`:
        // "pl"/"en"/"de"/"es"), so a plain prefix is both portable and
        // sufficient — no platform-specific API needed at all.
        let languageCode = locale.identifier.prefix(2).lowercased()
        let dayName: String
        let monthName: String
        if languageCode == "pl" {
            // Sunday=1…Saturday=7 → Monday-first index into `polishDays`.
            let mondayFirstIndex = (isoCalendar.component(.weekday, from: date) + 5) % 7
            dayName   = polishDays[mondayFirstIndex].capitalized
            monthName = polishMonths[month]
        } else {
            let formatter = Foundation.DateFormatter()
            formatter.locale = locale
            dayName   = formatter.weekdaySymbols[isoCalendar.component(.weekday, from: date) - 1].capitalized
            monthName = formatter.monthSymbols[month - 1]
        }
        return "\(dayName) \(day) \(monthName)\(yearSuffix)"
    }
}
