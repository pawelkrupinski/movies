import Foundation

enum DateFilter: Hashable {
    case anytime
    case today
    case tomorrow
    case week
    case specific(String) // YYYY-MM-DD

    // Dated options first (Dziś / Jutro / Tydzień), with the catch-all
    // `Kiedykolwiek` pushed to the rightmost slot so the bar reads as
    // "narrow → broad" left-to-right.
    static let presets: [DateFilter] = [.today, .tomorrow, .week, .anytime]

    var label: String {
        switch self {
        case .anytime:        return "Kiedykolwiek"
        case .today:          return "Dziś"
        case .tomorrow:       return "Jutro"
        case .week:           return "Tydzień"
        case .specific(let d): return d
        }
    }

    func matches(date dateString: String, now: Date = Date()) -> Bool {
        switch self {
        case .anytime:
            return true
        case .today:
            return dateString == DateFilter.iso(now)
        case .tomorrow:
            return dateString == DateFilter.iso(now.addingTimeInterval(86_400))
        case .week:
            let today = DateFilter.iso(now)
            let in7   = DateFilter.iso(now.addingTimeInterval(7 * 86_400))
            return dateString >= today && dateString <= in7
        case .specific(let d):
            return dateString == d
        }
    }

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw") ?? .current
        return cal
    }()

    private static let isoFormatter: DateFormatter = {
        let f = DateFormatter()
        f.calendar = warsawCalendar
        f.timeZone = warsawCalendar.timeZone
        f.dateFormat = "yyyy-MM-dd"
        f.locale = Locale(identifier: "en_US_POSIX")
        return f
    }()

    static func iso(_ date: Date) -> String { isoFormatter.string(from: date) }
}
