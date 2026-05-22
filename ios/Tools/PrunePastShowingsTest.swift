// Unit-test driver for `prunedPastShowings` / `ShowtimeClock`. Builds a
// synthetic film with past, near-past (inside the 30-min grace), and
// future slots across two cinemas, runs the prune transform against a
// fixed wall-clock, and asserts the web's behaviour:
//   - slots whose dateTime is at or before `now - 30min` are dropped
//   - slots at `now - 25min` (within the grace) are kept
//   - cinemas inside a day are re-sorted by earliest remaining slot
//   - cinema-groups / days / films that empty out disappear
//
// Usage:
//   ./Tools/run_prune_test.sh

import Foundation

@main
enum PrunePastShowingsTest {
    static func main() {
        var failures = 0
        func check(_ name: String, _ cond: Bool, _ extra: String = "") {
            if cond { print("✓ \(name)") }
            else { print("✗ \(name) \(extra)"); failures += 1 }
        }

        // Pin `now` to a fixed Warsaw moment so the fixture survives
        // DST jumps and runs deterministically across CI environments.
        let cal = warsawCalendar
        let nowComponents = DateComponents(
            calendar: cal, timeZone: cal.timeZone,
            year: 2026, month: 5, day: 22, hour: 18, minute: 0
        )
        guard let now = nowComponents.date else {
            print("✗ could not build fixed `now`"); exit(2)
        }
        let today = "2026-05-22"
        let tomorrow = "2026-05-23"

        // Helonki (cinema A): past 16:00, edge-case 17:30 (now - 30min,
        // dropped — the rule is strictly after `now - 30min`),
        // live 17:35 (within grace), future 19:00.
        let cinemaA = CinemaShowings(
            cinema: "Helonki", cinemaURL: nil,
            showtimes: [
                Showtime(time: "16:00", format: "2D", room: nil, bookingURL: nil),
                Showtime(time: "17:30", format: "2D", room: nil, bookingURL: nil),
                Showtime(time: "17:35", format: "2D", room: nil, bookingURL: nil),
                Showtime(time: "19:00", format: "2D", room: nil, bookingURL: nil),
            ])
        // Apollo (cinema B): one all-past day, one future slot at 18:30
        // (earlier than A's earliest remaining 17:35 → no, 17:35 < 18:30,
        // so A stays first today). Tomorrow Apollo at 10:00, alone.
        let cinemaB = CinemaShowings(
            cinema: "Apollo", cinemaURL: nil,
            showtimes: [
                Showtime(time: "15:00", format: "2D", room: nil, bookingURL: nil),
                Showtime(time: "18:30", format: "2D", room: nil, bookingURL: nil),
            ])
        // Muza (cinema C): EVERY slot today is in the past — the whole
        // cinema-group should drop out of today.
        let cinemaC = CinemaShowings(
            cinema: "Muza", cinemaURL: nil,
            showtimes: [
                Showtime(time: "10:00", format: "2D", room: nil, bookingURL: nil),
                Showtime(time: "12:30", format: "2D", room: nil, bookingURL: nil),
            ])
        // A re-ordering case: today, build cinemas in the WRONG order
        // (Apollo first, Helonki second, Muza last) so we can prove the
        // sort actually runs. After prune the expected order is
        // [Helonki @ 17:35, Apollo @ 18:30] (Muza fully dropped).
        let dayToday = DayShowings(date: today, label: "Czwartek 22 maja",
                                   cinemas: [cinemaB, cinemaA, cinemaC])
        let dayTomorrow = DayShowings(date: tomorrow, label: "Piątek 23 maja",
                                      cinemas: [CinemaShowings(
                                        cinema: "Apollo", cinemaURL: nil,
                                        showtimes: [Showtime(time: "10:00", format: "2D", room: nil, bookingURL: nil)]
                                      )])

        // An all-past film — every slot before `now - 30min`. Should be
        // dropped entirely so the films-tab list shrinks naturally.
        let allPastFilm = Film(
            title: "All-Past", posterURL: nil, fallbackPosterURLs: [],
            runtimeMinutes: 120, ratings: .empty,
            showings: [DayShowings(date: today, label: "Czwartek 22 maja", cinemas: [
                CinemaShowings(cinema: "Muza", cinemaURL: nil, showtimes: [
                    Showtime(time: "10:00", format: "2D", room: nil, bookingURL: nil),
                    Showtime(time: "13:00", format: "2D", room: nil, bookingURL: nil),
                ])
            ])])

        let liveFilm = Film(
            title: "Live", posterURL: nil, fallbackPosterURLs: [],
            runtimeMinutes: 90, ratings: .empty,
            showings: [dayToday, dayTomorrow])

        let pruned = [liveFilm, allPastFilm].prunedPastShowings(now: now)

        check("all-past film dropped",
              pruned.count == 1 && pruned[0].title == "Live",
              "(got \(pruned.map(\.title)))")

        guard let film = pruned.first(where: { $0.title == "Live" }) else {
            print("✗ Live film missing"); exit(2)
        }
        check("live film keeps both days", film.showings.count == 2,
              "(got \(film.showings.map(\.date)))")

        let todayDay = film.showings.first { $0.date == today }!
        check("Muza dropped (all-past)",
              !todayDay.cinemas.contains { $0.cinema == "Muza" })
        check("today has exactly 2 cinemas (Helonki, Apollo)",
              todayDay.cinemas.count == 2,
              "(got \(todayDay.cinemas.map(\.cinema)))")
        check("today cinema order = [Helonki, Apollo]",
              todayDay.cinemas.map(\.cinema) == ["Helonki", "Apollo"],
              "(got \(todayDay.cinemas.map(\.cinema)))")

        let helonki = todayDay.cinemas.first { $0.cinema == "Helonki" }!
        check("Helonki dropped 16:00 (past)",
              !helonki.showtimes.contains { $0.time == "16:00" })
        check("Helonki dropped 17:30 (exactly now - 30min)",
              !helonki.showtimes.contains { $0.time == "17:30" })
        check("Helonki kept 17:35 (inside 30-min grace)",
              helonki.showtimes.contains { $0.time == "17:35" })
        check("Helonki kept 19:00 (future)",
              helonki.showtimes.contains { $0.time == "19:00" })

        let apollo = todayDay.cinemas.first { $0.cinema == "Apollo" }!
        check("Apollo dropped 15:00 (past)",
              !apollo.showtimes.contains { $0.time == "15:00" })
        check("Apollo kept 18:30 (future)",
              apollo.showtimes.contains { $0.time == "18:30" })

        let tomorrowDay = film.showings.first { $0.date == tomorrow }!
        check("tomorrow's slots untouched",
              tomorrowDay.cinemas.count == 1
                && tomorrowDay.cinemas[0].showtimes.map(\.time) == ["10:00"])

        // Idempotency: running prune on an already-pruned list returns
        // the same shape. Guards against an accidental sort-isn't-stable
        // bug that would shuffle cinemas on every activation.
        let prunedAgain = pruned.prunedPastShowings(now: now)
        check("prune is idempotent",
              prunedAgain == pruned)

        if failures > 0 {
            print("FAILED with \(failures) check failures")
            exit(2)
        }
        print("ALL OK")
    }

    static var warsawCalendar: Calendar {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw") ?? .current
        return cal
    }
}
