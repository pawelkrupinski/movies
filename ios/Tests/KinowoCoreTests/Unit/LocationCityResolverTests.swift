// CoreLocation isn't on swift-corelibs (Linux CI), so this suite compiles to
// nothing there and runs on the macOS `swift test` parity leg. No real
// `CLLocationManager` is touched: the resolver's two CoreLocation commands go
// through the `LocationRequesting` seam, and the delegate callbacks are driven
// directly, so nothing here can raise a permission dialog.
#if canImport(CoreLocation)
import CoreLocation
import XCTest
@testable import KinowoCore

/// Records the CoreLocation commands the resolver issues instead of performing
/// them, so a test can assert both WHAT it asked for and WHEN.
final class RecordingLocationRequester: LocationRequesting {
    var authorizationStatus: CLAuthorizationStatus
    private(set) var authorizationRequests = 0
    private(set) var locationRequests = 0

    init(status: CLAuthorizationStatus) { authorizationStatus = status }

    func requestWhenInUseAuthorization() { authorizationRequests += 1 }
    func requestLocation() { locationRequests += 1 }
}

/// The "user granted when-in-use" status. `.authorizedWhenInUse` doesn't exist
/// on macOS, where `swift test` runs, and the resolver treats the two grants
/// identically — so the suite names whichever one the platform has.
#if os(iOS)
private let granted = CLAuthorizationStatus.authorizedWhenInUse
#else
private let granted = CLAuthorizationStatus.authorizedAlways
#endif

@MainActor
final class LocationCityResolverTests: XCTestCase {

    private let cities = [
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252, country: "pl"),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122, country: "pl"),
    ]

    /// The first-launch permission dialog is the USER reading a system alert —
    /// the time they spend there must not burn the deadline for the fix. It
    /// used to: one timer covered dialog + fix, so anyone who took more than
    /// `fixTimeout` to tap "Allow" got `.unavailable` and the gate dropped them
    /// on the manual city list — the very list the fix they just granted was
    /// meant to skip.
    func testSlowPermissionGrantStillResolvesTheDetectedCity() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.2)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await Task.yield()

        // The user takes longer than the fix deadline to answer the dialog.
        try? await Task.sleep(nanoseconds: 500_000_000)
        XCTAssertEqual(requester.authorizationRequests, 1)
        XCTAssertEqual(requester.locationRequests, 0, "no fix is asked for until the user has answered")

        requester.authorizationStatus = granted
        resolver.authorizationChanged(to: granted)
        XCTAssertEqual(requester.locationRequests, 1, "the grant is what asks for the fix")
        resolver.deliverFix(lat: 52.4064, lon: 16.9252)

        let result = await outcome
        XCTAssertEqual(result, .city(cities[0]))
    }

    /// The fix deadline still bites once we're actually waiting on the system:
    /// authorized, asked, nothing came back.
    func testAuthorizedButNoFixTimesOutToUnavailable() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.2)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 1)
        XCTAssertEqual(requester.authorizationRequests, 0, "an authorized user is never re-prompted")
    }

    /// A dialog that never gets answered — Location Services off device-wide and
    /// the "Turn On" alert dismissed, where no authorization callback ever comes
    /// — must not leave the gate spinning forever.
    func testUnansweredPermissionDialogGivesUpAfterTheAuthorizationDeadline() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 0.2, fixTimeout: 30)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
    }

    func testDeniedResolvesImmediatelyWithoutAskingForAFix() async {
        let requester = RecordingLocationRequester(status: .denied)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        let outcome = await resolver.resolve(in: "pl", cities: cities)

        XCTAssertEqual(outcome, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
        XCTAssertEqual(requester.authorizationRequests, 0)
    }

    /// A denial delivered through the dialog ends the wait too, rather than
    /// leaving it to the authorization deadline.
    func testDenialThroughTheDialogEndsTheWait() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await Task.yield()
        try? await Task.sleep(nanoseconds: 100_000_000)
        resolver.authorizationChanged(to: .denied)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
        XCTAssertEqual(requester.locationRequests, 0)
    }

    /// A fix outside every city's radius is still no city to offer.
    func testFixOutOfRangeResolvesToUnavailable() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let outcome = resolver.resolve(in: "pl", cities: cities)
        await Task.yield()
        try? await Task.sleep(nanoseconds: 100_000_000)
        resolver.deliverFix(lat: 0, lon: 0)

        let result = await outcome
        XCTAssertEqual(result, .unavailable)
    }

    /// `resolveIfAuthorized` is the silent app-open check: never prompts, and
    /// hands back the raw coordinate rather than a city.
    func testResolveIfAuthorizedReturnsTheRawCoordinate() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        async let coordinate = resolver.resolveIfAuthorized()
        await Task.yield()
        try? await Task.sleep(nanoseconds: 100_000_000)
        resolver.deliverFix(lat: 52.2297, lon: 21.0122)

        let result = await coordinate
        XCTAssertEqual(result, LocationCityResolver.Coordinate(lat: 52.2297, lon: 21.0122))
        XCTAssertEqual(requester.authorizationRequests, 0)
    }

    func testResolveIfAuthorizedStaysSilentWhenNotAuthorized() async {
        let requester = RecordingLocationRequester(status: .notDetermined)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 30)

        let coordinate = await resolver.resolveIfAuthorized()

        XCTAssertNil(coordinate)
        XCTAssertEqual(requester.authorizationRequests, 0, "the silent check never raises the dialog")
        XCTAssertEqual(requester.locationRequests, 0)
    }

    /// A no-fix timeout on the silent check resolves the coordinate request,
    /// not the gate's `Outcome` one.
    func testResolveIfAuthorizedTimesOutToNil() async {
        let requester = RecordingLocationRequester(status: granted)
        let resolver = LocationCityResolver(requester: requester, authorizationTimeout: 30, fixTimeout: 0.2)

        let coordinate = await resolver.resolveIfAuthorized()

        XCTAssertNil(coordinate)
        XCTAssertEqual(requester.locationRequests, 1)
    }
}
#endif
