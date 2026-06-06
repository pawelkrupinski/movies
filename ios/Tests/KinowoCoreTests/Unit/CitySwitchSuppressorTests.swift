import XCTest
@testable import KinowoCore

/// The post-login skip is a strict one-shot: a sign-in arms exactly one skipped
/// proximity check, and every check after that runs normally — so the user is
/// never re-prompted right after a Google / Facebook login, yet still gets the
/// offer when they genuinely travel to a nearer city later.
final class CitySwitchSuppressorTests: XCTestCase {

    func testDoesNotSkipWhenNoSignInHasStarted() {
        XCTAssertFalse(CitySwitchSuppressor().consumeShouldSkip())
    }

    func testSkipsExactlyOneCheckAfterSignInStarts() {
        let suppressor = CitySwitchSuppressor()
        suppressor.suppressNextCheck()
        XCTAssertTrue(suppressor.consumeShouldSkip(), "the post-login check is skipped")
        XCTAssertFalse(suppressor.consumeShouldSkip(), "the next check runs normally (one-shot)")
    }
}
