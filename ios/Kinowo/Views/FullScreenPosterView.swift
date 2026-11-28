import SwiftUI

/// The detail header / full-screen poster: `PosterChainImage` sized for a
/// large frame (contentMode `.fit`) with the panel-coloured placeholder
/// while the chain is being walked. A free function rather than another
/// wrapper struct — the walk, the on-disk cache and the backoff retry all
/// live in `PosterChainImage`, shared with the listing card.
@ViewBuilder
@MainActor
func detailPoster<NoPoster: View>(
    primary: URL,
    fallbacks: [URL],
    @ViewBuilder noPoster: @escaping () -> NoPoster
) -> some View {
    PosterChainImage(
        primary: primary,
        fallbacks: fallbacks,
        contentMode: .fit,
        loading: {
            Rectangle()
                .fill(Color.kinowoPanel)
                .aspectRatio(PosterMetrics.aspectRatio, contentMode: .fit)
                .overlay(ProgressView().tint(.gray))
        },
        noPoster: noPoster
    )
}

/// Full-screen poster viewer presented from `FilmDetailView` on a tap or
/// long-press of the header poster. Black backdrop, the whole poster shown
/// (fit), pinch-to-zoom and drag-to-pan up to 4×, dismissed by a tap on the
/// backdrop or the close button. Reuses `detailPoster` so the
/// fallback-walking logic lives in one place. Mirrors the Android
/// `FullScreenPoster`.
struct FullScreenPosterView: View {
    let primary: URL
    let fallbacks: [URL]
    let onClose: () -> Void

    @State private var scale: CGFloat = 1
    @State private var lastScale: CGFloat = 1
    @State private var offset: CGSize = .zero
    @State private var lastOffset: CGSize = .zero
    // Vertical translation of an in-progress swipe-to-dismiss (only while not
    // zoomed). The poster follows the finger; on release we either commit the
    // dismissal or spring back to centre.
    @State private var dismissOffset: CGSize = .zero

    /// A swipe whose vertical travel exceeds this commits the dismissal.
    /// Mirrors the Android viewer's 80dp threshold (`FullScreenPoster.kt`).
    private static let dismissThreshold: CGFloat = 100

    /// How long the poster takes to finish sliding off-screen once the swipe is
    /// committed, before the cover is removed.
    private static let flyOffDuration: Double = 0.25

    var body: some View {
        ZStack {
            // Fade the backdrop out as the poster is flicked away, so the
            // gesture reads as a dismissal rather than a pan.
            Color.black.opacity(backdropOpacity).ignoresSafeArea()
                .accessibilityIdentifier(A11y.FilmDetail.fullScreen)

            zoomablePoster
            closeButton
        }
        // A tap anywhere on the backdrop dismisses; the close Button and the
        // zoom/pan gestures take their touches first.
        .contentShape(Rectangle())
        .onTapGesture { onClose() }
    }

    private var zoomablePoster: some View {
        detailPoster(primary: primary, fallbacks: fallbacks) { EmptyView() }
            .scaleEffect(scale)
            .offset(x: offset.width + dismissOffset.width,
                    y: offset.height + dismissOffset.height)
            .gesture(
                MagnificationGesture()
                    .onChanged { scale = min(4, max(1, lastScale * $0)) }
                    .onEnded { _ in
                        lastScale = scale
                        // Snap back to centre once the user pinches all the
                        // way out — a panned-then-shrunk poster shouldn't
                        // get stranded off-screen.
                        if scale <= 1 { withAnimation { offset = .zero; lastOffset = .zero } }
                    }
            )
            .simultaneousGesture(
                DragGesture()
                    .onChanged { dragChanged($0) }
                    .onEnded { dragEnded($0) }
            )
    }

    private func dragChanged(_ value: DragGesture.Value) {
        if scale > 1 {
            // Zoomed in: a drag pans the magnified poster.
            offset = CGSize(width: lastOffset.width + value.translation.width,
                            height: lastOffset.height + value.translation.height)
        } else {
            // At rest (1×): a vertical drag flicks the poster away to dismiss.
            // Follow the finger vertically only so the gesture stays a clean up/down swipe.
            dismissOffset = CGSize(width: 0, height: value.translation.height)
        }
    }

    private func dragEnded(_ value: DragGesture.Value) {
        if scale > 1 {
            lastOffset = offset
        } else if abs(value.translation.height) > Self.dismissThreshold {
            flickAway(up: value.translation.height < 0)
        } else {
            withAnimation(.spring()) { dismissOffset = .zero }
        }
    }

    private var closeButton: some View {
        VStack {
            HStack {
                Spacer()
                Button(action: onClose) {
                    Image(systemName: "xmark")
                        .font(.system(size: 16, weight: .semibold))
                        .foregroundColor(.white)
                        .padding(10)
                        .background(Color.black.opacity(0.5), in: Circle())
                }
                .accessibilityIdentifier(A11y.FilmDetail.closeButton)
                .padding(.top, 8)
                .padding(.trailing, 16)
            }
            Spacer()
        }
    }

    /// Commit the swipe: slide the poster the rest of the way off-screen in the
    /// direction it was thrown, fading the backdrop with it, then remove the
    /// cover WITHOUT its default slide-down transition — that transition pulls
    /// the content back down regardless of swipe direction, which reads as the
    /// poster bouncing the wrong way on an up-swipe. We let the poster finish
    /// flying off, then drop the (now off-screen, transparent) cover instantly.
    private func flickAway(up: Bool) {
        let travel = UIScreen.main.bounds.height + 200
        withAnimation(.easeOut(duration: Self.flyOffDuration)) {
            dismissOffset = CGSize(width: 0, height: up ? -travel : travel)
        }
        Task { @MainActor in
            try? await Task.sleep(for: .seconds(Self.flyOffDuration))
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) { onClose() }
        }
    }

    /// Backdrop dims toward transparent in step with the swipe — fully clear by
    /// the time the poster has travelled a screen-height off, so the fly-off
    /// reads as a dismissal rather than a pan.
    private var backdropOpacity: Double {
        let progress = min(abs(dismissOffset.height) / UIScreen.main.bounds.height, 1)
        return 1 - progress
    }
}
