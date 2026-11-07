import SwiftUI
import UIKit

/// Reports whether the hosting window scene is in a landscape interface
/// orientation, read from UIKit (`UIWindowScene.effectiveGeometry`, which is
/// key-value observable), not from SwiftUI's layout pass.
///
/// `ContentView` bumps its root `.id` on each change to force a full re-layout
/// on every rotation: after a portrait⇄landscape flip the SwiftUI hierarchy can
/// stay stuck at the previous orientation's width. The size class alone is
/// not enough to key on: it flips with orientation only on a compact-height
/// phone. The iPhone Duo's unfolded inner display (669 x 951 pt) is regular
/// in both orientations, so it never flipped there and the grid kept the
/// other orientation's columns: clipped at both edges in portrait.
struct InterfaceOrientationReader: UIViewRepresentable {
    let onChange: (Bool) -> Void

    func makeUIView(context: Context) -> ReaderView {
        let view = ReaderView()
        view.isUserInteractionEnabled = false
        view.onChange = onChange
        return view
    }

    func updateUIView(_ view: ReaderView, context: Context) {
        view.onChange = onChange
    }

    final class ReaderView: UIView {
        var onChange: ((Bool) -> Void)?
        private var observation: NSKeyValueObservation?

        override func didMoveToWindow() {
            super.didMoveToWindow()
            observation = window?.windowScene?.observe(\.effectiveGeometry, options: [.initial, .new]) {
                [weak self] scene, _ in
                let landscape = scene.effectiveGeometry.interfaceOrientation.isLandscape
                DispatchQueue.main.async { self?.onChange?(landscape) }
            }
        }
    }
}

