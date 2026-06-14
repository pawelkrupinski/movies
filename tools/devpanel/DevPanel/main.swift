// DevPanel — a small always-on-top floating palette of dev actions for the
// movies repo. Each button opens a new Terminal window running the matching
// scripts/<name>.sh, so long-running sbt/gradle output stays visible and
// Ctrl-C reaches the process.
//
// The absolute scripts directory is baked into Info.plist (DevPanelScriptsDir)
// at build time, so the .app keeps working if moved out of the repo tree.

import AppKit

private struct Action {
    let title: String
    let subtitle: String
    let script: String
}

private let actions: [Action] = [
    Action(title: "Android → device",
           subtitle: "build + install on cable",
           script: "deploy-android.sh"),
    Action(title: "iOS → device",
           subtitle: "build + install on cable",
           script: "deploy-ios.sh"),
    Action(title: "Web server",
           subtitle: "sbt web/run · :9000",
           script: "run-web.sh"),
    Action(title: "Web + worker",
           subtitle: "sbt localStack · fixtures",
           script: "run-local-stack.sh"),
]

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var panel: NSPanel!
    private let scriptsDir: String =
        (Bundle.main.object(forInfoDictionaryKey: "DevPanelScriptsDir") as? String) ?? ""

    func applicationDidFinishLaunching(_ note: Notification) {
        let content = NSStackView()
        content.orientation = .vertical
        content.alignment = .leading
        content.spacing = 8
        content.edgeInsets = NSEdgeInsets(top: 12, left: 12, bottom: 12, right: 12)
        content.translatesAutoresizingMaskIntoConstraints = false

        content.addArrangedSubview(headerRow())
        for action in actions {
            content.addArrangedSubview(button(for: action))
        }

        let panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: 220, height: 10),
            styleMask: [.titled, .nonactivatingPanel, .utilityWindow, .hudWindow],
            backing: .buffered,
            defer: false)
        panel.title = "movies"
        panel.isFloatingPanel = true
        panel.level = .floating
        panel.hidesOnDeactivate = false
        panel.isMovableByWindowBackground = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        panel.contentView = content
        NSLayoutConstraint.activate([
            content.widthAnchor.constraint(equalToConstant: 220),
        ])
        panel.center()
        let topLeft = NSPoint(x: panel.frame.minX,
                              y: (NSScreen.main?.visibleFrame.maxY ?? panel.frame.maxY) - 40)
        panel.setFrameTopLeftPoint(topLeft)
        panel.orderFrontRegardless()
        self.panel = panel
    }

    private func headerRow() -> NSView {
        let label = NSTextField(labelWithString: "kinowo dev")
        label.font = .systemFont(ofSize: 11, weight: .semibold)
        label.textColor = .secondaryLabelColor

        let quit = NSButton(title: "✕", target: self, action: #selector(quit))
        quit.bezelStyle = .inline
        quit.isBordered = false
        quit.font = .systemFont(ofSize: 11)
        quit.toolTip = "Quit DevPanel"

        let row = NSStackView(views: [label, NSView(), quit])
        row.orientation = .horizontal
        row.distribution = .fill
        row.translatesAutoresizingMaskIntoConstraints = false
        row.widthAnchor.constraint(equalToConstant: 196).isActive = true
        return row
    }

    private func button(for action: Action) -> NSButton {
        let title = NSMutableAttributedString(
            string: action.title + "\n",
            attributes: [
                .font: NSFont.systemFont(ofSize: 13, weight: .medium),
                .foregroundColor: NSColor.labelColor,
            ])
        title.append(NSAttributedString(
            string: action.subtitle,
            attributes: [
                .font: NSFont.systemFont(ofSize: 10),
                .foregroundColor: NSColor.secondaryLabelColor,
            ]))

        let b = NSButton(title: "", target: self, action: #selector(run(_:)))
        b.attributedTitle = title
        b.identifier = NSUserInterfaceItemIdentifier(action.script)
        b.bezelStyle = .regularSquare
        b.alignment = .left
        b.imagePosition = .noImage
        b.translatesAutoresizingMaskIntoConstraints = false
        b.widthAnchor.constraint(equalToConstant: 196).isActive = true
        b.heightAnchor.constraint(equalToConstant: 44).isActive = true
        return b
    }

    @objc private func run(_ sender: NSButton) {
        guard let script = sender.identifier?.rawValue, !scriptsDir.isEmpty else {
            NSSound.beep(); return
        }
        let path = (scriptsDir as NSString).appendingPathComponent(script)
        let command = "bash \"\(path)\""
        let escaped = command.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
        let osa = """
        tell application "Terminal"
            activate
            do script "\(escaped)"
        end tell
        """
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/osascript")
        p.arguments = ["-e", osa]
        try? p.run()
    }

    @objc private func quit() { NSApp.terminate(nil) }
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
