// DevPanel — a small always-on-top floating palette of dev actions for the
// movies repo.
//
// The two device actions (Android, iOS) run their script as a background
// subprocess and stream its live output into a collapsible area inside the
// panel, with a Stop button. The two web actions are long-lived servers, so
// they open a Terminal window instead (Ctrl-C to stop, full scrollback).
//
// The absolute scripts directory is baked into Info.plist (DevPanelScriptsDir)
// at build time, so the .app keeps working if moved out of the repo tree.

import AppKit

// MARK: - Command runner (streams a subprocess line-by-line)

/// Runs a shell script as a child process, streaming merged stdout+stderr to
/// `onOutput` and the exit code to `onExit`, both on `callbackQueue`. The only
/// stateful infra here is the `Process`; everything above it (the UI, the
/// self-test) drives it the same way.
final class CommandRunner {
    private let callbackQueue: DispatchQueue
    private var process: Process?
    var onOutput: ((String) -> Void)?
    var onExit: ((Int32) -> Void)?

    init(callbackQueue: DispatchQueue = .main) { self.callbackQueue = callbackQueue }

    var isRunning: Bool { process?.isRunning ?? false }

    func run(executable: String, arguments: [String]) {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: executable)
        p.arguments = arguments
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = pipe

        pipe.fileHandleForReading.readabilityHandler = { [weak self] handle in
            let data = handle.availableData
            guard !data.isEmpty else { return }
            let text = String(decoding: data, as: UTF8.self)
            self?.callbackQueue.async { self?.onOutput?(text) }
        }
        p.terminationHandler = { [weak self] proc in
            pipe.fileHandleForReading.readabilityHandler = nil
            self?.process = nil
            self?.callbackQueue.async { self?.onExit?(proc.terminationStatus) }
        }

        self.process = p
        do {
            try p.run()
        } catch {
            self.process = nil
            callbackQueue.async {
                self.onOutput?("✗ failed to start: \(error.localizedDescription)\n")
                self.onExit?(-1)
            }
        }
    }

    /// SIGTERM the child (and its process group, so sbt/gradle children die too).
    func stop() {
        guard let p = process, p.isRunning else { return }
        kill(-p.processIdentifier, SIGTERM)
        p.terminate()
    }
}

// MARK: - Actions

private enum RunMode { case inline, terminal }

private struct Action {
    let title: String
    let subtitle: String
    let script: String
    let mode: RunMode
}

private let actions: [Action] = [
    Action(title: "Android → device", subtitle: "build · install · launch",
           script: "deploy-android.sh", mode: .inline),
    Action(title: "iOS → device", subtitle: "build · install · launch",
           script: "deploy-ios.sh", mode: .inline),
    Action(title: "Web server", subtitle: "sbt web/run · :9000",
           script: "run-web.sh", mode: .terminal),
    Action(title: "Web + worker", subtitle: "sbt localStack · fixtures",
           script: "run-local-stack.sh", mode: .terminal),
]

private let panelWidth: CGFloat = 320
private let logHeight: CGFloat = 260

// MARK: - App

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var panel: NSPanel!
    private let scriptsDir: String =
        (Bundle.main.object(forInfoDictionaryKey: "DevPanelScriptsDir") as? String) ?? ""

    private var runner: CommandRunner?
    private let textView = NSTextView()
    private var scrollView: NSScrollView!
    private let statusLabel = NSTextField(labelWithString: "idle")
    private let disclosure = NSButton()
    private let stopButton = NSButton()
    private var expanded = false

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
        content.addArrangedSubview(outputControlsRow())
        content.addArrangedSubview(outputView())

        let panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: panelWidth, height: 10),
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
            content.widthAnchor.constraint(equalToConstant: panelWidth),
        ])
        self.panel = panel

        setExpanded(false, resize: false)
        relayout()
        let topLeft = NSPoint(x: 40, y: (NSScreen.main?.visibleFrame.maxY ?? 800) - 40)
        panel.setFrameTopLeftPoint(topLeft)
        panel.orderFrontRegardless()
    }

    // MARK: views

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
        row.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
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
        b.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        b.heightAnchor.constraint(equalToConstant: 44).isActive = true
        return b
    }

    private func outputControlsRow() -> NSView {
        disclosure.title = "Output"
        disclosure.bezelStyle = .disclosure
        disclosure.setButtonType(.pushOnPushOff)
        disclosure.target = self
        disclosure.action = #selector(toggleOutput)

        statusLabel.font = .systemFont(ofSize: 10)
        statusLabel.textColor = .secondaryLabelColor

        stopButton.title = "Stop"
        stopButton.bezelStyle = .inline
        stopButton.controlSize = .small
        stopButton.font = .systemFont(ofSize: 10)
        stopButton.target = self
        stopButton.action = #selector(stopRunning)
        stopButton.isHidden = true

        let row = NSStackView(views: [disclosure, statusLabel, NSView(), stopButton])
        row.orientation = .horizontal
        row.distribution = .fill
        row.translatesAutoresizingMaskIntoConstraints = false
        row.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        return row
    }

    private func outputView() -> NSView {
        textView.isEditable = false
        textView.isRichText = false
        textView.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
        textView.backgroundColor = NSColor(white: 0.10, alpha: 1)
        textView.textColor = NSColor(white: 0.92, alpha: 1)
        textView.textContainerInset = NSSize(width: 6, height: 6)
        textView.autoresizingMask = [.width]

        let scroll = NSScrollView()
        scroll.documentView = textView
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        scroll.heightAnchor.constraint(equalToConstant: logHeight).isActive = true
        self.scrollView = scroll
        return scroll
    }

    // MARK: actions

    @objc private func run(_ sender: NSButton) {
        guard let script = sender.identifier?.rawValue, !scriptsDir.isEmpty else {
            NSSound.beep(); return
        }
        let path = (scriptsDir as NSString).appendingPathComponent(script)
        let action = actions.first { $0.script == script }
        switch action?.mode ?? .terminal {
        case .terminal: openInTerminal(path)
        case .inline: runInline(path, label: action?.title ?? script)
        }
    }

    private func runInline(_ path: String, label: String) {
        runner?.stop()
        setExpanded(true, resize: true)
        append("\n\u{1B}[0m── \(label) ─────────────\n")
        statusLabel.stringValue = "running…"
        statusLabel.textColor = .secondaryLabelColor
        stopButton.isHidden = false

        let runner = CommandRunner()
        runner.onOutput = { [weak self] in self?.append($0) }
        runner.onExit = { [weak self] code in
            self?.stopButton.isHidden = true
            self?.statusLabel.stringValue = code == 0 ? "done ✓" : "exited \(code)"
            self?.statusLabel.textColor = code == 0 ? .systemGreen : .systemRed
        }
        self.runner = runner
        // Own process group so Stop can signal the whole sbt/gradle tree.
        runner.run(executable: "/bin/bash", arguments: ["-lc", "exec bash \"\(path)\""])
    }

    private func openInTerminal(_ path: String) {
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

    private func append(_ text: String) {
        // Strip ANSI colour codes the scripts emit for Terminal.
        let clean = text.replacingOccurrences(
            of: "\u{1B}\\[[0-9;]*m", with: "", options: .regularExpression)
        textView.textStorage?.append(NSAttributedString(
            string: clean,
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .regular),
                .foregroundColor: NSColor(white: 0.92, alpha: 1),
            ]))
        textView.scrollToEndOfDocument(nil)
    }

    @objc private func stopRunning() { runner?.stop() }

    @objc private func toggleOutput() { setExpanded(!expanded, resize: true) }

    private func setExpanded(_ on: Bool, resize: Bool) {
        expanded = on
        disclosure.state = on ? .on : .off
        scrollView.isHidden = !on
        if resize { relayout() }
    }

    private func relayout() {
        guard let content = panel.contentView else { return }
        let size = content.fittingSize
        panel.setContentSize(NSSize(width: panelWidth, height: size.height))
    }

    @objc private func quit() {
        runner?.stop()
        NSApp.terminate(nil)
    }
}

// MARK: - Entry point

// Headless self-test: drives the real CommandRunner streaming path and exits
// 0/1. Lets test.sh assert the subprocess capture works without a GUI/click.
if ProcessInfo.processInfo.environment["DEVPANEL_SELFTEST"] == "1" {
    let q = DispatchQueue(label: "devpanel.selftest")
    let runner = CommandRunner(callbackQueue: q)
    var collected = ""
    var status: Int32 = -999
    let done = DispatchSemaphore(value: 0)
    runner.onOutput = { collected += $0 }
    runner.onExit = { status = $0; done.signal() }
    runner.run(executable: "/bin/sh", arguments: ["-c", "printf 'SELFTEST_OK\\n'"])
    if done.wait(timeout: .now() + 10) == .timedOut {
        FileHandle.standardError.write(Data("selftest: timed out\n".utf8))
        exit(2)
    }
    let ok = collected.contains("SELFTEST_OK") && status == 0
    print(ok ? "SELFTEST_OK status=\(status)"
             : "SELFTEST_FAIL collected=\(collected.debugDescription) status=\(status)")
    exit(ok ? 0 : 1)
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
