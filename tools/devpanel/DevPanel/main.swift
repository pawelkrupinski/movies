// DevPanel — a small always-on-top floating palette of dev actions for the
// movies repo.
//
// Every action runs its script as a background subprocess, streaming live
// output into an in-panel console. There are two consoles:
//   • Web    — shared by the two server actions; KEEPS its scrollback across
//              runs (long-lived servers you want to keep watching).
//   • Device — shared by Android + iOS; CLEARED at the start of each run.
// Each console is independently collapsible and has its own Stop button.
//
// Long-press (or right-click) any button to pick which git worktree to run the
// task in; the chosen path is handed to the script via DEVPANEL_REPO_ROOT.
//
// The absolute scripts directory is baked into Info.plist (DevPanelScriptsDir)
// at build time, so the .app keeps working if moved out of the repo tree.

import AppKit

// MARK: - Command runner (streams a subprocess)

/// Runs a shell script as a child process, streaming merged stdout+stderr to
/// `onOutput` and the exit code to `onExit`, both on `callbackQueue`.
final class CommandRunner {
    private let callbackQueue: DispatchQueue
    private var process: Process?
    var onOutput: ((String) -> Void)?
    var onExit: ((Int32) -> Void)?

    init(callbackQueue: DispatchQueue = .main) { self.callbackQueue = callbackQueue }

    var isRunning: Bool { process?.isRunning ?? false }

    func run(executable: String, arguments: [String], environment: [String: String]? = nil) {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: executable)
        p.arguments = arguments
        if let extra = environment {
            p.environment = ProcessInfo.processInfo.environment.merging(extra) { _, new in new }
        }
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

    /// SIGTERM the child's whole process group, so sbt/gradle children die too.
    func stop() {
        guard let p = process, p.isRunning else { return }
        kill(-p.processIdentifier, SIGTERM)
        p.terminate()
    }
}

// MARK: - A collapsible console (controls row + log view + a runner)

private let panelWidth: CGFloat = 340
private let logHeight: CGFloat = 220

final class ConsoleView: NSObject {
    let container = NSStackView()
    private let disclosure = NSButton()
    private let status = NSTextField(labelWithString: "idle")
    private let stopButton = NSButton()
    private let textView = NSTextView()
    private let scroll = NSScrollView()
    private var expanded = false
    private var runner: CommandRunner?

    private let clearsOnRun: Bool
    var onLayoutChange: (() -> Void)?

    init(title: String, clearsOnRun: Bool) {
        self.clearsOnRun = clearsOnRun
        super.init()

        disclosure.title = title
        disclosure.bezelStyle = .disclosure
        disclosure.setButtonType(.pushOnPushOff)
        disclosure.target = self
        disclosure.action = #selector(toggle)

        status.font = .systemFont(ofSize: 10)
        status.textColor = .secondaryLabelColor

        stopButton.title = "Stop"
        stopButton.bezelStyle = .inline
        stopButton.controlSize = .small
        stopButton.font = .systemFont(ofSize: 10)
        stopButton.target = self
        stopButton.action = #selector(stopRunning)
        stopButton.isHidden = true

        let controls = NSStackView(views: [disclosure, status, NSView(), stopButton])
        controls.orientation = .horizontal
        controls.distribution = .fill
        controls.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true

        textView.isEditable = false
        textView.isRichText = false
        textView.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
        textView.backgroundColor = NSColor(white: 0.10, alpha: 1)
        textView.textColor = NSColor(white: 0.92, alpha: 1)
        textView.textContainerInset = NSSize(width: 6, height: 6)
        textView.autoresizingMask = [.width]

        scroll.documentView = textView
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        scroll.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        scroll.heightAnchor.constraint(equalToConstant: logHeight).isActive = true

        container.orientation = .vertical
        container.alignment = .leading
        container.spacing = 6
        container.addArrangedSubview(controls)
        container.addArrangedSubview(scroll)
        scroll.isHidden = true
    }

    func run(scriptPath: String, label: String, repoRoot: String?) {
        runner?.stop()
        if clearsOnRun { textView.string = "" }
        setExpanded(true)
        let where_ = repoRoot.map { " @ \(($0 as NSString).lastPathComponent)" } ?? ""
        append("\n── \(label)\(where_) ─────────────\n")
        status.stringValue = "running…"
        status.textColor = .secondaryLabelColor
        stopButton.isHidden = false

        let r = CommandRunner()
        r.onOutput = { [weak self] in self?.append($0) }
        r.onExit = { [weak self] code in
            self?.stopButton.isHidden = true
            self?.status.stringValue = code == 0 ? "done ✓" : "exited \(code)"
            self?.status.textColor = code == 0 ? .systemGreen : .systemRed
        }
        self.runner = r
        let env = repoRoot.map { ["DEVPANEL_REPO_ROOT": $0] }
        r.run(executable: "/bin/bash", arguments: ["-lc", "exec bash \"\(scriptPath)\""], environment: env)
    }

    func stop() { runner?.stop() }

    @objc private func stopRunning() { runner?.stop() }

    @objc private func toggle() { setExpanded(!expanded) }

    private func setExpanded(_ on: Bool) {
        expanded = on
        disclosure.state = on ? .on : .off
        scroll.isHidden = !on
        onLayoutChange?()
    }

    private func append(_ text: String) {
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
}

// MARK: - Actions

private enum Console { case web, device }

private struct Action {
    let title: String
    let subtitle: String
    let script: String
    let console: Console
}

private let actions: [Action] = [
    Action(title: "Android → device", subtitle: "unlock · build · install · launch",
           script: "deploy-android.sh", console: .device),
    Action(title: "iOS → device", subtitle: "unlock · build · install · launch",
           script: "deploy-ios.sh", console: .device),
    Action(title: "Web server", subtitle: "sbt web/run · :9000",
           script: "run-web.sh", console: .web),
    Action(title: "Web + worker", subtitle: "sbt localStack · fixtures",
           script: "run-local-stack.sh", console: .web),
]

// MARK: - App

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var panel: NSPanel!
    private let scriptsDir: String =
        (Bundle.main.object(forInfoDictionaryKey: "DevPanelScriptsDir") as? String) ?? ""
    private lazy var repoRoot: String =
        URL(fileURLWithPath: scriptsDir)
            .deletingLastPathComponent().deletingLastPathComponent()
            .deletingLastPathComponent().path

    private let webConsole = ConsoleView(title: "Web output", clearsOnRun: false)
    private let deviceConsole = ConsoleView(title: "Device output", clearsOnRun: true)
    private var suppressClick: Set<String> = []

    func applicationDidFinishLaunching(_ note: Notification) {
        let content = NSStackView()
        content.orientation = .vertical
        content.alignment = .leading
        content.spacing = 8
        content.edgeInsets = NSEdgeInsets(top: 12, left: 12, bottom: 12, right: 12)
        content.translatesAutoresizingMaskIntoConstraints = false

        content.addArrangedSubview(headerRow())
        for action in actions { content.addArrangedSubview(button(for: action)) }
        content.addArrangedSubview(deviceConsole.container)
        content.addArrangedSubview(webConsole.container)
        webConsole.onLayoutChange = { [weak self] in self?.relayout() }
        deviceConsole.onLayoutChange = { [weak self] in self?.relayout() }

        let panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: panelWidth, height: 10),
            styleMask: [.titled, .nonactivatingPanel, .utilityWindow, .hudWindow],
            backing: .buffered, defer: false)
        panel.title = "movies"
        panel.isFloatingPanel = true
        panel.level = .floating
        panel.hidesOnDeactivate = false
        panel.isMovableByWindowBackground = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        panel.contentView = content
        content.widthAnchor.constraint(equalToConstant: panelWidth).isActive = true
        self.panel = panel

        relayout()
        panel.setFrameTopLeftPoint(NSPoint(x: 40, y: (NSScreen.main?.visibleFrame.maxY ?? 800) - 40))
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
        row.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        return row
    }

    private func button(for action: Action) -> NSButton {
        let title = NSMutableAttributedString(
            string: action.title + "\n",
            attributes: [.font: NSFont.systemFont(ofSize: 13, weight: .medium),
                         .foregroundColor: NSColor.labelColor])
        title.append(NSAttributedString(
            string: action.subtitle,
            attributes: [.font: NSFont.systemFont(ofSize: 10),
                         .foregroundColor: NSColor.secondaryLabelColor]))

        let b = NSButton(title: "", target: self, action: #selector(run(_:)))
        b.attributedTitle = title
        b.identifier = NSUserInterfaceItemIdentifier(action.script)
        b.bezelStyle = .regularSquare
        b.alignment = .left
        b.imagePosition = .noImage
        b.toolTip = "Click to run · long-press or right-click to pick a worktree"
        b.widthAnchor.constraint(equalToConstant: panelWidth - 24).isActive = true
        b.heightAnchor.constraint(equalToConstant: 44).isActive = true

        let lp = NSPressGestureRecognizer(target: self, action: #selector(longPress(_:)))
        lp.minimumPressDuration = 0.4
        b.addGestureRecognizer(lp)
        return b
    }

    // MARK: running

    @objc private func run(_ sender: NSButton) {
        guard let script = sender.identifier?.rawValue else { return }
        if suppressClick.remove(script) != nil { return }   // long-press already handled it
        start(script: script, repoRoot: nil)
    }

    private func start(script: String, repoRoot: String?) {
        guard !scriptsDir.isEmpty, let action = actions.first(where: { $0.script == script }) else {
            NSSound.beep(); return
        }
        let path = (scriptsDir as NSString).appendingPathComponent(script)
        let console = action.console == .web ? webConsole : deviceConsole
        console.run(scriptPath: path, label: action.title, repoRoot: repoRoot)
    }

    // MARK: worktree picker

    @objc private func longPress(_ gr: NSPressGestureRecognizer) {
        guard gr.state == .began, let button = gr.view as? NSButton,
              let script = button.identifier?.rawValue else { return }
        // Swallow the click that may follow the press; auto-clear so a later
        // genuine click isn't eaten.
        suppressClick.insert(script)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { self.suppressClick.remove(script) }
        worktreeMenu(forScript: script).popUp(positioning: nil, at: gr.location(in: button), in: button)
    }

    @objc private func runOnWorktree(_ item: NSMenuItem) {
        guard let info = item.representedObject as? [String: String], let script = info["script"] else { return }
        let root = (info["root"]?.isEmpty == false) ? info["root"] : nil
        start(script: script, repoRoot: root)
    }

    private func worktreeMenu(forScript script: String) -> NSMenu {
        let menu = NSMenu()
        let header = NSMenuItem(title: "Run on worktree:", action: nil, keyEquivalent: "")
        header.isEnabled = false
        menu.addItem(header)
        let trees = worktrees()
        for wt in trees {
            let isMain = wt.path == repoRoot
            let item = NSMenuItem(title: isMain ? "\(wt.name)  (main)" : wt.name,
                                  action: #selector(runOnWorktree(_:)), keyEquivalent: "")
            item.target = self
            item.representedObject = ["script": script, "root": isMain ? "" : wt.path]
            menu.addItem(item)
        }
        if trees.isEmpty {
            let none = NSMenuItem(title: "(no worktrees found)", action: nil, keyEquivalent: "")
            none.isEnabled = false
            menu.addItem(none)
        }
        return menu
    }

    private func worktrees() -> [(name: String, path: String)] {
        let out = runGit(["-C", repoRoot, "worktree", "list", "--porcelain"])
        return out.split(separator: "\n").compactMap { line in
            guard line.hasPrefix("worktree ") else { return nil }
            let path = String(line.dropFirst("worktree ".count))
            return ((path as NSString).lastPathComponent, path)
        }
    }

    private func runGit(_ args: [String]) -> String {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/git")
        p.arguments = args
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = Pipe()
        guard (try? p.run()) != nil else { return "" }
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        return String(decoding: data, as: UTF8.self)
    }

    // MARK: chrome

    private func relayout() {
        guard let content = panel.contentView else { return }
        panel.setContentSize(NSSize(width: panelWidth, height: content.fittingSize.height))
    }

    @objc private func quit() {
        webConsole.stop()
        deviceConsole.stop()
        NSApp.terminate(nil)
    }
}

// MARK: - Entry point

// Headless self-test: drives the real CommandRunner — output streaming AND the
// environment passthrough the worktree picker relies on — then exits 0/1. Lets
// test.sh verify the runtime path without a GUI/click.
if ProcessInfo.processInfo.environment["DEVPANEL_SELFTEST"] == "1" {
    func runOnce(_ exec: String, _ args: [String], _ env: [String: String]?) -> (String, Int32) {
        let q = DispatchQueue(label: "devpanel.selftest")
        let r = CommandRunner(callbackQueue: q)
        var out = ""; var st: Int32 = -999
        let done = DispatchSemaphore(value: 0)
        r.onOutput = { out += $0 }
        r.onExit = { st = $0; done.signal() }
        r.run(executable: exec, arguments: args, environment: env)
        _ = done.wait(timeout: .now() + 10)
        return (out, st)
    }

    let (o1, s1) = runOnce("/bin/sh", ["-c", "printf 'SELFTEST_OK\\n'"], nil)
    let (o2, s2) = runOnce("/bin/sh", ["-c", "printf 'ROOT=%s\\n' \"$DEVPANEL_REPO_ROOT\""],
                           ["DEVPANEL_REPO_ROOT": "/tmp/devpanel-selftest-root"])
    let ok = s1 == 0 && o1.contains("SELFTEST_OK")
        && s2 == 0 && o2.contains("ROOT=/tmp/devpanel-selftest-root")
    print(ok ? "SELFTEST_OK stream+env status=\(s1),\(s2)"
             : "SELFTEST_FAIL o1=\(o1.debugDescription) o2=\(o2.debugDescription) st=\(s1),\(s2)")
    exit(ok ? 0 : 1)
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
