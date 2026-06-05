#requires -Version 5.1
<#
  A zero-install, STREAMING chat relay to the Anthropic Messages API, in pure
  PowerShell.

  For a Windows box that has PowerShell but no Python and no Claude CLI. Nothing
  to install — it uses the built-in .NET HttpClient to stream Server-Sent Events
  and writes each token as it arrives. Run it directly (`.\claude-chat.ps1`) to
  chat at the machine, or wire it as an SSH ForceCommand so a remote desktop
  drops straight into the chat (see the notes at the bottom of this file).

  It talks to the *model* — no tools, can't read files or run commands. The
  brains are the API; this is just an I/O loop around it.

  Config (env var, or the fallback noted):
    ANTHROPIC_API_KEY  required — a real API key from console.anthropic.com
                       (NOT a Claude.ai / Claude Code subscription login).
                       Fallback: a file named .claude-api-key next to this script.
    CLAUDE_MODEL       optional — default claude-sonnet-4-6 (snappy + cheap);
                       set claude-opus-4-8 for the heavyweight.
    CLAUDE_SYSTEM      optional — a system prompt to steer the assistant.
#>

# Windows PowerShell 5.1 still defaults to old TLS; the API requires 1.2+.
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
# Keep non-ASCII (accents etc.) intact in and out of the console.
try { [Console]::OutputEncoding = [Text.Encoding]::UTF8 } catch {}
# .NET Framework needs this loaded explicitly; on PS7 it's a harmless no-op.
try { Add-Type -AssemblyName System.Net.Http -ErrorAction SilentlyContinue } catch {}

$ApiKey = $env:ANTHROPIC_API_KEY
if (-not $ApiKey) {
  $keyFile = Join-Path $PSScriptRoot '.claude-api-key'
  if (Test-Path $keyFile) { $ApiKey = (Get-Content -Raw $keyFile).Trim() }
}
if (-not $ApiKey) {
  Write-Error 'No API key. Set $env:ANTHROPIC_API_KEY or put one in a .claude-api-key file next to this script.'
  exit 1
}

$Model  = if ($env:CLAUDE_MODEL) { $env:CLAUDE_MODEL } else { 'claude-sonnet-4-6' }
$System = $env:CLAUDE_SYSTEM
$Url    = 'https://api.anthropic.com/v1/messages'

$client = [System.Net.Http.HttpClient]::new()
$client.Timeout = [TimeSpan]::FromMinutes(10)   # whole-stream ceiling, not per-token

# Streams one reply, writing tokens as they arrive. Returns the full text (or
# $null if the call failed, so the caller can drop the dangling user turn).
function Invoke-ClaudeStream {
  param([System.Collections.IEnumerable] $Messages)

  $bodyObj = @{
    model      = $Model
    max_tokens = 4096
    stream     = $true
    messages   = @($Messages)
  }
  if ($System) { $bodyObj['system'] = $System }
  $json = $bodyObj | ConvertTo-Json -Depth 12

  $req = [System.Net.Http.HttpRequestMessage]::new([System.Net.Http.HttpMethod]::Post, $Url)
  $req.Headers.Add('x-api-key', $ApiKey)
  $req.Headers.Add('anthropic-version', '2023-06-01')
  $req.Content = [System.Net.Http.StringContent]::new($json, [Text.Encoding]::UTF8, 'application/json')

  $sb = [Text.StringBuilder]::new()
  try {
    $resp = $client.SendAsync($req, [System.Net.Http.HttpCompletionOption]::ResponseHeadersRead).GetAwaiter().GetResult()
    if (-not $resp.IsSuccessStatusCode) {
      $errBody = $resp.Content.ReadAsStringAsync().GetAwaiter().GetResult()
      $msg = $errBody
      try { $msg = ($errBody | ConvertFrom-Json).error.message } catch {}
      Write-Host ""
      Write-Host "[HTTP $([int]$resp.StatusCode)] $msg" -ForegroundColor Red
      return $null
    }

    $stream = $resp.Content.ReadAsStreamAsync().GetAwaiter().GetResult()
    $reader = [System.IO.StreamReader]::new($stream, [Text.Encoding]::UTF8)
    try {
      while (-not $reader.EndOfStream) {
        $line = $reader.ReadLine()
        if (-not $line -or -not $line.StartsWith('data:')) { continue }
        $payload = $line.Substring(5).Trim()
        if (-not $payload) { continue }
        $evt = $payload | ConvertFrom-Json
        switch ($evt.type) {
          'content_block_delta' {
            if ($evt.delta.type -eq 'text_delta') {
              [Console]::Out.Write($evt.delta.text)   # token, no newline
              [Console]::Out.Flush()
              [void]$sb.Append($evt.delta.text)
            }
          }
          'error' {
            Write-Host ""
            Write-Host "[stream error] $($evt.error.message)" -ForegroundColor Red
          }
        }
      }
    } finally { $reader.Dispose() }
    return $sb.ToString()
  } catch {
    $detail = $_.ErrorDetails.Message
    if (-not $detail) { $detail = $_.Exception.Message }
    Write-Host ""
    Write-Host "[error] $detail" -ForegroundColor Red
    return $null
  } finally {
    if ($resp) { $resp.Dispose() }
    $req.Dispose()
  }
}

$messages = New-Object System.Collections.ArrayList

Write-Host "Claude ($Model) - type 'exit' or Ctrl-C to leave."
Write-Host "One message per line; the conversation is remembered for this session."
Write-Host ""

try {
  while ($true) {
    $line = Read-Host 'you'
    if ($null -eq $line) { break }              # Ctrl-Z / EOF
    $text = $line.Trim()
    if ($text -in @('exit', 'quit')) { break }
    if ($text -eq '') { continue }

    [void]$messages.Add(@{ role = 'user'; content = $text })

    Write-Host ""
    Write-Host -NoNewline "claude> "
    $reply = Invoke-ClaudeStream -Messages $messages
    Write-Host ""
    Write-Host ""

    if ($reply) { [void]$messages.Add(@{ role = 'assistant'; content = $reply }) }
    else        { $messages.RemoveAt($messages.Count - 1) }   # drop the turn we couldn't answer
  }
} finally {
  $client.Dispose()
  Write-Host 'bye.'
}

<#
  ── Run it locally on the Windows box ───────────────────────────────────────
    $env:ANTHROPIC_API_KEY = 'sk-ant-...'      # or drop a .claude-api-key file
    powershell -NoProfile -ExecutionPolicy Bypass -File .\claude-chat.ps1

  ── Reach it over SSH from another desktop (Windows OpenSSH Server) ──────────
  1. Enable the server once (admin PowerShell):
       Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
       Start-Service sshd ; Set-Service sshd -StartupType Automatic
  2. Put the API key where the script's fallback finds it (env vars don't
     reliably reach sshd-spawned processes):
       Set-Content "$HOME\.claude-api-key" 'sk-ant-...' -NoNewline
  3. Add the remote desktop's PUBLIC key to authorized_keys, locked to chat:
       command="powershell -NoProfile -ExecutionPolicy Bypass -File C:\Users\you\claude-chat.ps1",no-port-forwarding,no-agent-forwarding ssh-ed25519 AAAA... you@desktop
     - normal user: C:\Users\you\.ssh\authorized_keys
     - admin user : C:\ProgramData\ssh\administrators_authorized_keys
       (sshd ignores the per-user file for admins, and the file's ACLs must
        grant only SYSTEM + Administrators or sshd refuses it)
  4. From the desktop (its built-in ssh, nothing installed):  ssh you@windows-host
#>
