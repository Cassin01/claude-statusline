# claude-statusline

A four-row status line for [Claude Code](https://claude.com/claude-code), written in Haskell.

```
⎇ feature/branch ~/ghq/github.com/you/repo
5h 10% 7d 5% ▣ 30% Σ 450 ↑350 ↓100
5h resets at 09:30
金☀34°🌖 土☀35°🌗 日⛅33°🌗 月🌧29°🌗 火🌧28°🌘 水⛅31°🌘 木☀33°🌑 · ニュース見出し…
```

- **Row 1**: git branch · current directory (home-abbreviated, head-trimmed)
- **Row 2**: 5h/7d rate-limit usage · context window usage (blue < 50% ≤ yellow < 80% ≤ red) · cumulative session tokens (cache reads excluded)
- **Row 3**: local clock time the 5h rate-limit window resets
- **Row 4**: ambient ticker — a 7-day forecast (weekday · weather emoji · max
  temperature · moon phase per day) followed by the latest headlines from NHK,
  Hacker News, and Zenn. Each headline is an OSC 8 hyperlink: Cmd+click
  (macOS) or Ctrl+click opens the article in terminals that support hyperlinks
  (iTerm2, kitty, WezTerm, VS Code); others show plain text. Content wider
  than the terminal scrolls right-to-left, advancing one code point per
  wall-clock second (the status line only repaints when Claude Code refreshes
  it).

Empty rows are skipped. The binary reads the Claude Code statusLine JSON
protocol on stdin and writes ANSI-colored rows to stdout.

Row 4 never blocks on the network. The forecast chain is: [ipinfo.io](https://ipinfo.io)
resolves the location from the caller's IP (cached 24 h), then
[Open-Meteo](https://open-meteo.com) supplies the 7-day forecast for those
coordinates (cached 3 h); the news feeds — NHK RSS, the Hacker News front
page via [hnrss.org](https://hnrss.org), and [zenn.dev/feed](https://zenn.dev/feed)
— are each cached 20 min (three headlines per source); the moon phase is
computed locally. All entries live in `~/.cache/claude-statusline/` (XDG
cache), and a stale entry fires a detached background `curl` whose result
lands on a later invocation. Until the forecast cache warms up — or without
`curl` or connectivity — the row degrades to today's moon phase alone.

## Install

Requires GHC ≥ 9.10 and cabal (e.g. via [ghcup](https://www.haskell.org/ghcup/)).

```sh
make install   # copies the binary to ~/.local/bin/claude-statusline
```

Then point Claude Code at it in `~/.claude/settings.json`:

```json
"statusLine": {
  "type": "command",
  "command": "/path/to/.local/bin/claude-statusline",
  "padding": 0,
  "refreshInterval": 1
}
```

`refreshInterval` matters for row 4: Claude Code only re-runs the status line
command on conversation events (new assistant message, `/compact`, mode
changes), so without a periodic refresh the ticker freezes whenever the
session is idle. `refreshInterval: 1` re-runs it every second, which is what
drives the scroll.

## Development

```sh
make build
make test
```

The core (`src/Statusline/*.hs` except `Shell.hs` and `Cache.hs`) is pure:
all effects (stdin, env, git, transcript file, timezone, cache files) are
resolved in the IO shell and injected via `Render.Env`, so the whole
rendering pipeline is testable without a real git repository or TZ
manipulation.

Rows 1–3 count widths in code points, not terminal columns — East Asian wide
characters in branch names or paths may over-run (parity with the original
bash implementation). The row-4 ticker clips by display cells (CJK and emoji
count as two) since its content is routinely Japanese.
