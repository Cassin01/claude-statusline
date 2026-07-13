# claude-statusline

A four-row status line for [Claude Code](https://claude.com/claude-code), written in Haskell.



https://github.com/user-attachments/assets/b51bb047-394a-4077-878c-996de7dff370



- **Row 1**: git branch · current directory (home-abbreviated, head-trimmed)
- **Row 2**: 5h/7d rate-limit usage · context window usage (blue < 50% ≤ yellow < 80% ≤ red) · cumulative session tokens (cache reads excluded)
- **Row 3**: local clock time the 5h rate-limit window resets, plus — when
  the current burn rate would hit 100% before that reset — the predicted
  exhaustion time (`100% at ~HH:MM`, yellow). The rate is fitted over up to
  30 minutes of usage samples persisted in the XDG cache across invocations;
  the segment stays hidden until enough rising samples accumulate (≥ 60 s
  span) or when the pace comfortably outlasts the window
- **Row 4**: ambient ticker — a 7-day forecast (weekday · weather emoji · max
  temperature · moon phase per day) followed by the latest headlines from NHK,
  BBC, Hacker News, and Zenn. Each headline is an OSC 8 hyperlink: Cmd+click
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
coordinates (cached 3 h); the news feeds — NHK RSS, [BBC News RSS](https://feeds.bbci.co.uk/news/rss.xml),
the Hacker News front page via [hnrss.org](https://hnrss.org), and
[zenn.dev/feed](https://zenn.dev/feed)
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

## Configuration

Optional, at `~/.config/claude-statusline/config.json` (XDG config dir). The
file is re-read on every refresh, so edits apply within a second. Every key is
optional; a missing file, malformed JSON, or a wrong-typed key silently falls
back to the defaults shown below — the status line never fails over its
config. Validate with `jq . config.json` if something looks off.

```json
{
  "feeds": [
    { "name": "nhk",        "label": "NHK: ",  "url": "https://www.nhk.or.jp/rss/news/cat0.xml" },
    { "name": "bbc",        "label": "BBC: ",  "url": "https://feeds.bbci.co.uk/news/rss.xml" },
    { "name": "hackernews", "label": "HN: ",   "url": "https://hnrss.org/frontpage" },
    { "name": "zenn",       "label": "Zenn: ", "url": "https://zenn.dev/feed" }
  ],
  "headlineCount": 3,
  "rows": { "git": true, "usage": true, "reset": true, "ticker": true },
  "ttl": { "location": 86400, "forecast": 10800, "news": 1200 }
}
```

- **feeds** — RSS sources for row 4. `name` and `url` are required (invalid
  items are dropped); `label` defaults to `"<name>: "`. An explicit `[]`
  disables headlines. Feeds must expose RSS `<item>` elements with `<title>`
  (and ideally `<link>`).
- **headlineCount** — headlines per feed (clamped to 0–20).
- **rows** — enable/disable each row. Disabling `ticker` also stops all
  ambient network fetches.
- **ttl** — cache lifetimes in seconds for the location lookup, the forecast,
  and each news feed (minimum 60).

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
