# claude-statusline

A four-row status line for [Claude Code](https://claude.com/claude-code), written in Haskell.

```
⎇ feature/branch ~/ghq/github.com/you/repo
5h 10% 7d 5% ▣ 30% Σ 450 ↑350 ↓100
5h resets at 09:30
🌤️ +25°C · 🌖 78% · 今日のニュースの見出しが右から左へ流れる…
```

- **Row 1**: git branch · current directory (home-abbreviated, head-trimmed)
- **Row 2**: 5h/7d rate-limit usage · context window usage (blue < 50% ≤ yellow < 80% ≤ red) · cumulative session tokens (cache reads excluded)
- **Row 3**: local clock time the 5h rate-limit window resets
- **Row 4**: ambient ticker — weather ([wttr.in](https://wttr.in)) · moon phase
  (computed locally) · latest NHK news headlines. Content wider than the
  terminal scrolls right-to-left, advancing one code point per wall-clock
  second (the status line only repaints when Claude Code refreshes it).

Empty rows are skipped. The binary reads the Claude Code statusLine JSON
protocol on stdin and writes ANSI-colored rows to stdout.

Row 4 never blocks on the network: weather and news are served from
`~/.cache/claude-statusline/` (XDG cache), and a stale entry (30 min for
weather, 20 min for news) fires a detached background `curl` whose result
lands on a later invocation. Without `curl` or connectivity the row degrades
to the moon phase alone.

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

The core (`src/Statusline/*.hs` except `Shell.hs`) is pure: all effects
(stdin, env, git, transcript file, timezone) are resolved in the IO shell and
injected via `Render.Env`, so the whole rendering pipeline is testable without
a real git repository or TZ manipulation.

Rows 1–3 count widths in code points, not terminal columns — East Asian wide
characters in branch names or paths may over-run (parity with the original
bash implementation). The row-4 ticker clips by display cells (CJK and emoji
count as two) since its content is routinely Japanese.
