# claude-statusline

A three-row status line for [Claude Code](https://claude.com/claude-code), written in Haskell.

```
⎇ feature/branch ~/ghq/github.com/you/repo
5h 10% 7d 5% ▣ 30% Σ 450 ↑350 ↓100
5h resets at 09:30
```

- **Row 1**: git branch · current directory (home-abbreviated, head-trimmed)
- **Row 2**: 5h/7d rate-limit usage · context window usage (blue < 50% ≤ yellow < 80% ≤ red) · cumulative session tokens (cache reads excluded)
- **Row 3**: local clock time the 5h rate-limit window resets

Empty rows are skipped. The binary reads the Claude Code statusLine JSON
protocol on stdin and writes ANSI-colored rows to stdout.

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
  "padding": 0
}
```

## Development

```sh
make build
make test
```

The core (`src/Statusline/*.hs` except `Shell.hs`) is pure: all effects
(stdin, env, git, transcript file, timezone) are resolved in the IO shell and
injected via `Render.Env`, so the whole rendering pipeline is testable without
a real git repository or TZ manipulation.

Widths are counted in code points, not terminal columns — East Asian wide
characters in branch names or paths may over-run (parity with the original
bash implementation).
