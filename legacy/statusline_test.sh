#!/usr/bin/env bash
# Tests for statusline.sh.
#
# Two layers:
#   1. Unit — the pure helpers hum(), mid_ellipsis() and path_head_trim(),
#             sourced in isolation via STATUSLINE_SOURCE=1.
#   2. E2E  — the full script fed JSON on stdin. Row 1 (branch + single-space
#             cwd path) is always present, so status segments are asserted on
#             row 2; ANSI colours are checked on the raw output.
#             Row 2 order: limits · context · tokens, joined by a single space.
#             Row 3 (5h reset time) appears only when resets_at is present.
#
# Each helper is exercised across normal, boundary, abnormal and extreme
# inputs. No external framework required: run `bash statusline_test.sh`.
set -uo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
STATUSLINE="$SCRIPT_DIR/../statusline.sh"

# Load the helpers without rendering a status line.
STATUSLINE_SOURCE=1 source "$STATUSLINE"

TMPROOT=$(mktemp -d)
trap 'rm -rf "$TMPROOT"' EXIT
NONGIT="$TMPROOT/nongit"; mkdir -p "$NONGIT"

ESC=$(printf '\033')
strip_ansi() { sed "s/${ESC}\[[0-9;]*m//g"; }

tests_run=0; tests_failed=0
ok()  { tests_run=$((tests_run + 1)); printf 'ok %d - %s\n' "$tests_run" "$1"; }
nok() { tests_run=$((tests_run + 1)); tests_failed=$((tests_failed + 1))
        printf 'not ok %d - %s\n     expected: [%s]\n     actual:   [%s]\n' \
               "$tests_run" "$1" "$2" "$3"; }
is()       { [ "$2" = "$3" ] && ok "$1" || nok "$1" "$2" "$3"; }
contains() { case "$2" in *"$3"*) ok "$1";; *) nok "$1" "…$3…" "$2";; esac; }

# Run the status line with a non-git working dir so the "." cwd fallback can
# never pick up an ambient repository.
render()      { ( cd "$NONGIT" && printf '%s' "$1" | bash "$STATUSLINE" ); }
render_row1() { render "$1" | sed -n '1p' | strip_ansi; }   # branch + cwd path
render_row2() { render "$1" | sed -n '2p' | strip_ansi; }   # tokens/limits/ctx
render_row3() { render "$1" | sed -n '3p' | strip_ansi; }   # 5h reset time

# JSON builders — temp paths never contain quotes, so interpolation is safe.
json_ctx()    { printf '{"context_window":{"used_percentage":%s}}' "$1"; }
json_5h()     { printf '{"rate_limits":{"five_hour":{"used_percentage":%s}}}' "$1"; }
# used_percentage + resets_at (epoch seconds) — exercises row 3.
json_5h_reset() { printf '{"rate_limits":{"five_hour":{"used_percentage":%s,"resets_at":%s}}}' "$1" "$2"; }
json_7d()     { printf '{"rate_limits":{"seven_day":{"used_percentage":%s}}}' "$1"; }
json_limits() { printf '{"rate_limits":{"five_hour":{"used_percentage":%s},"seven_day":{"used_percentage":%s}}}' "$1" "$2"; }
json_tokens() { printf '{"transcript_path":"%s"}' "$1"; }
json_branch() { printf '{"workspace":{"current_dir":"%s"}}' "$1"; }
# dir transcript h5 d7 ctx — every segment at once.
json_all()    { printf '{"workspace":{"current_dir":"%s"},"transcript_path":"%s","rate_limits":{"five_hour":{"used_percentage":%s},"seven_day":{"used_percentage":%s}},"context_window":{"used_percentage":%s}}' "$@"; }

# ======================================================================
# hum() — human-readable byte/token counts
# ======================================================================
echo "# hum() — normal"
is "hum 42"          "42"     "$(hum 42)"
is "hum 500"         "500"    "$(hum 500)"
is "hum 1500 → k"    "1.5k"   "$(hum 1500)"
is "hum 2500000 → M" "2.50M"  "$(hum 2500000)"

echo "# hum() — boundary"
is "hum 0"                  "0"       "$(hum 0)"
is "hum 999 (k floor -1)"   "999"     "$(hum 999)"
is "hum 1000 (k floor)"     "1.0k"    "$(hum 1000)"
is "hum 999500"             "999.5k"  "$(hum 999500)"
is "hum 999999 (rounds up)" "1000.0k" "$(hum 999999)"
is "hum 1000000 (M floor)"  "1.00M"   "$(hum 1000000)"

echo "# hum() — abnormal"
is "hum '' (empty)"    "0"  "$(hum '')"
is "hum -5 (negative)" "-5" "$(hum -5)"
# Non-numeric falls through awk's string comparison ("abc" >= "1000000" is true
# byte-wise) → documents current behaviour; callers only ever pass integers.
is "hum abc (non-numeric)" "0.00M" "$(hum abc)"

echo "# hum() — extreme"
is "hum 123456789"              "123.46M"     "$(hum 123456789)"
is "hum 2147483647 (int32 max)" "2147.48M"    "$(hum 2147483647)"
is "hum 9999999999"             "10000.00M"   "$(hum 9999999999)"
is "hum 5000000000000"          "5000000.00M" "$(hum 5000000000000)"

# ======================================================================
# mid_ellipsis() — middle truncation with …
# ======================================================================
echo "# mid_ellipsis() — normal"
is "fits within max"  "main"       "$(mid_ellipsis main 10)"
is "truncate 24 → 10" "featu…name" "$(mid_ellipsis feature/long-branch-name 10)"
is "truncate 10 → 6"  "abc…ij"     "$(mid_ellipsis abcdefghij 6)"

echo "# mid_ellipsis() — boundary"
is "n == max (no cut)"        "abcde" "$(mid_ellipsis abcde 5)"
is "n == max+1"               "ab…ef" "$(mid_ellipsis abcdef 5)"
is "max == 3 (min for …)"     "a…f"   "$(mid_ellipsis abcdef 3)"
is "max == 2 (< 3, hard cut)" "ab"    "$(mid_ellipsis abcdef 2)"
is "max == 0"                 ""      "$(mid_ellipsis abcdef 0)"
is "empty string"             ""      "$(mid_ellipsis '' 5)"
is "single char, max 1"       "x"     "$(mid_ellipsis x 1)"

echo "# mid_ellipsis() — abnormal"
# Negative max: printf treats a negative precision as omitted → whole string.
is "negative max (graceful)" "abcdef" "$(mid_ellipsis abcdef -1)"

echo "# mid_ellipsis() — extreme"
bigA=$(printf 'a%.0s' {1..100})
is "100 chars → 5"          "aa…aa" "$(mid_ellipsis "$bigA" 5)"
is "max far exceeds length" "short" "$(mid_ellipsis short 1000)"

# ======================================================================
# path_head_trim() — head-trim a path, prefixing …/
# ======================================================================
echo "# path_head_trim() — normal"
is "fits within max"      "/a/b"     "$(path_head_trim /a/b 20)"
is "drop leading comps"   "…/bin"    "$(path_head_trim /usr/local/bin 10)"
is "keep tail component"  "…/main.go" "$(path_head_trim src/app/main.go 10)"

echo "# path_head_trim() — boundary"
is "len == max (no trim)"    "abcde" "$(path_head_trim abcde 5)"
is "len == max+1 (1 comp)"   "…cdef" "$(path_head_trim abcdef 5)"
is "…/tail fits exactly"     "…/bcd" "$(path_head_trim /a/bcd 5)"

echo "# path_head_trim() — abnormal"
is "max < 2 (hard cut)" "a" "$(path_head_trim aaaa 1)"
is "max == 0"           ""  "$(path_head_trim aaaa 0)"

echo "# path_head_trim() — extreme"
is "single long component" "…gfilename" "$(path_head_trim verylongfilename 10)"
is "many components → tail" "…/final"    "$(path_head_trim /a/b/c/d/e/f/g/h/final 8)"
is "max far exceeds length" "short"      "$(path_head_trim short 1000)"

# ======================================================================
# End-to-end rendering — row 2 status segments
# ======================================================================
echo "# render — context percentage (value + colour thresholds)"
is "ctx 42 → text"               "▣ 42%" "$(render_row2 "$(json_ctx 42)")"
is "ctx 42.7 → decimal stripped" "▣ 42%" "$(render_row2 "$(json_ctx 42.7)")"
contains "ctx 49 → blue"              "$(render "$(json_ctx 49)")" "${ESC}[34m▣ 49%"
contains "ctx 50 → yellow (boundary)" "$(render "$(json_ctx 50)")" "${ESC}[33m▣ 50%"
contains "ctx 65 → yellow"            "$(render "$(json_ctx 65)")" "${ESC}[33m▣ 65%"
contains "ctx 80 → red (boundary)"    "$(render "$(json_ctx 80)")" "${ESC}[31m▣ 80%"
contains "ctx 85 → red"               "$(render "$(json_ctx 85)")" "${ESC}[31m▣ 85%"

echo "# render — session limits"
is "5h + 7d"             "5h 10% 7d 5%"   "$(render_row2 "$(json_limits 10 5)")"
is "5h only"             "5h 99%"         "$(render_row2 "$(json_5h 99)")"
is "7d only"             "7d 3%"          "$(render_row2 "$(json_7d 3)")"
is "5h decimal stripped" "5h 12%"         "$(render_row2 "$(json_5h 12.9)")"

echo "# render — 5h reset time (row 3, TZ-pinned)"
# 1735723800 = 2025-01-01 09:30:00 UTC
is "resets_at → row 3 clock time" "5h resets at 09:30" \
   "$(TZ=UTC render_row3 "$(json_5h_reset 23 1735723800)")"
contains "row 3 reset is cyan" \
   "$(TZ=UTC render "$(json_5h_reset 23 1735723800)")" "${ESC}[36m5h resets at 09:30"
is "no rate_limits → no row 3"          "" "$(render_row3 '{}')"
is "5h without resets_at → no row 3"    "" "$(render_row3 "$(json_5h 99)")"
contains "far-future epoch still formats HH:MM" \
   "$(TZ=UTC render_row3 "$(json_5h_reset 50 9999999999)")" "5h resets at "

echo "# render — session tokens from transcript"
TRANSCRIPT="$TMPROOT/transcript.jsonl"
cat > "$TRANSCRIPT" <<'EOF'
{"type":"assistant","message":{"usage":{"input_tokens":100,"cache_creation_input_tokens":50,"output_tokens":30}}}
{"type":"assistant","message":{"usage":{"input_tokens":200,"output_tokens":70,"cache_read_input_tokens":9999}}}
{"type":"user","message":{"usage":{"input_tokens":5,"output_tokens":5}}}
EOF
# in = (100+50) + 200 = 350 ; out = 30 + 70 = 100 ; cache-read & non-assistant ignored
is "tokens summed, cache-read excluded" "Σ 450 ↑350 ↓100" "$(render_row2 "$(json_tokens "$TRANSCRIPT")")"

CACHE_ONLY="$TMPROOT/cache_only.jsonl"
echo '{"type":"assistant","message":{"usage":{"cache_read_input_tokens":9999}}}' > "$CACHE_ONLY"
is "cache-read-only → no token segment"    "" "$(render_row2 "$(json_tokens "$CACHE_ONLY")")"
is "missing transcript file → no segment"  "" "$(render_row2 "$(json_tokens "$TMPROOT/does-not-exist.jsonl")")"

echo "# render — no status inputs"
is "empty object → empty row 2"   "" "$(render_row2 '{}')"
is "malformed json → empty row 2" "" "$(render_row2 'not valid json at all')"

# ======================================================================
# End-to-end rendering — row 1 branch & cwd path
# ======================================================================
echo "# render — git branch"
GITREPO="$TMPROOT/repo"; mkdir -p "$GITREPO"
git -C "$GITREPO" init -q
git -C "$GITREPO" -c user.email=t@example.com -c user.name=tester commit -q --allow-empty -m init
git -C "$GITREPO" checkout -q -b feat/x
contains "branch shown"            "$(render_row1 "$(json_branch "$GITREPO")")" "⎇ feat/x"
contains "branch is magenta"       "$(render "$(json_branch "$GITREPO")")"      "${ESC}[35m⎇ feat/x"
contains "branch via .cwd fallback" "$(render_row1 '{"cwd":"'"$GITREPO"'"}')"   "⎇ feat/x"

echo "# render — cwd path (row 1 always present)"
contains "short path shown verbatim" "$(render_row1 '{"workspace":{"current_dir":"/short"}}')" "/short"
contains "home dir abbreviated to ~" "$(render_row1 "$(json_branch "$HOME")")"  "~"

echo "# render — composition across both rows"
row1=$(render_row1 "$(json_all "$GITREPO" "$TRANSCRIPT" 10 5 30)")
row2=$(render_row2 "$(json_all "$GITREPO" "$TRANSCRIPT" 10 5 30)")
contains "everything: row 1 has branch"   "$row1" "⎇ feat/x"
is       "everything: row 2 full segments" "5h 10% 7d 5% ▣ 30% Σ 450 ↑350 ↓100" "$row2"
is "branch present + ctx-only row 2" "▣ 30%" \
   "$(render_row2 '{"workspace":{"current_dir":"'"$GITREPO"'"},"context_window":{"used_percentage":30}}')"
is "limits + ctx joined on row 2" "5h 10% 7d 5% ▣ 30%" \
   "$(render_row2 '{"rate_limits":{"five_hour":{"used_percentage":10},"seven_day":{"used_percentage":5}},"context_window":{"used_percentage":30}}')"

printf '\n%d tests, %d failed\n' "$tests_run" "$tests_failed"
[ "$tests_failed" -eq 0 ]
