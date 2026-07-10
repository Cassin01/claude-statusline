#!/usr/bin/env bash
# Claude Code status line (three rows):
#   row 1: git branch (left) · cwd path (right)
#   row 2: session (5h/7d) limits · context usage · session tokens
#   row 3: local clock time the 5h rate-limit window resets
#
# Tests: tests/statusline_test.sh — run `bash tests/statusline_test.sh`
set -uo pipefail

hum() { awk -v n="$1" 'BEGIN{ if(n>=1000000) printf "%.2fM",n/1000000;
  else if(n>=1000) printf "%.1fk",n/1000; else printf "%d",n }'; }

# middle-truncate with … so both ends of a long branch stay visible
mid_ellipsis() {
  local s=$1 max=$2 n=${#1}
  [ "$n" -le "$max" ] && { printf '%s' "$s"; return; }
  [ "$max" -lt 3 ]    && { printf '%.*s' "$max" "$s"; return; }
  local keep=$((max - 1)) head tail off
  head=$(((keep + 1) / 2)); tail=$((keep / 2)); off=$((n - tail))
  printf '%s…%s' "${s:0:head}" "${s:off}"
}

# head-trim a path to <=max cols by dropping leading components, prefixing …/
path_head_trim() {
  local s=$1 max=$2 rest=$1
  [ "${#s}" -le "$max" ] && { printf '%s' "$s"; return; }
  while :; do
    local next=${rest#*/}                     # drop leading component
    [ "$next" = "$rest" ] && break            # no '/' left
    rest=$next
    [ $((2 + ${#rest})) -le "$max" ] && { printf '…/%s' "$rest"; return; }
  done
  [ "$max" -lt 2 ] && { printf '%.*s' "$max" "$rest"; return; }
  printf '…%s' "${rest: -$((max - 1))}"        # single long component
}

# When sourced (e.g. by tests), stop here so the helpers above can be
# exercised in isolation without rendering a status line.
[ "${STATUSLINE_SOURCE:-}" = 1 ] && return 0

input=$(cat)

RESET=$'\033[0m'; DIM=$'\033[2m'
BLUE=$'\033[34m'; CYAN=$'\033[36m'; YELLOW=$'\033[33m'
RED=$'\033[31m'; MAGENTA=$'\033[35m'; GREEN=$'\033[32m'

# single jq pass; one field per line so absent values stay aligned
# (reading tab-separated would collapse empty fields via IFS whitespace).
{ read -r cwd; read -r ctx_pct; read -r h5; read -r d7; read -r h5_reset; read -r transcript; } < <(
  printf '%s' "$input" | jq -r '
    (.workspace.current_dir // .cwd // "."),
    (.context_window.used_percentage // ""),
    (.rate_limits.five_hour.used_percentage // ""),
    (.rate_limits.seven_day.used_percentage // ""),
    (.rate_limits.five_hour.resets_at // ""),
    (.transcript_path // "")' 2>/dev/null
)
[ -z "$cwd" ] && cwd="."

# --- row 1: branch + context ---
branch=$(git -C "$cwd" branch --show-current 2>/dev/null || true)
[ -z "$branch" ] && branch=$(git -C "$cwd" rev-parse --short HEAD 2>/dev/null || true)

ctx_plain=""; ctx_seg=""
if [ -n "$ctx_pct" ]; then
  p=${ctx_pct%.*}
  if   [ "$p" -ge 80 ]; then c=$RED
  elif [ "$p" -ge 50 ]; then c=$YELLOW
  else                       c=$BLUE; fi
  ctx_plain="▣ ${p}%"
  ctx_seg="${c}${ctx_plain}${RESET}"
fi

cols=${COLUMNS:-80}

# right-hand path: cwd, home-abbreviated, head-trimmed to an appropriate length
disp=$cwd
case "$disp" in
  "$HOME")   disp="~" ;;
  "$HOME"/*) disp="~${disp#"$HOME"}" ;;
esac
path_cap=$(( cols / 2 ))                               # appropriate length: <= half the row
[ "$path_cap" -gt 40 ] && path_cap=40
[ "$path_cap" -lt 10 ] && path_cap=10
disp=$(path_head_trim "$disp" "$path_cap")
path_w=${#disp}
path_seg="${DIM}${disp}${RESET}"

# left-hand branch, capped so "⎇ branch path" fits one line
seg_branch=""
if [ -n "$branch" ]; then
  branch_max=$(( cols - 2 - 1 - path_w ))             # "⎇ " + single-space separator
  [ "$branch_max" -lt 8 ] && branch_max=8             # floor so it never collapses
  branch=$(mid_ellipsis "$branch" "$branch_max")
  seg_branch="${MAGENTA}⎇ ${branch}${RESET}"
fi

# branch and path joined by a single space (no right-alignment)
if [ -n "$seg_branch" ]; then
  row1="${seg_branch} ${path_seg}"
else
  row1="$path_seg"
fi

# --- row 2: session tokens + limits ---
# cumulative session tokens (exclude cache reads to avoid double-count)
tok_in=0; tok_out=0
if [ -n "$transcript" ] && [ -f "$transcript" ]; then
  read -r tok_in tok_out < <(
    jq -r 'select(.type=="assistant") | .message.usage
           | [ ((.input_tokens // 0) + (.cache_creation_input_tokens // 0)),
               (.output_tokens // 0) ] | @tsv' "$transcript" 2>/dev/null \
    | awk '{i+=$1; o+=$2} END{printf "%d %d", i+0, o+0}'
  )
fi
seg_tokens=""
if [ "$((tok_in + tok_out))" -gt 0 ]; then
  seg_tokens="${GREEN}Σ $(hum $((tok_in + tok_out)))${RESET} ${DIM}↑$(hum "$tok_in") ↓$(hum "$tok_out")${RESET}"
fi

sess=""
[ -n "$h5" ] && sess="5h ${h5%.*}%"
if [ -n "$d7" ]; then
  [ -n "$sess" ] && sess="${sess} "
  sess="${sess}7d ${d7%.*}%"
fi
seg_limits=""
[ -n "$sess" ] && seg_limits="${CYAN}${sess}${RESET}"

row2="$seg_limits"
if [ -n "$ctx_seg" ]; then
  [ -n "$row2" ] && row2="${row2} "
  row2="${row2}${ctx_seg}"
fi
if [ -n "$seg_tokens" ]; then
  [ -n "$row2" ] && row2="${row2} "
  row2="${row2}${seg_tokens}"
fi

# --- row 3: 5h rate-limit reset time (local HH:MM) ---
row3=""
case "$h5_reset" in
  ''|*[!0-9]*) ;;                                      # absent / non-numeric → skip
  *)
    hhmm=$(date -r "$h5_reset" +%H:%M 2>/dev/null) \
      || hhmm=$(date -d "@$h5_reset" +%H:%M 2>/dev/null)
    [ -n "$hhmm" ] && row3="${CYAN}5h resets at ${hhmm}${RESET}"
    ;;
esac

# --- emit (skip empty rows) ---
out=""
for r in "$row1" "$row2" "$row3"; do
  [ -z "$r" ] && continue
  [ -n "$out" ] && out="$out"$'\n'
  out="$out$r"
done
printf '%s' "$out"
