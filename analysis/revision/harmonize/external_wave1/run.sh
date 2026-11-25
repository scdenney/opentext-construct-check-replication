#!/bin/bash
# Run an R expression/script with stderr captured; only show truncated Error lines.
ERR=${HARMONIZE_STDERR:-${TMPDIR:-/tmp}/harmonize_ext_last_stderr.txt}  # path changed on recovery (2026-11-17)
cd "$(dirname "$0")/../../../.."  # path changed on recovery (2026-11-17): repository root (was data/external; build.R paths are relative to the root)
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript "$@" 2>"$ERR" | grep -v "^$"
echo "[stderr lines: $(wc -l < "$ERR")]"; grep -E "^Error" "$ERR" | cut -c1-90
