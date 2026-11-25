#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT INT TERM

sed -n 's/^[[:space:]]*PC\(Mizar[A-Za-z]*Nbr\)[[:space:]]*=[[:space:]]*\([0-9]*\);/\1=\2/p' \
  "$ROOT/base/pcmizver.pas" > "$TMPDIR/mml.ini"

for src in "$ROOT"/tests/test_*.dpr; do
  name=$(basename "$src" .dpr)
  fpc -Mdelphi -Fu"$ROOT/base" -Fu"$ROOT/kernel" -FE"$TMPDIR" -FU"$TMPDIR" "$src" >/dev/null
  MIZFILES="$TMPDIR" "$TMPDIR/$name"
done
