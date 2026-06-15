#!/bin/bash
# Returns 0 if cyclic error reproduced, 1 otherwise.
FILE="core/src/main/scala/MutableDB.scala"
echo "[repro] clean compile..."
CLEAN_OUT=$(sbtn.bat 'core/clean; core/compile' 2>&1)
if ! echo "$CLEAN_OUT" | grep -q "success"; then
  echo "[repro] CLEAN COMPILE FAILED ▒^`^t cannot test"
  echo "$CLEAN_OUT" | tail -30
  exit 2
fi
# Inject trigger after first line
TMP=$(mktemp)
head -1 "$FILE" > "$TMP"
echo "// repro-trigger" >> "$TMP"
tail -n +2 "$FILE" >> "$TMP"
mv "$TMP" "$FILE"
echo "[repro] incremental compile..."
INC_OUT=$(sbtn.bat core/compile 2>&1)
# Revert trigger
grep -v "^// repro-trigger" "$FILE" > "$FILE.tmp" && mv "$FILE.tmp" "$FILE"
if echo "$INC_OUT" | grep -q "Cyclic reference involving val <import>" && \
   echo "$INC_OUT" | grep -q "DFDecimal.scala"; then
  echo "[repro] OK ▒^`^t reproduced cyclic error"
  exit 0
else
  echo "[repro] FAIL ▒^`^t bug did not reproduce"
  echo "$INC_OUT" | tail -30
  exit 1
fi
