#!/bin/bash
# Run the double-dipping simulation for every registered dataset with a frozen
# A1 lock and no summary yet (SK/TW is always rerun so it carries the
# fixed-topic variant). Sequential; 16 cores per dataset; one log per dataset.
cd "$(dirname "$0")/../.."
OUTD=analysis/output/revision/doubledip
mkdir -p "$OUTD"
for lock in revision/locks/*.a1.k1.json; do
  id=$(basename "$lock" .a1.k1.json)
  if [ -f "$OUTD/${id}_summary.csv" ] && [ "$id" != "sktw_naturalization_2024" ]; then continue; fi
  echo "=== $id $(date +%H:%M)"
  LANG=en_US.UTF-8 /usr/bin/Rscript analysis/revision/rv_08_doubledip.R "$id" 200 8 16 > "$OUTD/${id}.log" 2>&1
  tail -3 "$OUTD/${id}.log"
done
echo "all done $(date +%H:%M)"
