#!/bin/bash
# Rerun the split procedure of the double-dipping simulation for every dataset
# that has results, after the projection of the estimation fold was corrected
# (19 Nov 2026; stm_documents() in rv_config.R). The full-sample columns are
# carried over (SPLIT_ONLY=1). Resumable: a dataset with a .splitfix stamp is
# skipped. Usage: bash analysis/revision/rv_08_rerun_split.sh [cores=12]
cd "$(dirname "$0")/../.."
CORES=${1:-12}
OUTD=analysis/output/revision/doubledip
for f in "$OUTD"/*_perm.csv; do
  id=$(basename "$f" _perm.csv)
  [ -f "$OUTD/${id}.splitfix" ] && continue
  echo "=== $id $(date +%H:%M)"
  if SPLIT_ONLY=1 LANG=en_US.UTF-8 OMP_NUM_THREADS=1 /usr/bin/Rscript analysis/revision/rv_08_doubledip.R "$id" 200 8 "$CORES" > "$OUTD/${id}.splitfix.log" 2>&1; then
    date > "$OUTD/${id}.splitfix"
  else
    echo "FAILED $id"; tail -3 "$OUTD/${id}.splitfix.log"
  fi
done
echo "all done $(date +%H:%M)"
