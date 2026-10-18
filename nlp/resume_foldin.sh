#!/bin/bash
# resume_foldin.sh -- continue a fold-in after an interruption (lid close,
# crash, kill). Reads ~/wayback_foldin.log, finds the steps already DONE in
# this run, and executes only the remaining ones in order. Safe because every
# step writes its output only on successful completion: an interrupted step
# left its old output untouched and simply re-runs.
#
# The one special case is dfm_raw.rds: run_foldin.sh deletes it at launch so
# tag-complete/topics/partisan rebuild the DFM over the new corpus. If
# tag-complete already REBUILT it this run, deleting it again would waste the
# rebuild -- so it is deleted only when tag-complete has not yet completed.
#
# Run:  nohup bash nlp/resume_foldin.sh > ~/institutional_logs/resume_foldin.log 2>&1 &

set -uo pipefail
REPO="/Users/zaynesember/GitRepos/pressR"
RS="/usr/local/bin/Rscript"
LOG="$HOME/wayback_foldin.log"
cd "$REPO" || exit 1
caffeinate -i -w $$ &

if pgrep -f "run_foldin.sh" >/dev/null 2>&1; then
  echo "run_foldin.sh is still alive -- nothing to resume. Let it run."
  exit 0
fi

say(){ echo "$(date '+%F %T') $*" | tee -a "$LOG"; }
step(){ local name="$1"; shift; local t0=$SECONDS; say "=== START $name ==="
  if "$@" >> "$LOG" 2>&1; then say "=== DONE  $name ($(( (SECONDS-t0)/60 )) min) ==="
  else local rc=$?; say "!!! FAILED $name (rc=$rc) -- aborting."; exit 1; fi; }

done_step(){ grep -qE "=== DONE  $1 " "$LOG"; }

STEPS=(rebuild-duckdb families tag-complete topics sentiment persist-sentiment
       attack partisan network readability persist-readability prep-dashboard)
CMDS=("nlp/run_rebuild.R" "nlp/run_families.R" "nlp/run_tag_complete.R"
      "nlp/run_topics.R" "nlp/run_sentiment.R" "nlp/dashboard/persist_sentiment.R"
      "nlp/run_attack.R" "nlp/run_partisan.R" "nlp/run_network.R"
      "nlp/run_readability.R" "nlp/dashboard/persist_readability.R"
      "nlp/dashboard/prep_dashboard.R")

say "RESUME: checking which steps this run already completed"
if ! done_step "tag-complete"; then
  say "tag-complete not yet done -- removing dfm_raw.rds so the DFM rebuilds fresh"
  rm -f "$HOME/Library/Application Support/org.R-project.R/R/pressR_nlp/dfm_raw.rds"
fi

for i in "${!STEPS[@]}"; do
  if done_step "${STEPS[$i]}"; then
    say "skip ${STEPS[$i]} (already DONE this run)"
  else
    step "${STEPS[$i]}" "$RS" "${CMDS[$i]}"
  fi
done

say "===== FOLD-IN COMPLETE (resumed) -- corpus includes every external/ source ====="
