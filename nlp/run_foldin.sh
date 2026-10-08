#!/bin/bash
# Full NLP fold-in: rebuild the DuckDB releases table over the COMBINED corpus
# (scraped + Stout + Wang&Tucker + external/wayback), then re-run every analysis
# layer in dependency order so the recovered Wayback releases enter families,
# tags, topics, sentiment, attack, partisan, network, readability and the
# dashboard aggregates.
#
# HEAVY: ~4-10h. families / tag-complete / topics are the long poles and do NOT
# checkpoint mid-run, so run this in a window where the laptop stays AWAKE to
# completion (lid OPEN + plugged in -- caffeinate below blocks idle sleep, but a
# lid close still force-sleeps and would restart whatever step is mid-flight).
# DuckDB is single-writer: this stops any reader on :7788 first; relaunch the
# dashboard afterward. Per-step output + timing -> ~/wayback_foldin.log.
# Resume after a failure: each step persists its own output, so fix the cause and
# re-run the remaining steps (comment out the ones already DONE in the log).
set -uo pipefail
REPO="/Users/zaynesember/GitRepos/pressR"
RS="/usr/local/bin/Rscript"
LOG="$HOME/wayback_foldin.log"
cd "$REPO" || exit 1
caffeinate -i -w $$ &              # block idle sleep for this script's lifetime
: > "$LOG"
say(){ echo "$(date '+%F %T') $*" | tee -a "$LOG"; }
step(){ local name="$1"; shift; local t0=$SECONDS; say "=== START $name ==="
  if "$@" >> "$LOG" 2>&1; then say "=== DONE  $name ($(( (SECONDS-t0)/60 )) min) ==="
  else local rc=$?; say "!!! FAILED $name (rc=$rc) -- aborting. Fix, then re-run remaining steps."; exit 1; fi; }

say "Fold-in starting. Stopping any DuckDB reader on :7788."
lsof -ti tcp:7788 2>/dev/null | xargs kill 2>/dev/null || true; sleep 1

# rebuild-duckdb file.remove()s press.duckdb -- including every analysis table
# the later steps rewrite -- so keep a copy to roll back to if the chain dies.
NLP="$HOME/Library/Application Support/org.R-project.R/R/pressR_nlp"
BAK="$NLP/backups/press_pre_foldin_$(date +%Y%m%d_%H%M%S).duckdb"
mkdir -p "$NLP/backups"
say "Backing up press.duckdb -> $BAK"
cp "$NLP/press.duckdb" "$BAK" || { say "!!! backup failed -- aborting before touching the DB."; exit 1; }

# dfm_raw.rds is an if-exists-reuse cache read by tag-complete/topics/partisan;
# left stale it silently pins those three layers to the PRE-fold-in corpus.
say "Removing dfm_raw.rds so the DFM rebuilds over the new corpus."
rm -f "$HOME/Library/Application Support/org.R-project.R/R/pressR_nlp/dfm_raw.rds"

step "rebuild-duckdb"      "$RS" nlp/run_rebuild.R
step "families"            "$RS" nlp/run_families.R
step "tag-complete"        "$RS" nlp/run_tag_complete.R
step "topics"              "$RS" nlp/run_topics.R
step "sentiment"           "$RS" nlp/run_sentiment.R
step "persist-sentiment"   "$RS" nlp/dashboard/persist_sentiment.R
step "attack"              "$RS" nlp/run_attack.R
step "partisan"            "$RS" nlp/run_partisan.R
step "network"             "$RS" nlp/run_network.R
step "readability"         "$RS" nlp/run_readability.R
step "persist-readability" "$RS" nlp/dashboard/persist_readability.R
step "prep-dashboard"      "$RS" nlp/dashboard/prep_dashboard.R

say "===== FOLD-IN COMPLETE -- corpus now includes every external/ source (wayback + institutional; expect ~1.04M releases) ====="
say "Relaunch the dashboard: Rscript -e 'shiny::runApp(\"nlp/dashboard/app.R\", port=7788L)'"
