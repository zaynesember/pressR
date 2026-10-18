#!/usr/bin/env Rscript
# Rebuild the DuckDB releases table over the COMBINED corpus (scraped + folded
# external Stout/Wang&Tucker) with a `source` provenance column, and refresh the
# identity stoplist (now including folded member names). Foreground, ~minutes.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
H <- "/Users/zaynesember/GitRepos/pressR/nlp/R"
source(file.path(H, "00_foundation.R"))
source(file.path(H, "01_build_duckdb.R"))
suppressMessages({ library(DBI); library(duckdb); library(dplyr) })
t0 <- Sys.time()

message("== Rebuilding DuckDB (scraped + external, source-tagged) ==")
nlp_build_duckdb(overwrite = TRUE)

con <- dbConnect(duckdb::duckdb(), dbdir = nlp_duckdb_path())
members <- dbGetQuery(con, "SELECT DISTINCT name FROM releases WHERE name IS NOT NULL")$name
stop_id <- nlp_identity_stoplist(members)
saveRDS(stop_id, file.path(nlp_out_dir(), "identity_stoplist.rds"))
message(sprintf("Identity stoplist: %d terms -> identity_stoplist.rds", length(stop_id)))

src <- dbGetQuery(con, "SELECT source, COUNT(*) n, SUM(usable::INT) usable,
  MIN(date) dmin, MAX(date) dmax,
  ROUND(100.0*AVG(has_body::INT),1) pct_body,
  ROUND(100.0*AVG((tags IS NOT NULL)::INT),1) pct_tagged
  FROM releases GROUP BY source ORDER BY n DESC")
cat("\n================ DUCKDB REBUILT ================\n"); print(src)
tot <- dbGetQuery(con, "SELECT COUNT(*) n, SUM(usable::INT) usable FROM releases")
cat(sprintf("TOTAL: %s releases (%s usable)\n",
  format(tot$n, big.mark=","), format(tot$usable, big.mark=",")))
cat("chamber x party:\n")
print(dbGetQuery(con, "SELECT chamber, party, COUNT(*) n FROM releases
                       WHERE party IN ('D','R') GROUP BY 1,2 ORDER BY 1,2"))
dbDisconnect(con, shutdown = TRUE)
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units="mins"))))
