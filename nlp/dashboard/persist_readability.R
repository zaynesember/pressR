#!/usr/bin/env Rscript
# Persist readability.rds into a DuckDB `readability` table (run at fold-in, after
# the family/tag/topic writers, single-writer -> no race).
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(DBI); library(duckdb) })
rd <- readRDS(file.path(nlp_out_dir(), "readability.rds"))
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "readability", as.data.frame(rd), overwrite = TRUE)
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_read_url ON readability(url)")
cat(sprintf("readability table written: %s rows\n", format(nrow(rd), big.mark = ",")))
dbDisconnect(con, shutdown = TRUE)
