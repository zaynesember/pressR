#!/usr/bin/env Rscript
# Persist sentiment.rds into a DuckDB `sentiment` table so the dashboard can join
# it live (Explore + downloads) and prep_dashboard can aggregate it. Run AFTER the
# family/tag/topic chain (single writer -> no race).
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(DBI); library(duckdb) })
sent <- readRDS(file.path(nlp_out_dir(), "sentiment.rds"))
sent <- as.data.frame(sent[, c("url", "sentiment", "sentiment_sd", "word_count")])
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "sentiment", sent, overwrite = TRUE)
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sent_url ON sentiment(url)")
cat(sprintf("sentiment table written: %s rows\n", format(nrow(sent), big.mark = ",")))
dbDisconnect(con, shutdown = TRUE)
