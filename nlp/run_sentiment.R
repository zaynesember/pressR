#!/usr/bin/env Rscript
# Precompute sentence-level sentiment (sentimentr, valence-shifter aware) for every
# usable release in the COMBINED corpus. Streams year files (duckdb-free, so it can
# run alongside the family/tag jobs), parallelizes within each year, writes
# sentiment.rds (url, source, party, chamber, year, sentiment, sd, word_count).
# CAVEAT: off-the-shelf sentiment is weak on congressional/promotional prose; the
# dashboard presents this with caveats and conditions on issue where possible.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(sentimentr); library(data.table); library(parallel) })
t0 <- Sys.time()
NCORE <- max(1L, parallel::detectCores() - 3L)   # leave headroom for a parallel family run
message(sprintf("sentiment precompute on %d cores", NCORE))

score_chunk <- function(txt, pos) {
  data.table::setDTthreads(1L)
  s <- sentimentr::sentiment_by(sentimentr::get_sentences(txt))
  data.table(pos = pos, sentiment = s$ave_sentiment, sentiment_sd = s$sd,
             word_count = s$word_count)
}

parts <- nlp_stream_years(function(df, yr) {
  df <- nlp_add_flags(df); df <- df[df$usable, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  b <- df$body
  grp <- (seq_along(b) - 1L) %% NCORE                # round-robin chunks
  res <- mclapply(split(seq_along(b), grp),
                  function(ii) score_chunk(b[ii], ii), mc.cores = NCORE)
  out <- rbindlist(res); setorder(out, pos)
  data.table(url = df$url, source = df$source, party = df$party,
             chamber = df$chamber, year = as.integer(yr),
             sentiment = out$sentiment, sentiment_sd = out$sentiment_sd,
             word_count = out$word_count)
})
sent <- rbindlist(parts)
saveRDS(sent, file.path(nlp_out_dir(), "sentiment.rds"))

cat("\n================ SENTIMENT ================\n")
cat(sprintf("scored: %s releases\n", format(nrow(sent), big.mark = ",")))
cat("overall ave_sentiment summary:\n"); print(summary(sent$sentiment))
cat("\nmean sentiment by party (D/R):\n")
print(sent[party %in% c("D","R"), .(mean = round(mean(sentiment),4),
      median = round(median(sentiment),4), n = .N), by = party])
cat("\nmean sentiment by source:\n")
print(sent[, .(mean = round(mean(sentiment),4), n = .N), by = source])
cat(sprintf("\nsentiment.rds written. done in %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
