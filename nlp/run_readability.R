#!/usr/bin/env Rscript
# Readability & sentence-complexity layer. Per usable release, compute classic
# measures via quanteda.textstats: Flesch reading ease, Flesch-Kincaid grade,
# Gunning Fog, mean sentence length (words/sentence), mean syllables/word.
# Streams year files, parallel within year; writes readability.rds (persisted to
# DuckDB at fold-in). Aggregated in the dashboard by party / year / issue / source.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(quanteda.textstats); library(data.table); library(parallel) })
t0 <- Sys.time(); OUT <- nlp_out_dir()
NCORE <- max(1L, parallel::detectCores() - 3L)
MEAS <- c("Flesch", "Flesch.Kincaid", "FOG", "meanSentenceLength", "meanWordSyllables")
message(sprintf("readability on %d cores", NCORE))

score <- function(txt, pos) {
  data.table::setDTthreads(1L)
  r <- quanteda.textstats::textstat_readability(txt, measure = MEAS, remove_hyphens = FALSE)
  data.table(pos = pos, flesch = r$Flesch, fk_grade = r$Flesch.Kincaid, fog = r$FOG,
             sent_len = r$meanSentenceLength, syll = r$meanWordSyllables)
}
parts <- nlp_stream_years(function(df, yr) {
  df <- nlp_add_flags(df); df <- df[df$usable & !is.na(df$body), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  b <- df$body; grp <- (seq_along(b) - 1L) %% NCORE
  res <- mclapply(split(seq_along(b), grp), function(ii) score(b[ii], ii), mc.cores = NCORE)
  out <- rbindlist(res); setorder(out, pos)
  data.table(url = df$url, source = df$source, party = df$party, chamber = df$chamber,
             year = as.integer(yr),
             flesch = round(out$flesch, 2), fk_grade = round(out$fk_grade, 2),
             fog = round(out$fog, 2), sent_len = round(out$sent_len, 2),
             syll = round(out$syll, 3))
})
rd <- rbindlist(parts)
# clean non-finite / degenerate values (very short or odd releases)
for (m in c("flesch","fk_grade","fog","sent_len","syll")) rd[!is.finite(get(m)), (m) := NA]
saveRDS(rd, file.path(OUT, "readability.rds"))

cat(sprintf("\n================ READABILITY ================\n"))
cat(sprintf("scored: %s releases\n", format(nrow(rd), big.mark = ",")))
cat("overall mean measures:\n")
print(rd[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE), 2)),
         .SDcols = c("flesch","fk_grade","fog","sent_len","syll")])
cat("\nmean Flesch-Kincaid grade by party:\n")
print(rd[party %in% c("D","R"), .(fk_grade = round(mean(fk_grade, na.rm = TRUE), 2),
       sent_len = round(mean(sent_len, na.rm = TRUE), 1), n = .N), by = party])
cat("\nFK grade by 4-year era:\n")
rd[, era := cut(year, breaks = c(2003,2010,2014,2018,2022,2027), labels = c("04-10","11-14","15-18","19-22","23-26"))]
print(rd[!is.na(era), .(fk_grade = round(mean(fk_grade, na.rm = TRUE), 2)), by = era][order(era)])
cat(sprintf("\nreadability.rds written. done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
