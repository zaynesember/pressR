#!/usr/bin/env Rscript
# Sentiment #2: targeted attack / tone score. Decomposes flat document polarity
# into DIRECTED stance: locate sentences that name a target entity (presidents
# resolved by date to the incumbent, parties, party leaders, agencies, China...),
# score that sentence with sentimentr (valence-shifter & negation aware), and
# aggregate to (a) a per-release OUT-PARTY ATTACK score (negativity aimed at the
# other party's president / party / leaders) + in-party support, and (b) entity-
# stance summaries (tone toward X by speaker party over time). z>0 sentiment =
# supportive, attack = -sentiment toward an out-party target.
#   Rscript nlp/run_attack.R sample   # one year, print examples + face-validity
#   Rscript nlp/run_attack.R          # full corpus + write tables + validation sample
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(sentimentr); library(stringi); library(data.table)
  library(parallel); library(DBI); library(duckdb) })
t0 <- Sys.time(); OUT <- nlp_out_dir()
mode <- (commandArgs(trailingOnly = TRUE)[1] %||% "full"); SAMPLE <- identical(mode, "sample")
NCORE <- max(1L, parallel::detectCores() - 3L)

## ---- gazetteer + incumbents ----------------------------------------------
GAZ <- fread("/Users/zaynesember/GitRepos/pressR/nlp/crosswalks/entity_aliases.csv",
             colClasses = "character")
esc <- function(a) stri_replace_all_regex(a, "([.\\\\])", "\\\\$1")
GAZ[, regex := vapply(strsplit(aliases, ";", fixed = TRUE), function(v)
  sprintf("\\b(%s)\\b", paste(esc(trimws(v)), collapse = "|")), character(1))]
ANY_PAT <- sprintf("\\b(%s)\\b", paste(unlist(lapply(strsplit(GAZ$aliases, ";", fixed = TRUE),
  function(v) esc(trimws(v)))), collapse = "|"))
PARTISAN <- c("president", "party", "leader")          # types with an in/out-party

INC <- data.table(
  start = as.Date(c("2001-01-20","2009-01-20","2017-01-20","2021-01-20","2025-01-20")),
  end   = as.Date(c("2009-01-20","2017-01-20","2021-01-20","2025-01-20","2099-01-01")),
  name  = c("bush","obama","trump","biden","trump"),
  party = c("R","D","R","D","R"))
resolve_inc <- function(dt) {
  i <- which(dt >= INC$start & dt < INC$end)[1]
  if (is.na(i)) c(NA, NA) else c(INC$name[i], INC$party[i])
}

## ---- per-release directed scoring ----------------------------------------
score_release <- function(body, dt, keep_text = FALSE) {
  sents <- get_sentences(body)[[1]]
  if (length(sents) < 1) return(NULL)
  lc <- stri_trans_tolower(sents)
  rows <- vector("list", nrow(GAZ))
  for (e in seq_len(nrow(GAZ))) {
    m <- which(stri_detect_regex(lc, GAZ$regex[e]))
    if (!length(m)) next
    eid <- GAZ$entity_id[e]; etype <- GAZ$entity_type[e]; tp <- GAZ$party[e]
    if (eid == "incumbent") { r <- resolve_inc(dt); if (is.na(r[1])) next; eid <- r[1]; tp <- r[2] }
    rows[[e]] <- data.table(entity_id = eid, entity_type = etype, tparty = tp, sidx = m)
  }
  rows <- rbindlist(rows)
  if (!nrow(rows)) return(NULL)
  rows <- unique(rows, by = c("entity_id", "sidx"))
  usid <- sort(unique(rows$sidx))
  sc <- sentiment_by(sents[usid])
  smap <- setNames(sc$ave_sentiment, as.character(usid))
  rows[, sentiment := smap[as.character(sidx)]]
  if (keep_text) rows[, text := sents[sidx]]
  rows[!is.na(sentiment)]
}

# score a data.frame of releases (one year), parallel over rows
score_df <- function(df, keep_text = FALSE) {
  df <- nlp_add_flags(df)
  df <- df[df$usable & !is.na(df$party) & df$party %in% c("D","R"), , drop = FALSE]
  hit <- stri_detect_regex(stri_trans_tolower(df$body), ANY_PAT)
  df <- df[hit, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  res <- mclapply(seq_len(nrow(df)), function(i) {
    data.table::setDTthreads(1L)
    H <- score_release(df$body[i], as.Date(df$date[i]), keep_text)
    if (is.null(H)) return(NULL)
    H[, `:=`(url = df$url[i], sp_party = df$party[i], chamber = df$chamber[i],
             year = as.integer(format(as.Date(df$date[i]), "%Y")))]
    H
  }, mc.cores = NCORE)
  rbindlist(res)
}

## ---- SAMPLE mode: validate on one year ------------------------------------
if (SAMPLE) {
  f <- nlp_year_files(); f <- f[grepl("releases-2019", f, fixed = TRUE)][1]
  df <- readRDS(f)
  set.seed(1); df <- df[sample(nrow(df), min(4000L, nrow(df))), ]
  message(sprintf("sample: scoring up to %d releases from 2019 ...", nrow(df)))
  M <- score_df(df, keep_text = TRUE)
  M[, out := tparty %in% c("D","R") & tparty != sp_party & entity_type %in% PARTISAN]
  cat(sprintf("\nmention-sentences: %s | out-party-target: %s\n",
    format(nrow(M), big.mark=","), format(sum(M$out), big.mark=",")))
  cat("\n-- face validity: mean sentiment toward president, by speaker party --\n")
  print(M[entity_id %in% c("trump","biden","obama"),
          .(mean=round(mean(sentiment),3), n=.N), by=.(entity_id, sp_party)][order(entity_id, sp_party)])
  cat("\n-- 14 example OUT-PARTY-ATTACK sentences (most negative) --\n")
  ex <- M[out == TRUE][order(sentiment)][1:14]
  for (i in seq_len(nrow(ex))) cat(sprintf("  [%.2f %s->%s] %s\n", ex$sentiment[i], ex$sp_party[i],
    ex$entity_id[i], substr(gsub("\\s+"," ",ex$text[i]), 1, 150)))
  cat("\n-- 8 example SUPPORTIVE (in-party) sentences --\n")
  inp <- M[tparty==sp_party & entity_type %in% PARTISAN][order(-sentiment)][1:8]
  for (i in seq_len(nrow(inp))) cat(sprintf("  [%.2f %s->%s] %s\n", inp$sentiment[i], inp$sp_party[i],
    inp$entity_id[i], substr(gsub("\\s+"," ",inp$text[i]), 1, 150)))
  cat(sprintf("\n[sample] done in %.1f min — nothing written\n", as.numeric(difftime(Sys.time(),t0,units="mins"))))
  quit(save = "no")
}

## ---- FULL run -------------------------------------------------------------
message(sprintf("== Targeted tone over all usable D/R releases (%d cores) ==", NCORE))
parts <- nlp_stream_years(function(df, yr) score_df(df, keep_text = FALSE))
M <- rbindlist(parts)
message(sprintf("  %s mention-sentences across %s releases",
  format(nrow(M), big.mark=","), format(uniqueN(M$url), big.mark=",")))

M[, out := tparty %in% c("D","R") & tparty != sp_party & entity_type %in% PARTISAN]
M[, inp := tparty %in% c("D","R") & tparty == sp_party & entity_type %in% PARTISAN]
attack  <- M[out == TRUE, .(out_attack = round(-mean(sentiment), 4), n_out = .N), by = url]
support <- M[inp == TRUE, .(in_support = round(mean(sentiment), 4), n_in = .N), by = url]
attack_scores <- merge(attack, support, by = "url", all = TRUE)
entity_stance <- M[, .(mean_sentiment = round(mean(sentiment), 4), n = .N),
                   by = .(entity_id, entity_type, tparty, sp_party, chamber, year)]

con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "attack_scores", as.data.frame(attack_scores), overwrite = TRUE)
dbWriteTable(con, "entity_stance", as.data.frame(entity_stance), overwrite = TRUE)
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_attack_url ON attack_scores(url)")
dbDisconnect(con, shutdown = TRUE)
# validation sample (with text) for inspection
set.seed(7); vs <- M[out == TRUE][sample(.N, min(500L, .N))]
saveRDS(list(attack_scores = attack_scores, entity_stance = entity_stance), file.path(OUT, "attack.rds"))

cat(sprintf("\n================ TARGETED TONE ================\n"))
cat(sprintf("releases with an out-party attack: %s | with in-party support: %s\n",
  format(sum(!is.na(attack_scores$out_attack)), big.mark=","),
  format(sum(!is.na(attack_scores$in_support)), big.mark=",")))
cat("\nmean president stance by speaker party (in/out-party mirror):\n")
print(entity_stance[entity_id %in% c("obama","trump","biden"),
  .(mean = round(weighted.mean(mean_sentiment, n), 3), n = sum(n)), by = .(entity_id, sp_party)][order(entity_id, sp_party)])
cat("\nmean out-party attack score by speaker party (higher = more hostile):\n")
am <- merge(attack_scores[!is.na(out_attack), .(url, out_attack)],
            data.table::as.data.table(M[, .(sp_party = sp_party[1]), by = url]), by = "url")
print(am[, .(mean_attack = round(mean(out_attack), 4), n = .N), by = sp_party])
cat(sprintf("\nartifacts: attack.rds, duckdb attack_scores/entity_stance\n"))
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
