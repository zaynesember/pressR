#!/usr/bin/env Rscript
# Slice 3: Structural Topic Model (unsupervised topic discovery + party/time
# effects). The EXACT full-corpus topic-trend view comes from the slice-2 issue
# labels (issue_trends.rds, all 436k); STM is the complementary *discovered*-topic
# layer. Per the scaling reality (full-corpus STM = overnight, single-threaded),
# fit on a family-deduped STRATIFIED SAMPLE -- topics are stable well before full N.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({
  library(quanteda); library(stm); library(dplyr); library(Matrix)
  library(DBI); library(duckdb)
})
t0 <- Sys.time(); OUT <- nlp_out_dir(); set.seed(42)
FIT_N    <- 60000L      # docs to fit STM on (stratified by year, family-deduped)
SEARCH_N <- 15000L      # subsample for searchK
KS       <- c(40L, 55L, 70L)

## ---- load cached raw DFM + meta, dedup by family, sample -----------------
message("== Loading DFM + meta, dedup by family ==")
dfm_all <- readRDS(file.path(OUT, "dfm_raw.rds"))           # 403k x ~52k, docnames=url
fam <- readRDS(file.path(OUT, "release_family.rds"))[, c("url","family_id")]
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path(), read_only = TRUE)
md  <- dbGetQuery(con, "SELECT url, party, chamber, year FROM releases WHERE usable")
dbDisconnect(con, shutdown = TRUE)

urls <- docnames(dfm_all)
meta <- data.frame(url = urls, stringsAsFactors = FALSE) |>
  left_join(fam, by = "url") |> left_join(md, by = "url")
keep <- !is.na(meta$party) & meta$party %in% c("D","R") & !is.na(meta$year)
# dedup: one row per family (collapse coordinated/boilerplate text)
keep <- keep & !duplicated(meta$family_id)
idx_keep <- which(keep)
message(sprintf("  family-deduped D/R docs: %s", format(length(idx_keep), big.mark=",")))

# stratified sample by year
md2 <- meta[idx_keep, ]
samp <- md2 |> mutate(row = idx_keep) |> group_by(year) |>
  slice_sample(n = ceiling(FIT_N / length(unique(md2$year)))) |> ungroup()
samp <- samp[sample(nrow(samp), min(FIT_N, nrow(samp))), ]
message(sprintf("  STM fit sample: %s docs (%d–%d)", format(nrow(samp), big.mark=","),
                min(samp$year), max(samp$year)))

## ---- DFM -> stm input -----------------------------------------------------
d <- dfm_all[samp$row, ]
d <- dfm_trim(d, min_docfreq = 20, docfreq_type = "count")
d <- dfm_trim(d, max_docfreq = 0.4, docfreq_type = "prop")
message(sprintf("  STM vocab: %s features", format(nfeat(d), big.mark=",")))
stm_in <- convert(d, to = "stm",
                  docvars = data.frame(party = samp$party, chamber = samp$chamber,
                                       year = samp$year, url = samp$url))
docs <- stm_in$documents; vocab <- stm_in$vocab; smeta <- stm_in$meta

## ---- searchK on a subsample ----------------------------------------------
message(sprintf("== searchK over K=%s on %s docs ==", paste(KS, collapse=","), format(SEARCH_N, big.mark=",")))
ss <- sample(length(docs), min(SEARCH_N, length(docs)))
sk <- searchK(docs[ss], vocab, K = KS,
              prevalence = ~ party + chamber + s(year), data = smeta[ss, ],
              init.type = "Spectral", cores = 1L, verbose = FALSE)
res <- sk$results
res$score <- as.numeric(res$semcoh) + 8 * as.numeric(res$exclus)  # coherence + exclusivity
K <- as.integer(res$K[[which.max(res$score)]])
saveRDS(sk, file.path(OUT, "stm_searchk.rds"))
message(sprintf("  chosen K=%d (semcoh %.1f, exclus %.2f)", K,
                res$semcoh[[which.max(res$score)]], res$exclus[[which.max(res$score)]]))

## ---- fit final model ------------------------------------------------------
message(sprintf("== Fitting STM K=%d on %s docs (Spectral) ==", K, format(length(docs), big.mark=",")))
fit <- stm(docs, vocab, K = K, prevalence = ~ party + chamber + s(year),
           data = smeta, init.type = "Spectral", max.em.its = 75, verbose = FALSE)

## ---- topic dictionary + party/time effects --------------------------------
lt <- labelTopics(fit, n = 10)
topic_dict <- data.frame(
  topic = seq_len(K),
  frex = apply(lt$frex, 1, paste, collapse = ", "),
  prob = apply(lt$prob, 1, paste, collapse = ", "),
  prevalence = round(colMeans(fit$theta), 4)
)
# exemplar urls per topic
ex <- tryCatch(findThoughts(fit, texts = smeta$url, n = 3)$index, error = function(e) NULL)
if (!is.null(ex)) topic_dict$examples <- vapply(seq_len(K),
  function(k) paste(smeta$url[ex[[k]]], collapse = " | "), character(1))

eff <- estimateEffect(1:K ~ party + s(year), fit, metadata = smeta, uncertainty = "Global")
# tidy topic x year x party prevalence straight from the per-doc topic props (theta)
theta <- as.data.frame(fit$theta); names(theta) <- paste0("T", seq_len(K))
theta$year <- smeta$year; theta$party <- smeta$party
typ <- theta |> group_by(year, party) |>
  summarise(across(starts_with("T"), mean), .groups = "drop") |>
  tidyr::pivot_longer(starts_with("T"), names_to = "topic", values_to = "mean_prev") |>
  mutate(topic = as.integer(sub("T","",topic)))

saveRDS(list(fit = fit, K = K, topic_dict = topic_dict, eff = eff,
             meta = smeta), file.path(OUT, "stm_model.rds"))
saveRDS(topic_dict, file.path(OUT, "topic_dictionary.rds"))
saveRDS(typ, file.path(OUT, "topic_trends.rds"))
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "topic_dictionary", topic_dict, overwrite = TRUE)
dbWriteTable(con, "topic_trends", as.data.frame(typ), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)

cat(sprintf("\n================ TOPIC MODEL (STM) ================\n"))
cat(sprintf("K=%d topics, fit on %s family-deduped docs\n", K, format(length(docs), big.mark=",")))
cat("\nTop 15 topics by prevalence (FREX terms):\n")
td <- topic_dict[order(-topic_dict$prevalence), ][1:15, ]
for (i in seq_len(nrow(td))) cat(sprintf("  T%-2d (%.3f): %s\n", td$topic[i], td$prevalence[i], substr(td$frex[i], 1, 80)))
cat(sprintf("\nartifacts: stm_model.rds, topic_dictionary.rds, topic_trends.rds, duckdb tables\n"))
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units="mins"))))
