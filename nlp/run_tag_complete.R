#!/usr/bin/env Rscript
# Slice 2: multi-label issue-tag completion.
# Train per-issue binary classifiers on the office-tagged releases (labels via the
# canonicalization crosswalk), evaluate with GROUP-AWARE splits (by family_id, so
# coordinated/reused text can't leak), then predict issues for the whole corpus.
# Honest caveats baked into the eval: metrics reported split by chamber AND by CMS,
# because tag availability is MNAR (generic/guid offices are 0% tagged).
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
H <- "/Users/zaynesember/GitRepos/pressR/nlp/R"
source(file.path(H, "00_foundation.R"))
suppressMessages({
  library(quanteda); library(glmnet); library(Matrix); library(dplyr)
  library(stringi); library(parallel)
})
quanteda_options(threads = max(1L, parallel::detectCores() - 2L))
t0 <- Sys.time()
OUT <- nlp_out_dir()

## ---- crosswalk + canonicalization ----------------------------------------
cw <- read.csv("/Users/zaynesember/GitRepos/pressR/nlp/crosswalks/issue_crosswalk.csv",
               stringsAsFactors = FALSE)
cw$raw_norm <- stri_trans_tolower(stri_trim_both(cw$raw_tag))
cw_map <- setNames(cw$canonical, cw$raw_norm)
ALL_ISSUES <- sort(unique(unlist(strsplit(cw$canonical[cw$canonical != "DROP"], ";"))))

# raw ';'-joined tag string -> character vector of canonical issues (may be empty)
canonicalize <- function(tagstr) {
  if (is.na(tagstr) || !nzchar(tagstr)) return(character(0))
  raw <- stri_trans_tolower(stri_trim_both(strsplit(tagstr, ";", fixed = TRUE)[[1]]))
  can <- cw_map[raw]; can <- can[!is.na(can) & can != "DROP"]
  unique(unlist(strsplit(can, ";", fixed = TRUE)))
}

## ---- 1. Build the usable-doc DFM (per-year, rbind sparse) -----------------
message("== Building DFM over usable releases (per-year) ==")
fam <- readRDS(file.path(OUT, "release_family.rds"))[, c("url","family_id")]
stoplist <- c(stopwords::stopwords("en"), readRDS(file.path(OUT, "identity_stoplist.rds")))

build_year_dfm <- function(df, yr) {
  df <- nlp_add_flags(df); df <- df[df$usable, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  toks <- df$body |>
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
           remove_url = TRUE) |>
    tokens_tolower() |>
    tokens_remove(stoplist) |>
    tokens_select(min_nchar = 3L)
  d <- dfm(toks)
  docnames(d) <- df$url          # unique across corpus -> rbind won't collide
  docvars(d, "url") <- df$url
  d
}
raw_cache <- file.path(OUT, "dfm_raw.rds")
if (file.exists(raw_cache)) {
  message("  loading cached raw DFM (skip tokenization)")
  dfm_all <- readRDS(raw_cache)
} else {
  year_dfms <- nlp_stream_years(build_year_dfm)
  year_dfms <- year_dfms[!vapply(year_dfms, is.null, logical(1))]
  dfm_all <- do.call(rbind, year_dfms); rm(year_dfms); gc(FALSE)
  saveRDS(dfm_all, raw_cache)
}
dfm_all <- dfm_trim(dfm_all, min_docfreq = 20, docfreq_type = "count")     # count floor
dfm_all <- dfm_trim(dfm_all, max_docfreq = 0.5, docfreq_type = "prop")     # drop ubiquitous
message(sprintf("  DFM: %s docs x %s features", format(ndoc(dfm_all), big.mark=","),
                format(nfeat(dfm_all), big.mark=",")))

## ---- 2. Assemble meta + labels, aligned to DFM rows ----------------------
meta <- docvars(dfm_all) |> select(url)
# pull per-doc metadata + raw tags from duckdb (cheap, no bodies)
suppressMessages({ library(DBI); library(duckdb) })
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path(), read_only = TRUE)
md <- dbGetQuery(con, "SELECT url, party, chamber, cms, tags, year FROM releases")
dbDisconnect(con, shutdown = TRUE)
meta <- meta |> left_join(md, by = "url") |> left_join(fam, by = "url")
stopifnot(nrow(meta) == ndoc(dfm_all))
meta$canon <- lapply(meta$tags, canonicalize)
meta$tagged <- lengths(meta$canon) > 0
message(sprintf("  usable docs: %s | with >=1 canonical label (train pool): %s",
                format(nrow(meta), big.mark=","), format(sum(meta$tagged), big.mark=",")))

## ---- 3. Group-aware split by family_id -----------------------------------
set.seed(42)
fams_u <- unique(meta$family_id)
grp <- sample(c("train","test"), length(fams_u), replace = TRUE, prob = c(0.8, 0.2))
split_of <- setNames(grp, as.character(fams_u))
meta$split <- split_of[as.character(meta$family_id)]
# validation = 15% of train families (for lambda + threshold)
tr_fams <- fams_u[grp == "train"]
val_fams <- sample(tr_fams, length(tr_fams) * 0.15)
meta$split[meta$family_id %in% val_fams] <- "val"

tagged <- which(meta$tagged)
idx_tr  <- intersect(tagged, which(meta$split == "train"))
idx_val <- intersect(tagged, which(meta$split == "val"))
idx_te  <- intersect(tagged, which(meta$split == "test"))
message(sprintf("  train/val/test tagged docs: %s / %s / %s",
                format(length(idx_tr),big.mark=","), format(length(idx_val),big.mark=","),
                format(length(idx_te),big.mark=",")))

## ---- 4. tf-idf with idf fixed from TRAIN ---------------------------------
idf <- log(length(idx_tr) / (1 + colSums(dfm_all[idx_tr, ] > 0)))
apply_tfidf <- function(d) {
  x <- as(d, "CsparseMatrix")
  x@x <- 1 + log(x@x)                       # sublinear tf
  x %*% Diagonal(x = idf)
}
X <- apply_tfidf(dfm_all)                    # full matrix; subset by index below
colnames(X) <- colnames(dfm_all)

## ---- 5. Per-label glmnet (lambda+threshold via val), eval on test --------
label_mat <- function(idx) {
  m <- matrix(0L, nrow = length(idx), ncol = length(ALL_ISSUES),
              dimnames = list(NULL, ALL_ISSUES))
  cl <- meta$canon[idx]
  for (i in seq_along(cl)) if (length(cl[[i]])) m[i, cl[[i]]] <- 1L
  m
}
Ytr <- label_mat(idx_tr); Yval <- label_mat(idx_val); Yte <- label_mat(idx_te)
Xtr <- X[idx_tr, ]; Xval <- X[idx_val, ]; Xte <- X[idx_te, ]
keep_lab <- ALL_ISSUES[colSums(Ytr) >= 50]   # need enough positives
message(sprintf("== Training %d issue classifiers (glmnet) ==", length(keep_lab)))

f1_at <- function(p, y, thr) { yp <- p >= thr
  tp <- sum(yp & y==1); fp <- sum(yp & y==0); fn <- sum(!yp & y==1)
  if (tp==0) return(0); pr <- tp/(tp+fp); rc <- tp/(tp+fn); 2*pr*rc/(pr+rc) }

fit_one <- function(lab) {
  y <- Ytr[, lab]
  fit <- glmnet(Xtr, y, family = "binomial", alpha = 0, standardize = FALSE)
  pv <- predict(fit, Xval, type = "response")           # val preds across lambda path
  yv <- Yval[, lab]
  # pick lambda maximizing best-threshold F1 on val
  thrs <- seq(0.1, 0.9, by = 0.05)
  best <- which.max(apply(pv, 2, function(col) max(vapply(thrs, function(t) f1_at(col, yv, t), 0))))
  pcol <- pv[, best]
  thr  <- thrs[which.max(vapply(thrs, function(t) f1_at(pcol, yv, t), 0))]
  list(lab = lab, beta = fit$beta[, best], a0 = fit$a0[best], lambda = fit$lambda[best], thr = thr)
}
models <- mclapply(keep_lab, fit_one, mc.cores = 6L)
names(models) <- keep_lab

# Test eval (overall + by chamber + by CMS)
predict_lab <- function(m, Xm) as.numeric(Xm %*% m$beta + m$a0) |> (\(z) 1/(1+exp(-z)))()
te_party <- meta$party[idx_te]; te_ch <- meta$chamber[idx_te]; te_cms <- meta$cms[idx_te]
ev <- lapply(models, function(m) {
  p <- predict_lab(m, Xte); y <- Yte[, m$lab]
  data.frame(label=m$lab, pos=sum(y), thr=m$thr, f1=f1_at(p, y, m$thr),
             f1_house=f1_at(p[te_ch=="house"], y[te_ch=="house"], m$thr),
             f1_senate=f1_at(p[te_ch=="senate"], y[te_ch=="senate"], m$thr))
})
ev <- bind_rows(ev) |> arrange(desc(pos))
macroF1 <- mean(ev$f1)
microF1 <- { p <- do.call(cbind, lapply(models, predict_lab, Xm=Xte))
  yp <- sweep(p, 2, vapply(models,`[[`,0,"thr"), ">="); ym <- Yte[, keep_lab]
  tp<-sum(yp&ym==1); fp<-sum(yp&ym==0); fn<-sum(!yp&ym==1)
  pr<-tp/(tp+fp); rc<-tp/(tp+fn); 2*pr*rc/(pr+rc) }
message(sprintf("== TEST macro-F1=%.3f  micro-F1=%.3f (group-held-out) over %d labels ==", macroF1, microF1, nrow(ev)))
print(ev, row.names = FALSE)
# MNAR caveat: the eval set is office-tagged docs, whose CMS mix differs from the
# untagged target (generic/guid offices are 0% tagged -> predictions there are
# extrapolation, NOT measured by this test F1).
cat("\nTest-set CMS mix (where eval F1 is actually measured):\n"); print(sort(table(te_cms), decreasing=TRUE))
cat("Untagged-target CMS mix (where we predict but CANNOT validate):\n")
print(sort(table(meta$cms[!meta$tagged]), decreasing=TRUE))

## ---- 6. Predict issues for ALL usable docs -> sidecar --------------------
message("== Predicting issues for all usable releases ==")
P <- sapply(models, function(m) predict_lab(m, X))       # ndoc x nlabel probs
pred_bin <- sweep(P, 2, vapply(models, `[[`, 0, "thr"), ">=")
pred_issues <- vapply(seq_len(nrow(P)), function(i) {
  labs <- keep_lab[pred_bin[i, ]]; if (length(labs)) paste(labs, collapse=";") else NA_character_
}, character(1))
top_i <- max.col(P, ties.method = "first")
sidecar <- tibble(
  url = meta$url,
  office_issues = vapply(meta$canon, function(z) if(length(z)) paste(z, collapse=";") else NA_character_, ""),
  predicted_issues = pred_issues,
  top_issue = keep_lab[top_i],
  top_prob = round(P[cbind(seq_len(nrow(P)), top_i)], 3),
  n_pred = rowSums(pred_bin),
  label_source = ifelse(meta$tagged, "office", "predicted")
)
saveRDS(list(models=models, idf=idf, vocab=colnames(X), eval=ev, macroF1=macroF1,
             keep_lab=keep_lab), file.path(OUT, "tagmodel.rds"))
saveRDS(sidecar, file.path(OUT, "issue_labels.rds"))
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "issue_labels", sidecar, overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)

cat(sprintf("\n================ TAG COMPLETION ================\n"))
cat(sprintf("issues modeled: %d | TEST macro-F1: %.3f\n", length(keep_lab), macroF1))
cat(sprintf("office-tagged (kept): %s | predicted-labeled: %s | now have >=1 issue: %s of %s usable (%.1f%%)\n",
  format(sum(sidecar$label_source=="office"),big.mark=","),
  format(sum(sidecar$label_source=="predicted" & !is.na(sidecar$predicted_issues)),big.mark=","),
  format(sum(!is.na(sidecar$predicted_issues) | !is.na(sidecar$office_issues)),big.mark=","),
  format(nrow(sidecar),big.mark=","),
  100*mean(!is.na(sidecar$predicted_issues) | !is.na(sidecar$office_issues))))
cat("\nartifacts: tagmodel.rds, issue_labels.rds, duckdb table issue_labels\n")
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units="mins"))))
