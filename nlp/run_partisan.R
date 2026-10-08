#!/usr/bin/env Rscript
# Slice 4: partisan language. Monroe, Colaresi & Quinn (2008) "Fightin' Words"
# weighted log-odds (z, informative Dirichlet prior) for Democrat-vs-Republican
# distinctive words, computed (a) OVERALL and (b) WITHIN each canonical issue --
# conditioning on the issue isolates framing/rhetoric from agenda (i.e. which
# party simply talks about a topic more). De-leaked by family_id (one doc per
# message family) so coordinated / cross-source boilerplate can't dominate. Uses
# the cached content DFM (stopwords + member/state/role identity terms already
# removed). z > 0 => Democratic-distinctive, z < 0 => Republican-distinctive.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
source("/Users/zaynesember/GitRepos/pressR/nlp/R/00_foundation.R")
suppressMessages({ library(quanteda); library(Matrix); library(dplyr); library(tidyr)
  library(tidylo); library(DBI); library(duckdb) })
t0 <- Sys.time(); OUT <- nlp_out_dir()
TOPN <- 40L; MIN_DOCS <- 200L; MIN_WORD <- 25L

## ---- load DFM + meta, de-leak by family ----------------------------------
message("== Loading DFM + meta, de-leaking by family ==")
dfm_all <- readRDS(file.path(OUT, "dfm_raw.rds"))            # ~856k x V, docnames=url
fam <- readRDS(file.path(OUT, "release_family.rds"))[, c("url", "family_id")]
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path(), read_only = TRUE)
md  <- dbGetQuery(con, "SELECT url, party FROM releases WHERE usable AND party IN ('D','R')")
il  <- dbGetQuery(con, "SELECT url, COALESCE(office_issues, predicted_issues) iss
                        FROM issue_labels WHERE COALESCE(office_issues, predicted_issues) IS NOT NULL")
dbDisconnect(con, shutdown = TRUE)

urls <- docnames(dfm_all)
meta <- data.frame(url = urls, drow = seq_along(urls), stringsAsFactors = FALSE) |>
  inner_join(md, by = "url") |> left_join(fam, by = "url")
meta <- meta[!is.na(meta$family_id), ]
meta <- meta[!duplicated(meta$family_id), ]                  # de-leak: one per family
message(sprintf("  de-leaked D/R docs: %s", format(nrow(meta), big.mark = ",")))

## ---- subset + trim DFM to a stable content vocabulary --------------------
D <- dfm_all[meta$drow, ]
D <- dfm_trim(D, min_termfreq = 100, termfreq_type = "count")
D <- dfm_trim(D, max_docfreq = 0.5, docfreq_type = "prop")
# Strip byline/markup artifacts that leak identity rather than rhetoric: party-
# state tags like "r-ga"/"d-ca" (which encode party directly -> pure leakage),
# template junk, and honorific/abbreviation tokens. (Deliberately NOT removing
# first names or state words -- they overlap real content: bill, mark, mexico,
# jersey.) Residual proper-noun noise is caveated in the dashboard.
D <- dfm_remove(D, pattern = "^[dri]-[a-z]{2}$", valuetype = "regex")
D <- dfm_remove(D, c("icon", "preview", "click", "u.s", "h.r", "s.res", "h.res",
                     "m.d", "ph.d", "p.m", "a.m"))
party <- meta$party
meta$lrow <- seq_len(nrow(meta))                             # row index into D
message(sprintf("  vocab after trim: %s features", format(nfeat(D), big.mark = ",")))

## ---- Monroe weighted log-odds (tidylo), D-vs-R per scope -----------------
score_scope <- function(rows) {
  rows <- unique(rows); pr <- party[rows]
  if (sum(pr == "D") < MIN_DOCS || sum(pr == "R") < MIN_DOCS) return(NULL)
  cd <- colSums(D[rows[pr == "D"], , drop = FALSE])
  cr <- colSums(D[rows[pr == "R"], , drop = FALSE])
  tibble(party = rep(c("D", "R"), each = length(cd)),
         word  = rep(names(cd), 2L),
         n     = c(as.numeric(cd), as.numeric(cr))) |>
    bind_log_odds(party, word, n) |>
    pivot_wider(names_from = party, values_from = c(n, log_odds_weighted)) |>
    transmute(word, z = log_odds_weighted_D, n_d = n_D, n_r = n_R) |>
    filter(n_d + n_r >= MIN_WORD)
}
top_terms <- function(z, scope) {
  if (is.null(z) || !nrow(z)) return(NULL)
  bind_rows(slice_max(z, z, n = TOPN) |> mutate(lean = "D"),
            slice_min(z, z, n = TOPN) |> mutate(lean = "R")) |>
    transmute(scope, lean, word, z = round(z, 2), n_d, n_r)
}

res <- list(); scopes <- list()
add_scope <- function(rows, scope) {
  rows <- unique(rows)
  tt <- top_terms(score_scope(rows), scope)
  if (!is.null(tt)) {
    res[[scope]] <<- tt
    pr <- party[rows]
    scopes[[scope]] <<- tibble(scope = scope, n_d_docs = sum(pr == "D"), n_r_docs = sum(pr == "R"))
  }
}
message("== Scoring overall + per-issue ==")
add_scope(meta$lrow, "(overall)")
ilj <- il |> inner_join(meta[, c("url", "lrow")], by = "url") |>
  separate_rows(iss, sep = ";") |> mutate(iss = trimws(iss)) |> filter(nzchar(iss))
for (isx in sort(unique(ilj$iss))) add_scope(ilj$lrow[ilj$iss == isx], isx)

partisan_terms  <- bind_rows(res)
partisan_scopes <- bind_rows(scopes)

## ---- persist --------------------------------------------------------------
saveRDS(list(terms = partisan_terms, scopes = partisan_scopes),
        file.path(OUT, "partisan.rds"))
con <- dbConnect(duckdb::duckdb(), nlp_duckdb_path())
dbWriteTable(con, "partisan_terms",  as.data.frame(partisan_terms),  overwrite = TRUE)
dbWriteTable(con, "partisan_scopes", as.data.frame(partisan_scopes), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)

cat(sprintf("\n================ PARTISAN LANGUAGE ================\n"))
cat(sprintf("scopes: %d (overall + %d issues) | term rows: %s\n",
  nrow(partisan_scopes), nrow(partisan_scopes) - 1L, format(nrow(partisan_terms), big.mark = ",")))
show_scope <- function(s) {
  cat(sprintf("\n[%s]\n", s))
  d <- partisan_terms |> filter(scope == s, lean == "D") |> slice_max(z, n = 12)
  r <- partisan_terms |> filter(scope == s, lean == "R") |> slice_min(z, n = 12)
  cat("  D:", paste(d$word, collapse = ", "), "\n")
  cat("  R:", paste(r$word, collapse = ", "), "\n")
}
show_scope("(overall)")
for (s in intersect(c("Health Care", "Immigration & Border", "Environment & Climate"),
                    partisan_scopes$scope)) show_scope(s)
cat(sprintf("\nartifacts: partisan.rds, duckdb tables partisan_terms/partisan_scopes\n"))
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
