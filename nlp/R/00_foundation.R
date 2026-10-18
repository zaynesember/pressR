# NLP foundation utilities for the pressR corpus.
# -----------------------------------------------
# Kept OUTSIDE the package R/ (the package is scraper-only; this is a separate
# analysis code path). Source this, then 01_build_duckdb.R and 02_near_dup.R.
#
# Design rule: NEVER load the whole archive at once. read_archive() does
# bind_rows over every year file's bodies (~GBs). Stream one year partition at a
# time via nlp_stream_years(), drop bodies after use.

suppressMessages({
  library(dplyr)
  library(stringi)
})

# Resolve archive + output locations via the package (load_all'd by the caller).
nlp_out_dir <- function() {
  d <- file.path(dirname(tools::R_user_dir("pressR", "data")), "pressR_nlp")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}
nlp_duckdb_path <- function() file.path(nlp_out_dir(), "press.duckdb")

# Year-partition paths, oldest->newest (wraps the package primitives, which are
# internal -> use :::, valid under devtools::load_all).
nlp_year_files <- function() pressR:::archive_files(pressR:::archive_dir())

# Folded external datasets (Stout, Wang & Tucker) live in their own dir as
# year-partitioned RDS with the archive schema + a `source` column, so they never
# touch the scraped archive. Built by nlp/run_external.R.
nlp_external_dir <- function(create = FALSE) {
  d <- file.path(nlp_out_dir(), "external")
  if (create) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}
nlp_external_files <- function() {
  d <- nlp_external_dir()
  if (!dir.exists(d)) return(character(0))
  # recursive so subdirs (e.g. external/wayback/) are streamed too; each source
  # tags its own `source` column, so the duckdb build keeps provenance straight.
  sort(list.files(d, pattern = "releases-[0-9]{4}\\.rds$", full.names = TRUE, recursive = TRUE))
}

# Stream over year partitions: read one releases-YYYY.rds, apply `fun(df, year)`,
# collect results. `fun` should return a SMALL object (never the bodies) so peak
# RAM stays at one year's data. Returns a list keyed by year. With
# include_external=TRUE (default) the folded external year files are streamed too;
# every df is guaranteed a `source` column ("scraped" for the archive partitions).
nlp_stream_years <- function(fun, quiet = FALSE, include_external = TRUE) {
  files <- nlp_year_files()
  if (include_external) files <- c(files, nlp_external_files())
  out <- vector("list", length(files))
  for (i in seq_along(files)) {
    yr <- sub("^.*releases-([0-9]{4})\\.rds$", "\\1", files[i])
    if (!quiet) message(sprintf("  [%s] %s", yr, basename(files[i])))
    df <- readRDS(files[i])
    if (is.null(df$source)) df$source <- "scraped"
    out[i] <- list(fun(df, yr))   # single-bracket+list keeps NULL returns (side-effect funs)
    rm(df); gc(FALSE)
  }
  names(out) <- sub("^.*releases-([0-9]{4})\\.rds$", "\\1", files)
  out
}

# Aggressive normalization for dedup/shingling: lowercase, strip URLs, drop all
# non-alphanumeric (keep single spaces), collapse whitespace. Two releases that
# differ only in punctuation/whitespace/case hash identically.
nlp_normalize_body <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stri_replace_all_regex(x, "https?://\\S+|www\\.\\S+", " ")
  x <- stri_trans_tolower(x)
  x <- stri_replace_all_regex(x, "[^a-z0-9]+", " ")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

# Light display cleanup (keep case/punct; strip residual URLs + nav phrases).
NAV_PHRASES <- c("read more", "click here", "share this", "print this page",
                 "printer-friendly", "press release", "f t", "facebook twitter")
nlp_clean_display <- function(x) {
  x <- ifelse(is.na(x), NA_character_, x)
  x <- stri_replace_all_regex(x, "https?://\\S+", "")
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

# Spanish-language heuristic: share of tokens that are common Spanish function
# words. >0.06 flags the rare (~0.3%) Spanish releases.
.es_words <- c("de","la","el","los","las","una","para","con","por","que","del",
               "en","su","sus","como","mas","pero","este","esta","nuestro",
               "nuestra","sobre","tambien","ser","fue","han","hoy")
nlp_is_spanish <- function(x) {
  toks <- stri_split_regex(stri_trans_tolower(ifelse(is.na(x), "", x)), "\\s+")
  vapply(toks, function(w) {
    if (length(w) < 10) return(FALSE)
    mean(w %in% .es_words) > 0.06
  }, logical(1))
}

# Per-release derived columns + quality flags (operates on one year's df).
nlp_add_flags <- function(df) {
  body <- df$body
  nchar_body <- ifelse(is.na(body), 0L, nchar(body))
  ntok <- ifelse(is.na(body), 0L, stri_count_regex(body, "\\S+"))
  df$body_nchar <- nchar_body
  df$body_ntokens <- ntok
  df$has_body <- !is.na(body) & nchar_body > 0
  df$is_stub <- df$has_body & (nchar_body < 50 | ntok < 30)
  df$has_url <- !is.na(body) & stri_detect_regex(body, "https?://")
  df$has_nav <- !is.na(body) &
    stri_detect_regex(stri_trans_tolower(body),
                      "read more|click here|share this|print this page")
  df$is_spanish <- nlp_is_spanish(body)
  # usable_for_nlp: has real prose, not a stub, not spanish
  df$usable <- df$has_body & !df$is_stub & !df$is_spanish
  df
}

# Build a congressional identity stoplist so similarity/keyness/topics reflect
# message content, not letterhead identity: member surnames (from `name`,
# "Last, First"), role words, state names, and a few corpus boilerplate terms.
nlp_identity_stoplist <- function(member_names) {
  surnames <- stri_trans_tolower(stri_trim_both(sub(",.*$", "", member_names)))
  surnames <- unique(surnames[nchar(surnames) >= 3])
  states <- stri_trans_tolower(c(state.name, "district of columbia", "puerto rico",
    "guam", "american samoa", "virgin islands", "northern mariana islands"))
  roles <- c("congressman", "congresswoman", "congressmember", "senator",
             "representative", "rep", "sen", "hon", "honorable", "press",
             "release", "statement", "washington", "dc", "office", "today",
             "announced", "issued", "following")
  unique(c(surnames, states, roles))
}
