# Coordinated-messaging / near-duplicate family detection.
# ------------------------------------------------------
# Produces family_id: groups of releases that share the SAME or NEARLY-SAME body
# (joint statements, delegation announcements, reused party talking points).
# This is the de-leak primitive every supervised/topic direction consumes, AND a
# substantive research signal (which talking points spread across members/parties).
#
# Method: (1) exact-dup via unique normalized body (instant, robust floor);
# (2) near-dup via MinHash + banded LSH over UNIQUE bodies, length-windowed so
# each textreuse pass stays small; (3) union edges -> connected components (igraph)
# -> family_id. Exact families are guaranteed a SUBSET of near-dup families.

suppressMessages({ library(dplyr); library(stringi); library(igraph) })

# Load the minimal columns needed for dedup (streamed, usable docs only).
nlp_load_for_dedup <- function() {
  parts <- nlp_stream_years(function(df, yr) {
    df <- nlp_add_flags(df)
    df <- df[df$usable, , drop = FALSE]
    if (is.null(df$source)) df$source <- "scraped"
    tibble(url = df$url, name = df$name, party = df$party, chamber = df$chamber,
           source = df$source, date = as.Date(df$date), ntok = df$body_ntokens,
           nbody = nlp_normalize_body(df$body))
  })
  bind_rows(parts)
}

# Near-dup edges among UNIQUE texts, found with length-windowed MinHash+LSH.
# Returns a 2-col integer matrix of edges (indices into `utexts`).
nlp_near_dup_edges <- function(utexts, utok,
                               window = 20000L, overlap = 4000L,
                               n_hash = 120L, bands = 30L, jaccard = 0.7,
                               quiet = FALSE) {
  stopifnot(requireNamespace("textreuse", quietly = TRUE))
  mh <- textreuse::minhash_generator(n = n_hash, seed = 1L)
  ord <- order(utok)                       # sort by length: near-dups cluster
  n <- length(utexts)
  step <- max(1L, window - overlap)
  starts <- seq(1L, n, by = step)
  edge_list <- list()
  for (s in starts) {
    idx <- ord[s:min(s + window - 1L, n)]
    txt <- utexts[idx]
    keep <- stri_count_regex(txt, "\\S+") >= 5L   # need >=5 tokens for 5-grams
    idx <- idx[keep]; txt <- txt[keep]
    if (length(txt) < 2L) next
    if (!quiet) message(sprintf("  LSH window %d-%d (%d docs)", s, min(s+window-1L,n), length(txt)))
    corpus <- suppressWarnings(textreuse::TextReuseCorpus(
      text = setNames(txt, as.character(idx)),
      tokenizer = textreuse::tokenize_ngrams, n = 5,
      minhash_func = mh, keep_tokens = TRUE, progress = FALSE, skip_short = TRUE))
    if (length(corpus) < 2L) next
    buckets <- textreuse::lsh(corpus, bands = bands, progress = FALSE)
    cand <- textreuse::lsh_candidates(buckets)
    if (nrow(cand) == 0L) next
    cmp <- textreuse::lsh_compare(cand, corpus, textreuse::jaccard_similarity, progress = FALSE)
    cmp <- cmp[!is.na(cmp$score) & cmp$score >= jaccard, , drop = FALSE]
    if (nrow(cmp) == 0L) next
    edge_list[[length(edge_list) + 1L]] <-
      cbind(as.integer(cmp$a), as.integer(cmp$b))
    rm(corpus, buckets, cand, cmp); gc(FALSE)
  }
  if (length(edge_list) == 0L) return(matrix(integer(0), ncol = 2))
  unique(do.call(rbind, edge_list))
}

# Full pipeline: returns list(per_release tibble, families tibble).
nlp_build_families <- function(near_dup = TRUE, jaccard = 0.7, quiet = FALSE) {
  if (!quiet) message("Loading usable releases (streamed)...")
  d <- nlp_load_for_dedup()
  if (!quiet) message(sprintf("  %s usable releases", format(nrow(d), big.mark=",")))

  # Unique texts: nodes of the family graph. Exact dups collapse to one node.
  ut <- unique(d$nbody)
  d$ut_id <- match(d$nbody, ut)
  utok <- d$ntok[match(seq_along(ut), d$ut_id)]   # a token count per unique text

  edges <- matrix(integer(0), ncol = 2)
  if (near_dup) {
    if (!quiet) message(sprintf("Near-dup MinHash/LSH over %s unique texts...", format(length(ut), big.mark=",")))
    edges <- nlp_near_dup_edges(ut, utok, jaccard = jaccard, quiet = quiet)
    if (!quiet) message(sprintf("  %s near-dup edges (jaccard>=%.2f)", format(nrow(edges), big.mark=","), jaccard))
  }

  # Components over unique-text nodes (exact dups already share a node).
  g <- make_empty_graph(n = length(ut), directed = FALSE)
  if (nrow(edges) > 0) g <- add_edges(g, t(edges))
  ut_family <- components(g)$membership
  d$family_id <- ut_family[d$ut_id]

  # Per-family annotations.
  fam <- d %>%
    group_by(family_id) %>%
    summarise(
      n_docs = n(),
      n_members = n_distinct(name),
      n_parties = n_distinct(party[!is.na(party)]),
      n_chambers = n_distinct(chamber),
      n_sources = n_distinct(source),
      parties = paste(sort(unique(party[!is.na(party)])), collapse="/"),
      sources = paste(sort(unique(source)), collapse="/"),
      date_min = min(date, na.rm=TRUE), date_max = max(date, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      is_reused = n_docs > 1,
      cross_member = n_members > 1,
      cross_party = n_parties > 1,
      cross_chamber = n_chambers > 1,
      cross_source = n_sources > 1,   # same text in >1 collection = mechanical dup,
                                      # NOT coordinated messaging -> filterable
      span_days = as.integer(date_max - date_min)
    )

  per <- d %>%
    select(url, name, party, chamber, source, date, family_id, ut_id) %>%
    left_join(fam %>% select(family_id, n_docs, n_members, is_reused,
                             cross_member, cross_party, cross_chamber, cross_source),
              by = "family_id")

  list(per_release = per, families = fam)
}
