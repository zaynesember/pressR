#!/usr/bin/env Rscript
# Re-run coordinated-messaging / near-dup family detection over the COMBINED
# corpus (scraped + folded external). Reuses the existing DuckDB releases table;
# overwrites families + release_family (now carrying cross_source, so mechanical
# cross-collection duplicates are distinguishable from genuine coordination).
# Heavy: MinHash/LSH over ~850k usable bodies.
suppressMessages(devtools::load_all("/Users/zaynesember/GitRepos/pressR"))
H <- "/Users/zaynesember/GitRepos/pressR/nlp/R"
source(file.path(H, "00_foundation.R"))
source(file.path(H, "02_near_dup.R"))
suppressMessages({ library(DBI); library(duckdb); library(dplyr) })
t0 <- Sys.time()
mode <- (commandArgs(trailingOnly = TRUE)[1] %||% "full")
near <- !identical(mode, "exact")

message("== Building families over the combined corpus ==")
fam <- nlp_build_families(near_dup = near, jaccard = 0.7)
saveRDS(fam$families,    file.path(nlp_out_dir(), "families.rds"))
saveRDS(fam$per_release, file.path(nlp_out_dir(), "release_family.rds"))

con <- dbConnect(duckdb::duckdb(), dbdir = nlp_duckdb_path())
dbWriteTable(con, "families", fam$families, overwrite = TRUE)
dbWriteTable(con, "release_family",
  fam$per_release %>% select(url, family_id, n_docs, is_reused,
    cross_member, cross_party, cross_chamber, cross_source), overwrite = TRUE)

per <- fam$per_release; fams <- fam$families
exact_ok <- per %>% group_by(ut_id) %>% summarise(nf = n_distinct(family_id)) %>%
  summarise(bad = sum(nf > 1)) %>% pull(bad)
cat("\n================ FAMILIES (combined) ================\n")
cat(sprintf("usable: %s | reused: %s (%.1f%%)\n", format(nrow(per), big.mark=","),
    format(sum(per$is_reused), big.mark=","), 100*mean(per$is_reused)))
cat(sprintf("families: %s | reused: %s\n",
    format(nrow(fams), big.mark=","), format(sum(fams$is_reused), big.mark=",")))
cat(sprintf("  cross_member %s | cross_party %s | cross_chamber %s | cross_source %s\n",
  format(sum(fams$cross_member), big.mark=","), format(sum(fams$cross_party), big.mark=","),
  format(sum(fams$cross_chamber), big.mark=","), format(sum(fams$cross_source), big.mark=",")))
cat(sprintf("validation: exact-dup split across families (must be 0): %d\n", exact_ok))
genuine <- fams %>% filter(is_reused, cross_member)
cat(sprintf("reused multi-member families: %s | of which purely cross_source (mechanical): %s\n",
  format(nrow(genuine), big.mark=","), format(sum(genuine$cross_source & genuine$n_sources>1), big.mark=",")))
dbDisconnect(con, shutdown = TRUE)
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units="mins"))))
