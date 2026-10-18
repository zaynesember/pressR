#!/usr/bin/env Rscript
# Precompute every aggregate the Shiny dashboard needs into ONE small RDS, so the
# app loads instantly and does ZERO heavy work at runtime (precompute-then-serve).
# The big `releases` table stays in DuckDB; the app queries it live only for the
# Explore tab + family/issue drill-downs.
suppressMessages({ library(DBI); library(duckdb); library(dplyr); library(tidyr) })

NLP <- file.path(dirname(tools::R_user_dir("pressR", "data")), "pressR_nlp")
con <- dbConnect(duckdb::duckdb(), file.path(NLP, "press.duckdb"), read_only = TRUE)

q <- function(sql) dbGetQuery(con, sql)
splitfn <- function(col) sprintf("unnest(string_split(%s, ';'))", col)
has_table <- function(t) t %in% dbListTables(con)

# ---- overview headline stats ----
ov <- q("SELECT
  COUNT(*) n_total,
  SUM(usable::INT) n_usable,
  MIN(date) dmin, MAX(date) dmax,
  ROUND(100.0*AVG(has_body::INT),1) pct_body,
  ROUND(100.0*AVG((tags IS NOT NULL)::INT),1) pct_office_tagged,
  SUM((chamber='house')::INT) n_house, SUM((chamber='senate')::INT) n_senate
  FROM releases")
fam_ov <- q("SELECT COUNT(*) n_fam, SUM(is_reused::INT) n_reused_fam,
  SUM(cross_party::INT) n_xparty, SUM(cross_chamber::INT) n_xchamber FROM families")
reuse <- q("SELECT ROUND(100.0*AVG(is_reused::INT),1) pct_reused FROM release_family")
lab <- q("SELECT ROUND(100.0*AVG((COALESCE(office_issues,predicted_issues) IS NOT NULL)::INT),1) pct_labeled FROM issue_labels")

# ---- source provenance (scraped + folded external datasets) ----
sources <- q("SELECT source, COUNT(*) n, SUM(usable::INT) usable,
  MIN(date) dmin, MAX(date) dmax,
  ROUND(100.0*AVG(has_body::INT),1) pct_body,
  ROUND(100.0*AVG((tags IS NOT NULL)::INT),1) pct_tagged
  FROM releases GROUP BY source ORDER BY n DESC")

# ---- sentiment aggregates (only if the precomputed sentiment table is present) ----
sentiment_trends <- sentiment_by_issue <- sentiment_sources <- NULL
if (has_table("sentiment")) {
  sentiment_trends <- q("SELECT r.year, r.party, ROUND(AVG(s.sentiment),4) mean_sent,
      COUNT(*) n FROM sentiment s JOIN releases r USING(url)
      WHERE r.party IN ('D','R') AND r.usable AND r.year IS NOT NULL
      GROUP BY 1,2 ORDER BY 1,2")
  sentiment_by_issue <- q(sprintf("SELECT trim(iss) issue, r.party,
      ROUND(AVG(s.sentiment),4) mean_sent, COUNT(*) n
      FROM sentiment s JOIN releases r USING(url) JOIN issue_labels il USING(url),
      %s AS u(iss)
      WHERE r.party IN ('D','R') AND trim(iss) <> ''
      GROUP BY 1,2 HAVING COUNT(*) >= 100", splitfn("COALESCE(il.office_issues, il.predicted_issues)")))
  sentiment_sources <- q("SELECT r.source, ROUND(AVG(s.sentiment),4) mean_sent,
      ROUND(MEDIAN(s.sentiment),4) med_sent, COUNT(*) n
      FROM sentiment s JOIN releases r USING(url) GROUP BY 1 ORDER BY 4 DESC")
}

# ---- sentiment divergence by issue (D-R gap over time, baseline-adjusted) ----
sentiment_divergence <- NULL
if (has_table("sentiment")) {
  dv <- q("WITH dedup AS (
      SELECT r.url, r.party, r.year, s.sentiment, COALESCE(il.office_issues, il.predicted_issues) iss,
             ROW_NUMBER() OVER (PARTITION BY rf.family_id ORDER BY r.url) rn
      FROM releases r JOIN sentiment s USING(url) LEFT JOIN release_family rf USING(url)
           JOIN issue_labels il USING(url)
      WHERE r.party IN ('D','R') AND r.usable AND COALESCE(il.office_issues, il.predicted_issues) IS NOT NULL),
    long AS (SELECT party, year, sentiment, trim(u.iss) issue
             FROM dedup, unnest(string_split(iss, ';')) AS u(iss) WHERE rn = 1 AND trim(u.iss) <> '')
    SELECT issue, party, year, AVG(sentiment) ms, COUNT(*) n
    FROM long WHERE year BETWEEN 2010 AND 2025 GROUP BY 1,2,3")
  wdv <- dv |> tidyr::pivot_wider(names_from = party, values_from = c(ms, n)) |>
    filter(!is.na(ms_D), !is.na(ms_R), n_D >= 30, n_R >= 30) |> mutate(gap = ms_D - ms_R)
  base_yr <- wdv |> group_by(year) |>
    summarise(overall = round(weighted.mean(gap, n_D + n_R), 4), .groups = "drop")
  wdv <- wdv |> left_join(base_yr, by = "year") |> mutate(excess = gap - overall)
  div <- wdv |> group_by(issue) |> summarise(
      early = mean(excess[year %in% 2010:2013]), late = mean(excess[year %in% 2021:2024]),
      level = mean(gap[year %in% 2021:2024]),
      ne = sum(year %in% 2010:2013), nl = sum(year %in% 2021:2024), .groups = "drop") |>
    filter(ne >= 2, nl >= 2) |>
    transmute(issue, early = round(early, 4), late = round(late, 4),
              change = round(late - early, 4), level = round(level, 4)) |>
    arrange(desc(change))
  sentiment_divergence <- list(by_issue = div, baseline = base_yr)
}

# ---- issue trends (within-year share), office-only and office+predicted ----
issue_trend <- function(label_expr) {
  long <- q(sprintf("
    SELECT r.year, r.party, r.chamber, trim(iss) AS issue FROM releases r
    JOIN issue_labels il USING(url), %s AS u(iss)
    WHERE r.usable AND r.party IN ('D','R') AND %s IS NOT NULL AND trim(iss)<>''",
    splitfn(sprintf("COALESCE(%s)", label_expr)), label_expr))
  denom <- q(sprintf("
    SELECT r.year, r.party, r.chamber, COUNT(*) n_rel FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND %s IS NOT NULL GROUP BY 1,2,3", label_expr))
  long %>% count(year, party, chamber, issue, name = "n_issue") %>%
    left_join(denom, by = c("year","party","chamber")) %>%
    mutate(share = n_issue / n_rel)
}
issue_trends_all    <- issue_trend("COALESCE(il.office_issues, il.predicted_issues)")
issue_trends_office <- issue_trend("il.office_issues")
issues <- sort(unique(issue_trends_all$issue))

# ---- topic model tables ----
topic_dict   <- q("SELECT * FROM topic_dictionary ORDER BY prevalence DESC")
topic_trends <- q("SELECT * FROM topic_trends")

# ---- coordinated-messaging families (top reused multi-member) ----
fam_top <- q("
  SELECT f.family_id, f.n_docs, f.n_members, f.parties, f.sources,
         f.cross_party, f.cross_chamber, f.cross_source,
         f.date_min, f.date_max, f.span_days, ANY_VALUE(r.title) rep_title
  FROM families f JOIN release_family rf USING(family_id) JOIN releases r USING(url)
  WHERE f.is_reused AND f.n_members >= 2
  GROUP BY f.family_id, f.n_docs, f.n_members, f.parties, f.sources,
           f.cross_party, f.cross_chamber, f.cross_source,
           f.date_min, f.date_max, f.span_days
  ORDER BY f.n_members DESC, f.n_docs DESC LIMIT 1000")

# ---- filter vocab ----
members <- q("SELECT DISTINCT name, party, chamber, state FROM releases WHERE name IS NOT NULL ORDER BY name")
years   <- q("SELECT DISTINCT year FROM releases WHERE year IS NOT NULL ORDER BY year")$year

# ---- partisan language: Monroe weighted log-odds (only if present) ----
partisan_terms <- partisan_scopes <- NULL
if (has_table("partisan_terms")) {
  partisan_terms  <- q("SELECT * FROM partisan_terms")
  partisan_scopes <- q("SELECT * FROM partisan_scopes")
}

# ---- coordination network: members linked by shared message families ----
network <- NULL
if (has_table("network_nodes")) {
  nn   <- q("SELECT * FROM network_nodes")
  ne   <- q("SELECT * FROM network_edges")
  nts  <- q("SELECT * FROM network_ts")
  nsum <- q("SELECT * FROM network_summary")
  brokers <- nn[nn$party %in% c("D","R"), ]
  brokers <- brokers[order(-brokers$cross_party_strength), ][seq_len(min(25, nrow(brokers))), ]
  thr <- if (nrow(ne) > 600) sort(ne$weight, decreasing = TRUE)[600] else min(ne$weight)
  xpi <- if (has_table("network_xparty_issue")) q("SELECT * FROM network_xparty_issue ORDER BY xparty_rate DESC") else NULL
  xpc <- if (has_table("network_xparty_chamber")) q("SELECT * FROM network_xparty_chamber") else NULL
  network <- list(nodes = nn, edges = ne, ts = nts, summary = nsum, brokers = brokers,
                  xparty_issue = xpi, xparty_chamber = xpc,
                  w_min = min(ne$weight), w_max = max(ne$weight), w_default = round(thr, 3))
}

# ---- targeted tone / attack score (if present) ----
attack <- NULL
if (has_table("attack_scores")) {
  dd <- "WITH d AS (
      SELECT a.out_attack, r.name, r.party, r.year,
        ROW_NUMBER() OVER (PARTITION BY rf.family_id ORDER BY a.url) rn
      FROM attack_scores a JOIN releases r USING(url) LEFT JOIN release_family rf USING(url)
      WHERE r.party IN ('D','R') AND a.out_attack IS NOT NULL)
    SELECT * FROM d WHERE rn = 1"           # de-leak: one release per message family
  abyp <- q(sprintf("SELECT party, year, ROUND(AVG(out_attack),4) mean_attack, COUNT(*) n
    FROM (%s) WHERE year BETWEEN 2005 AND 2026 GROUP BY 1,2 ORDER BY 1,2", dd))
  abrokers <- q(sprintf("SELECT name, party, ROUND(AVG(out_attack),4) mean_attack, COUNT(*) n
    FROM (%s) GROUP BY 1,2 HAVING COUNT(*) >= 40 ORDER BY mean_attack DESC LIMIT 25", dd))
  ets <- q("SELECT entity_id, entity_type, sp_party, year,
    ROUND(SUM(mean_sentiment*n)/SUM(n),4) mean, SUM(n) n FROM entity_stance
    WHERE sp_party IN ('D','R') GROUP BY 1,2,3,4 HAVING SUM(n) >= 20")
  asum <- q(sprintf("SELECT party, ROUND(AVG(out_attack),4) mean_attack FROM (%s) GROUP BY 1", dd))
  attack <- list(by_party_year = abyp, brokers = abrokers, entity_ts = ets, summary = asum,
                 entities = sort(unique(ets$entity_id)))
}

# ---- readability / sentence complexity (if present) ----
readability <- NULL
if (has_table("readability")) {
  rd_party <- q("SELECT party, year, ROUND(AVG(fk_grade),2) fk_grade, ROUND(AVG(flesch),1) flesch,
      ROUND(AVG(fog),2) fog, ROUND(AVG(sent_len),1) sent_len, ROUND(AVG(syll),3) syll, COUNT(*) n
    FROM readability WHERE party IN ('D','R') AND year BETWEEN 2005 AND 2026 GROUP BY 1,2 ORDER BY 1,2")
  rd_issue <- q(sprintf("SELECT trim(iss) issue, ROUND(AVG(rd.fk_grade),2) fk_grade,
      ROUND(AVG(rd.flesch),1) flesch, ROUND(AVG(rd.fog),2) fog, ROUND(AVG(rd.sent_len),1) sent_len,
      ROUND(AVG(rd.syll),3) syll, COUNT(*) n
    FROM readability rd JOIN issue_labels il USING(url), %s AS u(iss)
    WHERE trim(iss) <> '' GROUP BY 1 HAVING COUNT(*) >= 200",
    splitfn("COALESCE(il.office_issues, il.predicted_issues)")))
  rd_source <- q("SELECT source, ROUND(AVG(fk_grade),2) fk_grade, ROUND(AVG(flesch),1) flesch,
      ROUND(AVG(sent_len),1) sent_len, COUNT(*) n FROM readability GROUP BY 1 ORDER BY 5 DESC")
  readability <- list(by_party_year = rd_party, by_issue = rd_issue, by_source = rd_source)
}

# ---- POWER & STATUS: negativity flips + issue ownership ----------------------
# Two findings sharing one thesis: things that look like fixed party traits
# (meanness, issue ownership) are really status/context variables that track who
# holds power. Built from attack_scores + entity_stance (Finding A) and
# issue_labels (Finding B).
power <- NULL
if (has_table("attack_scores") && has_table("issue_labels")) {

  ## FINDING A -- "Negativity is opposition behavior; it flips with the WH" ------
  # A release "names + directs sentiment at an out-party figure" exactly when it
  # has an out_attack score (== n_out > 0). Share = such releases / all party-coded
  # releases with attack scores, by party x year. The D and R lines cross at each
  # presidential transition.
  attack_outparty_year <- q("
    SELECT r.year, r.party,
           ROUND(AVG((a.out_attack IS NOT NULL)::INT), 4) share_outparty,
           SUM((a.out_attack IS NOT NULL)::INT) n_outparty,
           COUNT(*) n_scored
    FROM attack_scores a JOIN releases r USING(url)
    WHERE r.party IN ('D','R') AND r.year BETWEEN 2010 AND 2025
    GROUP BY 1, 2 ORDER BY 1, 2")

  # Controlled stat: out-party-targeting releases are markedly more net-negative
  # (document sentiment < 0) than in-party-targeting releases, while the RAW
  # D-vs-R net-negativity gap is tiny -> opposition STATUS, not party, drives it.
  attack_controlled <- if (has_table("sentiment")) q("
    SELECT
      ROUND(AVG(CASE WHEN a.out_attack IS NOT NULL THEN (s.sentiment < 0)::INT END), 4) outparty_netneg,
      ROUND(AVG(CASE WHEN a.in_support IS NOT NULL AND a.out_attack IS NULL
                     THEN (s.sentiment < 0)::INT END), 4) inparty_netneg,
      ROUND(AVG(CASE WHEN r.party = 'D' AND a.out_attack IS NOT NULL THEN (a.out_attack < 0)::INT END), 4) d_netneg_out,
      ROUND(AVG(CASE WHEN r.party = 'R' AND a.out_attack IS NOT NULL THEN (a.out_attack < 0)::INT END), 4) r_netneg_out
    FROM attack_scores a JOIN releases r USING(url) JOIN sentiment s USING(url)
    WHERE r.party IN ('D','R')") else NULL

  # Polarized perception: per-entity mean directed sentiment by SPEAKER party.
  # The same figure is praised by co-partisans and attacked by rivals.
  entity_stance_party <- q("
    SELECT entity_id, entity_type, sp_party,
           ROUND(SUM(mean_sentiment * n) / SUM(n), 4) mean_sent, SUM(n) n
    FROM entity_stance WHERE sp_party IN ('D','R')
    GROUP BY 1, 2, 3 HAVING SUM(n) >= 200 ORDER BY entity_id, sp_party")

  # Most-attacked entities overall (lowest mean directed sentiment).
  entity_most_attacked <- q("
    SELECT entity_id, entity_type,
           ROUND(SUM(mean_sentiment * n) / SUM(n), 4) mean_sent, SUM(n) n
    FROM entity_stance GROUP BY 1, 2 HAVING SUM(n) >= 500
    ORDER BY mean_sent ASC LIMIT 15")

  ## FINDING B -- "Who owns each issue" + ownership flips with power ------------
  # Attention rate = share of a party's labeled releases that touch an issue
  # (de-multilabel via unnest). Ownership = log2(R rate / D rate), normalized for
  # each party's total output. Ranking is robust to label_source (MNAR).
  lab_expr <- "COALESCE(il.office_issues, il.predicted_issues)"
  own_long <- q(sprintf("
    SELECT r.party, trim(iss) issue
    FROM releases r JOIN issue_labels il USING(url), %s AS u(iss)
    WHERE r.usable AND r.party IN ('D','R') AND %s IS NOT NULL AND trim(iss) <> ''",
    splitfn(lab_expr), lab_expr))
  own_denom <- q(sprintf("
    SELECT r.party, COUNT(*) n_party FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND %s IS NOT NULL GROUP BY 1", lab_expr))
  issue_ownership <- own_long |> count(party, issue, name = "n_issue") |>
    left_join(own_denom, by = "party") |> mutate(rate = n_issue / n_party) |>
    select(party, issue, rate, n_issue) |>
    tidyr::pivot_wider(names_from = party, values_from = c(rate, n_issue)) |>
    filter(!is.na(rate_D), !is.na(rate_R)) |>
    mutate(log2_RD = log2(rate_R / rate_D), n_total = n_issue_D + n_issue_R) |>
    filter(n_total >= 300) |>
    transmute(issue, log2_RD = round(log2_RD, 3),
              ratio = round(rate_R / rate_D, 2), n_total) |>
    arrange(log2_RD)

  # MNAR robustness check: ownership on office-tagged labels only. The RANKING is
  # what's reported; office-only is a sanity comparison (Spearman shown in UI).
  oo_long <- q("
    SELECT r.party, trim(iss) issue
    FROM releases r JOIN issue_labels il USING(url),
         unnest(string_split(il.office_issues, ';')) AS u(iss)
    WHERE r.usable AND r.party IN ('D','R') AND il.office_issues IS NOT NULL AND trim(iss) <> ''")
  oo_denom <- q("
    SELECT r.party, COUNT(*) n_party FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND il.office_issues IS NOT NULL GROUP BY 1")
  issue_ownership_office <- oo_long |> count(party, issue, name = "n_issue") |>
    left_join(oo_denom, by = "party") |> mutate(rate = n_issue / n_party) |>
    select(party, issue, rate, n_issue) |>
    tidyr::pivot_wider(names_from = party, values_from = c(rate, n_issue)) |>
    filter(!is.na(rate_D), !is.na(rate_R), n_issue_D + n_issue_R >= 300) |>
    transmute(issue, log2_RD_office = round(log2(rate_R / rate_D), 3))
  own_corr <- issue_ownership |> inner_join(issue_ownership_office, by = "issue")
  ownership_spearman <- round(cor(own_corr$log2_RD, own_corr$log2_RD_office,
                                  method = "spearman"), 3)

  # Dominant-issue (top_issue) attention over time -- one issue per release, so
  # shares are directly comparable. Economy & Jobs falls; Health Care spikes 2020.
  top_den <- q("
    SELECT r.year, COUNT(*) n FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND r.year BETWEEN 2010 AND 2025
      AND il.top_issue IS NOT NULL GROUP BY 1")
  issue_attention_year <- q("
    SELECT r.year, il.top_issue issue, COUNT(*) n
    FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND r.year BETWEEN 2010 AND 2025
      AND il.top_issue IS NOT NULL GROUP BY 1, 2") |>
    left_join(top_den, by = "year", suffix = c("", "_den")) |>
    mutate(share = round(n / n_den, 4)) |>
    transmute(year, issue, share, n)

  # Ownership flips with power: R-share of an issue's combined attention by year
  # (>0.5 = R-owned that year). Immigration flips D-leaning -> R-owned.
  oy_long <- q(sprintf("
    SELECT r.year, r.party, trim(iss) issue
    FROM releases r JOIN issue_labels il USING(url), %s AS u(iss)
    WHERE r.usable AND r.party IN ('D','R') AND r.year BETWEEN 2010 AND 2025
      AND %s IS NOT NULL AND trim(iss) <> ''", splitfn(lab_expr), lab_expr))
  oy_denom <- q(sprintf("
    SELECT r.year, r.party, COUNT(*) n_party FROM releases r JOIN issue_labels il USING(url)
    WHERE r.usable AND r.party IN ('D','R') AND r.year BETWEEN 2010 AND 2025
      AND %s IS NOT NULL GROUP BY 1, 2", lab_expr))
  issue_ownership_year <- oy_long |> count(year, party, issue, name = "n_issue") |>
    left_join(oy_denom, by = c("year", "party")) |> mutate(rate = n_issue / n_party) |>
    select(year, party, issue, rate, n_issue) |>
    tidyr::pivot_wider(names_from = party, values_from = c(rate, n_issue)) |>
    filter(!is.na(rate_D), !is.na(rate_R), n_issue_D + n_issue_R >= 200) |>
    transmute(year, issue,
              r_share = round(rate_R / (rate_R + rate_D), 4),
              log2_RD = round(log2(rate_R / rate_D), 4),
              n = n_issue_D + n_issue_R)

  power <- list(
    attack_outparty_year = attack_outparty_year,
    attack_controlled    = attack_controlled,
    entity_stance_party  = entity_stance_party,
    entity_most_attacked = entity_most_attacked,
    issue_ownership      = issue_ownership,
    ownership_spearman   = ownership_spearman,
    issue_attention_year = issue_attention_year,
    issue_ownership_year = issue_ownership_year,
    flip_issues = sort(unique(issue_ownership_year$issue)),
    attn_issues = issue_attention_year |> group_by(issue) |>
      summarise(tot = sum(n), .groups = "drop") |> arrange(desc(tot)) |> pull(issue))
}

dbDisconnect(con, shutdown = TRUE)

dash <- list(
  overview = c(as.list(ov), as.list(fam_ov), as.list(reuse), as.list(lab)),
  sources = sources,
  issue_trends_all = issue_trends_all, issue_trends_office = issue_trends_office,
  issues = issues, topic_dict = topic_dict, topic_trends = topic_trends,
  fam_top = fam_top, members = members, years = years,
  sentiment_trends = sentiment_trends, sentiment_by_issue = sentiment_by_issue,
  sentiment_sources = sentiment_sources, sentiment_divergence = sentiment_divergence,
  partisan_terms = partisan_terms, partisan_scopes = partisan_scopes,
  network = network, attack = attack, readability = readability,
  power = power,
  built_at = as.character(Sys.time())
)
dir.create(file.path(NLP, "dashboard"), showWarnings = FALSE)
saveRDS(dash, file.path(NLP, "dashboard", "dashboard.rds"))
cat("dashboard.rds written:\n")
cat(sprintf("  overview: %s releases, %s usable, %s..%s\n",
  format(ov$n_total,big.mark=","), format(ov$n_usable,big.mark=","), ov$dmin, ov$dmax))
cat(sprintf("  issue_trends rows: %s (all) / %s (office) | issues: %d\n",
  nrow(issue_trends_all), nrow(issue_trends_office), length(issues)))
cat(sprintf("  topics: %d | top families: %s | members: %s\n",
  nrow(topic_dict), nrow(fam_top), nrow(members)))
cat(sprintf("  sources: %s | sentiment: %s\n",
  paste(sources$source, collapse=", "),
  if (is.null(sentiment_trends)) "ABSENT (run sentiment + persist first)" else
    sprintf("%d trend rows, %d issue rows", nrow(sentiment_trends), nrow(sentiment_by_issue))))
cat(sprintf("  partisan: %s\n",
  if (is.null(partisan_terms)) "ABSENT (run nlp/run_partisan.R first)" else
    sprintf("%d term rows across %d scopes", nrow(partisan_terms), nrow(partisan_scopes))))
cat(sprintf("  network: %s\n",
  if (is.null(network)) "ABSENT (run nlp/run_network.R first)" else
    sprintf("%d members, %d edges, %d communities", network$summary$n_nodes,
            network$summary$n_edges, network$summary$n_communities)))
cat(sprintf("  attack: %s\n",
  if (is.null(attack)) "ABSENT (run nlp/run_attack.R first)" else
    sprintf("%d party-year rows, %d entities, %d brokers", nrow(attack$by_party_year),
            length(attack$entities), nrow(attack$brokers))))
cat(sprintf("  readability: %s\n",
  if (is.null(readability)) "ABSENT (run nlp/run_readability.R + persist first)" else
    sprintf("%d party-year rows, %d issues", nrow(readability$by_party_year), nrow(readability$by_issue))))
cat(sprintf("  power: %s\n",
  if (is.null(power)) "ABSENT (needs attack_scores + issue_labels)" else
    sprintf("%d attack party-year rows, %d ranked issues (office-only Spearman %.3f), %d attention-trend rows",
            nrow(power$attack_outparty_year), nrow(power$issue_ownership),
            power$ownership_spearman, nrow(power$issue_attention_year))))
